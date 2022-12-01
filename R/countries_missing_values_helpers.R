library(purrr)
library(rlang)
library(dplyr)
library(stringr)

# Helper: convert string to symbol (to assign variable name via parameter)
col_sym_conv <- function(x) {
  rlang::sym(x)
}

remove_NA_columns <- function(dat) {
  # remove columns that have all NA
  dat[,colSums(is.na(dat))<nrow(dat)]
}

fill_missing_values_in_years <- function(df, based_year, year_step_back, country, sector, pillar){

infrasap_dat_mod_modified <- infrasap::dat
infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"
  
  
df <- df %>%
  # Bind cols
  dplyr::bind_cols(
                  infrasap_dat_mod_modified %>%
                    dplyr::filter(.data$`Country Name` == country) %>%
                    dplyr::filter(.data$`Indicator Sector` %in% sector) %>%
                    dplyr::filter(.data$`Indicator Pillar` == pillar) %>%
                    dplyr::select(year_step_back)
  ) %>%
  dplyr::mutate(
    year_pop = dplyr::if_else(is.na(!!col_sym_conv(based_year)) & !is.na(!!col_sym_conv(year_step_back)), as.numeric(year_step_back), .data$year_pop),
    !!based_year := dplyr::if_else(is.na(!!col_sym_conv(based_year)), !!col_sym_conv(year_step_back), !!col_sym_conv(based_year))
  ) 

  return(df)

}

country_to_compare <- function(countryName, sc, pi, available_years_in_use, df_years_col){
  
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"

  df_cn <- infrasap_dat_mod_modified %>%
    dplyr::filter(.data$`Country Name` == countryName) %>%
    dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
    dplyr::filter(.data$`Indicator Pillar` == pi) %>%
    dplyr::select(.data$`Country Name`, .data$`Indicator Name`, available_years_in_use) %>%
    dplyr::left_join(df_years_col, by=c("Indicator Name"))

  temp <- purrr::map(1:length(available_years_in_use), function(b){
    df_cn <<- df_cn %>%
      dplyr::mutate(year_pop = dplyr::if_else(.data$year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}")), .data$year_pop)
      )
  })[length(available_years_in_use)]

  df_cn <- temp %>% data.frame()

  df_cn <- df_cn %>% dplyr::select(-dplyr::contains(available_years_in_use)) %>%
    dplyr::rename(
      `Country Name` = .data$Country.Name,
      `Indicator Name` = .data$Indicator.Name
    )  %>%
    tidyr::pivot_wider(
      names_from = .data$`Country Name`,
      values_from = .data$year_pop
    )

  return(df_cn)

}



# get years for data
get_last_year <- function(cn, sc, bm = NULL) {
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"
  
  if(is.null(bm)) {
    temp <- infrasap_dat_mod_modified %>% 
      dplyr::filter(.data$`Country Name` == cn) %>% 
      dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
      dplyr::select(.data$`Country Name`, .data$`1990`:.data$`2020`) %>%
      remove_NA_columns() %>%
      dplyr::select(-.data$`Country Name`)
    
    # get years for benchmark 
    temp_bm <- infrsap_dat_bm_mod_modfied %>%
      # dplyr::filter(Grouping == bm_type) %>% 
      dplyr::filter(.data$`Sector` %in% sc) %>%
      remove_NA_columns()
    
    # get intersection of years to populate year input
    year_choices <- intersect(names(temp), names(temp_bm))
    year_choices <- year_choices[length(year_choices)]
  } else {
    temp <- infrasap_dat_mod_modified %>% 
      dplyr::filter(.data$`Country Name` == cn) %>% 
      dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
      dplyr::select(.data$`Country Name`, .data$`1990`:.data$`2020`, bm)
    
    # get type of benchmark to subset benchmark data by
    bm_type <- unlist(unique(temp[,bm]))
    
    temp <- remove_NA_columns(temp)
    temp <- temp %>% dplyr::select(-.data$`Country Name`, -bm)
    
    # get years for benchmark 
    temp_bm <- infrsap_dat_bm_mod_modfied %>%
      dplyr::filter(.data$Grouping %in% bm_type) %>% 
      dplyr::filter(.data$`Sector` %in% sc) %>%
      remove_NA_columns()
    # get intersection of years to populate year input
    year_choices <- dplyr::intersect(names(temp), names(temp_bm))
    year_choices <- year_choices[length(year_choices)]
  }
  return(year_choices)
}


join_df_with_ordered_layout <- function(df_main, df_layout) {
  df <- df_layout %>% dplyr::left_join(df_main, 
                                by = c('Indicator Sub-Pillar' = 'Sub-Pillar',
                                       'Indicator Topic' = 'Topic',
                                       'Indicator Name' = 'Indicator')
                                      ) %>%
                      dplyr::rename(
                             `Sub-Pillar` = .data$`Indicator Sub-Pillar`,
                             `Topic` = .data$`Indicator Topic`,
                             `Indicator` = .data$`Indicator Name`
                            )
  
  return(df)
}


### =============
### =============

#scd function color code

scd_color_encode <- function(df, value, cn, bm) {

df <- df %>%
  dplyr::mutate(!!col_sym_conv(value) := dplyr::case_when(
    # TRUE ~ "0",
    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
    
    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
    TRUE ~ "0"
  )
  ) %>% 
  # Fill NAs with grey color
  dplyr::mutate(!!col_sym_conv(value) := dplyr::case_when(
    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
    TRUE ~ !!col_sym_conv(value)
  )) %>%
  dplyr::mutate(!!col_sym_conv(value) := !!col_sym_conv(value) %>% as.numeric())

  return(df)

}


# get years for data
get_year_scd <- function(cn, bm, year_position = NULL){

  # get data based on inputs
  temp <- infrasap::scd_dat %>%
    dplyr::filter(.data$`Country Name`%in% cn) %>%
    # filter(`Indicator Sector` %in% sc) %>%
    dplyr::select(.data$`1990` : .data$`2020`) %>%
    remove_NA_columns()
  # benchmark data 
  temp_bm <- infrasap::scd_bm %>% 
    dplyr::filter(.data$Grouping %in% bm) %>%
    remove_NA_columns()
  
  # get intersection of years to populate year input
  year_choices <- dplyr::intersect(names(temp), names(temp_bm))
  
  if(!is.null(year_position)) {
    year_choices <- year_choices[length(year_choices)]
  } else {
    year_choices <- as.character(min(as.numeric(year_choices)))
  }
  
  if(is.null(bm)){
    without_bm <- infrasap::scd_dat %>% 
      dplyr::filter(.data$`Country Name`%in% cn) %>%
      dplyr::select(.data$`1990`:.data$`2020`) %>%
      remove_NA_columns()
    
    if(!is.null(year_position)) {
      year_choices <- as.character(without_bm %>% colnames() %>% as.numeric() %>% max(na.rm = TRUE))
    } else {
      year_choices <- as.character(without_bm %>% colnames() %>% as.numeric() %>% min(na.rm = TRUE))
    }
  }
  return(year_choices)
}


# scd fill missing values
fill_missing_values_in_years_scd <- function(df, based_year, year_step_back, country){
  df <- df %>%
    # Bind cols
    dplyr::bind_cols(
      infrasap::scd_dat %>% 
        dplyr::filter(.data$`Country Name`%in% country) %>% 
        # dplyr::filter(`Indicator Sector` %in% sector) %>%
        dplyr::select(year_step_back)
    ) %>%
    dplyr::mutate(
      year_pop = dplyr::if_else(is.na(!!col_sym_conv(based_year)) & !is.na(!!col_sym_conv(year_step_back)), as.numeric(year_step_back), .data$year_pop),
      !!based_year := dplyr::if_else(is.na(!!col_sym_conv(based_year)), !!col_sym_conv(year_step_back), !!col_sym_conv(based_year))
    )
  
  
  return(df)
  
}


country_to_compare_scd <- function(countryName, available_years_in_use, df_years_col){
  
  df_cn <- infrasap::scd_dat %>%
    dplyr::filter(.data$`Country Name` == countryName) %>%
    # dplyr::filter(`Indicator Sector` %in% sc) %>%
    dplyr::select(.data$`Country Name`, .data$`Indicator Name`, available_years_in_use) %>%
    dplyr::left_join(df_years_col, by=c("Indicator Name"))
  
  
  temp <- purrr::map(1:length(available_years_in_use), function(b){
    df_cn <<- df_cn %>%
      dplyr::mutate(year_pop = dplyr::if_else(.data$year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}")), .data$year_pop)
      )
  })[length(available_years_in_use)]
  
  df_cn <- temp %>% data.frame()
  
  df_cn <- df_cn %>% dplyr::select(-dplyr::contains(available_years_in_use)) %>%
    dplyr::rename(
      `Country Name` = .data$Country.Name,
      `Indicator Name` = .data$Indicator.Name
    )  %>%
    tidyr::pivot_wider(
      names_from = .data$`Country Name`,
      values_from = .data$year_pop
    )
  
  return(df_cn)
  
}

replace_null_to_na <- function(x) {
  if(is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
}

countries_to_compare_into_one_string <- function(input_countries_vector) {
  if(length(input_countries_vector) > 0) {
    input_countries_vector <- input_countries_vector %>% stringr::str_flatten(', ')
    return(as.character(stringr::str_glue('({input_countries_vector})')))
  } else {
    return("")
  }
}



add_article_to_selected_country <- function(selected_country) {
  if(selected_country == "Bahamas, The") {
      selected_country <- "the Bahamas"
  }
  
  if(selected_country == "Gambia, The") {
    selected_country <- "the Gambia"
  }
  
  if(selected_country %in% infrasap::scd_dat_countries_with_article) {
    selected_country <- as.character(stringr::str_glue('the {selected_country}'))
  }
  
  return(selected_country)
  
}