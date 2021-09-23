library(purrr)
library(rlang)
library(dplyr)
library(stringr)

# Helper: convert string to symbol (to asign variable name via parameter)
col_sym_conv <- function(x) {
  rlang::sym(x)
}

fill_missing_values_in_years <- function(df, based_year, year_step_back, country, sector, pillar){

df <- df %>%
  # Bind cols
  dplyr::bind_cols(
                  infrasap::dat %>%
                    dplyr::filter(`Country Name` == country) %>%
                    dplyr::filter(`Indicator Sector` %in% sector) %>%
                    dplyr::filter(`Indicator Pillar` == pillar) %>%
                    dplyr::select(year_step_back)
  ) %>%
  dplyr::mutate(
    year_pop = dplyr::if_else(is.na(!!col_sym_conv(based_year)) & !is.na(!!col_sym_conv(year_step_back)), as.numeric(year_step_back), year_pop),
    !!based_year := dplyr::if_else(is.na(!!col_sym_conv(based_year)), !!col_sym_conv(year_step_back), !!col_sym_conv(based_year))
  ) 

  return(df)

}

country_to_compare <- function(countryName, sc, pi, available_years_in_use, df_years_col){

  df_cn <- infrasap::dat %>%
    dplyr::filter(`Country Name` == countryName) %>%
    dplyr::filter(`Indicator Sector` %in% sc) %>%
    dplyr::filter(`Indicator Pillar` == pi) %>%
    dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
    left_join(df_years_col, by=c("Indicator Name"))

  temp <- map(1:length(available_years_in_use), function(b){
    df_cn <<- df_cn %>%
      mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(str_glue("{available_years_in_use[b]}")), year_pop)
      )
  })[length(available_years_in_use)]

  df_cn <- temp %>% data.frame()

  df_cn <- df_cn %>% dplyr::select(-contains(available_years_in_use)) %>%
    rename(
      `Country Name` = Country.Name,
      `Indicator Name` = Indicator.Name
    )  %>%
    pivot_wider(
      names_from = `Country Name`,
      values_from = year_pop
    )

  return(df_cn)

}



# get years for data
get_last_year <- function(cn, sc, bm){
  temp <- infrasap::dat %>% 
    dplyr::filter(`Country Name` == cn) %>% 
    dplyr::filter(`Indicator Sector` %in% sc) %>%
    dplyr::select(`Country Name`, `1990`:`2017-2021`, bm ) 
  
  # get type of benchmark to subset benchmark data by
  bm_type <- unique(temp[,bm])
  
  # remove columns that have all NA
  temp <- temp[,colSums(is.na(temp))<nrow(temp)]
  temp <- temp %>% select(-`Country Name`, -bm)
  
  # get years for benchmark 
  temp_bm <- infrasap::dat_bm %>%
    dplyr::filter(Grouping == bm_type) %>% 
    dplyr::filter(`Sector` %in% sc)
  temp_bm <- temp_bm[,colSums(is.na(temp_bm))<nrow(temp_bm)]
  
  # get intersection of years to populate year input
  year_choices <- intersect(names(temp), names(temp_bm))
  year_choices <- year_choices[length(year_choices)]
  
  return(year_choices)
  
}

### =============
### =============

#scd function color code

scd_color_encode <- function(df, value, cn, bm) {

df <- df %>%
  mutate(!!col_sym_conv(value) := dplyr::case_when(
    # TRUE ~ "0",
    (df %>% select(contains(cn))) >= (df %>% select(contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
    ((df %>% select(contains(cn))) >= ((df %>% select(contains(bm))) * 0.9) & (df %>% select(contains(cn))) < (df %>% select(contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
    (df %>% select(contains(cn))) < ((df %>% select(contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
    
    (df %>% select(contains(cn))) <= (df %>% select(contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
    ((df %>% select(contains(cn))) <= ((df %>% select(contains(bm))) * 1.1) & (df %>% select(contains(cn))) > (df %>% select(contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
    (df %>% select(contains(cn))) > ((df %>% select(contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
    TRUE ~ "0"
  )
  ) %>% 
  # Fill NAs with grey color
  mutate(!!col_sym_conv(value) := dplyr::case_when(
    is.na(df %>% select(contains(cn))) ~ "0",
    TRUE ~ !!col_sym_conv(value)
  )) %>%
  mutate(!!col_sym_conv(value) := !!col_sym_conv(value) %>% as.numeric())

  return(df)

}


# get years for data
get_year_scd <- function(cn, bm, year_position = NULL){

  # get data based on inputs
  temp <- infrasap::scd_dat %>% 
    filter(`Country Name`%in% cn) %>%
    # filter(`Indicator Sector` %in% sc) %>%
    select(`1990`:`2017-2021`)

  # remove columns that have all NA
  temp <- temp[,colSums(is.na(temp))<nrow(temp)]
  
  # benchmark data 
  temp_bm <- infrasap::scd_bm %>% 
    filter(Grouping %in% bm) 
  # %>%
  #   filter(Sector %in% "Energy") 
  
  # remove columns that have all NA
  temp_bm <- temp_bm[,colSums(is.na(temp_bm))<nrow(temp_bm)]
  
  # get intersection of years to populate year input
  year_choices <- intersect(names(temp), names(temp_bm))
  
  if(!is.null(year_position)) {
    year_choices <- year_choices[length(year_choices)]
  } else {
    year_choices <- as.character(min(as.numeric(year_choices)))
  }
  
  if(is.null(bm)){
    without_bm <- infrasap::scd_dat %>% 
      filter(`Country Name`%in% cn) %>%
      # filter(`Indicator Sector` %in% sc) %>%
      select(`1990`:`2017-2021`)
    without_bm <- without_bm[,colSums(is.na(without_bm))<nrow(without_bm)]
    
    if(!is.null(year_position)) {
      year_choices <- as.character(without_bm %>% colnames() %>% as.numeric() %>% max(., na.rm = TRUE))
    } else {
      year_choices <- as.character(without_bm %>% colnames() %>% as.numeric() %>% min(., na.rm = TRUE))
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
        dplyr::filter(`Country Name`%in% country) %>% 
        # dplyr::filter(`Indicator Sector` %in% sector) %>%
        dplyr::select(year_step_back)
    ) %>%
    dplyr::mutate(
      year_pop = dplyr::if_else(is.na(!!col_sym_conv(based_year)) & !is.na(!!col_sym_conv(year_step_back)), as.numeric(year_step_back), year_pop),
      !!based_year := dplyr::if_else(is.na(!!col_sym_conv(based_year)), !!col_sym_conv(year_step_back), !!col_sym_conv(based_year))
    )
  
  
  return(df)
  
}


country_to_compare_scd <- function(countryName, available_years_in_use, df_years_col){
  
  df_cn <- infrasap::scd_dat %>%
    dplyr::filter(`Country Name` == countryName) %>%
    # dplyr::filter(`Indicator Sector` %in% sc) %>%
    dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
    left_join(df_years_col, by=c("Indicator Name"))
  
  
  temp <- map(1:length(available_years_in_use), function(b){
    df_cn <<- df_cn %>%
      mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(str_glue("{available_years_in_use[b]}")), year_pop)
      )
  })[length(available_years_in_use)]
  
  df_cn <- temp %>% data.frame()
  
  df_cn <- df_cn %>% dplyr::select(-contains(available_years_in_use)) %>%
    rename(
      `Country Name` = Country.Name,
      `Indicator Name` = Indicator.Name
    )  %>%
    pivot_wider(
      names_from = `Country Name`,
      values_from = year_pop
    )
  
  return(df_cn)
  
}
