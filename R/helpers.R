empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers                                                                                                                                                                        ") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
  return(p)
} 

has_port <- function(x) {
  any(grepl('Port', x))
}

open_bracket <- function(x) {
  grepl('(', x, fixed = TRUE)
}

benchmark_dropdown_manipulation <- function(dat, dat_bm, cn) {
  # get sector names for this country
  temp <- dat %>% 
    dplyr::filter(`Country Name` == cn) %>% 
    dplyr::mutate(group=1) %>%
    dplyr::select(Region, `OECD Member`, IncomeGroup, Isolated, Mountainous, `Low Population Density`, `Oil Exporter`, `Human Capital`, `Fragile`) %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "key", values_to = "value", values_drop_na = TRUE) %>%
    dplyr::inner_join(dat_bm, by=c('value'='Grouping')) %>%
    dplyr::count(key, value, name = "counts")
  #Seems redundant and doing the same thing as last two lines above. 
  #temp <- dplyr::inner_join(temp,dat_bm, by=c('value'='Grouping'))
  #temp <- temp %>% dplyr::count(key, value, name = "counts") 
  # get a list of benchmarks for the country selected
  sort(temp$key[temp$key %in% c("IncomeGroup", "Region")])
}

country_to_compare_vec <- function(dat, cn, sc, pl) {
  dat %>%
    dplyr::filter(Region %in% unique(Region[`Country Name` == cn]),`Indicator Sector` %in% sc, 
                  `Indicator Pillar` == pl, `Country Name` != cn) %>%
    dplyr::distinct(`Country Name`) %>% 
    dplyr::slice(1:3) %>% 
    dplyr::pull()
}


get_years <- function(dat, dat_bm) {
  # get intersection of years to populate year input
  dat %>%
    dplyr::select(-Mode) %>%
    names() %>%
    intersect(names(dat_bm))
}

country_to_compare_list <- function(dat, sc, pl) {
  dat %>%
    dplyr::filter(`Indicator Sector` %in% sc, `Indicator Pillar` == pl) %>%
    dplyr::distinct(`Country Name`) %>% 
    dplyr::pull()
}

data_for_df_r <- function(dat, cn, sc, pl, yr) {
  dat %>%
    dplyr::filter(`Country Name` == cn, `Indicator Sector` %in% sc, `Indicator Pillar` == pl) %>%
    dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr, `Region`) %>%
    dplyr::mutate(year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr)))
}

year_max_column <- function(dat, year_vec) {
  cols <- names(dat)
  for (i in seq_along(year_vec)) {
    cn <- sum(grepl(year_vec[i], cols))
    if(cn > 0) {
      if(cn == 1) {
        yr_max_column <- year_vec[i]
      } else {
        yr_max_column <- as.character(stringr::str_glue("{year_vec[i]}.y"))
      }
      break
    }
  }
  return(yr_max_column)
}


case_when_for_value_setting <- function(dat, cn, col, new_col) {
  dat %>%
    dplyr::mutate({{new_col}} := dplyr::case_when(
      .data[[cn]] >= {{col}} & `Type of Benchmark` == "Upper" ~ "3",
      .data[[cn]] >= ({{col}} * 0.9) & .data[[cn]] < {{col}} & `Type of Benchmark` == "Upper" ~ "2",
      .data[[cn]] < ({{col}} * 0.9) & `Type of Benchmark` == "Upper" ~ "1",
      .data[[cn]] <= {{col}} & `Type of Benchmark` == "Lower" ~ "3",
      .data[[cn]] <= ({{col}} * 1.1) & .data[[cn]] > {{col}} & `Type of Benchmark` == "Lower" ~ "2",
      .data[[cn]] > ({{col}} * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
      TRUE ~ "0"), 
      # Fill NAs with grey color
      {{new_col}} := as.numeric(dplyr::case_when(
        is.na(.data[[cn]]) ~ "0",
        TRUE ~ {{new_col}}
      )))
}


case_when_for_value_setting_chr <- function(dat, cn, col, new_col) {
  dat %>%
    dplyr::mutate({{new_col}} := dplyr::case_when(
      .data[[cn]] >= .data[[col]] & `Type of Benchmark` == "Upper" ~ "3",
      .data[[cn]] >= (.data[[col]] * 0.9) & .data[[cn]] < .data[[col]] & `Type of Benchmark` == "Upper" ~ "2",
      .data[[cn]] < (.data[[col]] * 0.9) & `Type of Benchmark` == "Upper" ~ "1",
      .data[[cn]] <= .data[[col]] & `Type of Benchmark` == "Lower" ~ "3",
      .data[[cn]] <= (.data[[col]] * 1.1) & .data[[cn]] > .data[[col]] & `Type of Benchmark` == "Lower" ~ "2",
      .data[[cn]] > (.data[[col]] * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
      TRUE ~ "0"), 
      # Fill NAs with grey color
      {{new_col}} := as.numeric(dplyr::case_when(
        is.na(.data[[cn]]) ~ "0",
        TRUE ~ {{new_col}}
      )))
}
