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


