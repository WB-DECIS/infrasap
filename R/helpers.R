empty_plot <- function(title = NULL) {
  p <- plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::config(
      displayModeBar = FALSE
    ) %>%
    plotly::layout(
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
    dplyr::filter(.data$`Country Name` == cn) %>% 
    dplyr::mutate(group=1) %>%
    dplyr::select(.data$Region, .data$`OECD Member`, .data$IncomeGroup, .data$Isolated, .data$Mountainous, .data$`Low Population Density`, .data$`Oil Exporter`, .data$`Human Capital`, .data$`Fragile`) %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "key", values_to = "value", values_drop_na = TRUE) %>%
    dplyr::inner_join(dat_bm, by=c('value'='Grouping')) %>%
    dplyr::count(.data$key, .data$value, name = "counts")
  #Seems redundant and doing the same thing as last two lines above. 
  #temp <- dplyr::inner_join(temp,dat_bm, by=c('value'='Grouping'))
  #temp <- temp %>% dplyr::count(key, value, name = "counts") 
  # get a list of benchmarks for the country selected
  sort(temp$key[temp$key %in% c("IncomeGroup", "Region")])
}

country_to_compare_vec <- function(dat, cn, sc, pl) {
  dat %>%
    dplyr::filter(.data$Region %in% unique(.data$Region[.data$`Country Name` == cn]),.data$`Indicator Sector` %in% sc, 
                  .data$`Indicator Pillar` == pl, .data$`Country Name` != cn) %>%
    dplyr::distinct(.data$`Country Name`) %>% 
    dplyr::slice(1:3) %>% 
    dplyr::pull()
}


get_years <- function(dat, dat_bm) {
  # get intersection of years to populate year input
  dat %>%
    dplyr::select(-"Mode", -"Indicator Sector") %>%
    names() %>%
    intersect(names(dat_bm))
}

country_to_compare_list <- function(dat, sc, pl) {
  dat %>%
    dplyr::filter(.data$`Indicator Sector` %in% sc, .data$`Indicator Pillar` == pl) %>%
    dplyr::distinct(.data$`Country Name`) %>% 
    dplyr::pull()
}

data_for_df_r <- function(dat, cn, sc, pl, yr) {
  dat %>%
    dplyr::filter(.data$`Country Name` == cn, .data$`Indicator Sector` %in% sc, .data$`Indicator Pillar` == pl) %>%
    dplyr::select(.data$`Country Name`,.data$`Indicator Sector`,.data$`Indicator Sub-Pillar` ,.data$`Indicator Name`, .data$`Indicator Topic`, .data$`Type of Benchmark`, yr, .data$`Region`) %>%
    dplyr::mutate(year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr)))
}

year_max_column <- function(dat, year_vec) {
  cols <- names(dat)
  for (i in seq_along(year_vec)) {
    cn <- sum(grepl(year_vec[i], cols))
    if(cn > 0) {
      if(cn == 1) {
        yr_max_column <- as.character(year_vec[i])
      } else {
        yr_max_column <- as.character(stringr::str_glue("{year_vec[i]}.y"))
      }
      break
    }
  }
  return(yr_max_column)
}


#' Title
#'
#' @param dat dataframe
#' @param cn name of column as character
#' @param col column name as symbol
#' @param new_col new column name to be created passed as symbol
#' 
#' @importFrom rlang .data
#' @importFrom rlang :=
#' 
#' @return Data frame
#' 
case_when_for_value_setting <- function(dat, cn, col, new_col) {
  dat %>%
    dplyr::mutate({{new_col}} := dplyr::case_when(
      round(.data[[cn]], 2) == round({{col}}, 2) ~ "2",
      .data[[cn]] >= {{col}} & .data$`Type of Benchmark` == "Upper" ~ "3",
      .data[[cn]] >= ({{col}} * 0.9) & .data[[cn]] < {{col}} & .data$`Type of Benchmark` == "Upper" ~ "2",
      .data[[cn]] < ({{col}} * 0.9) & .data$`Type of Benchmark` == "Upper" ~ "1",
      .data[[cn]] <= {{col}} & .data$`Type of Benchmark` == "Lower" ~ "3",
      .data[[cn]] <= ({{col}} * 1.1) & .data[[cn]] > {{col}} & .data$`Type of Benchmark` == "Lower" ~ "2",
      .data[[cn]] > ({{col}} * 1.1) & (.data$`Type of Benchmark` == "Lower") ~ "1",
      TRUE ~ "0"), 
      # Fill NAs with grey color
      {{new_col}} := as.numeric(dplyr::case_when(
        is.na(.data[[cn]]) ~ "0",
        TRUE ~ {{new_col}}
      )))
}


case_when_for_value_setting_chr <- function(dat, cn, col, new_col, tab = 'infrasap') {
  if(tab == 'scd') new_col <- as.character(new_col)
  dat %>%
    dplyr::mutate({{new_col}} := dplyr::case_when(
      round(.data[[cn]], 2) == round(.data[[col]], 2) ~ 2,
      .data[[cn]] >= .data[[col]] & .data$`Type of Benchmark` == "Upper" ~ 3,
      .data[[cn]] >= (.data[[col]] * 0.9) & .data[[cn]] < .data[[col]] & .data$`Type of Benchmark` == "Upper" ~ 2,
      .data[[cn]] < (.data[[col]] * 0.9) & .data$`Type of Benchmark` == "Upper" ~ 1,
      .data[[cn]] <= .data[[col]] & .data$`Type of Benchmark` == "Lower" ~ 3,
      .data[[cn]] <= (.data[[col]] * 1.1) & .data[[cn]] > .data[[col]] & .data$`Type of Benchmark` == "Lower" ~ 2,
      .data[[cn]] > (.data[[col]] * 1.1) & (.data$`Type of Benchmark` == "Lower") ~ 1,
      TRUE ~ 0))
  #NA's are automatically assigned TRUE value i.e 0
}


select_and_round <- function(dat, country_vec, ...) {
  dat %>% 
    dplyr::select(..., dplyr::all_of(country_vec), dplyr::starts_with('value')) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(country_vec), round, 2))
}


indicator_trend_data_manipulation <- function(dat, ic, sc, yr, col = "Country Name") {
  dat %>%
    dplyr::filter(.data$`Indicator Name` %in% ic) %>%
    dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
    dplyr::select(Grouping = col,.data$`1990`:.data$`2020`) %>%
    tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
    tidyr::drop_na() %>%
    dplyr::filter(.data$key >= yr[1], .data$key<=yr[2])
}

get_colors <- function(x, k = 3) {
  RColorBrewer::brewer.pal(n = max(length(unique(x)), k), name = 'Set1')
}


indicator_trend_plot1 <- function(data, var, col_pal, y_axis, plot_title, mytext, thm) {
  ggplot2::ggplot(data, ggplot2::aes(.data$key, .data$value, fill = .data[[var]], text = mytext)) +
    ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
    ggplot2::scale_fill_manual(name = '', values = col_pal) +
    ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
    match.fun(thm)()
}

indicator_trend_plot2 <- function(data, var, col_pal, y_axis, plot_title, mytext, thm) {
  ggplot2::ggplot(data, ggplot2::aes(.data$key, .data$value, group = .data[[var]], color = .data[[var]], text = mytext)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(name = '', values = col_pal) + 
    ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
    match.fun(thm)()
}

theme1 <- function() {
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                   axis.title.y = ggplot2::element_text(size = 8))
}

theme2 <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                   axis.title.y = ggplot2::element_text(size = 10, colour = "#28313d"),
                   axis.title.x = ggplot2::element_text(size = 10, colour = "#28313d"),
                   axis.text = ggplot2::element_text(size = 11, colour = "#28313d"),
                   plot.title = ggplot2::element_text(colour = "#28313d"),
                   axis.ticks = ggplot2::element_line(colour = "#ebebeb"))
}
