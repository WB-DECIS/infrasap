library(purrr)
library(rlang)
library(dplyr)
library(stringr)

# # Scope for now
y <- "2020"
range <- c((as.numeric(y) - 1):(as.numeric(2015)))
range <- as.character(range)

col_sym_conv <- function(x){
  rlang::sym(x)
}

# available_years <- c()
# 
# df_based <- infrasap::dat %>%
#   dplyr::filter(`Country Name` == "Japan") %>%
#   dplyr::filter(`Indicator Sector` %in% c("Energy", "National")) %>%
#   dplyr::filter(`Indicator Pillar` == "Connectivity") %>%
#   dplyr::select(`Country Name`,`Indicator Sector`,
#                 `Indicator Sub-Pillar` ,`Indicator Name`,
#                 `Indicator Topic`, `Type of Benchmark`, y, `Region`)
# 
# df_based <- df_based %>%
#   dplyr::mutate(
#     year_pop = dplyr::if_else(!is.na(!!col_sym_conv(y)), as.numeric(y), !!col_sym_conv(y))
#   )
# 
# fill_missing_values_in_years(df_based, "2020", "2019", "Japan", c("Energy", "National"), "Connectivity")

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

  # %>%
  # select(-year_step_back)

  return(df)

}



# End scope for now
# ### ========

country_to_compare <- function(countryName, sc, pi, available_years_in_use, df_years_col){

  df_cn <- infrasap::dat %>%
    dplyr::filter(`Country Name` == countryName) %>%
    dplyr::filter(`Indicator Sector` %in% sc) %>%
    dplyr::filter(`Indicator Pillar` == pi) %>%
    dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
    dplyr::left_join(df_years_col, by=c("Indicator Name"))

  temp <- purrr::map(1:length(available_years_in_use), function(b){
    df_cn <<- df_cn %>%
      dplyr::mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}")), year_pop)
      )
  })[length(available_years_in_use)]

  df_cn <- temp %>% data.frame()

# print(df_cn)

  df_cn <- df_cn %>% dplyr::select(-dplyr::contains(available_years_in_use)) %>%
    dplyr::rename(
      `Country Name` = Country.Name,
      `Indicator Name` = Indicator.Name
    )  %>%
    tidyr::pivot_wider(
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
  temp <- temp %>% dplyr::select(-`Country Name`, -bm)
  
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

