library(reticulate)
library(assertthat)
library(dplyr)

source_python("inst/app/www/token.py")

#### Total files needed ####
# master_vMar8.xlsx/Energy finance - DR0053166
# Transport_all_data_Oct27.xlsx - DR0053171
# SPC_final_vFeb25.xlsx - DR0086190
# IRF_data_Nov22.xlsx
# Benchmarks_Transport_Oct27.xlsx - DR0047111
# Benchmark_aggregates_infrasap_vMar8.xlsx - DR0047112
# Benchmarks_IRF_Nov22.xlsx
# Benchmark_spc_Feb25.xlsx - DR0086191
# All_SCD_indicators_ROH_v3.xlsx
# Ports_Data.xlsx - DR0065512
# benchmark values for country-indicator pair - DR0091284

#### Custom functions ####
replace_null_with_NA <- function(x) {
  if(inherits(x, "list")) {
    x[lengths(x) == 0] <- NA
    x <- sapply(x, `[`, 1)
  } 
  return(x)
}
#### master_vMar8.xlsx ####
dat = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter"))
# Filter only from FRANCE, applying filter
#dat1 = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ("[Country Code] = 'FRA'")))
# Filter only from FRANCE, applying filter and select only relevant columns
#dat2 = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ("[Country Code] = 'FRA'")), 
#       "Country,[Country Code],[Indicator Name],[2000],[2005],[2010],[2015]")

# Check if no NA values in Country Code
assert_that(sum(is.na(dat$`Country Code`)) == 0, 
  msg = "There are NA values in Country Code column. This can be fixed with using `tidyr::fill` but better to report this data discrepancy issue")

# Change in data generation script
# Rename 2nd column to country name
dat <- dat %>% rename("Country Name" = Country)
# Replace NULL with NA and change list to numeric vector
dat <- dat %>%  
  mutate(across(c(`Indicator Pillar`, `Indicator Sub-Pillar`, `1990`:`2022`), replace_null_with_NA), 
         across(`1990`:`2022`, as.numeric))


dat_transport = get_df("DR0053171", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
# Change in data generation script
# Rename 2nd column to country name
dat_transport <- dat_transport %>% rename("Country Name" = Country)

dat_spc = get_df("DR0086190", "data", list("top"  = 50, "skip" = 0, "filter" = ""))

# Change in data generation script
# Rename 4th column to country name
dat_spc <- dat_spc %>% 
  rename_with(~"Country Name", 4) %>%
  mutate(`Indicator Sector` = 'Cross-cutting')

data4 = get_df("DR0086190", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data5 = get_df("DR0047111", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data6 = get_df("DR0047112", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data7 = get_df("DR0086191", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data8 = get_df("DR0065512", "data", list("top"  = 50, "skip" = 0, "filter" = ""))