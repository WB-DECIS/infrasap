library(reticulate)
library(assertthat)
library(dplyr)
library(testthat)
library(readr)
library(tidyr)
library(rgdal)
library(tibble)

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
# Energy_Finance_benchmark_v04192023.csv - DR0091284

#### Custom functions ####
replace_null_with_NA <- function(x) {
  if(inherits(x, "list")) {
    x[lengths(x) == 0] <- NA
    x <- sapply(x, `[`, 1)
  } 
  return(x)
}
#### master_vMar8.xlsx ####
dat = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
#dat <- read_csv("../../Downloads/data_from_API/dat.csv")
# Filter only from FRANCE, applying filter
#dat1 = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ("[Country Code] = 'FRA'")))
# Filter only from FRANCE, applying filter and select only relevant columns
#dat2 = get_df("DR0053166", "data", list("top"  = 50, "skip" = 0, "filter" = ("[Country Code] = 'FRA'")), 
#       "Country,[Country Code],[Indicator Name],[2000],[2005],[2010],[2015]")

# Change in data generation script
# Rename 2nd column to country name
dat <- dat %>% rename("Country Name" = Country)

# Check if no NA values in Country Code
assert_that(sum(is.na(dat$`Country Code`)) == 0, 
  msg = "There are NA values in Country Code column. This can be fixed with using `tidyr::fill` but better to report this data discrepancy issue")


expect_equal(names(dat), c("Country Code", "Country Name", "Indicator Name", "Indicator Pillar", 
                           "Indicator Sub-Pillar", "Indicator Sector", "Indicator Topic", 
                           "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                           "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                           "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                           "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", 
                           "2022"))

dat <- dat %>% distinct(`Country Code`, `Indicator Name`, `Indicator Sector`, .keep_all = TRUE) 

expect_equal(sum(duplicated(dat)), 0)

bm = get_df("DR0091284", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
#bm <- read_csv("../../Downloads/data_from_API/bm.csv")

bm <- bm %>% 
  distinct(`Country Code`, `Indicator Name`, `Indicator Sector`, .keep_all = TRUE) %>%
  mutate(across(everything(), replace_null_with_NA)) %>%
  type_convert() %>% as_tibble()

expect_equal(sum(duplicated(bm)), 0)

expect_equal(names(bm), c("Country Code", "Indicator Name", "Indicator Sector", "Region", 
                          "Type of Benchmark", "IncomeGroup", "OECD Member", "Landlocked/Small Island", 
                          "Mountainous", "Oil Exporter", "Population Density", "Human Capital", 
                          "Fragility", "Region Benchmark Achieved 1990", "Region Benchmark Achieved 1991", 
                          "Region Benchmark Achieved 1992", "Region Benchmark Achieved 1993", 
                          "Region Benchmark Achieved 1994", "Region Benchmark Achieved 1995", 
                          "Region Benchmark Achieved 1996", "Region Benchmark Achieved 1997", 
                          "Region Benchmark Achieved 1998", "Region Benchmark Achieved 1999", 
                          "Region Benchmark Achieved 2000", "Region Benchmark Achieved 2001", 
                          "Region Benchmark Achieved 2002", "Region Benchmark Achieved 2003", 
                          "Region Benchmark Achieved 2004", "Region Benchmark Achieved 2005", 
                          "Region Benchmark Achieved 2006", "Region Benchmark Achieved 2007", 
                          "Region Benchmark Achieved 2008", "Region Benchmark Achieved 2009", 
                          "Region Benchmark Achieved 2010", "Region Benchmark Achieved 2011", 
                          "Region Benchmark Achieved 2012", "Region Benchmark Achieved 2013", 
                          "Region Benchmark Achieved 2014", "Region Benchmark Achieved 2015", 
                          "Region Benchmark Achieved 2016", "Region Benchmark Achieved 2017", 
                          "Region Benchmark Achieved 2018", "Region Benchmark Achieved 2019", 
                          "Region Benchmark Achieved 2020", "Region Benchmark Achieved 2021", 
                          "Region Benchmark Achieved 2022", "IncomeGroup Benchmark Achieved 1990", 
                          "IncomeGroup Benchmark Achieved 1991", "IncomeGroup Benchmark Achieved 1992", 
                          "IncomeGroup Benchmark Achieved 1993", "IncomeGroup Benchmark Achieved 1994", 
                          "IncomeGroup Benchmark Achieved 1995", "IncomeGroup Benchmark Achieved 1996", 
                          "IncomeGroup Benchmark Achieved 1997", "IncomeGroup Benchmark Achieved 1998", 
                          "IncomeGroup Benchmark Achieved 1999", "IncomeGroup Benchmark Achieved 2000", 
                          "IncomeGroup Benchmark Achieved 2001", "IncomeGroup Benchmark Achieved 2002", 
                          "IncomeGroup Benchmark Achieved 2003", "IncomeGroup Benchmark Achieved 2004", 
                          "IncomeGroup Benchmark Achieved 2005", "IncomeGroup Benchmark Achieved 2006", 
                          "IncomeGroup Benchmark Achieved 2007", "IncomeGroup Benchmark Achieved 2008", 
                          "IncomeGroup Benchmark Achieved 2009", "IncomeGroup Benchmark Achieved 2010", 
                          "IncomeGroup Benchmark Achieved 2011", "IncomeGroup Benchmark Achieved 2012", 
                          "IncomeGroup Benchmark Achieved 2013", "IncomeGroup Benchmark Achieved 2014", 
                          "IncomeGroup Benchmark Achieved 2015", "IncomeGroup Benchmark Achieved 2016", 
                          "IncomeGroup Benchmark Achieved 2017", "IncomeGroup Benchmark Achieved 2018", 
                          "IncomeGroup Benchmark Achieved 2019", "IncomeGroup Benchmark Achieved 2020", 
                          "IncomeGroup Benchmark Achieved 2021", "IncomeGroup Benchmark Achieved 2022", 
                          "OECD Member Benchmark Achieved 1990", "OECD Member Benchmark Achieved 1991", 
                          "OECD Member Benchmark Achieved 1992", "OECD Member Benchmark Achieved 1993", 
                          "OECD Member Benchmark Achieved 1994", "OECD Member Benchmark Achieved 1995", 
                          "OECD Member Benchmark Achieved 1996", "OECD Member Benchmark Achieved 1997", 
                          "OECD Member Benchmark Achieved 1998", "OECD Member Benchmark Achieved 1999", 
                          "OECD Member Benchmark Achieved 2000", "OECD Member Benchmark Achieved 2001", 
                          "OECD Member Benchmark Achieved 2002", "OECD Member Benchmark Achieved 2003", 
                          "OECD Member Benchmark Achieved 2004", "OECD Member Benchmark Achieved 2005", 
                          "OECD Member Benchmark Achieved 2006", "OECD Member Benchmark Achieved 2007", 
                          "OECD Member Benchmark Achieved 2008", "OECD Member Benchmark Achieved 2009", 
                          "OECD Member Benchmark Achieved 2010", "OECD Member Benchmark Achieved 2011", 
                          "OECD Member Benchmark Achieved 2012", "OECD Member Benchmark Achieved 2013", 
                          "OECD Member Benchmark Achieved 2014", "OECD Member Benchmark Achieved 2015", 
                          "OECD Member Benchmark Achieved 2016", "OECD Member Benchmark Achieved 2017", 
                          "OECD Member Benchmark Achieved 2018", "OECD Member Benchmark Achieved 2019", 
                          "OECD Member Benchmark Achieved 2020", "OECD Member Benchmark Achieved 2021", 
                          "OECD Member Benchmark Achieved 2022", "Landlocked/Small Island Benchmark Achieved 1990", 
                          "Landlocked/Small Island Benchmark Achieved 1991", "Landlocked/Small Island Benchmark Achieved 1992", 
                          "Landlocked/Small Island Benchmark Achieved 1993", "Landlocked/Small Island Benchmark Achieved 1994", 
                          "Landlocked/Small Island Benchmark Achieved 1995", "Landlocked/Small Island Benchmark Achieved 1996", 
                          "Landlocked/Small Island Benchmark Achieved 1997", "Landlocked/Small Island Benchmark Achieved 1998", 
                          "Landlocked/Small Island Benchmark Achieved 1999", "Landlocked/Small Island Benchmark Achieved 2000", 
                          "Landlocked/Small Island Benchmark Achieved 2001", "Landlocked/Small Island Benchmark Achieved 2002", 
                          "Landlocked/Small Island Benchmark Achieved 2003", "Landlocked/Small Island Benchmark Achieved 2004", 
                          "Landlocked/Small Island Benchmark Achieved 2005", "Landlocked/Small Island Benchmark Achieved 2006", 
                          "Landlocked/Small Island Benchmark Achieved 2007", "Landlocked/Small Island Benchmark Achieved 2008", 
                          "Landlocked/Small Island Benchmark Achieved 2009", "Landlocked/Small Island Benchmark Achieved 2010", 
                          "Landlocked/Small Island Benchmark Achieved 2011", "Landlocked/Small Island Benchmark Achieved 2012", 
                          "Landlocked/Small Island Benchmark Achieved 2013", "Landlocked/Small Island Benchmark Achieved 2014", 
                          "Landlocked/Small Island Benchmark Achieved 2015", "Landlocked/Small Island Benchmark Achieved 2016", 
                          "Landlocked/Small Island Benchmark Achieved 2017", "Landlocked/Small Island Benchmark Achieved 2018", 
                          "Landlocked/Small Island Benchmark Achieved 2019", "Landlocked/Small Island Benchmark Achieved 2020", 
                          "Landlocked/Small Island Benchmark Achieved 2021", "Landlocked/Small Island Benchmark Achieved 2022", 
                          "Mountainous Benchmark Achieved 1990", "Mountainous Benchmark Achieved 1991", 
                          "Mountainous Benchmark Achieved 1992", "Mountainous Benchmark Achieved 1993", 
                          "Mountainous Benchmark Achieved 1994", "Mountainous Benchmark Achieved 1995", 
                          "Mountainous Benchmark Achieved 1996", "Mountainous Benchmark Achieved 1997", 
                          "Mountainous Benchmark Achieved 1998", "Mountainous Benchmark Achieved 1999", 
                          "Mountainous Benchmark Achieved 2000", "Mountainous Benchmark Achieved 2001", 
                          "Mountainous Benchmark Achieved 2002", "Mountainous Benchmark Achieved 2003", 
                          "Mountainous Benchmark Achieved 2004", "Mountainous Benchmark Achieved 2005", 
                          "Mountainous Benchmark Achieved 2006", "Mountainous Benchmark Achieved 2007", 
                          "Mountainous Benchmark Achieved 2008", "Mountainous Benchmark Achieved 2009", 
                          "Mountainous Benchmark Achieved 2010", "Mountainous Benchmark Achieved 2011", 
                          "Mountainous Benchmark Achieved 2012", "Mountainous Benchmark Achieved 2013", 
                          "Mountainous Benchmark Achieved 2014", "Mountainous Benchmark Achieved 2015", 
                          "Mountainous Benchmark Achieved 2016", "Mountainous Benchmark Achieved 2017", 
                          "Mountainous Benchmark Achieved 2018", "Mountainous Benchmark Achieved 2019", 
                          "Mountainous Benchmark Achieved 2020", "Mountainous Benchmark Achieved 2021", 
                          "Mountainous Benchmark Achieved 2022", "Oil Exporter Benchmark Achieved 1990", 
                          "Oil Exporter Benchmark Achieved 1991", "Oil Exporter Benchmark Achieved 1992", 
                          "Oil Exporter Benchmark Achieved 1993", "Oil Exporter Benchmark Achieved 1994", 
                          "Oil Exporter Benchmark Achieved 1995", "Oil Exporter Benchmark Achieved 1996", 
                          "Oil Exporter Benchmark Achieved 1997", "Oil Exporter Benchmark Achieved 1998", 
                          "Oil Exporter Benchmark Achieved 1999", "Oil Exporter Benchmark Achieved 2000", 
                          "Oil Exporter Benchmark Achieved 2001", "Oil Exporter Benchmark Achieved 2002", 
                          "Oil Exporter Benchmark Achieved 2003", "Oil Exporter Benchmark Achieved 2004", 
                          "Oil Exporter Benchmark Achieved 2005", "Oil Exporter Benchmark Achieved 2006", 
                          "Oil Exporter Benchmark Achieved 2007", "Oil Exporter Benchmark Achieved 2008", 
                          "Oil Exporter Benchmark Achieved 2009", "Oil Exporter Benchmark Achieved 2010", 
                          "Oil Exporter Benchmark Achieved 2011", "Oil Exporter Benchmark Achieved 2012", 
                          "Oil Exporter Benchmark Achieved 2013", "Oil Exporter Benchmark Achieved 2014", 
                          "Oil Exporter Benchmark Achieved 2015", "Oil Exporter Benchmark Achieved 2016", 
                          "Oil Exporter Benchmark Achieved 2017", "Oil Exporter Benchmark Achieved 2018", 
                          "Oil Exporter Benchmark Achieved 2019", "Oil Exporter Benchmark Achieved 2020", 
                          "Oil Exporter Benchmark Achieved 2021", "Oil Exporter Benchmark Achieved 2022", 
                          "Population Density Benchmark Achieved 1990", "Population Density Benchmark Achieved 1991", 
                          "Population Density Benchmark Achieved 1992", "Population Density Benchmark Achieved 1993", 
                          "Population Density Benchmark Achieved 1994", "Population Density Benchmark Achieved 1995", 
                          "Population Density Benchmark Achieved 1996", "Population Density Benchmark Achieved 1997", 
                          "Population Density Benchmark Achieved 1998", "Population Density Benchmark Achieved 1999", 
                          "Population Density Benchmark Achieved 2000", "Population Density Benchmark Achieved 2001", 
                          "Population Density Benchmark Achieved 2002", "Population Density Benchmark Achieved 2003", 
                          "Population Density Benchmark Achieved 2004", "Population Density Benchmark Achieved 2005", 
                          "Population Density Benchmark Achieved 2006", "Population Density Benchmark Achieved 2007", 
                          "Population Density Benchmark Achieved 2008", "Population Density Benchmark Achieved 2009", 
                          "Population Density Benchmark Achieved 2010", "Population Density Benchmark Achieved 2011", 
                          "Population Density Benchmark Achieved 2012", "Population Density Benchmark Achieved 2013", 
                          "Population Density Benchmark Achieved 2014", "Population Density Benchmark Achieved 2015", 
                          "Population Density Benchmark Achieved 2016", "Population Density Benchmark Achieved 2017", 
                          "Population Density Benchmark Achieved 2018", "Population Density Benchmark Achieved 2019", 
                          "Population Density Benchmark Achieved 2020", "Population Density Benchmark Achieved 2021", 
                          "Population Density Benchmark Achieved 2022", "Human Capital Benchmark Achieved 1990", 
                          "Human Capital Benchmark Achieved 1991", "Human Capital Benchmark Achieved 1992", 
                          "Human Capital Benchmark Achieved 1993", "Human Capital Benchmark Achieved 1994", 
                          "Human Capital Benchmark Achieved 1995", "Human Capital Benchmark Achieved 1996", 
                          "Human Capital Benchmark Achieved 1997", "Human Capital Benchmark Achieved 1998", 
                          "Human Capital Benchmark Achieved 1999", "Human Capital Benchmark Achieved 2000", 
                          "Human Capital Benchmark Achieved 2001", "Human Capital Benchmark Achieved 2002", 
                          "Human Capital Benchmark Achieved 2003", "Human Capital Benchmark Achieved 2004", 
                          "Human Capital Benchmark Achieved 2005", "Human Capital Benchmark Achieved 2006", 
                          "Human Capital Benchmark Achieved 2007", "Human Capital Benchmark Achieved 2008", 
                          "Human Capital Benchmark Achieved 2009", "Human Capital Benchmark Achieved 2010", 
                          "Human Capital Benchmark Achieved 2011", "Human Capital Benchmark Achieved 2012", 
                          "Human Capital Benchmark Achieved 2013", "Human Capital Benchmark Achieved 2014", 
                          "Human Capital Benchmark Achieved 2015", "Human Capital Benchmark Achieved 2016", 
                          "Human Capital Benchmark Achieved 2017", "Human Capital Benchmark Achieved 2018", 
                          "Human Capital Benchmark Achieved 2019", "Human Capital Benchmark Achieved 2020", 
                          "Human Capital Benchmark Achieved 2021", "Human Capital Benchmark Achieved 2022", 
                          "Fragility Benchmark Achieved 1990", "Fragility Benchmark Achieved 1991", 
                          "Fragility Benchmark Achieved 1992", "Fragility Benchmark Achieved 1993", 
                          "Fragility Benchmark Achieved 1994", "Fragility Benchmark Achieved 1995", 
                          "Fragility Benchmark Achieved 1996", "Fragility Benchmark Achieved 1997", 
                          "Fragility Benchmark Achieved 1998", "Fragility Benchmark Achieved 1999", 
                          "Fragility Benchmark Achieved 2000", "Fragility Benchmark Achieved 2001", 
                          "Fragility Benchmark Achieved 2002", "Fragility Benchmark Achieved 2003", 
                          "Fragility Benchmark Achieved 2004", "Fragility Benchmark Achieved 2005", 
                          "Fragility Benchmark Achieved 2006", "Fragility Benchmark Achieved 2007", 
                          "Fragility Benchmark Achieved 2008", "Fragility Benchmark Achieved 2009", 
                          "Fragility Benchmark Achieved 2010", "Fragility Benchmark Achieved 2011", 
                          "Fragility Benchmark Achieved 2012", "Fragility Benchmark Achieved 2013", 
                          "Fragility Benchmark Achieved 2014", "Fragility Benchmark Achieved 2015", 
                          "Fragility Benchmark Achieved 2016", "Fragility Benchmark Achieved 2017", 
                          "Fragility Benchmark Achieved 2018", "Fragility Benchmark Achieved 2019", 
                          "Fragility Benchmark Achieved 2020", "Fragility Benchmark Achieved 2021", 
                          "Fragility Benchmark Achieved 2022"))


# Replace NULL with NA and change list to numeric vector
dat <- dat %>%  
  mutate(across(c(`Indicator Pillar`, `Indicator Sub-Pillar`, `1990`:`2022`), replace_null_with_NA), 
         across(c(`1990`:`2020`, starts_with("OECD")), as.numeric))

cols <- intersect(names(dat), names(bm))

dat <- dat %>% inner_join(bm, by = cols)

dat_transport = get_df("DR0053171", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
# Change in data generation script
# Rename 2nd column to country name
dat_transport <- dat_transport %>% 
  rename("Country Name" = Country) %>%
  select(1:48) %>%
  mutate(across(everything(), replace_null_with_NA), 
         across(c(`1990`:`2020`, starts_with("OECD")), as.numeric))

# Check column names of dat_transport
expect_equal(names(dat_transport), c("Country Name", "Country Code", "Indicator Name", "Indicator Pillar", 
    "Indicator Sector", "Mode", "Indicator Sub-Pillar", "Indicator Topic", "Region", "IncomeGroup", "Isolated", "Fragile", "Human Capital", 
    "Low Population Density", "Mountainous", "OECD Member", "Oil Exporter", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
    "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
    "2014", "2015", "2016", "2017", "2018", "2019", "2020"))

dat_spc = get_df("DR0086190", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
#dat_spc <- read_csv("../../Downloads/data_from_API/dat_spc.csv") 

# 1. Why all the column names are corrupted? they are present in 1st row of the data
# 2. 290+ columns have only NA in them. This is same as SPC_final_vFeb25.xlsx
# Drop columns with all NA, set first row as column names
dat_spc <- dat_spc %>%
  select(-1) %>%
  setNames(unlist(.[1, ])) %>% 
  slice(-1L) %>% 
  type_convert() %>%
  mutate(across(everything(), replace_null_with_NA), 
         across(c(`1990`:`2020`, starts_with("OECD")), as.numeric)) %>%
  # Change in data generation script
  rename("Country Name" = Country) %>%
  mutate(`Indicator Sector` = 'Cross-cutting')


expect_equal(names(dat_spc), c("Country Code", "Indicator Name", "Country Name", "Indicator Pillar", 
                               "Indicator Sub-Pillar", "Indicator Sector", "Indicator Topic", 
                               "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                               "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                               "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                               "2014", "2015", "2016", "2017", "2018", "2019", "2020", "Region", 
                               "Type of Benchmark", "IncomeGroup", "OECD Member", "Landlocked/Small Island", 
                               "Mountainous", "Oil Exporter", "Population Density", "Human Capital", 
                               "Fragility", "Region Benchmark Achieved 1990", "Region Benchmark Achieved 1991", 
                               "Region Benchmark Achieved 1992", "Region Benchmark Achieved 1993", 
                               "Region Benchmark Achieved 1994", "Region Benchmark Achieved 1995", 
                               "Region Benchmark Achieved 1996", "Region Benchmark Achieved 1997", 
                               "Region Benchmark Achieved 1998", "Region Benchmark Achieved 1999", 
                               "Region Benchmark Achieved 2000", "Region Benchmark Achieved 2001", 
                               "Region Benchmark Achieved 2002", "Region Benchmark Achieved 2003", 
                               "Region Benchmark Achieved 2004", "Region Benchmark Achieved 2005", 
                               "Region Benchmark Achieved 2006", "Region Benchmark Achieved 2007", 
                               "Region Benchmark Achieved 2008", "Region Benchmark Achieved 2009", 
                               "Region Benchmark Achieved 2010", "Region Benchmark Achieved 2011", 
                               "Region Benchmark Achieved 2012", "Region Benchmark Achieved 2013", 
                               "Region Benchmark Achieved 2014", "Region Benchmark Achieved 2015", 
                               "Region Benchmark Achieved 2016", "Region Benchmark Achieved 2017", 
                               "Region Benchmark Achieved 2018", "Region Benchmark Achieved 2019", 
                               "Region Benchmark Achieved 2020", "IncomeGroup Benchmark Achieved 1990", 
                               "IncomeGroup Benchmark Achieved 1991", "IncomeGroup Benchmark Achieved 1992", 
                               "IncomeGroup Benchmark Achieved 1993", "IncomeGroup Benchmark Achieved 1994", 
                               "IncomeGroup Benchmark Achieved 1995", "IncomeGroup Benchmark Achieved 1996", 
                               "IncomeGroup Benchmark Achieved 1997", "IncomeGroup Benchmark Achieved 1998", 
                               "IncomeGroup Benchmark Achieved 1999", "IncomeGroup Benchmark Achieved 2000", 
                               "IncomeGroup Benchmark Achieved 2001", "IncomeGroup Benchmark Achieved 2002", 
                               "IncomeGroup Benchmark Achieved 2003", "IncomeGroup Benchmark Achieved 2004", 
                               "IncomeGroup Benchmark Achieved 2005", "IncomeGroup Benchmark Achieved 2006", 
                               "IncomeGroup Benchmark Achieved 2007", "IncomeGroup Benchmark Achieved 2008", 
                               "IncomeGroup Benchmark Achieved 2009", "IncomeGroup Benchmark Achieved 2010", 
                               "IncomeGroup Benchmark Achieved 2011", "IncomeGroup Benchmark Achieved 2012", 
                               "IncomeGroup Benchmark Achieved 2013", "IncomeGroup Benchmark Achieved 2014", 
                               "IncomeGroup Benchmark Achieved 2015", "IncomeGroup Benchmark Achieved 2016", 
                               "IncomeGroup Benchmark Achieved 2017", "IncomeGroup Benchmark Achieved 2018", 
                               "IncomeGroup Benchmark Achieved 2019", "IncomeGroup Benchmark Achieved 2020", 
                               "OECD Member Benchmark Achieved 1990", "OECD Member Benchmark Achieved 1991", 
                               "OECD Member Benchmark Achieved 1992", "OECD Member Benchmark Achieved 1993", 
                               "OECD Member Benchmark Achieved 1994", "OECD Member Benchmark Achieved 1995", 
                               "OECD Member Benchmark Achieved 1996", "OECD Member Benchmark Achieved 1997", 
                               "OECD Member Benchmark Achieved 1998", "OECD Member Benchmark Achieved 1999", 
                               "OECD Member Benchmark Achieved 2000", "OECD Member Benchmark Achieved 2001", 
                               "OECD Member Benchmark Achieved 2002", "OECD Member Benchmark Achieved 2003", 
                               "OECD Member Benchmark Achieved 2004", "OECD Member Benchmark Achieved 2005", 
                               "OECD Member Benchmark Achieved 2006", "OECD Member Benchmark Achieved 2007", 
                               "OECD Member Benchmark Achieved 2008", "OECD Member Benchmark Achieved 2009", 
                               "OECD Member Benchmark Achieved 2010", "OECD Member Benchmark Achieved 2011", 
                               "OECD Member Benchmark Achieved 2012", "OECD Member Benchmark Achieved 2013", 
                               "OECD Member Benchmark Achieved 2014", "OECD Member Benchmark Achieved 2015", 
                               "OECD Member Benchmark Achieved 2016", "OECD Member Benchmark Achieved 2017", 
                               "OECD Member Benchmark Achieved 2018", "OECD Member Benchmark Achieved 2019", 
                               "OECD Member Benchmark Achieved 2020", "Landlocked/Small Island Benchmark Achieved 1990", 
                               "Landlocked/Small Island Benchmark Achieved 1991", "Landlocked/Small Island Benchmark Achieved 1992", 
                               "Landlocked/Small Island Benchmark Achieved 1993", "Landlocked/Small Island Benchmark Achieved 1994", 
                               "Landlocked/Small Island Benchmark Achieved 1995", "Landlocked/Small Island Benchmark Achieved 1996", 
                               "Landlocked/Small Island Benchmark Achieved 1997", "Landlocked/Small Island Benchmark Achieved 1998", 
                               "Landlocked/Small Island Benchmark Achieved 1999", "Landlocked/Small Island Benchmark Achieved 2000", 
                               "Landlocked/Small Island Benchmark Achieved 2001", "Landlocked/Small Island Benchmark Achieved 2002", 
                               "Landlocked/Small Island Benchmark Achieved 2003", "Landlocked/Small Island Benchmark Achieved 2004", 
                               "Landlocked/Small Island Benchmark Achieved 2005", "Landlocked/Small Island Benchmark Achieved 2006", 
                               "Landlocked/Small Island Benchmark Achieved 2007", "Landlocked/Small Island Benchmark Achieved 2008", 
                               "Landlocked/Small Island Benchmark Achieved 2009", "Landlocked/Small Island Benchmark Achieved 2010", 
                               "Landlocked/Small Island Benchmark Achieved 2011", "Landlocked/Small Island Benchmark Achieved 2012", 
                               "Landlocked/Small Island Benchmark Achieved 2013", "Landlocked/Small Island Benchmark Achieved 2014", 
                               "Landlocked/Small Island Benchmark Achieved 2015", "Landlocked/Small Island Benchmark Achieved 2016", 
                               "Landlocked/Small Island Benchmark Achieved 2017", "Landlocked/Small Island Benchmark Achieved 2018", 
                               "Landlocked/Small Island Benchmark Achieved 2019", "Landlocked/Small Island Benchmark Achieved 2020", 
                               "Mountainous Benchmark Achieved 1990", "Mountainous Benchmark Achieved 1991", 
                               "Mountainous Benchmark Achieved 1992", "Mountainous Benchmark Achieved 1993", 
                               "Mountainous Benchmark Achieved 1994", "Mountainous Benchmark Achieved 1995", 
                               "Mountainous Benchmark Achieved 1996", "Mountainous Benchmark Achieved 1997", 
                               "Mountainous Benchmark Achieved 1998", "Mountainous Benchmark Achieved 1999", 
                               "Mountainous Benchmark Achieved 2000", "Mountainous Benchmark Achieved 2001", 
                               "Mountainous Benchmark Achieved 2002", "Mountainous Benchmark Achieved 2003", 
                               "Mountainous Benchmark Achieved 2004", "Mountainous Benchmark Achieved 2005", 
                               "Mountainous Benchmark Achieved 2006", "Mountainous Benchmark Achieved 2007", 
                               "Mountainous Benchmark Achieved 2008", "Mountainous Benchmark Achieved 2009", 
                               "Mountainous Benchmark Achieved 2010", "Mountainous Benchmark Achieved 2011", 
                               "Mountainous Benchmark Achieved 2012", "Mountainous Benchmark Achieved 2013", 
                               "Mountainous Benchmark Achieved 2014", "Mountainous Benchmark Achieved 2015", 
                               "Mountainous Benchmark Achieved 2016", "Mountainous Benchmark Achieved 2017", 
                               "Mountainous Benchmark Achieved 2018", "Mountainous Benchmark Achieved 2019", 
                               "Mountainous Benchmark Achieved 2020", "Oil Exporter Benchmark Achieved 1990", 
                               "Oil Exporter Benchmark Achieved 1991", "Oil Exporter Benchmark Achieved 1992", 
                               "Oil Exporter Benchmark Achieved 1993", "Oil Exporter Benchmark Achieved 1994", 
                               "Oil Exporter Benchmark Achieved 1995", "Oil Exporter Benchmark Achieved 1996", 
                               "Oil Exporter Benchmark Achieved 1997", "Oil Exporter Benchmark Achieved 1998", 
                               "Oil Exporter Benchmark Achieved 1999", "Oil Exporter Benchmark Achieved 2000", 
                               "Oil Exporter Benchmark Achieved 2001", "Oil Exporter Benchmark Achieved 2002", 
                               "Oil Exporter Benchmark Achieved 2003", "Oil Exporter Benchmark Achieved 2004", 
                               "Oil Exporter Benchmark Achieved 2005", "Oil Exporter Benchmark Achieved 2006", 
                               "Oil Exporter Benchmark Achieved 2007", "Oil Exporter Benchmark Achieved 2008", 
                               "Oil Exporter Benchmark Achieved 2009", "Oil Exporter Benchmark Achieved 2010", 
                               "Oil Exporter Benchmark Achieved 2011", "Oil Exporter Benchmark Achieved 2012", 
                               "Oil Exporter Benchmark Achieved 2013", "Oil Exporter Benchmark Achieved 2014", 
                               "Oil Exporter Benchmark Achieved 2015", "Oil Exporter Benchmark Achieved 2016", 
                               "Oil Exporter Benchmark Achieved 2017", "Oil Exporter Benchmark Achieved 2018", 
                               "Oil Exporter Benchmark Achieved 2019", "Oil Exporter Benchmark Achieved 2020", 
                               "Population Density Benchmark Achieved 1990", "Population Density Benchmark Achieved 1991", 
                               "Population Density Benchmark Achieved 1992", "Population Density Benchmark Achieved 1993", 
                               "Population Density Benchmark Achieved 1994", "Population Density Benchmark Achieved 1995", 
                               "Population Density Benchmark Achieved 1996", "Population Density Benchmark Achieved 1997", 
                               "Population Density Benchmark Achieved 1998", "Population Density Benchmark Achieved 1999", 
                               "Population Density Benchmark Achieved 2000", "Population Density Benchmark Achieved 2001", 
                               "Population Density Benchmark Achieved 2002", "Population Density Benchmark Achieved 2003", 
                               "Population Density Benchmark Achieved 2004", "Population Density Benchmark Achieved 2005", 
                               "Population Density Benchmark Achieved 2006", "Population Density Benchmark Achieved 2007", 
                               "Population Density Benchmark Achieved 2008", "Population Density Benchmark Achieved 2009", 
                               "Population Density Benchmark Achieved 2010", "Population Density Benchmark Achieved 2011", 
                               "Population Density Benchmark Achieved 2012", "Population Density Benchmark Achieved 2013", 
                               "Population Density Benchmark Achieved 2014", "Population Density Benchmark Achieved 2015", 
                               "Population Density Benchmark Achieved 2016", "Population Density Benchmark Achieved 2017", 
                               "Population Density Benchmark Achieved 2018", "Population Density Benchmark Achieved 2019", 
                               "Population Density Benchmark Achieved 2020", "Human Capital Benchmark Achieved 1990", 
                               "Human Capital Benchmark Achieved 1991", "Human Capital Benchmark Achieved 1992", 
                               "Human Capital Benchmark Achieved 1993", "Human Capital Benchmark Achieved 1994", 
                               "Human Capital Benchmark Achieved 1995", "Human Capital Benchmark Achieved 1996", 
                               "Human Capital Benchmark Achieved 1997", "Human Capital Benchmark Achieved 1998", 
                               "Human Capital Benchmark Achieved 1999", "Human Capital Benchmark Achieved 2000", 
                               "Human Capital Benchmark Achieved 2001", "Human Capital Benchmark Achieved 2002", 
                               "Human Capital Benchmark Achieved 2003", "Human Capital Benchmark Achieved 2004", 
                               "Human Capital Benchmark Achieved 2005", "Human Capital Benchmark Achieved 2006", 
                               "Human Capital Benchmark Achieved 2007", "Human Capital Benchmark Achieved 2008", 
                               "Human Capital Benchmark Achieved 2009", "Human Capital Benchmark Achieved 2010", 
                               "Human Capital Benchmark Achieved 2011", "Human Capital Benchmark Achieved 2012", 
                               "Human Capital Benchmark Achieved 2013", "Human Capital Benchmark Achieved 2014", 
                               "Human Capital Benchmark Achieved 2015", "Human Capital Benchmark Achieved 2016", 
                               "Human Capital Benchmark Achieved 2017", "Human Capital Benchmark Achieved 2018", 
                               "Human Capital Benchmark Achieved 2019", "Human Capital Benchmark Achieved 2020", 
                               "Fragility Benchmark Achieved 1990", "Fragility Benchmark Achieved 1991", 
                               "Fragility Benchmark Achieved 1992", "Fragility Benchmark Achieved 1993", 
                               "Fragility Benchmark Achieved 1994", "Fragility Benchmark Achieved 1995", 
                               "Fragility Benchmark Achieved 1996", "Fragility Benchmark Achieved 1997", 
                               "Fragility Benchmark Achieved 1998", "Fragility Benchmark Achieved 1999", 
                               "Fragility Benchmark Achieved 2000", "Fragility Benchmark Achieved 2001", 
                               "Fragility Benchmark Achieved 2002", "Fragility Benchmark Achieved 2003", 
                               "Fragility Benchmark Achieved 2004", "Fragility Benchmark Achieved 2005", 
                               "Fragility Benchmark Achieved 2006", "Fragility Benchmark Achieved 2007", 
                               "Fragility Benchmark Achieved 2008", "Fragility Benchmark Achieved 2009", 
                               "Fragility Benchmark Achieved 2010", "Fragility Benchmark Achieved 2011", 
                               "Fragility Benchmark Achieved 2012", "Fragility Benchmark Achieved 2013", 
                               "Fragility Benchmark Achieved 2014", "Fragility Benchmark Achieved 2015", 
                               "Fragility Benchmark Achieved 2016", "Fragility Benchmark Achieved 2017", 
                               "Fragility Benchmark Achieved 2018", "Fragility Benchmark Achieved 2019", 
                               "Fragility Benchmark Achieved 2020"))


dat <- dat %>%
  # Change in data generation script
  rename("Isolated" = `Landlocked/Small Island`, "Low Population Density" = `Population Density`, "Fragile" = `Fragility`) %>%
  mutate(`Indicator Topic` = sapply(`Indicator Topic`, \(x) if(is.null(x)) NA else x))


dat <- bind_rows(dat, dat_transport, dat_spc) %>%
  mutate(`Indicator Sector`  = ifelse(`Indicator Sector` == 'Sector-specific Data Not Available', 'National', `Indicator Sector`), 
         Mode = replace(Mode, is.na(Mode), ''), 
         `Indicator Sector` = trimws(paste0(`Indicator Sector`, ' ', Mode))) %>%
  type_convert()

pattern <- "Senegal - Mali|Ethiopia - Djibouti|Cote d'Ivoire/Burkino Faso"

dat <- dat %>%
  filter(grepl(pattern, `Country Name`)) %>%
  mutate(`Country Name` = trimws(sub('\\(.*\\)', '', `Country Name`))) %>%
  separate_rows(`Country Name`, sep = '/|-') %>%
  bind_rows(dat %>% filter(!grepl(pattern, `Country Name`)))


dat <- dat %>%
  mutate(`Indicator Sub-Pillar` = tools::toTitleCase(trimws(gsub('Low|Poor|Expensive|Limited', '', `Indicator Sub-Pillar`))), 
         `Indicator Topic` = tools::toTitleCase(trimws(gsub('Low|High', '', `Indicator Topic`)))) 

# save(dat, file = 'data/dat.rda')
## `dat_country_income_region` Country, income, region data -----
dat_country_income_region <- infrasap::dat %>% distinct(`Country Name`, IncomeGroup, Region)

usethis::use_data(dat_country_income_region, overwrite = TRUE)


dat$irf_data <- FALSE

# load IRF data
#irf_indicators1 <- readxl::read_xlsx('data-raw/IRF_Data.xlsx')
irf_indicators <- readxl::read_xlsx('data-ddh/IRF_data_Nov22.xlsx')

# create column to indicate this is irf data
irf_indicators$irf_data <- TRUE
irf_indicators$`Type of Benchmark` <- NA
shared_names <- intersect(names(irf_indicators), names(dat))
irf_indicators <- irf_indicators %>% select(all_of(shared_names))
dat <- dat %>% select(all_of(shared_names))
dat <- rbind(dat, irf_indicators)
dat <- dat %>% filter(!is.na(`Indicator Sector`))
usethis::use_data(dat, overwrite = TRUE)

# bm_trans.csv
bm_trans = get_df("DR0047111", "data", list("top"  = 50, "skip" = 0, "filter" = ""))

bm_trans <- bm_trans %>%
  mutate(across(everything(), replace_null_with_NA), 
         across(`1990`:`2020`, as.numeric)) %>%
  rename(Grouping = Region, Indicator = `Indicator Name`)

# Benchmark_aggregates_infrasap_vNov17.xlsx
bm = get_df("DR0047112", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
bm <- bm %>%
  mutate(across(everything(), replace_null_with_NA), 
         across(`1990`:`2022`, as.numeric)) %>%
  rename(Grouping = Region, Indicator = `Indicator Name`)

bm$Mode <- NA

bm_irf <- readxl::read_xlsx('data-raw/Benchmarks_IRF_Nov22.xlsx') %>%
  rename(Grouping = Region, Indicator = `Indicator Name`)
bm_spc = get_df("DR0086191", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
bm_spc <- bm_spc %>%
  setNames(.[1, ]) %>%
  slice(-1L) %>% 
  type_convert()

expect_equal(names(bm_spc), c("Region", "Indicator Name", "2000", "2015"))

bm_spc <- bm_spc %>%
  rename(Grouping = Region, Indicator = `Indicator Name`) %>%
  mutate(Sector = "SPC")

dat_bm <- bind_rows(bm, bm_trans)
dat_bm <- bind_rows(dat_bm, bm_irf)
dat_bm$Mode <- ifelse(dat_bm$Mode == '0', NA, dat_bm$Mode)
dat_bm <- bind_rows(dat_bm, bm_spc)

dat_bm <- dat_bm %>%
  type_convert() %>%
  mutate(Sector = ifelse(grepl('Railway|Road|Port', Mode), 'Transport', Sector), 
         Sector = ifelse(Sector == 'Sector-specific Data Not Available', 'National', Sector), 
         Mode = replace(Mode, is.na(Mode), ''), 
         Sector = trimws(paste0(Sector, " ", Mode))) %>%
  distinct(Grouping, Indicator, .keep_all = TRUE)

# save data 
usethis::use_data(dat_bm, overwrite = TRUE)


# create a benchmark list
benchmark_list <- c("East Asia & Pacific", "Europe & Central Asia", "Fragile", "High income", "Isolated", "Latin America & Caribbean", "Low Human Capital", "Low income", "Low Population Density", "Lower middle income", "Middle East & North Africa", "Mountainous", "North America", "OECD members", "Oil Exporter", "South Asia","Sub-Saharan Africa", "Upper middle income" )

# save benchmark list
usethis::use_data(benchmark_list, overwrite = T)
##################################
# World shape files --------------
##################################

# read in shape files
world <- readOGR('data-raw/map_data_november_2021/Adm0_boudnaries2021.geojson')  
# merget wb_bound and world 
usethis::use_data(world, overwrite = TRUE)

map_location <- tibble(region = c('Entire World','East Asia & Pacific', 'Europe & Central Asia', 'Latin America & Caribbean', 'Middle East & North Africa', 'North America', 'Sub-Saharan Africa', 'South Asia'),
                       lat = c(0,10, 60, -15, 10, 55, 10, 10),
                       lon = c(0,100, 80, -60,30, -120, 30, 80),
                       zoom = c(1.7,2.5,  2.5,  2.5,2.5, 2.5, 2.5, 2.5))
usethis::use_data(map_location, overwrite = TRUE)


scd_indicators_tbl <- readxl::read_xlsx('data-raw/All_SCD_indicators_ROH_v3.xlsx', sheet = 2)

scd_indicators_tbl <- scd_indicators_tbl %>% select(X1 = `SCD Indicator`, X2 = `Corresponding indicator from All indicators` )
scd_indicators_tbl <- as.data.frame(scd_indicators_tbl)
scd_indicators_index <- scd_indicators_tbl %>%
  rowid_to_column() %>%
  filter(is.na(X2)) %>% select(-X2)

groups_pillar <- rep(scd_indicators_index$X1, c(diff(scd_indicators_index$rowid), nrow(scd_indicators_tbl) - (scd_indicators_index$rowid[nrow(scd_indicators_index)] - 1)))


scd_indicators <- scd_indicators_tbl %>%
  mutate(pillar = groups_pillar) %>% drop_na() %>% separate_rows(X2, sep = ';') %>% #slice(-22) %>% 
  rename(
    topic = X1,
    indicator = X2
  ) 

names(scd_indicators) <- c('new_name', 'old_name', 'grouping')


dat <- dat %>% 
  distinct(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Pillar`, `Indicator Sub-Pillar`, .keep_all = TRUE)

scd_dat <- inner_join(dat, scd_indicators, by=c('Indicator Name'='old_name'))
scd_dat$`Indicator Name` <- scd_dat$new_name

usethis::use_data(scd_dat, overwrite = TRUE)

# join with benchmark data
scd_bm <- inner_join(dat_bm, scd_indicators, by=c('Indicator'='old_name'))
scd_bm$`Indicator` <- scd_bm$new_name
scd_bm <- scd_bm %>% distinct(Grouping, Indicator, .keep_all = TRUE)
usethis::use_data(scd_bm, overwrite = TRUE)

# save scd_indicators
scd_indicators$old_name <- NULL
names(scd_indicators)[1] <- "Indicator Name"
usethis::use_data(scd_indicators, overwrite = TRUE)

scd_dat_countries_with_article <- scd_dat %>% 
  select(`Country Name`) %>% 
  distinct() %>%
  pull() 
scd_dat_countries_with_article <- scd_dat_countries_with_article[scd_dat_countries_with_article %>% 
                                                                   str_detect('Bahamas|Cayman|Central African|Channel Islands|Comoros|Czech Republic|Dominican Republic|Falkland Islands|Gambia|Isle|Ivory|Leeward|Maldives|Marshall|Netherlands|Philippines|Solomon|Turks|United|Virgin Islands')
] 
scd_dat_countries_with_article <- scd_dat_countries_with_article[!(scd_dat_countries_with_article %>% 
                                                                     str_detect('British'))
] 
usethis::use_data(scd_dat_countries_with_article, overwrite = TRUE)

dat_ports <- get_df("DR0065512", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
dat_ports <- readr::type_convert(dat_ports)
usethis::use_data(dat_ports, overwrite = TRUE)


energy__governance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Energy' & `Indicator Pillar` == 'Governance') %>%
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>%
  drop_na() %>%
  ungroup() %>%
  mutate(`Indicator Sub-Pillar` = forcats::as_factor(`Indicator Sub-Pillar`)) %>% 
  arrange(`Indicator Sub-Pillar`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

cross_cutting_governance <- dat %>%
  select(`Indicator Sector`,
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>%
  filter((`Indicator Sector` == 'Cross-cutting' | `Indicator Sector` == 'National') & `Indicator Pillar` == 'Governance') %>%
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>%
  drop_na() %>%
  ungroup() %>%
  mutate(`Indicator Sub-Pillar` = forcats::as_factor(`Indicator Sub-Pillar`)) %>% 
  arrange(`Indicator Sub-Pillar`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

energy__governance <- rbind(cross_cutting_governance, energy__governance)
energy__governance <- energy__governance[c(1:6, 8, 7, 10, 9, 11:14),]


transport__governance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Transport' & `Indicator Pillar` == 'Governance') %>%
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>%
  drop_na() %>%
  ungroup() %>%
  mutate(`Indicator Sub-Pillar` = forcats::as_factor(`Indicator Sub-Pillar`)) %>% 
  arrange(`Indicator Sub-Pillar`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

transport__governance <- rbind(cross_cutting_governance, transport__governance)
transport__governance <- transport__governance[c(1:6, 8, 7, 9:11),]

digital__governance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Digital Development' & `Indicator Pillar` == 'Governance') %>%
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>%
  drop_na() %>%
  ungroup() %>%
  mutate(`Indicator Sub-Pillar` = forcats::as_factor(`Indicator Sub-Pillar`)) %>% 
  arrange(`Indicator Sub-Pillar`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

energy_connectivity <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Energy' & `Indicator Pillar` == 'Connectivity') %>%
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>% 
  drop_na() %>%
  ungroup() %>%
  arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

energy_connectivity <- energy_connectivity[c(1:6, 21:23, 17, 16, 20, 18:19, 7, 15, 8:14), ] 

digital_connectivity <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Digital Development' & `Indicator Pillar` == 'Connectivity') %>% 
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>% 
  drop_na() %>%
  ungroup() %>% 
  arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

energy_finance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Energy' & `Indicator Pillar` == 'Finance') %>% 
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>% 
  drop_na() %>%
  ungroup() %>% 
  arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

energy_finance <- energy_finance[c(1:3, 6:8, 4:5, 9:14),]

digital_finance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Digital Development' & `Indicator Pillar` == 'Finance') %>% 
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>% 
  drop_na() %>%
  ungroup() %>% 
  arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

transport_finance <- dat %>%
  select(`Indicator Sector`, 
         `Indicator Sub-Pillar`, 
         `Indicator Topic`,
         `Indicator Name`, 
         `Indicator Pillar`, 
  ) %>% 
  filter(`Indicator Sector` == 'Transport' & `Indicator Pillar` == 'Finance') %>% 
  group_by(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`) %>% distinct() %>% 
  drop_na() %>%
  ungroup() %>% 
  arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
  select(-c(`Indicator Sector`, `Indicator Pillar`))

dat_layout <- list(
  energy__governance = energy__governance,
  transport__governance = transport__governance,
  digital__governance = digital__governance,
  energy__connectivity = energy_connectivity,
  digital__connectivity = digital_connectivity,
  energy__finance = energy_finance, 
  transport__finance = transport_finance
)


# dat_layout <- dat %>%
#   mutate(`Indicator Sector` = tolower(`Indicator Sector`), `Indicator Pillar` = tolower(`Indicator Pillar`)) %>%
#   select(`Indicator Sector`, `Indicator Sub-Pillar`, `Indicator Topic`,`Indicator Name`, `Indicator Pillar`) %>%
#   filter(!is.na(`Indicator Sector`) & !is.na(`Indicator Pillar`)) %>%
#   arrange(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
#   distinct(`Indicator Sub-Pillar`, `Indicator Topic`, `Indicator Name`, .keep_all = TRUE) %>%
#   unite(name, `Indicator Sector`, `Indicator Pillar`, sep = "__") %>%
#   split(.$name) %>%
#   map(~.x %>% select(-name))

usethis::use_data(dat_layout, overwrite = TRUE)







