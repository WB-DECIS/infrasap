library(reticulate)
library(assertthat)
library(dplyr)
library(testthat)
library(readr)
library(tidyr)

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

bm = get_df("DR0091284", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
#bm <- read_csv("../../Downloads/data_from_API/bm.csv")
bm <- bm %>% 
  mutate(across(everything(), replace_null_with_NA)) %>%
  type_convert() %>% as_tibble()

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

# Change in data generation script
# Rename 2nd column to country name
#dat <- dat %>% rename("Country Name" = Country)

# Replace NULL with NA and change list to numeric vector
dat <- dat %>%  
  mutate(across(c(`Indicator Pillar`, `Indicator Sub-Pillar`, `1990`:`2022`), replace_null_with_NA), 
         across(`1990`:`2022`, as.numeric))

#### Rough start ####
dat %>% 
  distinct(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Pillar`, 
           `Indicator Sub-Pillar`, `Indicator Sector`)
# Double data example
dat %>% filter(`Country Code` == "AFG", `Indicator Name` == "% population covered by 2G")
dat %>% 
  count(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Pillar`, 
           `Indicator Sub-Pillar`, `Indicator Sector`, sort = TRUE)

cols <- intersect(names(dat), names(bm))
rm1 <- dat %>% select(all_of(cols))

rm2 <- bm %>% select(all_of(cols)) 

dat %>% inner_join(bm, by = cols)
#### Rough end ####

dat <- bind_cols(dat, bm)
dat_transport = get_df("DR0053171", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
# Change in data generation script
# Rename 2nd column to country name
dat_transport <- dat_transport %>% 
  #rename("Country Name" = Country) %>%
  select(1:48) %>%
  mutate(across(everything(), replace_null_with_NA)) 

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
  select(where(~sum(is.na(.)) != 500)) %>%
  setNames(.[1, ]) %>%
  slice(-1L)


# Change in data generation script
# Rename 4th column to country name
dat_spc <- dat_spc %>% 
  rename("Country Name" = Country) %>%
  mutate(`Indicator Sector` = 'Cross-cutting')

expect_equal(names(dat_spc), c("Country Code", "Indicator Name", "Country Name", "2000", "2015", 
                               "Region", "IncomeGroup", "Landlocked/Small Island", "Mountainous", 
                               "Oil Exporter", "Population Density", "Human Capital", "Fragility", 
                               "Region Benchmark Achieved 2000", "Region Benchmark Achieved 2015", 
                               "IncomeGroup Benchmark Achieved 2000", "IncomeGroup Benchmark Achieved 2015", 
                               "Landlocked/Small Island Benchmark Achieved 2000", "Landlocked/Small Island Benchmark Achieved 2015", 
                               "Mountainous Benchmark Achieved 2000", "Mountainous Benchmark Achieved 2015", 
                               "Oil Exporter Benchmark Achieved 2000", "Oil Exporter Benchmark Achieved 2015", 
                               "Population Density Benchmark Achieved 2000", "Population Density Benchmark Achieved 2015", 
                               "Human Capital Benchmark Achieved 2000", "Human Capital Benchmark Achieved 2015", 
                               "Fragility Benchmark Achieved 2000", "Fragility Benchmark Achieved 2015", 
                               "Indicator Sector"))

dat

data4 = get_df("DR0086190", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data5 = get_df("DR0047111", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data5 <- data5 %>%
  mutate(across(everything(), replace_null_with_NA))

data6 = get_df("DR0047112", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data7 = get_df("DR0086191", "data", list("top"  = 50, "skip" = 0, "filter" = ""))
data8 = get_df("DR0065512", "data", list("top"  = 50, "skip" = 0, "filter" = ""))