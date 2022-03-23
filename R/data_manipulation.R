## Author: Shel Kariuki
## Date: 21st March 2021
## Description: This script contains all the data manipulation codes needed to generate the infrasap dashboard

#0.1 Load the libraries  -----------------------------------------------------------
library(dplyr)
library(tidyverse)

#0.2 reshape function  -----------------------------------------------------------
reshape_func <- function(dat, columns){
  dat <- dat %>% 
    tidyr::pivot_longer(cols = columns,  names_to = "Year", values_to = "Value", values_drop_na = TRUE)
  return(dat)
  
}

#1. Manipulate dat.rda -----------------------------------------------------------

## Rename sector name
load(file = "data/dat.rda")
dat_modified <- dat
rm(dat)

dat_modified$`Indicator Sector`[dat_modified$`Indicator Sector` == "Transport"] <- "Transport cross-cutting"
dat_modified$`Indicator Sector`[dat_modified$`Indicator Sector` == "National"] <- "Cross-cutting"

## reshape the data
# vars <- as.character(1990:2020) ## remember to edit this
vars <- grep("^19|^20", names(dat_modified), value = TRUE)
dat_modified <- reshape_func(dat_modified, vars)



#2. Manipulate dat_bm.rda -----------------------------------------------------------

## Rename sector name
load(file = "data/dat_bm.rda")
dat_bm_modified <- dat_bm
rm(dat_bm)
dat_bm_modified$Sector[dat_bm_modified$Sector == "Transport"] <- "Transport cross-cutting"
dat_bm_modified$Sector[dat_bm_modified$Sector == "National"] <- "Cross-cutting"

# vars <- as.character(1990:2020)

vars <- grep("^19|^20", names(dat_bm_modified), value = TRUE)

## reshape the data
dat_bm_modified <- reshape_func(dat_bm_modified, vars)


#3. Manipulate dat_ports.rda -----------------------------------------------------------

load(file = "data/dat_ports.rda")
dat_ports$...1 <- NULL
dat_ports_modified <- dat_ports
rm(dat_ports)

initial_vars <- c("Country Name", "Country Code", "Sub-national Unit", "Sub-national Unit Name",           
"Port Size", "Indicator Name", "Distribution1 Condition", "Region")

rba_cols <- grep("Region Benchmark Achieved ", names(dat_ports_modified), value = T, ignore.case = T)
rba_data <- dat_ports_modified[, c(initial_vars ,rba_cols) ]
rba_data <- rba_data %>% 
              reshape_func(rba_cols) %>% 
              mutate(Year = trimws(gsub("Region Benchmark Achieved ", "", Year))) %>% 
              rename(RBA_Value = Value)

psba_cols <- grep("Port Size Benchmark Achieved ", names(dat_ports_modified), value = T, ignore.case = T)
psba_data <- dat_ports_modified[, c(initial_vars ,psba_cols) ]
psba_data <- psba_data %>% 
  reshape_func(psba_cols) %>% 
  mutate(Year = trimws(gsub("Port Size Benchmark Achieved ", "", Year))) %>% 
  rename(PSBA_Value = Value)


# vars <- as.character(1990:2030)
vars <- grep("^19|^20", names(dat_ports_modified), value = TRUE)

other_ports_data <- dat_ports_modified %>% select(all_of(initial_vars), all_of(vars))
other_ports_data <- reshape_func(other_ports_data, vars)

dat_ports_datasets <- list(rba_data, psba_data, other_ports_data)
dat_ports_modified <- dat_ports_datasets %>% reduce(full_join, by=c(initial_vars, "Year"))


#4. Manipulate dat_ports_bm.rda -----------------------------------------------------------

load(file = "data/dat_ports_bm.rda")
dat_ports_bm$...1 <- NULL
dat_ports_bm_modified <- dat_ports_bm
rm(dat_ports_bm)


## reshape the data
# vars <- as.character(1990:2030)

vars <- grep("^19|^20", names(dat_ports_bm_modified), value = TRUE)
dat_ports_bm_modified <- dat_ports_bm_modified %>% 
  reshape_func( vars)


#5. Append the main data and benchmark data

## 5.1 other sectors
regions_nonports <- sort(unique(dat_modified$Region)) 
income_groups_nonports <- sort(unique(dat_modified$IncomeGroup)) 
mountainous_nonports <- sort(unique(dat_modified$Mountainous))
isolation_nonports <- sort(unique(dat_modified$Isolated))
oilexporters_nonports <- sort(unique(dat_modified$`Oil Exporter`))
humcapital_nonports <- sort(unique(dat_modified$`Human Capital`))
fragility_nonports <- sort(unique(dat_modified$Fragile))
popdensity_nonports <- sort(unique(dat_modified$`Low Population Density`))
oecd_nonports <- sort(unique(dat_modified$`OECD Member`))

dat_modified <- dat_modified %>% 
  mutate(Grouping2 = "country")

dat_bm_modified <- dat_bm_modified %>% 
                     mutate(Grouping2 = ifelse(Grouping %in%regions_nonports, "region", 
                                        ifelse(Grouping %in%income_groups_nonports, "income_group",
                                        ifelse(Grouping %in%mountainous_nonports, "mountainous",             
                                        ifelse(Grouping %in%isolation_nonports, "isolation",
                                        ifelse(Grouping %in%oilexporters_nonports, "oilexporters",
                                        ifelse(Grouping %in%humcapital_nonports, "humcapital",
                                        ifelse(Grouping %in%fragility_nonports, "fragility",
                                        ifelse(Grouping %in% popdensity_nonports, "popdensity",
                                        ifelse(Grouping %in% oecd_nonports, "oecdmember",
                                                      "others")))))))))) %>% 
                    rename(`Indicator Name` = Indicator,
                           `Indicator Sector` = Sector)

dat_appended <- bind_rows(dat_modified, dat_bm_modified)

dat_appended <- dat_appended %>% 
                 select(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Sector`,Grouping,
                        Grouping2, Year, Value, everything())

## 5.2 ports
regions_ports <- sort(unique(dat_ports_modified$Region)) 
portsize <- sort(unique(dat_ports_modified$`Port Size`)) 

dat_ports_modified <- dat_ports_modified %>% 
  mutate(Grouping2 = "country")

dat_ports_bm_modified <- dat_ports_bm_modified %>% 
  mutate(Grouping2 = ifelse(Grouping %in%regions_ports, "region", 
                            ifelse(Grouping %in%portsize, "portsize", "others")))

dat_ports_appended <- bind_rows(dat_ports_modified, dat_ports_bm_modified)

dat_ports_appended <- dat_ports_appended %>% 
  select(`Country Code`, `Country Name`, `Indicator Name`, Grouping,
         Grouping2, Year, Value, everything())

rm(dat_ports_datasets, rba_data, psba_data, other_ports_data, initial_vars, psba_cols, 
   rba_cols, vars, reshape_func)
rm(list = ls(pattern="_nonports$"))
rm(list = ls(pattern="_modified$"))
rm(portsize, regions_ports)

