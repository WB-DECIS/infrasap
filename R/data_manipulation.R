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
vars <- as.character(1990:2020) ## remember to edit this
dat_modified <- reshape_func(dat_modified, vars)



#2. Manipulate dat_bm.rda -----------------------------------------------------------

## Rename sector name
load(file = "data/dat_bm.rda")
dat_bm_modified <- dat_bm
rm(dat_bm)
dat_bm_modified$Sector[dat_bm_modified$Sector == "Transport"] <- "Transport cross-cutting"
dat_bm_modified$Sector[dat_bm_modified$Sector == "National"] <- "Cross-cutting"

vars <- as.character(1990:2020)

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


vars <- as.character(1990:2030)
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
vars <- as.character(1990:2030)
dat_ports_bm_modified <- dat_ports_bm_modified %>% 
  reshape_func( vars)

rm(dat_ports_datasets, rba_data, psba_data, other_ports_data, initial_vars, psba_cols, rba_cols, vars, reshape_func)
