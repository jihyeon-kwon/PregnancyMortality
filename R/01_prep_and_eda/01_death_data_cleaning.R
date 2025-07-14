# ==============================================================================
# Title       : 01_data_cleaning.R
# Project     : Pregnancy Associated Mortality
# Description : Cleans raw data and saves processed data for analysis
# Author      : Jihyeon Kwon
# Created on  : 2025-05-18
# ==============================================================================

# load packages -----
library(tidyverse)
library(here)


# load function -----
source(here("fun/process_death_data.R"))


# years -----
years <- 2018:2023

# process data using the function -----
# save it to .csv
read_from <- paste0(here("data/raw/death-data/mortality-us-all-county-"), years, ".txt")
save_to <- paste0(here("data/processed/preg-mortality-us-all-county-"), years, ".csv")

for (y in seq_along(years)) {
  process_death_data(
    read_from = read_from[y],
    save_to = save_to[y],
    year = years[y]
  )
}




# read in data for all years and combine -----

death <- map_dfr(save_to, ~ read_csv(.x, col_types = cols(sex = col_character())))


# aggregate by year and residence county -----

# by year and county only
death_aggr1 <- death %>%
  select(year, r_cy_fips, suicide, homicide, drug) %>% 
  group_by(year, r_cy_fips) %>%
  summarise(
    suicide_sum = sum(suicide, na.rm = TRUE),
    homicide_sum = sum(homicide, na.rm = TRUE),
    drug_sum = sum(drug, na.rm = TRUE),
    .groups = "drop"
  )

save(death_aggr1, file = here("data/processed/death-year-cty.Rdata"))

# also by raceth
death_aggr2 <- death %>%
  select(year, r_cy_fips, raceth, suicide, homicide, drug) %>% 
  group_by(year, r_cy_fips, raceth) %>%
  summarise(
    suicide_sum = sum(suicide, na.rm = TRUE),
    homicide_sum = sum(homicide, na.rm = TRUE),
    drug_sum = sum(drug, na.rm = TRUE),
    .groups = "drop"
  )

save(death_aggr2, file = here("data/processed/death-year-cty-race.Rdata"))
