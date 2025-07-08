# ==============================================================================
# Title       : 01_birth_data_cleaning.R
# Project     : Pregnancy Associated Mortality
# Description : Cleans raw data and saves processed data for analysis
# Author      : Jihyeon Kwon
# Created on  : 2025-07-01
# ==============================================================================

# for now, I only need county FIPS code + year data.

# load packages -----
library(tidyverse)
library(here)
library(haven)

# load function -----
source(here("fun/process_birth_data.R"))


# years -----
years <- 2018:2023
save_to <- paste0(here("data/processed/birth-us-all-county-"), years, ".csv")


# 1. using the R.data files -----
# 2018 & 2019
years_R <- 2018:2019
read_from_R <- paste0(here("data/raw/birth-data/births_"), years_R, ".Rdata")

y <- 1
load(read_from_R[y])
colnames(births18a) # only need year and cnty fips for now
birth <- births18a[, c("year", "cntyfips")]
save(birth, file = save_to[y])
rm(birth)

y <- 2
load(read_from_R[2])
colnames(births19a) # only need year and cnty fips for now
birth <- births19a[, c("year", "cntyfips")]
save(birth, file = save_to[y])
rm(birth)

# 2. using the .txt file -----
y <- 3
process_birth_txt(
  read_from = here("data/raw/birth-data/NATL2020us.AllCnty.txt"),
  save_to = save_to[y],
  year = years[y]
)


# 3. using the SAS file -----
years_SAS <- 2021:2023
read_from_SAS <- paste0(here("data/raw/birth-data/usbirth"), years_SAS, ".sas7bdat")

for (y in 4:6) {
  process_birth_sas(
    read_from = read_from_SAS[y-3],
    save_to = save_to[y],
    year = years[y]
  )
}



# read in data for all years and combine -----

birth <- map_dfr(save_to, ~ read_csv(.x))


# aggregate by year and residence county -----

# by year and county only
birth_aggr1 <- birth %>%
  group_by(year, cntyfips) %>%
  summarise(
    birth = n(),
    .groups = "drop"
  )

save(birth_aggr1, file = here("data/processed/birth-year-cty.Rdata"))
# 
# # also by raceth
# birth_aggr2 <- birth %>%
#   select(year, r_cy_fips, raceth, suicide, homicide, drug) %>% 
#   group_by(year, r_cy_fips, raceth) %>%
#   summarise(
#     suicide_sum = sum(suicide, na.rm = TRUE),
#     homicide_sum = sum(homicide, na.rm = TRUE),
#     drug_sum = sum(drug, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# save(birth_aggr2, file = here("data/processed/birth-year-cty-race.Rdata"))
