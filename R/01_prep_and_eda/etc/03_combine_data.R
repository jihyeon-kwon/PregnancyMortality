# ==============================================================================
# Title       : 03_combined_data.R
# Project     : Pregnancy Associated Mortality
# Description : Crude mortality rates by year, county, and race
# Author      : Jihyeon Kwon
# Created on  : 2025-07-02
# ==============================================================================

# load packages -----
library(tidyverse)
library(sf)
library(here)
library(tigris)
options(tigris_use_cache = TRUE)



# read in data and process -----
load(here("data/processed/birth-year-cty.Rdata"))
load(here("data/processed/death-year-cty.Rdata"))

# Download counties shapefile for 2018 and expand
counties_sf <- counties(year = 2018)

counties_sf %>%
  filter(STATEFP == "09")
data <- counties_sf %>%
  st_drop_geometry() %>%
  select(STATEFP, GEOID, NAME) %>%
  rename(stfips = STATEFP, cntyfips = GEOID, cntyname = NAME) %>%
  crossing(year = 2018:2023)

# add birth count to the data
data <- data %>% 
  left_join(birth_aggr1, by = c("year", "cntyfips"))
data$birth <- ifelse(is.na(data$birth), 0, data$birth)

# match the variable names
data <- data %>%
  rename(r_cy_fips = cntyfips)

# get the county level data -----

# merge with death data
data_cnty <- data %>%
  left_join(death_aggr1, by = c("r_cy_fips", "year")) %>%
  mutate(total_sum = if_else(
    is.na(suicide_sum) | is.na(homicide_sum) | is.na(drug_sum),
    0,
    suicide_sum + homicide_sum + drug_sum
  )) %>%
  relocate(total_sum, .after = drug_sum) %>%
  mutate(crude_rate = case_when(
    birth > 0 ~ total_sum / birth,
    TRUE ~ NA_real_
  ))
data_cnty$suicide_sum <- ifelse(is.na(data_cnty$suicide_sum), 0, data_cnty$suicide_sum)
data_cnty$homicide_sum <- ifelse(is.na(data_cnty$homicide_sum), 0, data_cnty$homicide_sum)
data_cnty$drug_sum <- ifelse(is.na(data_cnty$drug_sum), 0, data_cnty$drug_sum)

# load counties shapefile again
counties_sf <- counties_sf %>%
  rename(r_cy_fips = GEOID)

# keep only counties in your data_cnty
data_cnty_sf <- right_join(
  counties_sf,                          # includes geometry
  data_cnty %>% st_drop_geometry(),     # drop any existing geometry
  by = "r_cy_fips"
) %>%
  st_as_sf()  # ensure sf class

save(data_cnty, file = here("data/processed/preg-mortality-merged-cnty-2018-2023.Rdata"))
save(data_cnty_sf, file = here("data/processed/preg-mortality-merged-cnty-sf-2018-2023.Rdata"))


# get the state level data -----

# add state_fips column from the first two digits of r_cy_fips
data_state <- data_cnty %>%
  mutate(state_fips = substr(r_cy_fips, 1, 2)) %>%
  group_by(state_fips, year) %>%
  summarise(
    birth = sum(birth, na.rm = TRUE),
    suicide_sum = sum(suicide_sum, na.rm = TRUE),
    homicide_sum = sum(homicide_sum, na.rm = TRUE),
    drug_sum = sum(drug_sum, na.rm = TRUE),
    total_sum = sum(total_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(crude_rate = case_when(
    birth > 0 ~ total_sum / birth,
    TRUE ~ NA_real_
  ))

# load states shapefile
states_sf <- states(year = 2023) %>%
  rename(state_fips = STATEFP)

# keep only states in your data_state
data_state_sf <- left_join(
  states_sf,                             # includes geometry
  data_state %>% st_drop_geometry(),     # just data
  by = "state_fips"
) %>%
  st_as_sf()  # ensure sf class

# Save state-level data
save(data_state, file = here("data/processed/preg-mortality-merged-state-2018-2023.Rdata"))
save(data_state_sf, file = here("data/processed/preg-mortality-merged-state-sf-2018-2023.Rdata"))