# ==============================================================================
# Title       : 03_combined_data.R
# Project     : Pregnancy Associated Mortality
# Description : Combine Death Data for All Women Aged 10-44
# Author      : Jihyeon Kwon
# Created on  : 2025-06-23
# ==============================================================================

# load packages -----
library(tidyverse)
library(sf)
library(here)



# read in data and process -----
load(here("data/processed/pop-county-acs5-2023.Rdata"))
load(here("data/processed/death-year-cty.Rdata"))

# filter 'Overall' and expand to multiple years while keeping geometry
pop_county1 <- pop_county %>%
  filter(group == "Overall") %>%
  rename(r_cy_fips = GEOID) %>%
  st_as_sf() %>%  # ensure it's an sf object
  slice(rep(1:n(), each = length(2018:2023))) %>%
  mutate(year = rep(2018:2023, times = nrow(.) / length(2018:2023)))

# merge with death data
data1 <- pop_county1 %>%
  left_join(death_aggr1, by = c("r_cy_fips", "year")) %>%
  mutate(total_sum = if_else(
    is.na(suicide_sum) | is.na(homicide_sum) | is.na(drug_sum),
    0,
    suicide_sum + homicide_sum + drug_sum
  )) %>%
  relocate(total_sum, .after = drug_sum) %>%
  mutate(crude_rate = case_when(
    pop > 0 ~ total_sum / pop * 100000,
    TRUE ~ NA_real_
  )) %>%
  relocate(crude_rate, .after = total_sum)


save(data1, file = here("data/processed/mortality-merged-cruderate-2018-2023.Rdata"))