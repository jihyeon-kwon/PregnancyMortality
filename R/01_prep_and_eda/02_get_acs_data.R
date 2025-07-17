# ==============================================================================
# Title       : 02_get_acs_data.R
# Project     : Pregnancy Associated Mortality
# Description : Get the relevant population sizes for females in 10-44
# Author      : Jihyeon Kwon
# Created on  : 2025-06-22
# ==============================================================================

# load packages -----
library(tidyverse)
library(tidycensus)
library(here)



# Census API Key -----
census_api_key(
  key = "99d64071d02864e96e6455ed28f8f89ac7adee83",
  overwrite = TRUE,
  install = TRUE
  )

vars <- load_variables(2023, "acs5")

# get population data from Tidycensus -----

pop_state <- get_acs(
  geography = "state",
  survey = "acs5",
  variables = c(
    "B01001_001E",  # total population
    
    # Overall Female 10–44
    paste0("B01001_", sprintf("%03d", 29:38), "E"),
    
    # NH White Female 10–44
    paste0("B01001H_", sprintf("%03d", 20:26), "E"),
    
    # NH Black Female 10–44
    paste0("B01001B_", sprintf("%03d", 20:26), "E"),
    
    # AIAN Female 10–44
    paste0("B01001C_", sprintf("%03d", 20:26), "E"),
    
    # Asian Female 10–44
    paste0("B01001D_", sprintf("%03d", 20:26), "E"),
    
    # NHOPI Female 10–44
    paste0("B01001E_", sprintf("%03d", 20:26), "E"),
    
    # Hispanic Female 10–44
    paste0("B01001I_", sprintf("%03d", 20:26), "E")
  ),
  geometry = FALSE,
  year = 2023
)



pop_county <- get_acs(
  geography = "county",
  survey = "acs5",
  variables = c(
    "B01001_001E",  # total population
    
    # Overall Female 10–44
    paste0("B01001_", sprintf("%03d", 29:38)),
    
    # NH White Female 10–44
    paste0("B01001H_", sprintf("%03d", 20:26)),
    
    # NH Black Female 10–44
    paste0("B01001B_", sprintf("%03d", 20:26)),
    
    # AIAN Female 10–44
    paste0("B01001C_", sprintf("%03d", 20:26)),
    
    # Asian Female 10–44
    paste0("B01001D_", sprintf("%03d", 20:26)),
    
    # NHOPI Female 10–44
    paste0("B01001E_", sprintf("%03d", 20:26)),
    
    # Hispanic Female 10–44
    paste0("B01001I_", sprintf("%03d", 20:26))
  ),
  geometry = FALSE,
  year = 2023
)





# aggregate by ethnic group -----

# 1 : White 2: Black 3: American Indian 4: AAPI 5: Hispanic <- this is from death data

pop_state <- pop_state %>%
  filter(!variable %in% "B01001_001") %>% # remove total population for now
  mutate(
    group = case_when(
      variable %in% paste0("B01001_", sprintf("%03d", 29:38)) ~ "Overall",
      variable %in% paste0("B01001H_", sprintf("%03d", 20:26)) ~ "NH White",
      variable %in% paste0("B01001B_", sprintf("%03d", 20:26)) ~ "NH Black",
      variable %in% paste0("B01001C_", sprintf("%03d", 20:26)) ~ "AIAN",
      variable %in% paste0("B01001D_", sprintf("%03d", 20:26)) ~ "Asian",
      variable %in% paste0("B01001E_", sprintf("%03d", 20:26)) ~ "NHOPI",
      variable %in% paste0("B01001I_", sprintf("%03d", 20:26)) ~ "Hispanic"
    )
  ) %>%
  group_by(GEOID, NAME, group) %>%
  summarize(pop = sum(estimate, na.rm = TRUE), .groups = "drop")

# Q: Can I combine Asian + NHOPI and create AAPI?

pop_county <- pop_county %>%
  filter(!variable %in% "B01001_001") %>% # remove total population for now
  mutate(
    group = case_when(
      variable %in% paste0("B01001_", sprintf("%03d", 29:38)) ~ "Overall",
      variable %in% paste0("B01001H_", sprintf("%03d", 20:26)) ~ "NH White",
      variable %in% paste0("B01001B_", sprintf("%03d", 20:26)) ~ "NH Black",
      variable %in% paste0("B01001C_", sprintf("%03d", 20:26)) ~ "AIAN",
      variable %in% paste0("B01001D_", sprintf("%03d", 20:26)) ~ "Asian",
      variable %in% paste0("B01001E_", sprintf("%03d", 20:26)) ~ "NHOPI",
      variable %in% paste0("B01001I_", sprintf("%03d", 20:26)) ~ "Hispanic"
    )
  ) %>%
  group_by(GEOID, NAME, group) %>%
  summarize(pop = sum(estimate, na.rm = TRUE), .groups = "drop")



# save -----
# save this as .Rdata due to the spatial info.
save(
  pop_state,
  file = here("data/processed/pop-state-acs5-2023.Rdata")
  )

save(
  pop_county,
  file = here("data/processed/pop-county-acs5-2023.Rdata")
)

