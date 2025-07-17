# ==============================================================================
# Title       : 04_get_shape_files.R
# Project     : Pregnancy Mortality
# Description : Save State-level shapefile as .Rdata format
# Author      : Jihyeon Kwon
# Created on  : 2025-07-15
# ==============================================================================

# setup -----
library(tigris)
library(sf)


# download state-level shape file -----
state_sf <- states(year = 2023) %>%
  st_as_sf() %>%
  mutate(state_fips = sprintf("%02d", as.integer(STATEFP))) %>%
  filter(state_fips %in% c(sprintf("%02d", c(1:56)))) %>% # keep only 01–56
  filter(!state_fips %in% c("02", "15")) %>% # exclude Alaska (02) and Hawaii (15)
  select(state_name = NAME, state_fips, geometry)

plot(state_sf)

# save as .Rdata file -----
save(state_sf, file = here("data/processed/state-shape-file.Rdata"))