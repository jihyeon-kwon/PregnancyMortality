# ==============================================================================
# Title       : 05_combine_data.R
# Project     : Pregnancy Mortality
# Description : Create data with both Y and n (Pregnant & All Women)
# Author      : Jihyeon Kwon
# Created on  : 2025-07-15
# ==============================================================================



# library -----
library(sf)
library(here)
library(tidyverse)




# read in data -----
load(here("data/processed/pop-state-acs5-2023.Rdata")) # women aged 10-44 pop 2023
load(here("data/processed/death-year-state.Rdata")) # death (labeled with preg status) of either female or pregnant
load(here("data/processed/birth-year-state.Rdata")) # births by aged 10-44 (15-44 for 2018-2019)
load(here("data/processed/state-shape-file.Rdata")) # shape file for state 2023





# get geom info -----
adj <- poly2nb(pl = state_sf, row.names = state_sf$state_fips, queen = TRUE)
attr(adj, "class") <- "nb"
attr(adj, "region.id") <- state_sf$state_fips
attr(adj, "call") <- match.call()
attr(adj, "type") <- "queen"
attr(adj, "sym") <- TRUE

adj




# data for mortality among pregnant people aged 10-44 -----
colnames(death_aggr2)
unique(death_aggr2$preg)

Y_preg <- death_aggr2 %>%
  filter(preg == "Y") %>%
  filter(state_fips %in% state_sf$state_fips) # vary by year
dim(Y_preg) # this should have 49 * 6 rows but it doesn't which means that there was a state with 0 observations

n_preg <- birth_aggr2 %>%
  filter(state_fips %in% state_sf$state_fips) # vary by year
dim(n_preg) # this has 49 * 6 rows

# merge data so that both can have complete rows
Preg <- n_preg %>%
  left_join(Y_preg, by = c("state_fips", "year")) %>%
  mutate(across(c(suicide_sum, homicide_sum, drug_sum, birth), ~replace_na(.x, 0)))

dim(Preg)


# compare this to the JAMA table

state_name <- state_sf[, c("state_name", "state_fips")] %>%
  st_drop_geometry()

Y_preg %>%
  filter(year < 2023) %>%
  group_by(state_fips) %>%
  summarize(sum = sum(homicide_sum, suicide_sum, drug_sum)) %>%
  left_join(state_name, by = "state_fips") %>%
  arrange(state_name) %>%
  View() # almost the same but not identical





# data for mortality among women aged 10-44 -----
Y_all <- death_aggr2 %>%
  filter(state_fips %in% state_sf$state_fips) %>%
  group_by(year, state_fips) %>% # vary by year
  summarize(
    suicide_sum = sum(suicide_sum, na.rm = TRUE),
    homicide_sum = sum(homicide_sum, na.rm = TRUE),
    drug_sum = sum(drug_sum, na.rm = TRUE)
  )
dim(Y_all) # total 49*6 rows so complete dataset

n_all <- pop_state %>%
  filter(GEOID %in% state_sf$state_fips) %>%
  filter(group == "Overall") %>%
  select(state_fips = GEOID, pop) # just pop for 2023

All <- Y_all %>% 
  left_join(n_all, by = "state_fips")





# save
save(Preg, All, adj, state_sf,
     file = here("data/processed/state-data-for-modeling.Rdata"))
