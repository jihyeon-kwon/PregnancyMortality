# ==============================================================================
# Title       : 04_eda_crude_rates.R
# Project     : Pregnancy Associated Mortality
# Description : Crude mortality rates by year, county, and race
# Author      : Jihyeon Kwon
# Created on  : 2025-06-23
# ==============================================================================

# load packages -----
library(tidyverse)
library(sf)
library(here)
library(tigris)



# read in data -----
load(here("data/processed/preg-mortality-merged-cnty-2018-2023.Rdata"))
load(here("data/processed/preg-mortality-merged-state-2018-2023.Rdata"))


################
# County-level #
################

# map 1: all counties in contiguous U.S. -----

# filter for contiguous U.S. and plot with facet_wrap
contiguous_states <- setdiff(sprintf("%02d", 1:56), c("02", "15", "72", "78"))  # Exclude AK, HI, PR, VI

# get sf
sf_cnty <- counties(year = 2023) 

# ensure your FIPS vector is character
nrows <- dim(data_cnty)[[1]] / 6
fips_vector <- as.character(data_cnty$r_cy_fips[1:nrows])

# reorder `sf_cnty` to match your FIPS vector
sf_cnty <- sf_cnty[match(fips_vector, sf_cnty$GEOID), ]

# confirm they're aligned
all.equal(fips_vector, sf_cnty$GEOID)  # Should return TRUE

# get sf for just contiguous US
sf_cnty <- sf_cnty %>%
  filter(STATEFP %in% contiguous_states) %>%
  rename(r_cy_fips = GEOID)

# get data for just contiguous US
data_cnty <- data_cnty %>%
  filter(substr(r_cy_fips, 1, 2) %in% contiguous_states)

# add geometry by left_join on r_cy_fips
data_cnty <- left_join(data_cnty, sf_cnty %>% st_drop_geometry(), by = "r_cy_fips") %>%
  left_join(sf_cnty, by = "r_cy_fips")  # this adds back the geometry

# convert to sf object
data_cnty <- st_as_sf(data_cnty)

# # map
# p <- data_cnty %>%
#   ggplot(aes(fill = log(crude_rate))) +
#   geom_sf(size = 0.05) +
#   scale_fill_viridis_c(na.value = "white") +
#   labs(
#     title = "Log Crude Rate by County (2018–2023)",
#     fill = "Log Crude Rate"
#   ) +
#   facet_wrap(~year) +
#   theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "white", color = NA),
#     plot.background = element_rect(fill = "white", color = NA)
#   )
# 
# # save as PNG
# ggsave(
#   filename = "results/eda/log-crude-rate-map-all.png",
#   plot = p,
#   width = 12, height = 8, dpi = 300
# )


# map 2: top four populous states -----

# define state FIPS codes for NY, CA, FL, TX
state_fips_map <- c("NY" = "36", "CA" = "06", "FL" = "12", "TX" = "48")

# loop through each selected state and save the faceted map
for (state_name in names(state_fips_map)) {
  fips_code <- state_fips_map[state_name]
  
  # subset data for the state
  data_state <- data_cnty %>%
    filter(substr(r_cy_fips, 1, 2) == fips_code)
  
  # subset geometry for the state
  sf_state <- sf_cnty %>%
    filter(substr(r_cy_fips, 1, 2) == fips_code)
  
  # create map
  p_state <- data_state %>%
    ggplot(aes(fill = log(crude_rate))) +
    geom_sf(size = 0.1, color = "white") +
    scale_fill_viridis_c(na.value = "white") +
    labs(
      title = paste("Log Crude Rate by County in", state_name, "(2018–2023)"),
      fill = "Log Crude Rate"
    ) +
    facet_wrap(~year, drop = TRUE) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # save plot
  ggsave(
    filename = paste0("results/eda/log-crude-rate-map-", tolower(state_name), ".png"),
    plot = p_state,
    width = 10, height = 6, dpi = 300
  )
}




# map 3: South (definition by CDC) -----
# define FIPS codes for the Southern region (including DC = "11")
south_fips <- c(
  "01", "05", "10", "11", "12", "13", "21", "22",
  "24", "28", "37", "40", "45", "47", "48", "51", "54"
)

# filter and create map
p_south <- data1 %>%
  filter(substr(r_cy_fips, 1, 2) %in% south_fips) %>%
  ggplot(aes(fill = log(crude_rate))) +
  geom_sf(size = 0.05) +
  scale_fill_viridis_c(na.value = "white") +
  labs(
    title = "Log Crude Rate by County in the Southern U.S. (2018–2023)",
    fill = "Log Crude Rate"
  ) +
  facet_wrap(~year) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# save the map
ggsave(
  filename = "results/eda/log-crude-rate-map-south.png",
  plot = p_south,
  width = 12, height = 8, dpi = 300
)
