# ==============================================================================
# Title       : CAR_state.R
# Project     : Pregnancy Associated Mortality
# Description : CAR Models
# Author      : Jihyeon Kwon
# Created on  : 07/08/2025
# ==============================================================================


# library -----
library(tigris)
library(tidyverse)
library(sf)
library(spdep)
library(RSTr)
library(here)
library(viridis)

# read in data -----
load(here("data/processed/preg-mortality-merged-state-sf-2018-2023.Rdata"))

# get geom info -----
# create sf objects for contiguous US
contiguous_states <- setdiff(sprintf("%02d", 1:56), c("02", "15", "72", "78"))  # Exclude AK, HI, PR, VI

# Geometry for adjacency
state_geom <- data_state_sf %>%
  filter(year == min(year),
         state_fips %in% contiguous_states) %>%
  distinct(state_fips, .keep_all = TRUE) %>%
  st_as_sf()

adj <- poly2nb(pl = state_geom, row.names = state_geom$state_fips, queen = TRUE)
attr(adj, "class") <- "nb"
attr(adj, "region.id") <- state_geom$state_fips
attr(adj, "call") <- match.call()
attr(adj, "type") <- "queen"
attr(adj, "sym") <- TRUE

# prep data for CAR -----

# Filter data for selected states using FIPS
data <- data_state_sf %>%
  filter(state_fips %in% contiguous_states)

# Prepare data
df <- data[, c("state_fips", "year", 
               "homicide_sum", "suicide_sum", "drug_sum",
               "total_sum", "birth")] %>%
  st_drop_geometry()


# fit CAR -----

counts <- c("homicide_sum", "suicide_sum", "drug_sum")
years <- 2018:2023

for (count_var in counts) {
  for (y in seq_along(years)) {
    
    # Subset data to current year
    data_y <- list()
    data_y$Y <- df[[count_var]][df$year == years[y]]
    data_y$n <- df$birth[df$year == years[y]]
    
    # Initialize model
    name <- paste0("CAR_", count_var, "_", years[y])
    initialize_model(
      name = name,
      data = data_y,
      adjacency = adj,
      model = "ucar",
      A = Inf
    )
    
    # Run sampler and extract medians
    run_sampler(name = name, iterations = 10000)
    output <- load_samples(name = name, burn = 4000)
    medians <- get_medians(output)
    
    # Attach estimates to geometry and plot
    est <- medians * 1e5
    state_geom$est <- est
    
    p <- ggplot(state_geom) +
      geom_sf(aes(fill = est)) +
      scale_fill_viridis_c(na.value = "gray90") +
      labs(
        title = paste("Estimated", count_var, "rate in", years[y]),
        fill = "Rate per 100,000"
      ) +
      theme_minimal()
    
    # Save plot
    ggsave(
      filename = paste0("results/CAR/CAR_state_", count_var, "_", years[y], ".png"),
      plot = p,
      width = 10, height = 6, dpi = 300
    )
    
  }
}





# fit restricted CAR -----

counts <- c("homicide_sum", "suicide_sum", "drug_sum")
years <- 2018:2023

for (count_var in counts) {
  for (y in seq_along(years)) {
    
    # Subset data to current year
    data_y <- list()
    data_y$Y <- df[[count_var]][df$year == years[y]]
    data_y$n <- df$birth[df$year == years[y]]
    
    # Initialize model
    name <- paste0("CAR_", count_var, "_", years[y])
    initialize_model(
      name = name,
      data = data_y,
      adjacency = adj,
      model = "ucar",
      A = 6 #### THIS IS THE DIFFERENCE
    )
    
    # Run sampler and extract medians
    run_sampler(name = name, iterations = 10000)
    output <- load_samples(name = name, burn = 4000)
    medians <- get_medians(output)
    
    # Attach estimates to geometry and plot
    est <- medians * 1e5
    state_geom$est <- est
    
    p <- ggplot(state_geom) +
      geom_sf(aes(fill = est)) +
      scale_fill_viridis_c(na.value = "gray90") +
      labs(
        title = paste("Estimated", count_var, "rate in", years[y]),
        fill = "Rate per 100,000"
      ) +
      theme_minimal()
    
    # Save plot
    ggsave(
      filename = paste0("results/CAR/ResCAR_state_", count_var, "_", years[y], ".png"),
      plot = p,
      width = 10, height = 6, dpi = 300
    )
    
  }
}