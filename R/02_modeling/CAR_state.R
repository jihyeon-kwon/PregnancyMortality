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
load(here("data/processed/state-data-for-modeling.Rdata"))


# 1. CAR Model for Pregnant Mortality -----

causes <- c("homicide_sum", "suicide_sum", "drug_sum")
years <- 2018:2023
iter <- 5e4
burn <- 1e4

for (cause_var in causes) {
  for (y in seq_along(years)) {
    
    # Subset data to current year
    data <- list()
    data$Y <- unlist(Preg[Preg$year == years[y], cause_var])
    data$n <- unlist(Preg[Preg$year == years[y], "birth"])
    
    # Initialize model
    name <- paste0("CAR_Preg_", cause_var, "_", years[y])
    initialize_model(
      name = name,
      dir = here(),
      data = data,
      adjacency = adj,
      model = "ucar",
      A = Inf 
    )
    
    # Run sampler and extract medians
    run_sampler(name = name, dir = here(), iterations = iter)
    output <- load_samples(name = name, 
                           dir = here(),
                           param = c("theta", "Z", "beta", "tau2", "sig2"),
                           burn = 1000)
    output <- load_samples(name = name, 
                           dir = here(),
                           param = "Z",
                           burn = 1000)
    output <- 
    save(output, )
    
  }
}
