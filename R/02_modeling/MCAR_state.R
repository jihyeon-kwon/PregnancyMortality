# ==============================================================================
# Title       : CAR_state.R
# Project     : Pregnancy Associated Mortality
# Description : CAR Models: Preg ppl vs non-preg women 
#               (acconting for corr between two groups)
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
I <- 49
K <- 2

# 1. MCAR Model for Pregnant Mortality -----

causes <- c("homicide_sum", "suicide_sum", "drug_sum")
years <- 2018:2023
iter <- 6e4 # I want to increase it...
burn <- 1e4

for (cause_var in causes) {
  for (y in seq_along(years)) {
    
    # Subset data to current year
    data <- list()
    data$Y <- data$n <- matrix(NA, I, K)
    data$Y[, 1] <- unlist(Preg[Preg$year == years[y], cause_var])
    data$n[, 1] <- unlist(Preg[Preg$year == years[y], "birth"])
    data$Y[, 2] <- unlist(All[All$year == years[y], cause_var]) - 
      unlist(Preg[Preg$year == years[y], cause_var])
    data$n[, 2] <- unlist(All[All$year == years[y], "pop"]) -
      unlist(Preg[Preg$year == years[y], "birth"])

    dimnames(data$Y)[[1]] <- dimnames(data$n)[[1]] <- state_sf$state_name
    dimnames(data$Y)[[2]] <- dimnames(data$n)[[2]] <- c("Pregnant", "NotPregnant")
    
    # Initialize model
    name <- paste0("MCAR_", cause_var, "_", years[y])
    initialize_model(
      name = name,
      dir = here("results/output/"),
      method = "pois",
      data = data,
      adjacency = adj,
      model = "mcar",
      A = Inf
    )

    # Run sampler and extract medians
    run_sampler(
      name = name,
      dir = here("results/output/"),
      .show_plots = FALSE,
      # iteration = iter # this doesn't work?
    )

    # Save each Output
    param_names <- c("theta", "Z", "beta", "tau2", "G")

    for (param in param_names) {
      output_obj <- load_samples(
        name = name,
        dir = here("results/output/"),
        param = param,
        burn = burn
      )
      assign(paste0("output_", param), output_obj)
      assign(paste0("medians_", param), get_medians(output_obj))
    }

    save(
      list = c(
        paste0("output_", param_names),
        paste0("medians_", param_names)
      ),
      file = here("results/MCAR/output", paste0(name, "_results.Rdata"))
    )
    
    # Remove the duplicate (original samples)
    if (dir.exists(paste0(here("results/output/"), name))) {
      unlink(paste0(here("results/output/"), name), recursive = TRUE)
    }
  }
}
