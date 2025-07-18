# ==============================================================================
# Title       : MSTCAR_state.R
# Project     : Pregnancy Associated Mortality
# Description : MSTCAR: both group and temporal correlations
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
T <- 6 # 2018-2023

# 1. MCAR Model for Pregnant Mortality -----

causes <- c("homicide_sum", "suicide_sum", "drug_sum")
years <- 2018:2023
iter <- 6e4 # I want to increase it...
burn <- 1e4

for (cause_var in causes) {
  # Subset data to current year
  data <- list()
  data$Y <- data$n <- array(NA, dim = c(I, K, T))
  
  for (t in 1:T) {
    data$Y[, 1, t] <- unlist(Preg[Preg$year == years[t], cause_var])
    data$n[, 1, t] <- unlist(Preg[Preg$year == years[t], "birth"])
    data$Y[, 2, t] <- unlist(All[All$year == years[t], cause_var]) -
      unlist(Preg[Preg$year == years[t], cause_var])
    data$n[, 2, t] <- unlist(All[All$year == years[t], "pop"]) -
      unlist(Preg[Preg$year == years[t], "birth"])
  }
  
  dimnames(data$Y)[[1]] <- dimnames(data$n)[[1]] <- state_sf$state_name
  dimnames(data$Y)[[2]] <- dimnames(data$n)[[2]] <- c("Preg", "All")
  dimnames(data$Y)[[3]] <- dimnames(data$n)[[3]] <- years

  # Initialize model
  name <- paste0("MSTCAR_", cause_var)
  initialize_model(
    name = name,
    dir = here("results/output/"),
    method = "pois",
    data = data,
    adjacency = adj,
    model = "mstcar",
    rho_up = TRUE,
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
  param_names <- c("theta", "Z", "beta", "tau2", "G", "Ag", "rho")

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
    file = here("results/MSTCAR/output", paste0(name, "_results.Rdata"))
  )

  # Remove the duplicate (original samples)
  if (dir.exists(paste0(here("results/output/"), name))) {
    unlink(paste0(here("results/output/"), name), recursive = TRUE)
  }
}
