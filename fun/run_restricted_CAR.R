library(tidyverse)
library(sf)
library(spdep)
library(here)
library(tigris)
library(RSTr)

# Load merged data
load(here("data/processed/mortality-merged-cruderate-2018-2023.Rdata"))

# Filter data for selected states using FIPS
data_state <- data1 %>%
  filter(substr(r_cy_fips, 1, 2) %in% state_fips_vec)

# Prepare Y array
df_y <- data_state %>%
  mutate(group = "all") %>%
  select(r_cy_fips, year, group, total_sum) %>%
  pivot_wider(names_from = group, values_from = total_sum)

counties <- sort(unique(df_y$r_cy_fips))
years <- sort(unique(df_y$year))

Y_array <- array(NA_real_,
  dim = c(length(counties), 1, length(years)),
  dimnames = list(county = counties, group = "all", year = years)
)

for (i in seq_along(years)) {
  year_i <- years[i]
  temp <- df_y %>% filter(year == year_i)
  Y_array[match(temp$r_cy_fips, counties), 1, i] <- temp$all
}

# Prepare population array
df_n <- data_state %>%
  mutate(group = "all") %>%
  select(r_cy_fips, year, group, pop) %>%
  pivot_wider(names_from = group, values_from = pop)

n_array <- array(NA_real_,
  dim = c(length(counties), 1, length(years)),
  dimnames = list(county = counties, group = "all", year = years)
)

for (i in seq_along(years)) {
  year_i <- years[i]
  temp <- df_n %>% filter(year == year_i)
  n_array[match(temp$r_cy_fips, counties), 1, i] <- temp$all
}

state_list <- list(Y = Y_array, n = n_array)

# Geometry for adjacency
state_geom <- data_state %>%
  filter(year == min(year)) %>%
  distinct(r_cy_fips, .keep_all = TRUE) %>%
  st_as_sf()

adj <- poly2nb(pl = state_geom, row.names = state_geom$r_cy_fips, queen = TRUE)
attr(adj, "class") <- "nb"
attr(adj, "region.id") <- state_geom$r_cy_fips
attr(adj, "call") <- match.call()
attr(adj, "type") <- "queen"
attr(adj, "sym") <- TRUE

# File naming
if (is.null(file_suffix)) {
  file_tag <- paste(state_fips_vec, collapse = "_")
} else {
  file_tag <- file_suffix
}
model_name <- paste0("CAR_", file_tag)

# Run CAR model
initialize_model(name = model_name, data = state_list, adjacency = adj, A = 6)
run_sampler(name = model_name)
output <- load_samples(name = model_name, burn = 2000)
medians <- get_medians(output) * 100000

# Save results
save(output, medians, file = here("results", "CAR", paste0(model_name, "_A6_results.Rdata")))

# Get shapefiles using FIPS codes
fips_to_abbr <- tigris::fips_codes %>%
  filter(state_code %in% state_fips_vec) %>%
  distinct(state_code, state) %>%
  pull(state)

state_shapes <- map_dfr(fips_to_abbr, ~ counties(state = .x, year = 2023, cb = TRUE) %>% st_as_sf())

# Prepare medians for plotting
years <- dimnames(medians)[[3]]
medians_long <- lapply(years, function(y) {
  data.frame(
    GEOID = dimnames(medians)[[1]],
    value = medians[, , y],
    year = y
  )
}) %>% bind_rows()

if (ncol(medians_long) == 3) {
  colnames(medians_long)[2] <- "value"
}

shape_long <- state_shapes %>%
  left_join(medians_long, by = "GEOID")

# Plot
# Plot
p <- ggplot(shape_long) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(name = "Rate per 100,000") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white")
  ) +
  facet_wrap(~year) +
  ggtitle(paste0("CAR Estimates for FIPS: ", paste(state_fips_vec, collapse = ", ")))

# Save plot
ggsave(
  filename = here("results", "CAR", paste0(model_name, "_A6_map.png")),
  plot = p, width = 10, height = 8, dpi = 300
)
