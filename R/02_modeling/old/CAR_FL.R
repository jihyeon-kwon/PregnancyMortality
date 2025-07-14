# ==============================================================================
# Title       : CAR.R
# Project     : Pregnancy Associated Mortality
# Description : CAR Models
# Author      : Jihyeon Kwon
# Created on  : 2025-06-23
# ==============================================================================

# lead the function
source(here("fun/run_CAR.R"))

### There seems to be error with the RSTr package when fitting CAR and MCAR?..


# 1. South
state_fips_vec <- c(
  "01", "05", "10", "11", "12", "13", "21", "22",
  "24", "28", "37", "40", "45", "47", "48", "51", "54"
)
file_suffix <- "south"


# 2. SC & NC
state_fips_vec <- c("37", "45")
file_suffix <- "carolina"


# 3. CA
state_fips_vec <- "06"
file_suffix <- "ca"

# 4. FL
state_fips_vec <- "12"
file_suffix <- "fl"

# 5. NY
state_fips_vec <- "36"
file_suffix <- "ny"