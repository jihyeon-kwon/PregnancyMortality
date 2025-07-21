# ==============================================================================
# Title       : process_birth_data.R
# Project     : Pregnancy Associated Mortality
# Description : Convert .TXT file to Rdata that only includes relevant data
# Author      : Jihyeon Kwon
# Created on  : 2025-07-01
# ==============================================================================


process_birth_txt <- function(read_from, save_to, year) {
  
  # this one convert the .txt file to .Rdata
  
  # Prep for FIPS Code -----
  
  # Check for required package
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop("The 'tigris' package is required but not installed. Please install it using install.packages('tigris').")
  }
  
  # Load tigris for FIPS mapping
  library(tigris)
  options(tigris_use_cache = TRUE)
  
  # get the fips codes for states
  state_fips <- tigris::fips_codes %>%
    dplyr::select(state = state, state_code = state_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(state_code = sprintf("%02d", as.numeric(state_code)))
  
  
  # process the data -----
  
  # Load .txt file
  if (!grepl("\\.txt$", read_from, ignore.case = TRUE)) {
    warning("The input file does not have a .txt extension. Please ensure you are loading a .txt file.")
  } # ensure that it's in .txt format
  data <- read.delim(read_from, sep = "\t", header = F, stringsAsFactors = F) ########### CHANGE THIS LATER!!!
  
  # Empty data to store relevant mortality information
  birth <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  
  # Extract year and month
  birth$year <- as.numeric(substr(data$V1, 9, 12))
  birth$month <- as.numeric(substr(data$V1, 13, 14))
  
  # Create quarter variable
  birth <- birth %>%
    dplyr::mutate(quarter = dplyr::case_when(
      month >= 1 & month <= 3 ~ 1,
      month >= 4 & month <= 6 ~ 2,
      month >= 7 & month <= 9 ~ 3,
      month >= 10 & month <= 12 ~ 4
    ))
  
  # Manner of birth
  # IS ALL BIRTH DATA HERE LIVE BIRTH?
  
  
  # Occurence data -----
  
  # FIPS code for states
  birth$o_abstate <- substr(data$V1, 24, 25)
  birth <- dplyr::left_join(birth, state_fips, by = c("o_abstate" = "state")) %>%
    dplyr::rename(o_st_fips = state_code)
  
  # FIPS code for county (5 digits)
  birth$o_cy_fips <- as.character(paste0(birth$o_st_fips, as.character(substr(data$V1, 28, 30))))
  
  
  # Residence data -----
  
  # FIPS code for states
  birth$r_abstate <- substr(data$V1, 89, 90)
  birth <- dplyr::left_join(birth, state_fips, by = c("r_abstate" = "state")) %>%
    dplyr::rename(r_st_fips = state_code)
  
  # FIPS code for county (5 digits)
  birth$r_cy_fips <- as.character(paste0(birth$r_st_fips, as.character(substr(data$V1, 91, 93))))
  
  
  # If residential is missing, then use occurence
  birth$r_cy_fips[is.na(birth$r_cy_fips)] <- birth$o_cy_fips[is.na(birth$r_cy_fips)]
  
  
  # The Decendent -----
  birth$age <- as.numeric(substr(data$V1, 75, 76)) # single year: 12 (10-12 yr) / 13 (13 yr) / etc
  
  # race information: coding differ by year
  # add something here later
  
  
  # Subset data -----
  # age: 10 - 44 (thus code 12-44)
  
  preg_birth <- birth %>%
    filter(age %in% 12:44) %>%
    select(cntyfips = r_cy_fips,
           year)
  
  
  # save data -----
  write.csv(preg_birth, file = save_to, row.names = FALSE)
}







process_birth_sas <- function(read_from, save_to, year) {
  
  # this one convert the .txt file to .Rdata
  
  # Prep for FIPS Code -----
  
  # Check for required package
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop("The 'tigris' package is required but not installed. Please install it using install.packages('tigris').")
  }
  
  # Load tigris for FIPS mapping
  library(tigris)
  options(tigris_use_cache = TRUE)
  
  # get the fips codes for states
  state_fips <- tigris::fips_codes %>%
    dplyr::select(state = state, state_code = state_code) %>%
    dplyr::distinct() %>%
    dplyr::mutate(state_code = sprintf("%02d", as.numeric(state_code)))
  
  
  # process the data -----
  
  # Load haven for reading SAS files
  library(haven)
  
  # Check that file has .sas7bdat extension
  if (!grepl("\\.sas7bdat$", read_from, ignore.case = TRUE)) {
    warning("The input file does not have a .sas7bdat extension. Please ensure you are loading a SAS data file.")
  }
  
  # Read in the SAS file
  data <- read_sas(read_from)
  
  # Empty data to store relevant mortality information
  birth <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
  
  # Extract year and month
  birth$year <- data$DOB_YY
  birth$month <- data$DOB_MM
  
  # Create quarter variable
  birth <- birth %>%
    dplyr::mutate(quarter = dplyr::case_when(
      month >= 1 & month <= 3 ~ 1,
      month >= 4 & month <= 6 ~ 2,
      month >= 7 & month <= 9 ~ 3,
      month >= 10 & month <= 12 ~ 4
    ))
  
  # Manner of birth
  # IS ALL BIRTH DATA HERE LIVE BIRTH?
  
  
  # Occurence data -----
  
  # FIPS code for states
  birth$o_abstate <- data$OSTATE # I assume this is the occurrence?
  birth <- dplyr::left_join(birth, state_fips, by = c("o_abstate" = "state")) %>%
    dplyr::rename(o_st_fips = state_code)
  
  # FIPS code for county (5 digits)
  birth$o_cy_fips <- as.character(paste0(birth$o_st_fips, as.character(data$OCNTYFIPS)))
  
  
  # Residence data -----
  
  # FIPS code for states
  birth$r_abstate <- data$XMRSTATE # I assume this is the residence?
  birth <- dplyr::left_join(birth, state_fips, by = c("r_abstate" = "state")) %>%
    dplyr::rename(r_st_fips = state_code)
  
  # FIPS code for county (5 digits)
  birth$r_cy_fips <- as.character(paste0(birth$r_st_fips, sprintf("%03d", data$MRCNTYFIPS)))
  
  
  # If residential is missing, then use occurrence
  birth$r_cy_fips[is.na(birth$r_cy_fips)] <- birth$o_cy_fips[is.na(birth$r_cy_fips)]
  
  
  # The Decendent -----
  birth$age <- as.numeric(data$MAGER) # single year: 12 (10-12 yr) / 13 (13 yr) / etc
  
  # race information: coding differ by year
  # add something here later
  
  
  # Subset data -----
  # age: 10 - 44 (thus code 12-44)
  
  preg_birth <- birth %>%
    filter(age %in% 12:44) %>%
    select(cntyfips = r_cy_fips,
           year)
  
  
  # save data -----
  write.csv(preg_birth, file = save_to, row.names = FALSE)
}
