# ==============================================================================
# Title       : process_death_data.R
# Project     : Pregnancy Associated Mortality
# Description : Convert .TXT file to Rdata that only includes relevant data
# Author      : Jihyeon Kwon
# Created on  : 2025-06-23
# ==============================================================================

process_death_data <- function(read_from, save_to, year) {
  # ICD code for causes -----
  homicide_codes <- c(paste0("X", 85:100), paste0("Y0", 0:9))
  suicide_codes <- c(paste0("X", 60:84), "Y87.0", "U03")
  drug_codes <- c(
    paste0("X", 40:44),
    paste0("Y", 10:14),
    "D52.1", "D59.0", "D59.2", "D61.1", "D64.2",
    "E06.4", "E16.0", "E23.1", "E24.2", "E27.3", "E66.1",
    "G21.1", "G24.0", "G25.1", "G25.4", "G25.6", "G44.4", "G62.0", "G72.0",
    "I95.2",
    paste0("J70.", 2:4),
    "K85.3",
    "L10.5", "L27.0", "L27.1",
    "M10.2", "M32.0", "M80.4", "M81.4", "M83.5", "M87.1",
    "R50.2",
    paste0("R78.", 1:5),
    paste0("F11.", c(0:5, 7:9)),
    paste0("F12.", c(0:5, 7:9)),
    paste0("F13.", c(0:5, 7:9)),
    paste0("F14.", c(0:5, 7:9)),
    paste0("F15.", c(0:5, 7:9)),
    paste0("F16.", c(0:5, 7:9)),
    paste0("F18.", c(0:5, 7:9)),
    paste0("F19.", c(0:5, 7:9))
  )


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
  data <- read.delim(read_from, sep = "\t", header = F, stringsAsFactors = F)

  # Empty data to store relevant mortality information
  death <- data.frame(matrix(ncol = 0, nrow = nrow(data)))

  # Extract year and month
  death$year <- as.numeric(substr(data$V1, 102, 105))
  death$month <- as.numeric(substr(data$V1, 65, 66))

  # Create quarter variable
  death <- death %>%
    dplyr::mutate(quarter = dplyr::case_when(
      month >= 1 & month <= 3 ~ 1,
      month >= 4 & month <= 6 ~ 2,
      month >= 7 & month <= 9 ~ 3,
      month >= 10 & month <= 12 ~ 4
    ))

  # Manner of Death
  death$manner <- as.numeric(substr(data$V1, 107, 107))


  # Occurence data -----

  # FIPS code for states
  death$o_abstate <- substr(data$V1, 21, 22)
  death <- dplyr::left_join(death, state_fips, by = c("o_abstate" = "state")) %>%
    dplyr::rename(o_st_fips = state_code)

  # FIPS code for county (5 digits)
  death$o_cy_fips <- as.character(paste0(death$o_st_fips, as.character(substr(data$V1, 23, 25))))


  # Residence data -----

  # FIPS code for states
  death$r_abstate <- substr(data$V1, 29, 30)
  death <- dplyr::left_join(death, state_fips, by = c("r_abstate" = "state")) %>%
    dplyr::rename(r_st_fips = state_code)

  # FIPS code for county (5 digits)
  death$r_cy_fips <- as.character(paste0(death$r_st_fips, as.character(substr(data$V1, 35, 37))))


  # If residential is missing, then use occurence
  death$r_cy_fips[is.na(death$r_cy_fips)] <- death$o_cy_fips[is.na(death$r_cy_fips)]


  # Get Pregnancy -----
  death$preg <- substr(data$V1, 143, 143)


  # The Decendent -----

  death$sex <- substr(data$V1, 69, 69)
  death$age_group <- as.numeric(substr(data$V1, 77, 78))

  # race information: coding differ by year
  if (year >= 2003 & year <= 2020) {
    death$hispanic <- as.numeric(substr(data$V1, 488, 488))
    death$race <- as.numeric(substr(data$V1, 489, 490))
    death$race4cat <- as.numeric(substr(data$V1, 450, 450)) # 1 : White 2: Black 3: American Indian 4: AAPI

    # Create Hispanic indicator and 5-category race/ethnicity variable -----
    death$hispind <- ifelse(death$hispanic >= 1 & death$hispanic <= 5, 1, 0)
    death$raceth <- death$race4cat
    death$raceth[death$hispind == 1] <- 5 # Overwrite if Hispanic  # 1 : White 2: Black 3: American Indian 4: AAPI 5: Hispanic
  } else if (year == 2021) {
    death$hispanic <- as.numeric(substr(data$V1, 484, 486))
    death$race <- as.numeric(substr(data$V1, 489, 490)) # this is the 40 category race var

    # Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
    death$hispind <- ifelse(death$hispanic >= 200 & death$hispanic < 996, 1, 0)
    death <- death %>% mutate(raceth = case_when(
      race == 1 & hispind == 0 ~ 1, # NHW
      race %in% c(2, 15:18, 25:30, 35:39) & hispind == 0 ~ 2, # NHB
      race %in% c(3, 19:21, 31:33, 40) & hispind == 0 ~ 3, # NHAIAN
      race %in% c(4:14, 22:24, 34) & hispind == 0 ~ 4, # NHAPI
      hispind == 1 ~ 5 # Hisp
    ))
  } else if (year >= 2022) {
    death$hispanic <- as.numeric(substr(data$V1,484,486))
    death$race <- as.numeric(substr(data$V1,489,490)) # this is the 40 category race var
    
    # Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
    death$hispind <- ifelse(death$hispanic>=200 & death$hispanic<996,1,0)
    death<- death %>% mutate(raceth=case_when(
      race==1 & hispind==0 ~ 1, # NHW
      race %in% c(2,15:18,25:30,35:39) & hispind==0 ~ 2, #NHB
      race %in% c(3,19:21,31:33,40) & hispind==0 ~ 3, #NHAIAN
      race %in% c(4:14,22:24,34) & hispind==0 ~ 4, #NHAPI
      hispind==1 ~ 5 #Hisp
    ))
  } else {
    stop("Year not recognized. Please check the year input.")
  }



  # Underlying Cause -----

  death$ICD10 <- trimws(substr(data$V1, 146, 149)) # remove blank space
  death$suicide <- ifelse(death$ICD10 %in% suicide_codes, 1, 0)
  death$homicide <- ifelse(death$ICD10 %in% homicide_codes, 1, 0)
  death$drug <- ifelse(death$ICD %in% drug_codes, 1, 0)


  # Subset data -----
  # preg: 2, 3, or 4
  # age: 10 - 44 (thus code 8-14)

  preg_death <- death %>%
    filter(preg %in% c(2, 3, 4) &
      age_group %in% 8:14)


  # save data -----
  write.csv(preg_death, file = save_to, row.names = FALSE)
}
