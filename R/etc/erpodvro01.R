# NCGVR Project
# ERPO DVRO Analysis

library(dplyr)
library(haven)

setwd("/Users/jaquelynjahn/Library/CloudStorage/Dropbox/NCGVR ERPO grant/Analysis")

###### All gun-related deaths#####
###### Pulling in data from 2014-2019
load("~/Dropbox/Bail Project/Data/NCHS data/Deaths/gundeaths_merged.Rdata")

##### Reading in the 2020 .txt file
deaths20<-read.delim("/Users/jaquelynjahn/Library/CloudStorage/Dropbox/Bail Project/Data/NCHS data/Deaths/MULT2020.USPSAllCnty/MULT2020.USAllCnty.txt", sep="\t", header=F, stringsAsFactors = F)

# Data year
deaths20$year<-as.numeric(substr(deaths20$V1, 102,105))

# Death month
deaths20$month <- as.numeric(substr(deaths20$V1,65,66))

# Death quarter
deaths20<-deaths20 %>% mutate(quarter=case_when(
  month>=1 & month<=3 ~ 1,
  month>=4 & month<=6 ~ 2,
  month>=7 & month<=9 ~ 3,
  month>=10 & month<=12 ~ 4,
))

# Manner of death
deaths20$manner<-as.numeric(substr(deaths20$V1, 107,107))

### Occurrence
# State Abrev. Name - Occurrence
deaths20$o_abstate <- substr(deaths20$V1,21,22)

# State FIPS - Occurrence 
deaths20<- deaths20 %>% mutate(
  o_st_fips=case_when(
    o_abstate == "AL" ~ 1,
    o_abstate == "AK" ~ 2,
    o_abstate == "AZ" ~ 4,
    o_abstate == "AR" ~ 5,
    o_abstate == "AR" ~ 5,
    o_abstate == "CA" ~ 6,
    o_abstate == "CO" ~ 8,
    o_abstate == "CT" ~ 9,
    o_abstate == "DE" ~ 10,
    o_abstate == "DC" ~ 11,
    o_abstate == "FL" ~ 12,
    o_abstate == "GA" ~ 13,
    o_abstate == "HI" ~ 15,
    o_abstate == "ID" ~ 16,
    o_abstate == "IL" ~ 17,
    o_abstate == "IN" ~ 18,
    o_abstate == "IA" ~ 19,
    o_abstate == "KS" ~ 20,
    o_abstate == "KY" ~ 21,
    o_abstate == "LA" ~ 22,
    o_abstate == "ME" ~ 23,
    o_abstate == "MD" ~ 24,
    o_abstate == "MA" ~ 25,
    o_abstate == "MI" ~ 26,
    o_abstate == "MN" ~ 27,
    o_abstate == "MS" ~ 28,
    o_abstate == "MO" ~ 29,
    o_abstate == "MT" ~ 30,
    o_abstate == "NE" ~ 31,
    o_abstate == "NV" ~ 32,
    o_abstate == "NH" ~ 33,
    o_abstate == "NJ" ~ 34,
    o_abstate == "NM" ~ 35,
    o_abstate == "NY" ~ 36,
    o_abstate == "NC" ~ 37,
    o_abstate == "ND" ~ 38,
    o_abstate == "OH" ~ 39,
    o_abstate == "OK" ~ 40,
    o_abstate == "OR" ~ 41,
    o_abstate == "PA" ~ 42,
    o_abstate == "RI" ~ 44,
    o_abstate == "SC" ~ 45,
    o_abstate == "SD" ~ 46,
    o_abstate == "TN" ~ 47,
    o_abstate == "TX" ~ 48,
    o_abstate == "UT" ~ 49,
    o_abstate == "VT" ~ 50,
    o_abstate == "VA" ~ 51,
    o_abstate == "WA" ~ 53,
    o_abstate == "WV" ~ 54,
    o_abstate == "WI" ~ 55,
    o_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Occurrence
deaths20$o_county <- as.numeric(substr(deaths20$V1,23,25))

### 3 Residence
# State Abrev. Name - Residence
deaths20$r_abstate <- substr(deaths20$V1,29,30)

# State FIPS - Residence 
deaths20<- deaths20 %>% mutate(
  r_st_fips=case_when(
    r_abstate == "AL" ~ 1,
    r_abstate == "AK" ~ 2,
    r_abstate == "AZ" ~ 4,
    r_abstate == "AR" ~ 5,
    r_abstate == "AR" ~ 5,
    r_abstate == "CA" ~ 6,
    r_abstate == "CO" ~ 8,
    r_abstate == "CT" ~ 9,
    r_abstate == "DE" ~ 10,
    r_abstate == "DC" ~ 11,
    r_abstate == "FL" ~ 12,
    r_abstate == "GA" ~ 13,
    r_abstate == "HI" ~ 15,
    r_abstate == "ID" ~ 16,
    r_abstate == "IL" ~ 17,
    r_abstate == "IN" ~ 18,
    r_abstate == "IA" ~ 19,
    r_abstate == "KS" ~ 20,
    r_abstate == "KY" ~ 21,
    r_abstate == "LA" ~ 22,
    r_abstate == "ME" ~ 23,
    r_abstate == "MD" ~ 24,
    r_abstate == "MA" ~ 25,
    r_abstate == "MI" ~ 26,
    r_abstate == "MN" ~ 27,
    r_abstate == "MS" ~ 28,
    r_abstate == "MO" ~ 29,
    r_abstate == "MT" ~ 30,
    r_abstate == "NE" ~ 31,
    r_abstate == "NV" ~ 32,
    r_abstate == "NH" ~ 33,
    r_abstate == "NJ" ~ 34,
    r_abstate == "NM" ~ 35,
    r_abstate == "NY" ~ 36,
    r_abstate == "NC" ~ 37,
    r_abstate == "ND" ~ 38,
    r_abstate == "OH" ~ 39,
    r_abstate == "OK" ~ 40,
    r_abstate == "OR" ~ 41,
    r_abstate == "PA" ~ 42,
    r_abstate == "RI" ~ 44,
    r_abstate == "SC" ~ 45,
    r_abstate == "SD" ~ 46,
    r_abstate == "TN" ~ 47,
    r_abstate == "TX" ~ 48,
    r_abstate == "UT" ~ 49,
    r_abstate == "VT" ~ 50,
    r_abstate == "VA" ~ 51,
    r_abstate == "WA" ~ 53,
    r_abstate == "WV" ~ 54,
    r_abstate == "WI" ~ 55,
    r_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Residence
deaths20$r_county <- as.numeric(substr(deaths20$V1,35,37))

# Generating County FIPS for Occurrence and Residence
# Note: There are 6685 deaths with no residential address, setting these to occurrence county code
deaths20$r_cy_fips<-(deaths20$r_st_fips)*1000+deaths20$r_county
deaths20$o_cy_fips<-(deaths20$o_st_fips)*1000+deaths20$o_county
deaths20$r_cy_fips[is.na(deaths20$r_cy_fips)]<-deaths20$o_cy_fips[is.na(deaths20$r_cy_fips)]

### 4 The Decedent
deaths20$sex <- substr(deaths20$V1,69,69)
deaths20$hispanic <- as.numeric(substr(deaths20$V1,488,488))
deaths20$race <- as.numeric(substr(deaths20$V1,489,490))
deaths20$race4cat <- as.numeric(substr(deaths20$V1,450,450))
deaths20$age <- as.numeric(substr(deaths20$V1,77,78))

# Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
deaths20$hispind <- ifelse(deaths20$hispanic>=1 & deaths20$hispanic<=5,1,0)
deaths20$raceth <- deaths20$race4cat
deaths20$raceth[deaths20$hispind==1] <- 5

### 5 Underlying Cause
# ICD-10 Code
deaths20$ICD10<-substr(deaths20$V1,146,149)
deaths20$suicideind <- ifelse(deaths20$ICD10=="X72 " | deaths20$ICD10=="X73 " | deaths20$ICD10=="X74 ",1,0)

### Selecting Vars, Exporting CSV
# Sub-setting to only firearm related deaths (Unintentional, Suicide, Homicide)
# Unintentional: W32-W34
# Suicide: X72-X74
# Homicide: X93-X95 (not included: *U01.4 - Terrorism using firearm; a rarely used, provisional category)
# Undetermined: Y22-Y24
gd20 <- filter(deaths20, ICD10=="W32 " | ICD10=="W33 " | ICD10=="W34 " | ICD10=="X72 " | ICD10=="X73 " | ICD10=="X74 " | 
                 ICD10=="X93 " | ICD10=="X94 " | ICD10=="X95 " | ICD10=="Y22 " |  ICD10=="Y23 " |ICD10=="Y24 ")

# Selecting vars
gd20<-select(gd20,year,month,quarter,manner,o_abstate,o_st_fips,r_abstate,r_st_fips,r_cy_fips,o_cy_fips,sex,hispanic,race,hispind,race4cat,raceth,age,ICD10,suicideind)
head(gd20)
# Exporting to CSV
#write.csv(gd20, file="gundeaths20.csv")

# Last, bringing in 2021-2022
d21<-read.delim("/Users/jaquelynjahn/Library/CloudStorage/Dropbox/NCGVR ERPO grant/Analysis/MULT2021USAllCnty.txt", sep="\t", header=F, stringsAsFactors = F)

# Data year
d21$year<-as.numeric(substr(d21$V1, 102,105))

# Death month
d21$month <- as.numeric(substr(d21$V1,65,66))

# Death quarter
d21<-d21 %>% mutate(quarter=case_when(
  month>=1 & month<=3 ~ 1,
  month>=4 & month<=6 ~ 2,
  month>=7 & month<=9 ~ 3,
  month>=10 & month<=12 ~ 4,
))

# Manner of death
d21$manner<-as.numeric(substr(d21$V1, 107,107))

### Occurrence
# State Abrev. Name - Occurrence
d21$o_abstate <- substr(d21$V1,21,22)

# State FIPS - Occurrence 
d21<- d21 %>% mutate(
  o_st_fips=case_when(
    o_abstate == "AL" ~ 1,
    o_abstate == "AK" ~ 2,
    o_abstate == "AZ" ~ 4,
    o_abstate == "AR" ~ 5,
    o_abstate == "AR" ~ 5,
    o_abstate == "CA" ~ 6,
    o_abstate == "CO" ~ 8,
    o_abstate == "CT" ~ 9,
    o_abstate == "DE" ~ 10,
    o_abstate == "DC" ~ 11,
    o_abstate == "FL" ~ 12,
    o_abstate == "GA" ~ 13,
    o_abstate == "HI" ~ 15,
    o_abstate == "ID" ~ 16,
    o_abstate == "IL" ~ 17,
    o_abstate == "IN" ~ 18,
    o_abstate == "IA" ~ 19,
    o_abstate == "KS" ~ 20,
    o_abstate == "KY" ~ 21,
    o_abstate == "LA" ~ 22,
    o_abstate == "ME" ~ 23,
    o_abstate == "MD" ~ 24,
    o_abstate == "MA" ~ 25,
    o_abstate == "MI" ~ 26,
    o_abstate == "MN" ~ 27,
    o_abstate == "MS" ~ 28,
    o_abstate == "MO" ~ 29,
    o_abstate == "MT" ~ 30,
    o_abstate == "NE" ~ 31,
    o_abstate == "NV" ~ 32,
    o_abstate == "NH" ~ 33,
    o_abstate == "NJ" ~ 34,
    o_abstate == "NM" ~ 35,
    o_abstate == "NY" ~ 36,
    o_abstate == "NC" ~ 37,
    o_abstate == "ND" ~ 38,
    o_abstate == "OH" ~ 39,
    o_abstate == "OK" ~ 40,
    o_abstate == "OR" ~ 41,
    o_abstate == "PA" ~ 42,
    o_abstate == "RI" ~ 44,
    o_abstate == "SC" ~ 45,
    o_abstate == "SD" ~ 46,
    o_abstate == "TN" ~ 47,
    o_abstate == "TX" ~ 48,
    o_abstate == "UT" ~ 49,
    o_abstate == "VT" ~ 50,
    o_abstate == "VA" ~ 51,
    o_abstate == "WA" ~ 53,
    o_abstate == "WV" ~ 54,
    o_abstate == "WI" ~ 55,
    o_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Occurrence
d21$o_county <- as.numeric(substr(d21$V1,23,25))

### 3 Residence
# State Abrev. Name - Residence
d21$r_abstate <- substr(d21$V1,29,30)

# State FIPS - Residence 
d21<- d21 %>% mutate(
  r_st_fips=case_when(
    r_abstate == "AL" ~ 1,
    r_abstate == "AK" ~ 2,
    r_abstate == "AZ" ~ 4,
    r_abstate == "AR" ~ 5,
    r_abstate == "AR" ~ 5,
    r_abstate == "CA" ~ 6,
    r_abstate == "CO" ~ 8,
    r_abstate == "CT" ~ 9,
    r_abstate == "DE" ~ 10,
    r_abstate == "DC" ~ 11,
    r_abstate == "FL" ~ 12,
    r_abstate == "GA" ~ 13,
    r_abstate == "HI" ~ 15,
    r_abstate == "ID" ~ 16,
    r_abstate == "IL" ~ 17,
    r_abstate == "IN" ~ 18,
    r_abstate == "IA" ~ 19,
    r_abstate == "KS" ~ 20,
    r_abstate == "KY" ~ 21,
    r_abstate == "LA" ~ 22,
    r_abstate == "ME" ~ 23,
    r_abstate == "MD" ~ 24,
    r_abstate == "MA" ~ 25,
    r_abstate == "MI" ~ 26,
    r_abstate == "MN" ~ 27,
    r_abstate == "MS" ~ 28,
    r_abstate == "MO" ~ 29,
    r_abstate == "MT" ~ 30,
    r_abstate == "NE" ~ 31,
    r_abstate == "NV" ~ 32,
    r_abstate == "NH" ~ 33,
    r_abstate == "NJ" ~ 34,
    r_abstate == "NM" ~ 35,
    r_abstate == "NY" ~ 36,
    r_abstate == "NC" ~ 37,
    r_abstate == "ND" ~ 38,
    r_abstate == "OH" ~ 39,
    r_abstate == "OK" ~ 40,
    r_abstate == "OR" ~ 41,
    r_abstate == "PA" ~ 42,
    r_abstate == "RI" ~ 44,
    r_abstate == "SC" ~ 45,
    r_abstate == "SD" ~ 46,
    r_abstate == "TN" ~ 47,
    r_abstate == "TX" ~ 48,
    r_abstate == "UT" ~ 49,
    r_abstate == "VT" ~ 50,
    r_abstate == "VA" ~ 51,
    r_abstate == "WA" ~ 53,
    r_abstate == "WV" ~ 54,
    r_abstate == "WI" ~ 55,
    r_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Residence
d21$r_county <- as.numeric(substr(d21$V1,35,37))

# Generating County FIPS for Occurrence and Residence
# Note: There are 6685 deaths with no residential address, setting these to occurrence county code
d21$r_cy_fips<-(d21$r_st_fips)*1000+d21$r_county
d21$o_cy_fips<-(d21$o_st_fips)*1000+d21$o_county
d21$r_cy_fips[is.na(d21$r_cy_fips)]<-d21$o_cy_fips[is.na(d21$r_cy_fips)]

### 4 The Decedent
d21$sex <- substr(d21$V1,69,69)
d21$age <- as.numeric(substr(d21$V1,77,78))
d21$hispanic <- as.numeric(substr(d21$V1,484,486))

d21$race <- as.numeric(substr(d21$V1,489,490)) # this is the 40 category race var

# Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
d21$hispind <- ifelse(d21$hispanic>=200 & d21$hispanic<996,1,0)
d21<- d21 %>% mutate(raceth=case_when(
  race==1 & hispind==0 ~ 1, # NHW
  race %in% c(2,15:18,25:30,35:39) & hispind==0 ~ 2, #NHB
  race %in% c(3,19:21,31:33,40) & hispind==0 ~ 3, #NHAIAN
  race %in% c(4:14,22:24,34) & hispind==0 ~ 4, #NHAPI
  hispind==1 ~ 5 #Hisp
))

### 5 Underlying Cause
# ICD-10 Code
d21$ICD10<-substr(d21$V1,146,149)
d21$suicideind <- ifelse(d21$ICD10=="X72 " | d21$ICD10=="X73 " | d21$ICD10=="X74 ",1,0)

### Selecting Vars, Exporting CSV
# Sub-setting to only firearm related deaths (Unintentional, Suicide, Homicide)
# Unintentional: W32-W34
# Suicide: X72-X74
# Homicide: X93-X95 (not included: *U01.4 - Terrorism using firearm; a rarely used, provisional category)
# Undetermined: Y22-Y24
gd21 <- filter(d21, ICD10=="W32 " | ICD10=="W33 " | ICD10=="W34 " | ICD10=="X72 " | ICD10=="X73 " | ICD10=="X74 " | 
                 ICD10=="X93 " | ICD10=="X94 " | ICD10=="X95 " | ICD10=="Y22 " |  ICD10=="Y23 " |ICD10=="Y24 ")

# Selecting vars
gd21<-select(gd21,year,month,quarter,manner,o_abstate,o_st_fips,r_abstate,r_st_fips,r_cy_fips,o_cy_fips,sex,hispanic,race,hispind,raceth,age,ICD10,suicideind)
head(gd21)

# Exporting to CSV
#write.csv(gd21, file="gundeaths21.csv")

# 2022
d22<-read.delim("/Users/jaquelynjahn/Library/CloudStorage/Dropbox/NCGVR ERPO grant/Analysis/MULT2022USAllCnty.txt", sep="\t", header=F, stringsAsFactors = F)

# Data year
d22$year<-as.numeric(substr(d22$V1, 102,105))

# Death month
d22$month <- as.numeric(substr(d22$V1,65,66))

# Death quarter
d22<-d22 %>% mutate(quarter=case_when(
  month>=1 & month<=3 ~ 1,
  month>=4 & month<=6 ~ 2,
  month>=7 & month<=9 ~ 3,
  month>=10 & month<=12 ~ 4,
))

# Manner of death
d22$manner<-as.numeric(substr(d22$V1, 107,107))

### Occurrence
# State Abrev. Name - Occurrence
d22$o_abstate <- substr(d22$V1,21,22)

# State FIPS - Occurrence 
d22<- d22 %>% mutate(
  o_st_fips=case_when(
    o_abstate == "AL" ~ 1,
    o_abstate == "AK" ~ 2,
    o_abstate == "AZ" ~ 4,
    o_abstate == "AR" ~ 5,
    o_abstate == "AR" ~ 5,
    o_abstate == "CA" ~ 6,
    o_abstate == "CO" ~ 8,
    o_abstate == "CT" ~ 9,
    o_abstate == "DE" ~ 10,
    o_abstate == "DC" ~ 11,
    o_abstate == "FL" ~ 12,
    o_abstate == "GA" ~ 13,
    o_abstate == "HI" ~ 15,
    o_abstate == "ID" ~ 16,
    o_abstate == "IL" ~ 17,
    o_abstate == "IN" ~ 18,
    o_abstate == "IA" ~ 19,
    o_abstate == "KS" ~ 20,
    o_abstate == "KY" ~ 21,
    o_abstate == "LA" ~ 22,
    o_abstate == "ME" ~ 23,
    o_abstate == "MD" ~ 24,
    o_abstate == "MA" ~ 25,
    o_abstate == "MI" ~ 26,
    o_abstate == "MN" ~ 27,
    o_abstate == "MS" ~ 28,
    o_abstate == "MO" ~ 29,
    o_abstate == "MT" ~ 30,
    o_abstate == "NE" ~ 31,
    o_abstate == "NV" ~ 32,
    o_abstate == "NH" ~ 33,
    o_abstate == "NJ" ~ 34,
    o_abstate == "NM" ~ 35,
    o_abstate == "NY" ~ 36,
    o_abstate == "NC" ~ 37,
    o_abstate == "ND" ~ 38,
    o_abstate == "OH" ~ 39,
    o_abstate == "OK" ~ 40,
    o_abstate == "OR" ~ 41,
    o_abstate == "PA" ~ 42,
    o_abstate == "RI" ~ 44,
    o_abstate == "SC" ~ 45,
    o_abstate == "SD" ~ 46,
    o_abstate == "TN" ~ 47,
    o_abstate == "TX" ~ 48,
    o_abstate == "UT" ~ 49,
    o_abstate == "VT" ~ 50,
    o_abstate == "VA" ~ 51,
    o_abstate == "WA" ~ 53,
    o_abstate == "WV" ~ 54,
    o_abstate == "WI" ~ 55,
    o_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Occurrence
d22$o_county <- as.numeric(substr(d22$V1,23,25))

### 3 Residence
# State Abrev. Name - Residence
d22$r_abstate <- substr(d22$V1,29,30)

# State FIPS - Residence 
d22<- d22 %>% mutate(
  r_st_fips=case_when(
    r_abstate == "AL" ~ 1,
    r_abstate == "AK" ~ 2,
    r_abstate == "AZ" ~ 4,
    r_abstate == "AR" ~ 5,
    r_abstate == "AR" ~ 5,
    r_abstate == "CA" ~ 6,
    r_abstate == "CO" ~ 8,
    r_abstate == "CT" ~ 9,
    r_abstate == "DE" ~ 10,
    r_abstate == "DC" ~ 11,
    r_abstate == "FL" ~ 12,
    r_abstate == "GA" ~ 13,
    r_abstate == "HI" ~ 15,
    r_abstate == "ID" ~ 16,
    r_abstate == "IL" ~ 17,
    r_abstate == "IN" ~ 18,
    r_abstate == "IA" ~ 19,
    r_abstate == "KS" ~ 20,
    r_abstate == "KY" ~ 21,
    r_abstate == "LA" ~ 22,
    r_abstate == "ME" ~ 23,
    r_abstate == "MD" ~ 24,
    r_abstate == "MA" ~ 25,
    r_abstate == "MI" ~ 26,
    r_abstate == "MN" ~ 27,
    r_abstate == "MS" ~ 28,
    r_abstate == "MO" ~ 29,
    r_abstate == "MT" ~ 30,
    r_abstate == "NE" ~ 31,
    r_abstate == "NV" ~ 32,
    r_abstate == "NH" ~ 33,
    r_abstate == "NJ" ~ 34,
    r_abstate == "NM" ~ 35,
    r_abstate == "NY" ~ 36,
    r_abstate == "NC" ~ 37,
    r_abstate == "ND" ~ 38,
    r_abstate == "OH" ~ 39,
    r_abstate == "OK" ~ 40,
    r_abstate == "OR" ~ 41,
    r_abstate == "PA" ~ 42,
    r_abstate == "RI" ~ 44,
    r_abstate == "SC" ~ 45,
    r_abstate == "SD" ~ 46,
    r_abstate == "TN" ~ 47,
    r_abstate == "TX" ~ 48,
    r_abstate == "UT" ~ 49,
    r_abstate == "VT" ~ 50,
    r_abstate == "VA" ~ 51,
    r_abstate == "WA" ~ 53,
    r_abstate == "WV" ~ 54,
    r_abstate == "WI" ~ 55,
    r_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Residence
d22$r_county <- as.numeric(substr(d22$V1,35,37))

# Generating County FIPS for Occurrence and Residence
# Note: There are 6685 deaths with no residential address, setting these to occurrence county code
d22$r_cy_fips<-(d22$r_st_fips)*1000+d22$r_county
d22$o_cy_fips<-(d22$o_st_fips)*1000+d22$o_county
d22$r_cy_fips[is.na(d22$r_cy_fips)]<-d22$o_cy_fips[is.na(d22$r_cy_fips)]

### 4 The Decedent
d22$sex <- substr(d22$V1,69,69)
d22$age <- as.numeric(substr(d22$V1,77,78))
d22$hispanic <- as.numeric(substr(d22$V1,484,486))

d22$race <- as.numeric(substr(d22$V1,489,490)) # this is the 40 category race var

# Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
d22$hispind <- ifelse(d22$hispanic>=200 & d22$hispanic<996,1,0)
d22<- d22 %>% mutate(raceth=case_when(
  race==1 & hispind==0 ~ 1, # NHW
  race %in% c(2,15:18,25:30,35:39) & hispind==0 ~ 2, #NHB
  race %in% c(3,19:21,31:33,40) & hispind==0 ~ 3, #NHAIAN
  race %in% c(4:14,22:24,34) & hispind==0 ~ 4, #NHAPI
  hispind==1 ~ 5 #Hisp
))

### 5 Underlying Cause
# ICD-10 Code
d22$ICD10<-substr(d22$V1,146,149)
d22$suicideind <- ifelse(d22$ICD10=="X72 " | d22$ICD10=="X73 " | d22$ICD10=="X74 ",1,0)

### Selecting Vars, Exporting CSV
# Sub-setting to only firearm related deaths (Unintentional, Suicide, Homicide)
# Unintentional: W32-W34
# Suicide: X72-X74
# Homicide: X93-X95 (not included: *U01.4 - Terrorism using firearm; a rarely used, provisional category)
# Undetermined: Y22-Y24
gd22 <- filter(d22, ICD10=="W32 " | ICD10=="W33 " | ICD10=="W34 " | ICD10=="X72 " | ICD10=="X73 " | ICD10=="X74 " | 
                 ICD10=="X93 " | ICD10=="X94 " | ICD10=="X95 " | ICD10=="Y22 " |  ICD10=="Y23 " |ICD10=="Y24 ")

# Selecting vars
gd22<-select(gd22,year,month,quarter,manner,o_abstate,o_st_fips,r_abstate,r_st_fips,r_cy_fips,o_cy_fips,sex,hispanic,race,hispind,raceth,age,ICD10,suicideind)
head(gd22)

write.csv(gd22, file="gundeaths22.csv")

# 2023
d23<-read.delim("/Users/jaquelynjahn/Library/CloudStorage/Dropbox/NCGVR ERPO grant/Analysis/Mort2023USAllCounty.txt", sep="\t", header=F, stringsAsFactors = F)

# Data year
d23$year<-as.numeric(substr(d23$V1, 102,105))

# Death month
d23$month <- as.numeric(substr(d23$V1,65,66))

# Death quarter
d23<-d23 %>% mutate(quarter=case_when(
  month>=1 & month<=3 ~ 1,
  month>=4 & month<=6 ~ 2,
  month>=7 & month<=9 ~ 3,
  month>=10 & month<=12 ~ 4,
))

# Manner of death
d23$manner<-as.numeric(substr(d23$V1, 107,107))

### Occurrence
# State Abrev. Name - Occurrence
d23$o_abstate <- substr(d23$V1,21,22)

# State FIPS - Occurrence 
d23<- d23 %>% mutate(
  o_st_fips=case_when(
    o_abstate == "AL" ~ 1,
    o_abstate == "AK" ~ 2,
    o_abstate == "AZ" ~ 4,
    o_abstate == "AR" ~ 5,
    o_abstate == "AR" ~ 5,
    o_abstate == "CA" ~ 6,
    o_abstate == "CO" ~ 8,
    o_abstate == "CT" ~ 9,
    o_abstate == "DE" ~ 10,
    o_abstate == "DC" ~ 11,
    o_abstate == "FL" ~ 12,
    o_abstate == "GA" ~ 13,
    o_abstate == "HI" ~ 15,
    o_abstate == "ID" ~ 16,
    o_abstate == "IL" ~ 17,
    o_abstate == "IN" ~ 18,
    o_abstate == "IA" ~ 19,
    o_abstate == "KS" ~ 20,
    o_abstate == "KY" ~ 21,
    o_abstate == "LA" ~ 22,
    o_abstate == "ME" ~ 23,
    o_abstate == "MD" ~ 24,
    o_abstate == "MA" ~ 25,
    o_abstate == "MI" ~ 26,
    o_abstate == "MN" ~ 27,
    o_abstate == "MS" ~ 28,
    o_abstate == "MO" ~ 29,
    o_abstate == "MT" ~ 30,
    o_abstate == "NE" ~ 31,
    o_abstate == "NV" ~ 32,
    o_abstate == "NH" ~ 33,
    o_abstate == "NJ" ~ 34,
    o_abstate == "NM" ~ 35,
    o_abstate == "NY" ~ 36,
    o_abstate == "NC" ~ 37,
    o_abstate == "ND" ~ 38,
    o_abstate == "OH" ~ 39,
    o_abstate == "OK" ~ 40,
    o_abstate == "OR" ~ 41,
    o_abstate == "PA" ~ 42,
    o_abstate == "RI" ~ 44,
    o_abstate == "SC" ~ 45,
    o_abstate == "SD" ~ 46,
    o_abstate == "TN" ~ 47,
    o_abstate == "TX" ~ 48,
    o_abstate == "UT" ~ 49,
    o_abstate == "VT" ~ 50,
    o_abstate == "VA" ~ 51,
    o_abstate == "WA" ~ 53,
    o_abstate == "WV" ~ 54,
    o_abstate == "WI" ~ 55,
    o_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Occurrence
d23$o_county <- as.numeric(substr(d23$V1,23,25))

### 3 Residence
# State Abrev. Name - Residence
d23$r_abstate <- substr(d23$V1,29,30)

# State FIPS - Residence 
d23<- d23 %>% mutate(
  r_st_fips=case_when(
    r_abstate == "AL" ~ 1,
    r_abstate == "AK" ~ 2,
    r_abstate == "AZ" ~ 4,
    r_abstate == "AR" ~ 5,
    r_abstate == "AR" ~ 5,
    r_abstate == "CA" ~ 6,
    r_abstate == "CO" ~ 8,
    r_abstate == "CT" ~ 9,
    r_abstate == "DE" ~ 10,
    r_abstate == "DC" ~ 11,
    r_abstate == "FL" ~ 12,
    r_abstate == "GA" ~ 13,
    r_abstate == "HI" ~ 15,
    r_abstate == "ID" ~ 16,
    r_abstate == "IL" ~ 17,
    r_abstate == "IN" ~ 18,
    r_abstate == "IA" ~ 19,
    r_abstate == "KS" ~ 20,
    r_abstate == "KY" ~ 21,
    r_abstate == "LA" ~ 22,
    r_abstate == "ME" ~ 23,
    r_abstate == "MD" ~ 24,
    r_abstate == "MA" ~ 25,
    r_abstate == "MI" ~ 26,
    r_abstate == "MN" ~ 27,
    r_abstate == "MS" ~ 28,
    r_abstate == "MO" ~ 29,
    r_abstate == "MT" ~ 30,
    r_abstate == "NE" ~ 31,
    r_abstate == "NV" ~ 32,
    r_abstate == "NH" ~ 33,
    r_abstate == "NJ" ~ 34,
    r_abstate == "NM" ~ 35,
    r_abstate == "NY" ~ 36,
    r_abstate == "NC" ~ 37,
    r_abstate == "ND" ~ 38,
    r_abstate == "OH" ~ 39,
    r_abstate == "OK" ~ 40,
    r_abstate == "OR" ~ 41,
    r_abstate == "PA" ~ 42,
    r_abstate == "RI" ~ 44,
    r_abstate == "SC" ~ 45,
    r_abstate == "SD" ~ 46,
    r_abstate == "TN" ~ 47,
    r_abstate == "TX" ~ 48,
    r_abstate == "UT" ~ 49,
    r_abstate == "VT" ~ 50,
    r_abstate == "VA" ~ 51,
    r_abstate == "WA" ~ 53,
    r_abstate == "WV" ~ 54,
    r_abstate == "WI" ~ 55,
    r_abstate == "WY" ~ 56,
  )
)

# County (FIPS) - Residence
d23$r_county <- as.numeric(substr(d23$V1,35,37))

# Generating County FIPS for Occurrence and Residence
# Note: There are 6685 deaths with no residential address, setting these to occurrence county code
d23$r_cy_fips<-(d23$r_st_fips)*1000+d23$r_county
d23$o_cy_fips<-(d23$o_st_fips)*1000+d23$o_county
d23$r_cy_fips[is.na(d23$r_cy_fips)]<-d23$o_cy_fips[is.na(d23$r_cy_fips)]

### 4 The Decedent
d23$sex <- substr(d23$V1,69,69)
d23$age <- as.numeric(substr(d23$V1,77,78))
d23$hispanic <- as.numeric(substr(d23$V1,484,486))

d23$race <- as.numeric(substr(d23$V1,489,490)) # this is the 40 category race var

# Race/ethnicity 5 cat: White, Black, American Indian, API, Hispanic
d23$hispind <- ifelse(d23$hispanic>=200 & d23$hispanic<996,1,0)
d23<- d23 %>% mutate(raceth=case_when(
  race==1 & hispind==0 ~ 1, # NHW
  race %in% c(2,15:18,25:30,35:39) & hispind==0 ~ 2, #NHB
  race %in% c(3,19:21,31:33,40) & hispind==0 ~ 3, #NHAIAN
  race %in% c(4:14,22:24,34) & hispind==0 ~ 4, #NHAPI
  hispind==1 ~ 5 #Hisp
))

### 5 Underlying Cause
# ICD-10 Code
d23$ICD10<-substr(d23$V1,146,149)
d23$suicideind <- ifelse(d23$ICD10=="X72 " | d23$ICD10=="X73 " | d23$ICD10=="X74 ",1,0)

### Selecting Vars, Exporting CSV
# Sub-setting to only firearm related deaths (Unintentional, Suicide, Homicide)
# Unintentional: W32-W34
# Suicide: X72-X74
# Homicide: X93-X95 (not included: *U01.4 - Terrorism using firearm; a rarely used, provisional category)
# Undetermined: Y22-Y24
gd23 <- filter(d23, ICD10=="W32 " | ICD10=="W33 " | ICD10=="W34 " | ICD10=="X72 " | ICD10=="X73 " | ICD10=="X74 " | 
                 ICD10=="X93 " | ICD10=="X94 " | ICD10=="X95 " | ICD10=="Y22 " |  ICD10=="Y23 " |ICD10=="Y24 ")

# Selecting vars
gd23<-select(gd23,year,month,quarter,manner,o_abstate,o_st_fips,r_abstate,r_st_fips,r_cy_fips,o_cy_fips,sex,hispanic,race,hispind,raceth,age,ICD10,suicideind)
head(gd23)

write.csv(gd23, file="gundeaths23.csv")

### Merging all years
gdall1<-rbind(gundeaths,gd20)

gdall2<-select(gdall1,year,month,quarter,manner,o_abstate,o_st_fips,r_abstate,
               r_st_fips,r_cy_fips,o_cy_fips,
               sex,hispanic, race, hispind, raceth, age,ICD10,suicideind)

gdall<-rbind(gdall2,gd21,gd22,gd23)

save(gdall, file="gundeaths1423.RData")

