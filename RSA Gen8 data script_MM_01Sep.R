# SCRIPT INFORMATION ------------------------------------------------------


#TITLE: RSA Gen 8 data analysis 
#AUTHOR: Maxwell Mabona
#DATE OF LAST USE: 23 August 2024
#CONTACT: Maxwellm@nicd.ac.za


# LOAD REQUIRED PACKAGES -----------------------------------------------------------

pacman::p_load(
  rio,        #For functions of data importing and exporting
  here,       #For locate file path
  janitor,     #cleaning and calculations
  tidyverse,   #data management and visualization
  lubridate,   #for cleaning of dates
  epikit,      #for creating age categories
  gtsummary,    #for creating tables
  tidylog,       #to keep track of data cleaning steps
  skimr,        #quick summary of dataset
  visdat, 
  naniar,
  haven,
  openxlsx,
  writexl, 
  readxl, 
  dplyr
)


# IMPORTING DATA ----------------------------------------------------------

getwd()

DHIS2data <- read_excel("RSA_EPI_April 2024 export_01Sep24.xlsx") # 17 429 observations (including new MP and KZN data)

Sequenced <- read_excel("RSA_samples_sequenced.xlsx") #1 116 observations

Sequenced_All <- read_excel("RSA_samples_sequenced_All1.xlsx") #984 observations

MPNKZN_SS4ME <- read_excel("MPN_KZN_SS4ME_All.xlsx") # 15 501 observations

MPNKZN_SS4ME_Oct2 <- read_excel("MPN_KZN_SS4ME_All_Oct2.xlsx") # 15 501 observations

Classifications <- read_excel("Classifications_All.xlsx")

Facilities <- read_excel("Facilities_with_districts_full.xlsx")

# VIEW AND EXPLORE DATA ------------------------------------------------------------

#View(DHIS2data)

summary(DHIS2data)

names(DHIS2data)


# INITIAL DATA CLEANING -----------------------------------------------------------

# Create a new variable 'Province' based on the 'Organisation unit name'

DHIS2data <- DHIS2data %>%
  mutate(Province = case_when(
    substr(`Organisation unit name`, 1, 2) == "kz" ~ "KwaZulu-Natal",
    substr(`Organisation unit name`, 1, 2) == "lp" ~ "Limpopo",
    substr(`Organisation unit name`, 1, 2) == "mp" ~ "Mpumalanga",
    TRUE ~ NA_character_ 
  ))

DHIS2data <- DHIS2data %>%
  rename(Province1 = `Province`)

DHIS2data$'Date of Diagnosis' <- as.Date(DHIS2data$`Date of Diagnosis`, format = "%Y-%m-%d")

DHIS2data$'Date of notification' <- as.Date(DHIS2data$`Date of notification`, format = "%Y-%m-%d")

class(DHIS2data$`Date of Diagnosis`)

class(DHIS2data$`Date of notification`)

#Rename and Clean barcode variables

DHIS2data <- DHIS2data %>%
  rename(BARCODE = `Lab specimen ID/Barcode number`)

DHIS2data$BARCODE <- trimws(DHIS2data$BARCODE)

DHIS2data$NMCID_BARCODE1 <- trimws(DHIS2data$NMCID_BARCODE1)

Classifications$NMCID_BARCODE1 <- trimws(Classifications$NMCID_BARCODE1)

MPNKZN_SS4ME$BARCODE <- trimws(MPNKZN_SS4ME$BARCODE)
MPNKZN_SS4ME_Oct2$BARCODE <- trimws(MPNKZN_SS4ME_Oct2$BARCODE)

#De-duplicate DHIS2 data by barcode

DHIS2data_deduplicated <- DHIS2data %>%
  arrange(NMCID_BARCODE1, !is.na(`Date of notification`), desc(`Date of notification`)) %>%
  distinct(NMCID_BARCODE1, .keep_all = TRUE)    # 13,853 rows remaining

Classifications <- Classifications %>% distinct(NMCID_BARCODE1, .keep_all = TRUE)    # 6,341 rows remaining

DHIS2data_deduplicated <- DHIS2data_deduplicated %>%
  arrange(BARCODE, !is.na(`Date of notification`), desc(`Date of notification`)) %>%
  distinct(BARCODE, .keep_all = TRUE)    # 7 147 rows remaining

MPNKZN_SS4ME_Clean <- MPNKZN_SS4ME_Oct2 %>%
  distinct(LABID, .keep_all = TRUE)

#Add classification info for samples with barcodes and/or NMC ID

Classifications <- Classifications[, !(names(Classifications) %in% c('NMC Case ID', 'BARCODE', 'NMCID_BARCODE'))] #first drop these var on classifications dataframe

DHIS2data_deduplicated <- left_join(DHIS2data_deduplicated, Classifications, by = "NMCID_BARCODE1")  # 4,489 rows matched

# Add district info for all facilities

Facilities$`Organisation unit name` <- tolower(Facilities$`Organisation unit name`)
DHIS2data_deduplicated$`Organisation unit name` <- tolower(DHIS2data_deduplicated$`Organisation unit name`)

DHIS2data_deduplicated <- left_join(DHIS2data_deduplicated, Facilities, by = "Organisation unit name")


# OLDER CODE --------------------------------------------------------------



#Cleaning sequenced samples data and finding barcodes for it (It only has LABIDs)

Sequenced <- Sequenced %>%
  filter(!grepl("^LIM", `LAB ID`)) #drop 168 Limpopo Lab IDs

Sequenced <- Sequenced %>%
  rename(LABID = `LAB ID`)

Sequenced_matched <- left_join(Sequenced, MPNKZN_SS4ME, by = "LABID") #947 of 948 (99%) sequenced samples matched (One with incomplete LABID from Sequenced samples data)

Sequenced_matched1 <- Sequenced_matched %>%
  filter(!is.na(PROVINCE) & PROVINCE != "")

#Sequenced_unmatched <- Sequenced_matched %>%
  #filter(is.na(PROVINCE) |PROVINCE == "") 

print(sum(Sequenced_matched1$BARCODE == 'NOT GIVEN')) 

print(sum(Sequenced_matched1$BARCODE == 'N/A')) #50 sequenced samples have no barcode from SS4ME data (NOT GIVEN or N/A) Meaning we are only looking for 895 barcodes


# MATCHING SEQUENCED SAMPLES WITH EPI DATA --------------------------------


Sequenced_matched2 <- left_join(Sequenced_matched1, DHIS2data_deduplicated, by = "BARCODE") #652 (73%, 652/895) sequenced samples with matching Epi data 


Prelim_matched_data <- Sequenced_matched2 %>%
  filter(!is.na(`Organisation unit name`) & `Organisation unit name` != "")

write_xlsx(Prelim_matched_data, "Prelim_matched_data.xlsx")

#Data that did not match (sequenced samples not found on Epi data N=324)

Prelim_unmatched_data <- Sequenced_matched2 %>%
  filter(is.na(`Organisation unit name`) | `Organisation unit name` == "")

write_xlsx(Prelim_unmatched_data, "Prelim_unmatched_data.xlsx")

Prelim_unmatched_data %>% tabyl(PROVINCE) # 49% from KZN, 51% from MPN

#Sending matched and unmatched update 25 Aug 2024

colnames(Sequenced_matched2) <- toupper(colnames(Sequenced_matched2))

Sequenced_matched2 <- Sequenced_matched2 %>%
  select(-`#.Y`, -`PACKAGE RECEIVED BY (NAME)`)

Sequenced_matched2 <- Sequenced_matched2 %>%
  mutate(`HAS EPI DATA` = ifelse(is.na(`ORGANISATION UNIT NAME`) | `ORGANISATION UNIT NAME` == "", "Not yet", "Yes"))

write_xlsx(Sequenced_matched2, "All_sequenced_data.xlsx")


# LOOKING FOR PRIORITY SAMPLES WITHOUT MATCHES (sent by Andres) -----------------

#Matching using barcode

Sequenced_needed <- read_excel("RSA_samples_epidata_please.xlsx") #115 obs

Sequenced_needed <- Sequenced_needed %>%
  select(-`BARCODE`)

Sequenced_needed <- left_join(Sequenced_needed, MPNKZN_SS4ME, by = "LABID") # (100% matched, using LABIDs because some Barcodes are missing from the sequenced list)

print(sum(Sequenced_needed$BARCODE == 'NOT GIVEN')) 

print(sum(Sequenced_needed$BARCODE == 'N/A')) #9 needed sequenced samples have no barcode from SS4ME data (NOT GIVEN or N/A) Meaning we are only looking for 104 barcodes

Sequenced_needed1 <- left_join(Sequenced_needed, DHIS2data_deduplicated, by = "BARCODE") #41 needed samples with matching Epi data

write_xlsx(Sequenced_needed1, "Needed_sequences_all1.xlsx")


# FUZZY MATCHING UNMATCHED SEQUENCED SAMPLES ------------------------------

New_unmatched_data <- Sequenced_matched2 %>%
  filter(`HAS EPI DATA` == "Not yet") #New data frame of unmatched sequenced samples

New_unmatched_data <- New_unmatched_data %>%
  select(1:27) #dropping empty variables from DHIS2 data

#Creating a subset of DHIS2 data suitable for fuzzy matching of remaining unmatched cases

DHIS2data_remaining <- left_join(DHIS2data_deduplicated, Sequenced_matched1, by = "BARCODE")

DHIS2data_remaining <- DHIS2data_remaining %>%
  filter(is.na(`#.x`) | `#.x` == "")  #6,499 cases remain

DHIS2data_remaining <- DHIS2data_remaining %>%
  select(1:24)

DHIS2data_remaining1 <-DHIS2data_remaining

colnames(DHIS2data_remaining1) <- toupper(colnames(DHIS2data_remaining1))

DHIS2data_remaining1$PROVINCE1 <- toupper(DHIS2data_remaining1$PROVINCE1)

DHIS2data_remaining1$ `SEX` <- toupper(DHIS2data_remaining1$`SEX`)

DHIS2data_remaining1$ `ORGANISATION UNIT NAME` <- toupper(DHIS2data_remaining1$`ORGANISATION UNIT NAME`) #Changing all observations to upper case

# Standardizing and trimming Facility names

DHIS2data_remaining1$`ORGANISATION UNIT NAME` <- gsub("MP |KZ |HOSPITAL|CLINIC|CHC|PRIVATE|\\(UMHLABUYALINGANA\\)", 
                                             "", 
                                             DHIS2data_remaining1$`ORGANISATION UNIT NAME`)

DHIS2data_remaining1$`ORGANISATION UNIT NAME` <- trimws(DHIS2data_remaining1$`ORGANISATION UNIT NAME`) # Trim any leading or trailing whitespace

New_unmatched_data$`FACILITY NAME` <- gsub("HOSPITAL|CLINIC|CHC|PRIVATE|\\(UMHLABUYALINGANA\\)", 
                                                      "", 
                                                      New_unmatched_data$`FACILITY NAME`)

New_unmatched_data$`FACILITY NAME` <- trimws(New_unmatched_data$`FACILITY NAME`) # Trim any leading or trailing whitespace


#Change the Format of the DATE SAMPLE COLLECTED FROM PATIENT variable on sequenced data

New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT`[New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT` == "NOT GIVEN"] <- NA

New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.numeric(New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT`)

New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.Date(New_unmatched_data$`DATE SAMPLE COLLECTED FROM PATIENT`, 
                                                                   origin = "1899-12-30")

#Re format 'NOT GIVEN' as NA to allow dates to be date format

New_unmatched_data$`PATIENT AGE`[New_unmatched_data$`PATIENT AGE` == "NOT GIVEN"] <- NA

New_unmatched_data$`PATIENT GENDER`[New_unmatched_data$`PATIENT GENDER` == "NOT GIVEN"] <- NA

New_unmatched_data$`PATIENT AGE` <- as.numeric(New_unmatched_data$`PATIENT AGE`)

DHIS2data_remaining1$`AGE (YEARS)` <- round(as.numeric(DHIS2data_remaining1$`AGE (YEARS)`), 0) #Also re format age in epi data

#Load more packages needed for fuzzy join

library(fuzzyjoin)
library(stringdist)
library(gt)

# Performing fuzzy left join (Cannot use DOB, ID, Passport No, Names, Initials, Cellphone no)

Sequenced_rematched <- fuzzy_left_join(
  New_unmatched_data, 
  DHIS2data_remaining1,
  by = c(
    "PROVINCE" = "PROVINCE1",                 
   "FACILITY NAME" = "ORGANISATION UNIT NAME", 
    "PATIENT GENDER" = "SEX",                    
    "DATE SAMPLE COLLECTED FROM PATIENT" = "DATE OF DIAGNOSIS", 
    "PATIENT AGE" = "AGE (YEARS)"               
  ),
  match_fun = list(
    function(x, y) stringdist(x, y) < 2,     # for 'PROVINCE'
    function(x, y) stringdist(x, y) < 8,    # for 'FACILITY NAME'
    function(x, y) stringdist(x, y) < 2,     # for 'PAIENT GENDER' 
    function(x, y) abs(x - y) < 30,     # Difference in days
    function(x, y) is.na(x) | is.na(y) | abs(x - y) <= 7           # for 'PATIENT AGE'
  )
)


Fuzzy_Join_Results <- Sequenced_rematched %>%
  select(
    PROVINCE, 
    PROVINCE1, 
    `FACILITY NAME`, 
    `ORGANISATION UNIT NAME`, 
    `PATIENT GENDER`, 
    SEX, 
    `DATE SAMPLE COLLECTED FROM PATIENT`,
    `DATE OF DIAGNOSIS`,
    `PATIENT AGE`, 
    `AGE (YEARS)`
  )

View(Fuzzy_Join_Results) #85 Fuzzy matches

Fuzzy_matched_data <- Sequenced_rematched %>%
  filter(!is.na(`ORGANISATION UNIT NAME`) & `ORGANISATION UNIT NAME` != "")

write_xlsx(Fuzzy_matched_data, "Fuzzy_matched_data.xlsx")

All_unmatched_data <- Sequenced_rematched %>%
  filter(is.na(`ORGANISATION UNIT NAME`) | `ORGANISATION UNIT NAME` == "")

write_xlsx(All_unmatched_data, "All_unmatched_data.xlsx")

# LOOKING FOR PRIORITY SAMPLES WITHOUT MATCHES 2 (sent by Andres) -----------------

#Clean this subset first 

Sequenced_needed2 <- Sequenced_needed1%>%
  filter(is.na(`Organisation unit name`) | `Organisation unit name` == "")

Sequenced_needed2$`FACILITY NAME` <- gsub("HOSPITAL|CLINIC|CHC|PRIVATE|\\(UMHLABUYALINGANA\\)", 
                                           "", 
                                           Sequenced_needed2$`FACILITY NAME`)

Sequenced_needed2$`FACILITY NAME` <- trimws(Sequenced_needed2$`FACILITY NAME`) # Trim any leading or trailing whitespace


#Change the Format of the DATE SAMPLE COLLECTED FROM PATIENT variable on sequenced data

Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT`[Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT` == "NOT GIVEN"] <- NA

Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.numeric(Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT`)

Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.Date(Sequenced_needed2$`DATE SAMPLE COLLECTED FROM PATIENT`, 
                                                                   origin = "1899-12-30")

#Re format 'NOT GIVEN' as NA to allow dates to be date format

Sequenced_needed2$`PATIENT AGE`[Sequenced_needed2$`PATIENT AGE` == "NOT GIVEN"] <- NA

Sequenced_needed2$`PATIENT GENDER`[Sequenced_needed2$`PATIENT GENDER` == "NOT GIVEN"] <- NA

Sequenced_needed2$`PATIENT AGE` <- as.numeric(Sequenced_needed2$`PATIENT AGE`)

#FUZZY Matching

Needed_fuzzy <- fuzzy_left_join(
  Sequenced_needed2, 
  DHIS2data_remaining1,
  by = c(
    "PROVINCE" = "PROVINCE1",                 
    "FACILITY NAME" = "ORGANISATION UNIT NAME", 
    "PATIENT GENDER" = "SEX",                    
    "DATE SAMPLE COLLECTED FROM PATIENT" = "DATE OF DIAGNOSIS", 
    "PATIENT AGE" = "AGE (YEARS)"               
  ),
  match_fun = list(
    function(x, y) stringdist(x, y) < 2,     # for 'PROVINCE'
    function(x, y) stringdist(x, y) < 8,    # for 'FACILITY NAME'
    function(x, y) stringdist(x, y) < 2,     # for 'PAIENT GENDER' 
    function(x, y) abs(x - y) < 30,     # Difference in days
    function(x, y) is.na(x) | is.na(y) | abs(x - y) <= 7           # for 'PATIENT AGE'
  )
)


Fuzzy_Join_Results2 <- Needed_fuzzy %>%
  select(
    PROVINCE, 
    PROVINCE1, 
    `FACILITY NAME`, 
    `ORGANISATION UNIT NAME`, 
    `PATIENT GENDER`, 
    SEX, 
    `DATE SAMPLE COLLECTED FROM PATIENT`,
    `DATE OF DIAGNOSIS`,
    LABID,
    `PATIENT AGE`,
    CLASS,
    `AGE (YEARS)`
  )

View(Fuzzy_Join_Results2) #35 Fuzzy matches

Fuzzy_Join_Results2 %>% tabyl(CLASS) # 68% from KZN, 51% from MPN


write_xlsx(Fuzzy_Join_Results2, "Still_needed.xlsx")

Still_needed <-  Needed_fuzzy


# NEW CODE ----------------------------------------------------------------



####NEW 31 AUG - Matching ALL sequenced MPN and KZN Samples and descriptive analysis

Sequenced_All_Oct02 <- read_excel("sequenced_samples_30SEP2024.xlsx") #984 observations

LABIDs <- read_excel("LABIDs.xlsx")

LABIDs <- left_join(LABIDs, MPNKZN_SS4ME, by = "LABID") 

write_xlsx(LABIDs, "LABIDs_found.xlsx")

#Sequenced samples on ALL (recent) that have LABIDs for barcodes all dont actually have barcodes on SS4ME


Sequenced_Oct2_SS4ME <- left_join(Sequenced_All_Oct02, MPNKZN_SS4ME_Clean, by = "LABID") 

#write_xlsx(Sequenced_All_matched, "Barcodes_found.xlsx")

###Sequenced ALL has 984 observations

Sequenced_Oct2_SS4ME <- Sequenced_Oct2_SS4ME %>%
  rename(BARCODE = BARCODE.x)

Sequenced_matched_Oct02 <- left_join(Sequenced_Oct2_SS4ME, DHIS2data_deduplicated, by = "BARCODE") #1066 (66%, 1066/1588) sequenced samples with matching Epi data 


Prelim_matched_data1 <- Sequenced_matched_Oct02 %>%
  filter(!is.na(`Organisation unit name`) & `Organisation unit name` != "")

colnames(Prelim_matched_data1) <- toupper(colnames(Prelim_matched_data1))

write_xlsx(Prelim_matched_data1, "Prelim_matched_data1.xlsx")

write_xlsx(Sequenced_matched_Oct02, "Prelim_Matched_and_Unmatched_Oct2.xlsx")

#Fuzzy matching for non matched samples

Prelim_unmatched_data1 <- Sequenced_matched_Oct02 %>%
  filter(is.na(`Organisation unit name`) | `Organisation unit name` == "")

colnames(Prelim_unmatched_data1) <- toupper(colnames(Prelim_unmatched_data1))

Prelim_unmatched_data2 <- Prelim_unmatched_data1 %>%
  select(1:29)

#Prelim_unmatched_data2 <- left_join(Prelim_unmatched_data1, MPNKZN_SS4ME, by = "LABID")


#Creating a subset of DHIS2 data suitable for fuzzy matching of remaining unmatched cases

DHIS2data_remaining2 <- left_join(DHIS2data_deduplicated, Sequenced_Oct2_SS4ME, by = "BARCODE")

DHIS2data_remaining2 <- DHIS2data_remaining2 %>%
  filter(is.na(`LABID`) | `LABID` == "")  #6,499 cases remain

DHIS2data_remaining2 <- DHIS2data_remaining2 %>%
  select(1:27)


colnames(DHIS2data_remaining2) <- toupper(colnames(DHIS2data_remaining2))

DHIS2data_remaining2$PROVINCE1 <- toupper(DHIS2data_remaining2$PROVINCE1)

DHIS2data_remaining2$ `SEX` <- toupper(DHIS2data_remaining2$`SEX`)

DHIS2data_remaining2$ `ORGANISATION UNIT NAME` <- toupper(DHIS2data_remaining2$`ORGANISATION UNIT NAME`) #Changing all observations to upper case

# Standardizing and trimming Facility names

DHIS2data_remaining2$`ORGANISATION UNIT NAME` <- gsub("MP |KZ |HOSPITAL|CLINIC|CHC|PRIVATE|\\(UMHLABUYALINGANA\\)", 
                                                      "", 
                                                      DHIS2data_remaining2$`ORGANISATION UNIT NAME`)

DHIS2data_remaining2$`ORGANISATION UNIT NAME` <- trimws(DHIS2data_remaining2$`ORGANISATION UNIT NAME`) # Trim any leading or trailing whitespace

Prelim_unmatched_data1$`FACILITY NAME` <- gsub("HOSPITAL|CLINIC|CHC|PRIVATE|\\(UMHLABUYALINGANA\\)", 
                                           "", 
                                           Prelim_unmatched_data1$`FACILITY NAME`)

Prelim_unmatched_data1$`FACILITY NAME` <- trimws(Prelim_unmatched_data1$`FACILITY NAME`) # Trim any leading or trailing whitespace


#Change the Format of the DATE SAMPLE COLLECTED FROM PATIENT variable on sequenced data

Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT`[Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT` == "NOT GIVEN"] <- NA

Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.numeric(Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT`)

Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT` <- as.Date(Prelim_unmatched_data1$`DATE SAMPLE COLLECTED FROM PATIENT`, 
                                                                   origin = "1899-12-30")

#Re format 'NOT GIVEN' as NA to allow dates to be date format

Prelim_unmatched_data1$`PATIENT AGE`[Prelim_unmatched_data1$`PATIENT AGE` == "NOT GIVEN"] <- NA

Prelim_unmatched_data1$`PATIENT GENDER`[Prelim_unmatched_data1$`PATIENT GENDER` == "NOT GIVEN"] <- NA

Prelim_unmatched_data1$`PATIENT AGE` <- as.numeric(Prelim_unmatched_data1$`PATIENT AGE`)

DHIS2data_remaining2$`AGE (YEARS)` <- round(as.numeric(DHIS2data_remaining2$`AGE (YEARS)`), 0) #Also re format age in epi data

#Load more packages needed for fuzzy join

library(fuzzyjoin)
library(stringdist)
library(gt)

# Performing fuzzy left join (Cannot use DOB, ID, Passport No, Names, Initials, Cellphone no)

Sequenced_rematched2 <- fuzzy_left_join(
  Prelim_unmatched_data1, 
  DHIS2data_remaining2,
  by = c(
    "PROVINCE" = "PROVINCE1",                 
    "FACILITY NAME" = "ORGANISATION UNIT NAME", 
    "PATIENT GENDER" = "SEX",                    
    "DATE SAMPLE COLLECTED FROM PATIENT" = "DATE OF DIAGNOSIS" 
    #"PATIENT AGE" = "AGE (YEARS)"               
  ),
  match_fun = list(
    function(x, y) stringdist(x, y) < 2,     # for 'PROVINCE'
    function(x, y) stringdist(x, y) < 6,    # for 'FACILITY NAME'
    function(x, y) stringdist(x, y) < 2,     # for 'PAIENT GENDER' 
    function(x, y) abs(x - y) < 20     # Difference in days
    #function(x, y) is.na(x) | is.na(y) | abs(x - y) <= 7           # for 'PATIENT AGE'
  )
)


Fuzzy_Join_Results2 <- Sequenced_rematched2 %>%
  select(
    PROVINCE, 
    PROVINCE1, 
    `FACILITY NAME`, 
    `ORGANISATION UNIT NAME`, 
    `PATIENT GENDER`, 
    SEX, 
    `DATE SAMPLE COLLECTED FROM PATIENT`,
    `DATE OF DIAGNOSIS`,
    `PATIENT AGE`, 
    `AGE (YEARS)`
  )

View(Fuzzy_Join_Results2) #85 Fuzzy matches

Fuzzy_matched_data2 <- Sequenced_rematched2 %>%
  filter(!is.na(`ORGANISATION UNIT NAME`) & `ORGANISATION UNIT NAME` != "")

Fuzzy_matched_data2 <- Fuzzy_matched_data2 %>% distinct(BARCODE.x, .keep_all = TRUE)    #34 Fuzzy matches

Fuzzy_matched_data2$PROVINCE1 <- tolower(Fuzzy_matched_data2$PROVINCE1)
Fuzzy_matched_data2$SEX <- tolower(Fuzzy_matched_data2$SEX)
Fuzzy_matched_data2$`ORGANISATION UNIT NAME` <- tolower(Fuzzy_matched_data2$`ORGANISATION UNIT NAME`)


write_xlsx(Fuzzy_matched_data2, "Fuzzy_matched_data2.xlsx")

write_xlsx(Sequenced_rematched2, "FuzzyAndUnmatched.xlsx")

NoClassifications <- read_excel("NoClassifications.xlsx")

NewClassifications <- read_excel("NewClassifications.xlsx")

NoClassifications1 <- left_join(NoClassifications, NewClassifications, by = "BARCODE")

write_xlsx(NoClassifications1, "NoClassifications1.xlsx")

