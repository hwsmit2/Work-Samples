##################################################
#  ******		      *****			  *******		*********#
#  *** ***	   	 *** *** 	   ********		*********#
#  ***   ***	 	***   ***	  ***   			***      #
#  ***   ***		***   ***	  ***		    	***      #
#  ***   ***		***   ***	  ***			    ***      #
#  ***   ***		***   ***	  ********		******	 #
#  ***   ***		***   ***	  *********		******   #
#  ***   ***		***   ***	  		  ***		***      #
#  ***   ***		***   ***			    *** 	***	     #
#  ***   ***		***   ***			    ***		***      #
#  *** ***		   *** ***	   ********		*********#
#  ******		      *****		  ********		*********#
##################################################

##Discharge Diagnosis Data Submission R Code

#Program Details
#Filename: OD2A 2_0 Discharge_ED_HOSP_Rcode[COMBINED RACE AND ETHNICITY].R
#Author: CDC OD2A Drug Overdose Surveillance and Epidemiology (DOSE) Team

# PURPOSE:
#	Aggregating discharge diagnosis monthly Total ED visit data by age, sex, race and ethnicitiy, aggregating Total ED visits by county, and consolidating DX codes in line level data.
#
# Instructions:
# 		The steps for running this program include:
#1. Ensure all necessary packages are installed and loaded.
#2. Set working directory
#3. Review input dataset(s) for proper formatting of variables (see Technical Guidance)
#- Pre-execution Data cleaning needs will be specific to input data file formats
#4. Set parameters: "state" (ex: PA, GA, FL, etc.), "year" (ex: 2021), start and end dates.
#5. Execute full script. Script will further clean and process data. All will output to a created "Output Folder"
#6. Execute metadata script (bottom)
#7. Input metadata values into output Excel document. You MUST manually answer metadata questions.

# 		- see Technical Guidance Section [11] for detailed instructions.

#Program Functions:
# 1. Restrict Dataset to specified analysis date range.
# 2. Identify Total ED/HOSP visits by intent, sex, age group, and county of patient residence.
# 3. Export .xlsx file(s) containing aggregated counts and line-level data.

####################
#Necessary Packages:
####################

#options(java.parameters = "-Xmx16g")
#Ensure these packages are installed and loaded.
#install.packages("tidyverse")
library(tidyverse)
library(usmap)
#install.packages("readxl")
library(readxl)
#install.packages("openxlsx")
library(openxlsx)

############

#Working Directory

#[UPDATE HERE]
#Set Working Directory To path where you are keeping datasets and template files!
setwd("//cdc.gov/project/NCIPC_DOP_OD2A/DOSE/OD2A 2.0_DOSE/R_Codes")

#Processed files will output to a newly generated "Output" folder.

###########
#Data Input
###########

#Ensure correct filenames and data is located in an input folder inside the working directory above.
#Depending on input file format, use appropriate 'read' function.

#DATA
#[UPDATE HERE]
data <- read.csv("EXAMPLE INPUT FILE_fiveyr.csv") ##EDIT HERE to indicate input file
#data <- readRDS("EXAMPLE.rds") ##EDIT HERE to indicate input file
#data <- load("EXAMPLE.rda") ##EDIT HERE to indicate input file
#data <  read.sas7bdat("EXAMPLE.sas7bdat") ##EDIT HERE to indicate input file

#STANDARD COUNTY FILE PROVIDED BY DOSE (US CENSUS FIPS codes)
county_file <- read_xlsx("FIPS_codes_v1_04142023.xlsx", sheet = "fips")

#Output Template file PROVIDED BY DOSE
temp1_re_combined <-
  openxlsx::loadWorkbook("OD2A_S_Annual_Discharge_Template_re_combined.xlsx")
jurisdiction_re_combined_temp <-
  read.csv("Discharge_jurisdiction_Template_re_combined.csv")


###############
#SET PARAMETERS
###############

#[UPDATE HERE]
#Identify state/Jurisdiction by abbreviation: "GA", "PR", etc. for template labeling
state = "MO"

#[UPDATE HERE]
#IDENTIFY YEAR for Discharge Diagnosis Template labeling
Year = 2020

#Program set up to submit annually (12 months)!!!
startdate <- paste0(Year, "-01-01") #ymd format, no need to edit
enddate <- paste0(Year, "-12-31") #ymd format, no need to edit


############################
#Pre-execution Data Cleaning
############################

data1 <- data %>%
  mutate(
    AGE_cat = case_when(is.na(AGE) ~ NA, #when age is missing, age_cat = missing
                        TRUE ~ AGE),
    #else Age_cat = AGE
    AGE_GROUP = cut(
      AGE_cat,
      breaks = c(0, 10, 14, 24, 34, 44, 54, 64, 74, 84, Inf),
      labels = c(
        "0-10",
        "11-14",
        "15-24",
        "25-34",
        "35-44",
        "45-54",
        "55-64",
        "65-74",
        "75-84",
        "85+"
      ),
      include.lowest = TRUE
    )
  ) %>%
  mutate(
    SEX_cat = ifelse(SEX == "2", "Female", ifelse(SEX == "1", "Male", "Missing")),
    RACE_ETHNICITY_COMBINED_cat = case_when(
      RACE_ETHNICITY_COMBINED == 1 ~ "Non-Hispanic, White",
      RACE_ETHNICITY_COMBINED == 2 ~ "Non-Hispanic, Black or African American",
      RACE_ETHNICITY_COMBINED == 3 ~ "Non-Hispanic, American Indian or Alaska Native",
      RACE_ETHNICITY_COMBINED == 4 ~ "Non-Hispanic, Asian",
      RACE_ETHNICITY_COMBINED == 5 ~ "Non-Hispanic, Native Hawaiian or Other Pacific Islander",
      RACE_ETHNICITY_COMBINED == 6 ~ "Non-Hispanic, Other Race",
      RACE_ETHNICITY_COMBINED == 7 ~ "Non-Hispanic, Multiple Race",
      RACE_ETHNICITY_COMBINED == 8 ~ "Hispanic, Any Race",
      TRUE ~ "Unknown or Missing"
    )
  ) %>%
  mutate(
    DIS_DATE = strptime(data$DIS_DATE, "%m/%d/%Y"),
    ADM_DATE = strptime(data$ADM_DATE, "%m/%d/%Y")
  )

data1$dx_null = "" #Creates null dx variable in case ED data does not contain procedure codes
data1$proc_null = "" #Creates null procedure variable incase ED data does not contain procedure codes

levels(data1$AGE_GROUP) <- c(levels(data1$AGE_GROUP), 'Missing')
data1$AGE_GROUP[is.na(data1$AGE)] <- "Missing"

data1$year <- year(data1$DIS_DATE)
data1$month <- month.name[month(data1$DIS_DATE)]

############################
#Year of Data Restriction & DX Code Clean-up
############################

desired_data <-
  subset(data1, DIS_DATE >= startdate & DIS_DATE <= enddate)
desired_data_meta <-
  subset(data1, DIS_DATE >= startdate & DIS_DATE <= enddate)


#CONCATENATE ALL Discharge Diagnosis Columns and Procedure columns to each one column.

#MADE INTO FUNCTION
#Removes dots in ICD-10-CM Codes function
nodot_icd <-
  function(x)
    gsub("([a-zA-Z]\\d{2})\\.(\\d+)", "\\1\\2", x, perl = T)

discharge_diagnoses <- function(data) {
  data$dx_null = "" #Creates null dx variable in case ED data does not contain procedure codes
  
  dx_all <- data[, grep("^dx", colnames(data), ignore.case = TRUE)]
  dx_all <- names(dx_all)
  
  data <-
    unite(
      data,
      dx_all,
      dx_all,
      sep = ";",
      remove = FALSE,
      na.rm = TRUE
    ) #collapses all dx columns into one, removes NA.
  data <-
    unite(data,
          dx,
          dx_all,
          sep = ";",
          remove = FALSE,
          na.rm = TRUE) #collapses all dx columns into one, removes NA.
  
  #Removes dots in ICD-10-CM Codes
  data$dx <-
    nodot_icd(data$dx) #runs remove dots function nested within new function.
  
  data$proc_null = "" #Creates null procedure variable incase ED data does not contain procedure codes
  
  proc_all <- data[, grep("^proc", colnames(data), ignore.case = TRUE)]
  proc_all <- names(proc_all)
  
  data <-
    unite(
      data,
      proc_all,
      proc_all,
      sep = ";",
      remove = FALSE,
      na.rm = TRUE
    ) #collapses all proc columns into one, removes NA.
  
}

#run function
desired_data <- discharge_diagnoses(desired_data)

###################################
#Line-Level Processing and Output
###################################

#Identify lines with overdose codes:

######################
#OD2A Case Definitions
######################

#*** regular expression

#ALL Drugg overdoses (T36-T50) All INTENT INCLUDED
# OD: all drug

#desired_data$od <-str_count(desired_data$dx,"(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[123456](A|$)|((T3[679]9|T414|T427|T4[3579]9)[123456].(A|$))")
desired_data$od <-
  str_count(desired_data$dx, "(?:(T3[6-9]|T4[0-9]|T50))")

##################################
#=================================
##################################

#OUTPUT desired_data_ed to be line level output tab

line_level_output <- desired_data %>%
  filter(od > 0) %>%
  dplyr::select(
    REC_TYPE,
    JURISDICTION_OCC,
    JURISDICTION_RES,
    #
    COUNTY_NAME,
    STATE_COUNTY_FIPS,
    #= fips,#
    ADM_DATE,
    #
    DIS_DATE,
    SEX,
    AGE,
    #AGE_UNIT,#
    #RACE,
    #ETHNICITY,
    RACE_ETHNICITY_COMBINED,
    ASOURCE,
    DISPUNIFORM,
    # (discharge status),
    PAY1,
    #(expected primary payer)
    DX_ALL = dx_all,
    ADM_DX,
    PROC_ALL = proc_all
  )


line_level_output <- line_level_output %>%
  mutate(
    DIS_DATE = format(DIS_DATE, format = '%m/%d/%Y'),
    ADM_DATE = format(ADM_DATE, format = '%m/%d/%Y'),
    SEX = as.character(SEX),
    AGE = as.character(AGE),
    #AGE_UNIT=as.character(AGE_UNIT),
    RACE_ETHNICITY_COMBINED = as.character(RACE_ETHNICITY_COMBINED),
    ASOURCE = as.character(ASOURCE),
    DISPUNIFORM = as.character(DISPUNIFORM),
    PAY1 = as.character(PAY1),
  ) %>%
  mutate_at(
    c(
      'SEX',
      'AGE',
      'RACE_ETHNICITY_COMBINED',
      'ASOURCE',
      'DISPUNIFORM',
      'PAY1'
    ),
    ~ replace_na(., "")
  )



#Line-level output to separate worksheet

target.dir <- paste0(getwd(), "/Output/")#labels output folder
dir.create(target.dir) #Creates folder for outputs
#if(dir.exists(target.dir)){unlink(paste0(target.dir,"*"))}

write_csv(
  line_level_output,
  paste0(
    "Output/",
    state,
    "_OD2A_S_Annual_",
    Year,
    "_Discharge_Line_level_re_combined.csv"
  )
)#, colnames=TRUE, rownames = FALSE)


##################################################
##################################################

#END LINE-LEVEL OUTPUT

##################################################
##################################################


####################################################
### Aggregated Template for Discharge Data #########
####################################################

Month <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), times = 297)

jurisdiction_temp1 <-
  jurisdiction_re_combined_temp %>% slice(rep(1:n(), each = 12))

jurisdiction <- data.frame(jurisdiction_temp1, Month) %>%
  mutate(
    year = as.factor(Year),
    month = as.factor(month.name[Month]),
    SEX = as.factor(Sex),
    Age_Group = case_when(Age_Group == "14-Nov" ~ "11-14",
                          TRUE ~ Age_Group),
    AGE_GROUP = as.factor(Age_Group),
    RACE_ETHNICITY_COMBINED = as.factor(Race_Ethnicity_Combined) #CHANGE TO WHATEVER VARIABLE NAME IS IN INPUT TEMPLATE!!!
  )

#################################################
#ED AND HOSP WORKSHEETS
#######################

#Split by record type
#ED
ed <- desired_data %>%
  mutate(SEX = SEX_cat,
         RACE_ETHNICITY_COMBINED = RACE_ETHNICITY_COMBINED_cat) %>%
  filter(REC_TYPE == 1)

#HOSP
hosp <- desired_data %>%
  mutate(SEX = SEX_cat,
         RACE_ETHNICITY_COMBINED = RACE_ETHNICITY_COMBINED_cat) %>%
  filter(REC_TYPE == 2)

#############

#################################
# ED and HOSP Jurisdiction Tabs
#################################

#ED Jurisdiction Tab
if (nrow(ed) > 0) {
  Total_ed <- ed %>%
    group_by(year, month, SEX, AGE_GROUP, RACE_ETHNICITY_COMBINED) %>%
    tally() %>%
    mutate(
      year = as.factor(year),
      month = as.factor(month),
      SEX = as.factor(SEX),
      AGE_GROUP = as.factor(AGE_GROUP),
      RACE_ETHNICITY_COMBINED = as.factor(RACE_ETHNICITY_COMBINED)
    )
  
  #Total_ed$age_group <- as.character(Total_ed$AGE_GROUP)
  
  final_ed <- jurisdiction %>%
    left_join(Total_ed) %>%
    mutate(Jurisdiction = state) %>%
    dplyr::select(
      Jurisdiction,
      Year = year,
      Month = month,
      Sex = SEX,
      Age_Group = AGE_GROUP,
      Race_Ethnicity_Combined = RACE_ETHNICITY_COMBINED,
      Total_ed = n
    ) %>%
    mutate(Total_ed = as.numeric(Total_ed)) %>%
    add_column(Race_filler = " ", .after = "Race_Ethnicity_Combined")
  
  
} else{
  cat("ED data not submitted, thus not processed.")
}


if (nrow(hosp) > 0) {
  #HOSP Jurisdiction Tab
  Total_hosp <- hosp %>%
    group_by(year, month, SEX, AGE_GROUP, RACE_ETHNICITY_COMBINED) %>%
    tally() %>%
    mutate(
      year = as.factor(year),
      month = as.factor(month),
      SEX = as.factor(SEX),
      AGE_GROUP = as.factor(AGE_GROUP),
      RACE_ETHNICITY_COMBINEDE = as.factor(RACE_ETHNICITY_COMBINED)
    )
  
  #Total_hosp$age_group <- as.character(Total_hosp$AGE_GROUP)
  
  final_hosp <- jurisdiction %>%
    left_join(Total_hosp) %>%
    mutate(Jurisdiction = state) %>%
    dplyr::select(
      Jurisdiction,
      Year = year,
      Month = month,
      Sex = SEX,
      Age_Group = AGE_GROUP,
      Race_Ethnicity_Combined = RACE_ETHNICITY_COMBINED,
      Total_hosp = n
    ) %>%
    mutate(Total_hosp = as.numeric(Total_hosp)) %>%
    add_column(Race_filler = " ", .after = "Race_Ethnicity_Combined")
  
  
  
} else{
  cat("HOSP data not submitted, thus not processed.")
}

##########################################################
##########################################################
#county Tabs

#specifying state and relabeling
county_file <- county_file %>%
  filter(state == STABBREV) %>%
  mutate(
    JURISDICTION_OCC = STABBREV,
    COUNTY_NAME = CTYNAME,
    STATE_COUNTY_FIPS = FIPS
  )

#County Tab template

#template dataset. lowest common denominator
month <-
  sort(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), times = length(county_file$CTYNAME)))

county_tab <- data.frame(month, county_file) %>%
  mutate(year = Year,
         month = month.name[month]) %>%
  distinct()

county_test <- county_tab %>%
  dplyr::select(
    JURISDICTION_OCC,
    COUNTY_NAME,
    STATE_COUNTY_FIPS,
    Year = year,
    Month = month
  )

######################################
# COUNTY ED

if (nrow(ed) > 0) {
  county_ed_visits <- ed %>%
    filter(JURISDICTION_RES == state) %>%
    mutate(Jurisdiction = state,
           COUNTY_NAME = str_to_title(COUNTY_NAME, locale = "en"),
           STATE_COUNTY_FIPS = as.character(STATE_COUNTY_FIPS)
    ) %>%
    group_by(JURISDICTION_OCC,
             #COUNTY_NAME,
             STATE_COUNTY_FIPS,
             year,
             month) %>%
    tally() %>%
    dplyr::select(
      JURISDICTION_OCC,
      #COUNTY_NAME,
      STATE_COUNTY_FIPS,
      Year = year,
      Month = month,
      Total_ED_visits = n
    )
  
  #merged together template and datafile
  county_final_ed <- county_test %>%
    left_join(county_ed_visits,
              by = c("JURISDICTION_OCC", "STATE_COUNTY_FIPS", "Year", "Month")) %>%
    dplyr::select(
      Jurisdiction = JURISDICTION_OCC,
      County_name = COUNTY_NAME,
      State_County_FIPS = STATE_COUNTY_FIPS,
      Year,
      Month,
      Total_ED_visits
    ) %>%
    mutate_at(c('Total_ED_visits'),  ~ na_if(., 0))
} else{
  cat("ED data not submitted, thus not processed.")
}

######################################
# COUNTY HOSP

if (nrow(hosp) > 0) {
  county_hosp_visits <- hosp %>%
    filter(JURISDICTION_RES == state) %>%
    mutate(Jurisdiction = state,
           COUNTY_NAME = str_to_title(COUNTY_NAME, locale = "en"),
           STATE_COUNTY_FIPS = as.character(STATE_COUNTY_FIPS)
    ) %>%
    group_by(JURISDICTION_OCC,
             #COUNTY_NAME,
             STATE_COUNTY_FIPS,
             year,
             month) %>%
    tally() %>%
    dplyr::select(
      JURISDICTION_OCC,
      #COUNTY_NAME,
      STATE_COUNTY_FIPS,
      Year = year,
      Month = month,
      Total_HOSP_visits = n
    )
  
  county_final_hosp <- county_test %>%
    left_join(
      county_hosp_visits,
      by = c("JURISDICTION_OCC", "STATE_COUNTY_FIPS", "Year", "Month")
    ) %>%
    dplyr::select(
      Jurisdiction = JURISDICTION_OCC,
      County_name = COUNTY_NAME,
      State_County_FIPS = STATE_COUNTY_FIPS,
      Year,
      Month,
      Total_HOSP_visits
    ) %>%
    mutate_at(c('Total_HOSP_visits'),  ~ na_if(., 0))
} else{
  cat("HOSP data not submitted, thus not processed.")
}

#END data processing script

#################################
#Metadata Script
#################################

#######
#ED Metadata
#######
#INPUT METADATA FIGURES INTO APPROPRIATE METADATA TABS

if (nrow(ed) > 0) {
  #ED
  ed_meta <- desired_data_meta %>%
    filter(REC_TYPE == 1,)
  
  dx_meta1 <- ed_meta[, grep("^dx", colnames(data), ignore.case = TRUE)]
  dx_meta1 <- replace(dx_meta1,is.na(dx_meta1),"")
  
  dx_meta <- dx_meta1 %>%
    mutate(dx_counter = rowSums(!dx_meta1 == "")) # Sum of codes in row.
  
  #Meta-Data Code
  # 4. Percent of ED visits with no dx codes
  meta_nodx = round((sum(dx_meta$dx_counter == 0) / nrow(ed)) * 100, 2)
  print(paste0(meta_nodx, "%"))
  
  writeData(
    temp1_re_combined,
    "Metadata_ED_dis",
    meta_nodx,
    startRow = 22,
    startCol = 2,
    colNames = F
  )
  
  # 5. Max number of discharge codes
  
  
  meta_maxdx <- ncol(dx_meta1) - 1
  print(meta_maxdx)
  
  writeData(
    temp1_re_combined,
    "Metadata_ED_dis",
    meta_maxdx,
    startRow = 26,
    startCol = 2
  )
  
  # 6. Average number of discharge codes
  
  meta_avgdx = round(mean(dx_meta$dx_counter), 0) #Average number of dx codes per ED visit
  print(meta_avgdx)
  
  writeData(
    temp1_re_combined,
    "Metadata_ED_dis",
    meta_avgdx,
    startRow = 30,
    startCol = 2
  )
} else{
  cat("ED data not submitted, thus not processed.")
}

#######
#HOSP Metadata
#######
#INPUT METADATA FIGURES INTO APPROPRIATE METADATA TABS

if (nrow(hosp) > 0) {
  #HOSP
  hosp_meta <- desired_data_meta %>%
    filter(REC_TYPE == 2,)
  
  dx_meta1 <- hosp_meta[, grep("^dx", colnames(data), ignore.case = TRUE)]
  dx_meta1 <- replace(dx_meta1,is.na(dx_meta1),"")
  
  dx_meta <- dx_meta1 %>%
    mutate(dx_counter = rowSums(!dx_meta1 == "")) # Sum of codes in row. [NOTE NEEDS UPDATE TO is.na() language]
  
  #Meta-Data Code
  # 4. Percent of HOSP visits with no dx codes
  meta_nodx = round((sum(dx_meta$dx_counter == 0) / nrow(hosp)) * 100, 2)
  print(paste0(meta_nodx, "%"))
  
  writeData(
    temp1_re_combined,
    "Metadata_HOSP_dis",
    meta_nodx,
    startRow = 22,
    startCol = 2,
    colNames = F
  )
  
  # 5. Max number of discharge codes
  
  
  meta_maxdx <- ncol(dx_meta1) - 1
  print(meta_maxdx)
  
  writeData(
    temp1_re_combined,
    "Metadata_HOSP_dis",
    meta_maxdx,
    startRow = 26,
    startCol = 2
  )
  
  # 6. Average number of discharge codes
  meta_avgdx = round(mean(dx_meta$dx_counter), 0) #Average number of dx codes per ED visit
  print(meta_avgdx)
  
  writeData(
    temp1_re_combined,
    "Metadata_HOSP_dis",
    meta_avgdx,
    startRow = 30,
    startCol = 2
  )
} else{
  cat("HOSP data not submitted, thus not processed.")
}

############################################################
############################################################
################# Output Program ###########################
############################################################
############################################################

#Set up output directory
#must run before output program
#NECESSARY FOR ED AND HOSP

#STEP PREVIOUSLY DONE, just in case no output folder
#target.dir<-paste0(getwd(),"/Output/")#labels output folder
#dir.create(target.dir) #Creates folder for outputs
#if(dir.exists(target.dir)){unlink(paste0(target.dir,"*"))}

###################################
#ED
###################################

if (nrow(ed) > 0) {
  #JURISDICTION SHEET "final_ed"
  writeData(
    temp1_re_combined,
    "Jurisdiction_Rpt_ED_dis",
    final_ed,
    startRow = 2,
    startCol = 1,
    colNames = TRUE
  )
  
  #county worksheet ED "county_final_ed"
  writeData(
    temp1_re_combined,
    "County_Rpt_ED_dis",
    county_final_ed,
    startRow = 2,
    startCol = 1,
    colNames = TRUE
  )
  
} else{
  cat("ED data not submitted, thus not processed.")
}

###################################
#HOSP
###################################


if (nrow(hosp) > 0) {
  #JURISDICTION SHEET "final_hosp"
  writeData(
    temp1_re_combined,
    "Jurisdiction_Rpt_HOSP_dis",
    final_hosp,
    startRow = 2,
    startCol = 1,
    colNames = TRUE
  )
  
  #county worksheet HOSP "county_final_hosp"
  writeData(
    temp1_re_combined,
    "County_Rpt_HOSP_dis",
    county_final_hosp,
    startRow = 2,
    startCol = 1,
    colNames = TRUE
  )
  
  #LINE-LEVEL SAVED TO SEPARATE DATASET
  
} else{
  cat("HOSP data not submitted, thus not processed.")
}

#Save workbook
openxlsx::saveWorkbook(
  temp1_re_combined,
  file = paste0(
    target.dir,
    state,
    "_OD2A_S_Annual_",
    Year,
    "_Discharge_Aggregate_re_combined.xlsx"
  ),
  overwrite = T
)


###############################################
######### END OUTPUT PROGRAM ##################
###############################################

#REVIEW OUTPUT AND ADD NECESSARY METADATA RESPONSES!!!!!!!
#REVIEW OUTPUT AND ADD NECESSARY METADATA RESPONSES!!!!!!!
#REVIEW OUTPUT AND ADD NECESSARY METADATA RESPONSES!!!!!!!
#REVIEW OUTPUT AND ADD NECESSARY METADATA RESPONSES!!!!!!!
#REVIEW OUTPUT AND ADD NECESSARY METADATA RESPONSES!!!!!!!

#####
#Fin#
#####
