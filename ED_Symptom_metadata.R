library(haven)
library(Hmisc)
library(stringr)
library(ggplot2)
library(tidyverse)
library(psych)
library(zoo)
library(plotly)
library(xlsx)

data <- read_sas("//cdc.gov/project/NCIPC_DOP_OD2A/DOSE/DOSE Papers/ED_Symptoms/SAS/Datasets/mar2122opioid_7site.sas7bdat")

data <- read.csv("C:/Users/pqw6/OneDrive - CDC/OD2A 2.0 Update Files/race_ethnicity_test_data_jan22_dec22.csv.csv")









#NEED TO UPDATE WITH AZ DATA DETAILS
data_az <- read.csv("C:/Users/pqw6/OneDrive - CDC/ED Symptom Severity/data/az_opioid_data_details_jan19_jan23.csv")

data1 <- data_az %>%
  mutate(
    cc_length=nchar(dataDetails.ChiefComplaintParsed),
    dd_count= str_count(dataDetails.CCDD, ";")-1, #counting semi-colons minus 1 for last code.
    #facility = as.factor(dataDetails.HospitalName)   #factor Site
    facility = dataDetails.HospitalName
  )

describe(data1[, c("cc_length","dd_count")])

#Graph by MonthYear

#Summarize
facility_summary <- data1 %>% 
  group_by(facility,dataDetails.MonthYear) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    cc_mean = mean(cc_length),
    dd_mean = mean(dd_count),
    cc_median = median(cc_length),
    dd_median = median(dd_count)
    )%>%
  mutate(
    date=as.yearmon(dataDetails.MonthYear)
  )

str(facility_summary)
View(facility_summary)

##### MEAN
az_cc_length_plot1 <- ggplot(facility_summary, aes(x=date, y=cc_mean))+
  geom_line(aes(color=facility))+
    labs(y="Mean CC Length", x="Date", title = "Mean CC Character Length by Facility")
  #+
  # stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")

az_cc_length_plot1


ggplotly(az_cc_length_plot1)


#####
#####
#####
#####
#####

#CO
data_co <- read.csv("C:/Users/pqw6/OneDrive - CDC/ED Symptom Severity/data/co_opioid_data_details_jan19_jan23.csv")

data1 <- data_co %>%
  mutate(
    cc_length=nchar(dataDetails.ChiefComplaintParsed),
    dd_count= str_count(dataDetails.CCDD, ";")-1, #counting semi-colons minus 1 for last code.
    #facility = as.factor(dataDetails.HospitalName)   #factor Site
    facility = dataDetails.HospitalName
  )

describe(data1[, c("cc_length","dd_count")])

#Graph by MonthYear

#Summarize
facility_summary <- data1 %>% 
  group_by(facility,dataDetails.MonthYear) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    cc_mean = mean(cc_length),
    dd_mean = mean(dd_count),
    cc_median = median(cc_length),
    dd_median = median(dd_count)
  )%>%
  mutate(
    date=as.yearmon(dataDetails.MonthYear)
  )

str(facility_summary)
View(facility_summary)

##### MEAN
co_cc_length_plot1 <- ggplot(facility_summary, aes(x=date, y=cc_mean))+
  geom_line(aes(color=facility))+
  labs(y="Mean CC Length", x="Date", title = "Mean CC Character Length by Facility")
#+
# stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")

co_cc_length_plot1


ggplotly(co_cc_length_plot1)


#####
#NE

data_ne <- read.csv("C:/Users/pqw6/OneDrive - CDC/ED Symptom Severity/data/ne_opioid_data_details_jan19_jan23.csv")

data1 <- data_ne %>%
  mutate(
    cc_length=nchar(dataDetails.ChiefComplaintParsed),
    dd_count= str_count(dataDetails.CCDD, ";")-1, #counting semi-colons minus 1 for last code.
    #facility = as.factor(dataDetails.HospitalName)   #factor Site
    facility = dataDetails.HospitalName
  )

describe(data1[, c("cc_length","dd_count")])

#Graph by MonthYear

#Summarize
facility_summary <- data1 %>% 
  group_by(facility,dataDetails.MonthYear) %>%                      # Group data
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    cc_mean = mean(cc_length),
    dd_mean = mean(dd_count),
    cc_median = median(cc_length),
    dd_median = median(dd_count)
  )%>%
  mutate(
    date=as.yearmon(dataDetails.MonthYear)
  )

str(facility_summary)
View(facility_summary)

##### MEAN
ne_cc_length_plot1 <- ggplot(facility_summary, aes(x=date, y=cc_mean))+
  geom_line(aes(color=facility))+
  labs(y="Mean CC Length", x="Date", title = "Mean CC Character Length by Facility")
#+
# stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")

ne_cc_length_plot1


plot<- ggplotly(ne_cc_length_plot1)
















######### MEDIAN

az_cc_length_plot2 <- ggplot(facility_summary, aes(x=date, y=cc_median))+
  geom_line(aes(color=facility))+
  labs(y="Median CC Length", x="Date", title = "Median CC Character Length by Facility")
#+
# stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")

az_cc_length_plot2


ggplotly(az_cc_length_plot2)








#+ scale_x_discrete(limits=c("SITE", "SITE"))

(cc_length_plot2 <- ggplot(data1, aes(x=reorder(site,-cc_length,nr.rm =TRUE), y=cc_length))+
  geom_boxplot(notch=TRUE, outlier.colour="red", outlier.size=0.5, outlier.shape=8)+
    labs(y="CC Length", x="Site reordered by mean CC Length",title = "CC Character Length (including spaces)"))+
  stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")


(dd_count_plot1 <- ggplot(data1, aes(x=site, y=dd_count))+
    geom_boxplot(notch=TRUE, outlier.colour="red", outlier.size=0.5, outlier.shape=8)+
    labs(y="DD Count", x="Site", title = "DD Count")+
    stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple")
)


#

(dd_count_plot2 <- ggplot(data1, aes(x=reorder(site,-dd_count,nr.rm =TRUE), y=dd_count))+
    geom_boxplot(notch=TRUE, outlier.colour="red", outlier.size=0.5, outlier.shape=8)+
    labs(y="DD Count", x="Site reordered by mean DD Count",title = "DD Count Ordered")+
  stat_summary(fun=mean, geom="point", shape=23, size=0.4, color="purple"))
  #+ scale_x_discrete(limits=c("MO")))

library(xlsx)
write.xlsx(as.data.frame(site_summary), file="//cdc.gov/project/NCIPC_DOP_OD2A/DOSE/DOSE Papers/ED_Symptoms/metadata/ccdd_state_metadata.xlsx", sheetName="Summary", row.names=FALSE)


######################
######################
######################
######################
######################
######################

#NEED TO PULL IN SAS DATASET, then output csv for Joinpoint

reformat <- read_sas("//cdc/project/NCIPC_DOP_OD2A/DOSE/DOSE Papers/ED_Symptoms/SAS/Datasets/jan18sep22_allsite4d.sas7bdat")
#write.xlsx(reformat, file ="//cdc/project/NCIPC_DOP_OD2A/DOSE/DOSE Papers/ED_Symptoms/SAS/Datasets/jan18sep22_allsite4d.xlsx")
write.csv(reformat,  file ="//cdc/project/NCIPC_DOP_OD2A/DOSE/DOSE Papers/ED_Symptoms/SAS/Datasets/jan18sep22_allsite4d.csv")
