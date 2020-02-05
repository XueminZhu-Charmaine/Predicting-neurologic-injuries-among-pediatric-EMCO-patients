# Data pre-processing for ECMO ELSO data set.
# By Vy Tran
# Goals: 
 ## Breakdown ELSO data by by neurological complications
 ## other types of complications
 ## Demographic for entire ELSO data set (~500) 


setwd("D:/vytran/OneDrive - JHSPH/ECMO")
library(readxl)
library(tidyverse)

ELSO_JHH_Data = read_excel("//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/ELSO_JHH_Data.xls")

# Apply exclusion condition on ELSO:
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data, RunNo < 2 ) # Keep patients with 1 EMCO run
nrow(ELSO_JHH_Data_included)
which( colnames(ELSO_JHH_Data_included)=="HoursECMO")
ELSO_JHH_Data_included = ELSO_JHH_Data_included[complete.cases(ELSO_JHH_Data_included[ , 8]),] #Remove rows with NA in "HoursECMO"
sum(is.na(ELSO_JHH_Data_included$HoursECMO))
nrow(ELSO_JHH_Data_included)

ELSO_JHH_Data_included = transform(ELSO_JHH_Data_included, HoursECMO = as.numeric(HoursECMO), AgeDays = as.numeric(AgeDays))
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data_included, HoursECMO > 6) # Keep patients with more than 6 hours on ECMO
nrow(ELSO_JHH_Data_included)

#Filtering for age using AgeYears:
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data_included, AgeYears < 18.1) # Keep only patients less than 18 years old
nrow(ELSO_JHH_Data_included)

# Handle Date-time data column:
library(lubridate) 
class(ELSO_JHH_Data_included$TimeOff)
head(ELSO_JHH_Data_included$TimeOff)
ELSO_JHH_Data_included = ELSO_JHH_Data_included %>% mutate(TimeOff = mdy_hms(TimeOff))
ELSO_JHH_Data_included$TimeOff = as.POSIXct(ELSO_JHH_Data_included$TimeOff)

#Filtering for Patients between June 1, 2011 – December 31, 2018:
date1 = as.POSIXct("2011-06-01 00:00:00") #lower bound
date2 = as.POSIXct("2018-12-31 23:59:00") #upper bound
time_range = interval(date1, date2)   #desired range
ELSO_JHH_Data_included_final = ELSO_JHH_Data_included[ELSO_JHH_Data_included$TimeOff %within% time_range,]
nrow(ELSO_JHH_Data_included_final)

#Demographic for included ELSO data set:
total_patients_ELSO = nrow(ELSO_JHH_Data_included_final)
total_patients_ELSO

#Races:
table(ELSO_JHH_Data_included_final$Races)

# Ages:
hist(ELSO_JHH_Data_included_final$AgeDays)
mean(ELSO_JHH_Data_included_final$AgeDays)
sd(ELSO_JHH_Data_included_final$AgeDays)
max(ELSO_JHH_Data_included_final$AgeDays)
min(ELSO_JHH_Data_included_final$AgeDays)

#Sexes:
table(ELSO_JHH_Data_included_final$Sex)

####Preprocess label data:
ELSO_complication = read_excel("//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/ELSO_complication.xlsx")
colnames(ELSO_complication)
names(ELSO_complication)[names(ELSO_complication) == "PatientId"] = "PatientID"
ELSO_with_complication = merge(ELSO_JHH_Data_included_final, ELSO_complication, by="PatientID" )
nrow(ELSO_with_complication)
length(unique(ELSO_with_complication$PatientID))
label_all_complication = as.data.frame(table(ELSO_with_complication$Description))

#Export label space for all complications:
library(xlsx)
write.xlsx(label_all_complication, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/Included_ELSO_all_complication_2011_to_2018.xlsx")

