# Data pre-processing for ECMO ELSO data set.
# By Vy Tran
# Goals: 
 ## Breakdown ELSO data by neurological complications and other types of complications
 ## Obtain demographic distribution for entire ELSO data set (~500) 

library(readxl)
library(tidyverse)
library(lubridate)
library(xlsx)

ELSO_JHH_Data = read_excel("//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/ELSO_JHH_Data.xls")
str(ELSO_JHH_Data)
summary(ELSO_JHH_Data)

# Apply exclusion condition on ELSO:

## Keep patients with 1 EMCO run
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data, RunNo < 2 ) 
nrow(ELSO_JHH_Data_included)

## Remove rows with NA in "HoursECMO"
which( colnames(ELSO_JHH_Data_included)=="HoursECMO")
ELSO_JHH_Data_included = ELSO_JHH_Data_included[complete.cases(ELSO_JHH_Data_included[ , 8]),] 
sum(is.na(ELSO_JHH_Data_included$HoursECMO))
nrow(ELSO_JHH_Data_included)

## Keep patients with more than 6 hours on ECMO
ELSO_JHH_Data_included = transform(ELSO_JHH_Data_included, HoursECMO = as.numeric(HoursECMO), AgeYears = as.numeric(AgeYears))
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data_included, HoursECMO > 6) 
nrow(ELSO_JHH_Data_included)

## Keep only patients less than 18 years old
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data_included, AgeYears < 18) 
nrow(ELSO_JHH_Data_included)

# Handle date-time data column:
class(ELSO_JHH_Data_included$TimeOff)
head(ELSO_JHH_Data_included$TimeOff)
ELSO_JHH_Data_included = ELSO_JHH_Data_included %>% mutate(TimeOff = mdy_hms(TimeOff))
ELSO_JHH_Data_included$TimeOff = as.POSIXct(ELSO_JHH_Data_included$TimeOff)

#Keep only patients between June 1, 2011 – December 31, 2018:
date1 = as.POSIXct("2011-06-01 00:00:00") #lower bound
date2 = as.POSIXct("2018-12-31 23:59:00") #upper bound
time_range = interval(date1, date2)   #desired range
ELSO_JHH_Data_included_final = ELSO_JHH_Data_included[ELSO_JHH_Data_included$TimeOff %within% time_range,]
nrow(ELSO_JHH_Data_included_final)

# Export the data:
write.xlsx(ELSO_JHH_Data_included_final, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/ELSO_JHH_Data_included_final.xlsx")


#Demographic for included ELSO data set:

## Races:
table(ELSO_JHH_Data_included_final$Races)

## Ages:
hist(ELSO_JHH_Data_included_final$AgeYears)
mean(ELSO_JHH_Data_included_final$AgeYears)
sd(ELSO_JHH_Data_included_final$AgeYears)
max(ELSO_JHH_Data_included_final$AgeYears)
min(ELSO_JHH_Data_included_final$AgeYears)

#Sexes:
table(ELSO_JHH_Data_included_final$Sex)

##########################################################

# Preprocess complication data:
ELSO_complication = read_excel("//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/ELSO_complication.xlsx")
colnames(ELSO_complication)
names(ELSO_complication)[names(ELSO_complication) == "PatientId"] = "PatientID"
ELSO_with_complication = merge(ELSO_JHH_Data_included_final, ELSO_complication, by="PatientID")
nrow(ELSO_with_complication)
length(unique(ELSO_with_complication$PatientID))

# Create a dataframe containing count of complications and codes:
label_all_complications = as.data.frame(table(ELSO_with_complication$Description))
colnames(label_all_complications) = c("Description", "Count")
which(colnames(ELSO_with_complication)=="Description")
which(colnames(ELSO_with_complication)=="Code")
Codes = ELSO_with_complication[, c(128,129)]
label_all_complications =  unique(merge(Codes, label_all_complications, by = "Description"))
view(label_all_complications)
#Export label space for all complications:
write.xlsx(label_all_complications, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/Included_ELSO_all_complication_2011_to_2018.xlsx")

