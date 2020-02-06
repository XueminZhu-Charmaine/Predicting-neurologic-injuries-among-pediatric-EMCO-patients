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
sum(is.na(ELSO_JHH_Data_included$RunNo))

## Keep patients with more than 6 hours on ECMO
sum(is.na(ELSO_JHH_Data$HoursECMO))
ELSO_JHH_Data_included = transform(ELSO_JHH_Data_included, HoursECMO = as.numeric(HoursECMO), AgeYears = as.numeric(AgeYears))
ELSO_JHH_Data_included = dplyr::filter(ELSO_JHH_Data_included, HoursECMO > 6) 
nrow(ELSO_JHH_Data_included)
sum(is.na(ELSO_JHH_Data_included$HoursECMO))
sum(is.na(ELSO_JHH_Data_included$TimeOff))

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
write.xlsx(ELSO_JHH_Data_included_final, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/ELSO_JHH_Data_included_final_updated.xlsx")


#Demographic for included ELSO data set:

## Races:
table(ELSO_JHH_Data_included_final$Races)
sum(is.na(ELSO_JHH_Data_included_final$Races))

### Calculate % of race:

percent = function(x,y){
  x*100/y
}

races = c(63, 46, 9, 8, 9, 2, 6)

for (i in races){
  z = percent(x = i, y = 143)
  print (z)
}

## Ages:
mean(ELSO_JHH_Data_included_final$AgeYears)
sd(ELSO_JHH_Data_included_final$AgeYears)
sum(is.na(ELSO_JHH_Data_included_final$AgeYears))

#Sexes:
table(ELSO_JHH_Data_included_final$Sex)
sum(is.na((ELSO_JHH_Data_included_final$Sex)))

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

#Count and calculate the percentage of all complications:
comp = c(87,25,53,4,1,72,10,8,5)

for (i in comp){
  z = percent(x = i, y = 143)
  print (z)
}

#Export label space for all complications:
write.xlsx(label_all_complications, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/Included_ELSO_all_complication_2011_to_2018.xlsx")

# Check how much out final ELSO list and Dr. Bembea's final list match:
Bembea = read.csv(file = "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/2020_01_24_ELSO_JHH_Linked_Data.csv")
matched_patients = merge(Bembea, ELSO_JHH_Data_included_final_by_time_on, by = "PatientID")
nrow(matched_patients)
#########################

# Filter by Time On:
# Handle date-time data column:
class(ELSO_JHH_Data_included$TimeOn)
head(ELSO_JHH_Data_included$TimeOn)
ELSO_JHH_Data_included = ELSO_JHH_Data_included %>% mutate(TimeOn = mdy_hms(TimeOn))
ELSO_JHH_Data_included$TimeOn = as.POSIXct(ELSO_JHH_Data_included$TimeOn)

#Keep only patients between June 1, 2011 – December 31, 2018:
date1 = as.POSIXct("2011-06-01 00:00:00") #lower bound
date2 = as.POSIXct("2018-12-31 23:59:00") #upper bound
time_range = interval(date1, date2)   #desired range
ELSO_JHH_Data_included_final_by_time_on = ELSO_JHH_Data_included[ELSO_JHH_Data_included$TimeOn %within% time_range,]
nrow(ELSO_JHH_Data_included_final_by_time_on)

# Export the data:
write.xlsx(ELSO_JHH_Data_included_final, "//win.ad.jhu.edu/data/accm-research$/bembeaarcpicu2/ECMOAnon/TeamShiba/DataProc/ELSO_JHH_Data_included_final_updated.xlsx")
check_2_methods =  merge(ELSO_JHH_Data_included_final, ELSO_JHH_Data_included_final_by_time_on, by = "PatientID")
difference = setdiff(ELSO_JHH_Data_included_final_by_time_on$PatientID, ELSO_JHH_Data_included_final$PatientID)
difference

mistmatchedPT = ELSO_JHH_Data_included_final_by_time_on %>%
  filter(str_detect(PatientID, difference))
mistmatchedPT$HoursECMO
mistmatchedPT$TimeOff
# This patient had time off in 2019