 library(rmarkdown)
library(lubridate)
 library(rvest)
 library(magrittr)
 library(stringr)
 library(dplyr)
 library(zoo)
 library(tidyr)
 library(scales)
 library(ggthemes)
 library(RColorBrewer)
 library(reshape2)
 library(knitr)
 library(tidyverse)
library(expss)
library(pander)
 library(kableExtra)
 library(blob)
date1<-as.Date("2021-07-06") 
date2<-as.Date(Sys.Date()-days(1)) 


# create data sets
bedded_df<-load_dsBedded(date2)
clinics_df<-load_dsClinics(date2)%>% select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
                                            -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
                                            -SameDayScheduleRateArrowImage,-AverageLagArrowImage) 
communityservices_df<-load_dsCommunityServices(date2)
currentstaffing_df<-load_dsCurrentStaffing(date2)
passfailcheck_df<-load_dsPassFailCheck(date2)
specialtyclinics_df<-load_dsSpecialtyClinics(date2)
#staffingpassfailcheck_df<-load_dsStaffingPassFailCheck(date2)
surgery_df<-load_dsSurgery(date2)
system_df<-load_dsSystem(date2)
urgentemergent_df<-load_dsUrgentEmergent(date2)
errormessage<-loadErrorMessage(date2)



tibble(bedded_df)
tibble(clinics_df)
data.frame(communityservices_df)
data.frame(currentstaffing_df)
data.frame(specialtyclinics_df)
data.frame(surgery_df)
data.frame(system_df)
data.frame(urgentemergent_df)

clinics_df<-clinics_df %>% select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
                                                     -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
                                                     -SameDayScheduleRateArrowImage,-AverageLagArrowImage) 
clinics_df<-data.frame(t(clinics_df)) %>%select(X2,X1) 
