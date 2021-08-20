library(rmarkdown)
library(lubridate)
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
library(kableExtra)
library(scales)
library(forcats)
library(fontawesome)
library(gt)

date1<-as.Date("2021-07-06") 
date2<-as.Date(Sys.Date()-days(1)) 

percent <- function(x){
  paste0(round(x*100,1),"%")
}

round_percent <- function(x){
  paste0(round(x*100,0),"%")
}

comma<- function(x){
  format(x, big.mark = ",", scientific = FALSE) 
}

asterisk<- function(x){
  paste0(x, "*")
}
#SPECIALTY CLINIC BY LOCATION
fvspecialtyclinics_df<-data.frame(load_dsFVSpecialtyClinics(date2))%>% 
  select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
         -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
         -SameDayScheduleRateArrowImage,-AverageLagArrowImage)%>% 
  mutate_at(c("Delta_CountArrivedVisits","Delta_NoShowSameDayCancelRate",
              "Delta_CountScheduled","Delta_CountSameDaySchedule","Delta_SameDayScheduleRate",
              "Delta_AverageLag"),percent)%>%
  mutate_at(c("CD_NoShowSameDayCancelRate","CD_SameDayScheduleRate"),round_percent)%>%
  mutate_at(c("CD_CountArrivedVisits","CD_CountSameDaySchedule","CD_CountScheduled"),comma)%>%
  mutate_at(c("CD_CountScheduled", "Delta_CountScheduled"), asterisk)

fvspecialtyclinics_df<-data.frame(t(fvspecialtyclinics_df)) %>%select(X2,X1)%>%
  rename(fv_campus="X1",metro_fv="X2")%>%
  tibble::rownames_to_column(var="specialty_clinic_metrics")

f2=c("Arrived Visit Volume","Missed Appointments/Same Day Cancel Rate","Same Day Schedule Count",
     "Same Day Schedule Rate",paste0("Scheduled for ",
                                     weekdays(Sys.Date())," (",
                                     format(Sys.Date(),"%m/%d/%y"),")"))

fv_campus_actuals<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="CD_CountArrivedVisits"|
           specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"|
           specialty_clinic_metrics=="CD_CountSameDaySchedule"|
           specialty_clinic_metrics=="CD_SameDayScheduleRate"|
           specialty_clinic_metrics=="CD_CountScheduled"|specialty_clinic_metrics=="CD_AverageLag")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                                                  weekdays(Sys.Date())," (",
                                                                                                  format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(fv_campus_actuals="fv_campus")%>%
  select(specialty_clinic_metrics2,fv_campus_actuals)

fv_campus_delta<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="Delta_CountArrivedVisits"|
           specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"|
           specialty_clinic_metrics=="Delta_CountSameDaySchedule"|
           specialty_clinic_metrics=="Delta_SameDayScheduleRate"|
           specialty_clinic_metrics=="Delta_CountScheduled"|
           specialty_clinic_metrics=="Delta_AverageLag")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                                                     weekdays(Sys.Date())," (",
                                                                                                     format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(fv_campus_delta="fv_campus")%>%
  select(fv_campus_delta)

fv_campus_arrow<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="CountArrivedVisitsArrow"|
           specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"|
           specialty_clinic_metrics=="CountSameDayScheduleArrow"|
           specialty_clinic_metrics=="SameDayScheduleRateArrow"|
           specialty_clinic_metrics=="CountScheduledArrow"|
           specialty_clinic_metrics=="AverageLagArrow")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                                                    weekdays(Sys.Date())," (",
                                                                                                    format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(fv_campus=(sub("_.*", "", fv_campus)))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(fv_campus_arrow="fv_campus")%>%
  select(fv_campus_arrow)


metro_fv_actuals<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="CD_CountArrivedVisits"|
           specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"|
           specialty_clinic_metrics=="CD_CountSameDaySchedule"|
           specialty_clinic_metrics=="CD_SameDayScheduleRate"|
           specialty_clinic_metrics=="CD_CountScheduled"|
           specialty_clinic_metrics=="CD_AverageLag")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                                                  weekdays(Sys.Date())," (",
                                                                                                  format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(metro_fv_actuals="metro_fv")%>%
  select(metro_fv_actuals)

metro_fv_delta<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="Delta_CountArrivedVisits"|
           specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"|
           specialty_clinic_metrics=="Delta_CountSameDaySchedule"|
           specialty_clinic_metrics=="Delta_SameDayScheduleRate"|
           specialty_clinic_metrics=="Delta_CountScheduled"|
           specialty_clinic_metrics=="Delta_AverageLag")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                                                     weekdays(Sys.Date())," (",
                                                                                                     format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(metro_fv_delta="metro_fv")%>%
  select(metro_fv_delta)

metro_fv_arrow<-fvspecialtyclinics_df%>%select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="CountArrivedVisitsArrow"|
           specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"|
           specialty_clinic_metrics=="CountSameDayScheduleArrow"|
           specialty_clinic_metrics=="SameDayScheduleRateArrow"|
           specialty_clinic_metrics=="CountScheduledArrow"|
           specialty_clinic_metrics=="AverageLagArrow")%>%
  mutate(specialty_clinic_metrics2=case_when(specialty_clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                                             specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                                             specialty_clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                                             specialty_clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                                             specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                                                    weekdays(Sys.Date())," (",
                                                                                                    format(Sys.Date(),"%m/%d/%y"), ")"),
                                             specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(metro_fv=(sub("_.*", "", metro_fv)))%>%
  arrange(factor(specialty_clinic_metrics2,f2))%>%
  rename(metro_fv_arrow="metro_fv")%>%
  select(metro_fv_arrow)

fvspecialty_clinic_metrics_table<-cbind(fv_campus_actuals, fv_campus_delta, fv_campus_arrow, metro_fv_actuals, metro_fv_delta, metro_fv_arrow)
fvspecialty_clinic_metrics_df<-rbind(c(" ", paste0(format(Sys.Date()-days(1),"%m/%d/%y")),
                                     paste0("%","$\\delta$", "from Prior Time Period"), 
                                     paste0(format(Sys.Date()-days(1),"%m/%d/%y")),
                                     paste0("%","$\\delta$", "from Prior Time Period")), fvspecialty_clinic_metrics_table)


#CENSUS METRICS
fvbedded_df<-data.frame(load_dsFVBedded(date2))%>%
  select(-CountAdmissionsArrowImage,-CountDischargesArrowImage,
         -InpatientCensusNICUArrowImage,-InpatientCensusPICUArrowImage,
         -InpatientCensusAcuteArrowImage,-InpatientTotalCensusArrowImage, -OtherBeddedCensusNICUArrowImage,
         -OtherBeddedCensusPICUArrowImage,-OtherBeddedCensusAcuteArrowImage,-OtherBeddedTotalCensusArrowImage,
         -TotalCensusArrowImage, -CD_InpatientCensusPICU, -Delta_InpatientCensusPICU,-CD_OtherBeddedCensusPICU,
         -Delta_OtherBeddedCensusPICU)%>% 
  mutate_at(c("Delta_CountAdmissions","Delta_CountDischarges","Delta_InpatientCensusNICU",
              "Delta_InpatientCensusAcute","Delta_InpatientTotalCensus",
              "Delta_OtherBeddedCensusNICU","Delta_OtherBeddedCensusAcute",
              "Delta_OtherBeddedTotalCensus","Delta_TotalCensus"),percent)%>%
  mutate_at(c("CD_CountAdmissions","CD_CountDischarges","CD_InpatientCensusNICU",
              "CD_InpatientCensusAcute","CD_InpatientTotalCensus",
              "CD_OtherBeddedCensusNICU","CD_OtherBeddedCensusAcute",
              "CD_OtherBeddedTotalCensus","TotalCensus"),comma)

fvbedded_df<-data.frame(t(fvbedded_df)) %>%select(t.fvbedded_df.)%>%
  rename(fv_bed="t.fvbedded_df.")%>%
  tibble::rownames_to_column(var="bedded_metrics")

f5<-c("Midnight Census - Inpatient","Inpatient NICU",
      "Inpatient Acute Care", "Inpatient Total", "Midnight Census - Other Bedded", "Other Bedded NICU",
      "Other Bedded Acute Care", "Other Bedded Total",
      "Midnight Census - Total","Admission - All Bedded","Discharges - All Bedded")

fv_bed_actuals<-rbind(fvbedded_df, c("Midnight Census - Inpatient"," ", " "),c("Midnight Census - Other Bedded"," ", " ")) %>%select(bedded_metrics,fv_bed)%>%
  filter(bedded_metrics=="CD_CountAdmissions"|
           bedded_metrics=="CD_CountDischarges"|
           bedded_metrics=="CD_InpatientCensusNICU"|
           bedded_metrics=="CD_InpatientCensusAcute"|
           bedded_metrics=="CD_InpatientTotalCensus"|
           bedded_metrics=="CD_OtherBeddedCensusNICU"|
           bedded_metrics=="CD_OtherBeddedCensusAcute"|
           bedded_metrics=="CD_OtherBeddedTotalCensus"|
           bedded_metrics=="TotalCensus"|
           bedded_metrics=="Midnight Census - Inpatient"|
           bedded_metrics=="Midnight Census - Other Bedded")%>%
  mutate(bedded_metrics2=
           case_when(bedded_metrics=="CD_CountAdmissions"~ "Admission - All Bedded",
                     bedded_metrics=="CD_CountDischarges"~"Discharges - All Bedded",
                     bedded_metrics=="CD_InpatientCensusNICU"~"Inpatient NICU",
                     bedded_metrics=="CD_InpatientCensusAcute"~"Inpatient Acute Care",
                     bedded_metrics=="CD_InpatientTotalCensus"~"Inpatient Total",
                     bedded_metrics=="CD_OtherBeddedCensusNICU"~"Other Bedded NICU",
                     bedded_metrics=="CD_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                     bedded_metrics=="CD_OtherBeddedTotalCensus"~"Other Bedded Total",
                     bedded_metrics=="TotalCensus"~"Midnight Census - Total",
                     bedded_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     bedded_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"
           ))%>%
  arrange(factor(bedded_metrics2,f5))%>%
  rename(fv_bedded_actuals="fv_bed")%>%
  select(bedded_metrics2,fv_bedded_actuals)

fv_bedded_delta<-rbind(fvbedded_df, c("Midnight Census - Inpatient"," ", " "),c("Midnight Census - Other Bedded"," "," "))%>%select(bedded_metrics,fv_bed)%>%
  filter(bedded_metrics=="Delta_CountAdmissions"|
           bedded_metrics=="Delta_CountDischarges"|
           bedded_metrics=="Delta_InpatientCensusNICU"|
           bedded_metrics=="Delta_InpatientCensusAcute"|
           bedded_metrics=="Delta_InpatientTotalCensus"|
           bedded_metrics=="Delta_OtherBeddedCensusNICU"|
           bedded_metrics=="Delta_OtherBeddedCensusAcute"|
           bedded_metrics=="Delta_OtherBeddedTotalCensus"|
           bedded_metrics=="Delta_TotalCensus"|
           bedded_metrics=="Midnight Census - Inpatient"|
           bedded_metrics=="Midnight Census - Other Bedded")%>%
  mutate(bedded_metrics2=
           case_when(bedded_metrics=="Delta_CountAdmissions"~ "Admission - All Bedded",
                     bedded_metrics=="Delta_CountDischarges"~"Discharges - All Bedded",
                     bedded_metrics=="Delta_InpatientCensusNICU"~"Inpatient NICU",
                     bedded_metrics=="Delta_InpatientCensusAcute"~"Inpatient Acute Care",
                     bedded_metrics=="Delta_InpatientTotalCensus"~"Inpatient Total",
                     bedded_metrics=="Delta_OtherBeddedCensusNICU"~"Other Bedded NICU",
                     bedded_metrics=="Delta_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                     bedded_metrics=="Delta_OtherBeddedTotalCensus"~"Other Bedded Total",
                     bedded_metrics=="Delta_TotalCensus"~"Midnight Census - Total",
                     bedded_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     bedded_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  arrange(factor(bedded_metrics2,f5))%>%
  rename(fv_bedded_delta="fv_bed")%>%
  select(fv_bedded_delta)

fv_bedded_arrow<-rbind(fvbedded_df, c("Midnight Census - Inpatient"," ", " "),c("Midnight Census - Other Bedded"," ", " "))%>%select(bedded_metrics,fv_bed)%>%
  filter(bedded_metrics=="CountAdmissionsArrow"|
           bedded_metrics=="CountDischargesArrow"|
           bedded_metrics=="InpatientCensusNICUArrow"|
           bedded_metrics=="InpatientCensusAcuteArrow"|
           bedded_metrics=="InpatientTotalCensusArrow"|
           bedded_metrics=="OtherBeddedCensusNICUArrow"|
           bedded_metrics=="OtherBeddedCensusAcuteArrow"|
           bedded_metrics=="OtherBeddedTotalCensusArrow"|
           bedded_metrics=="TotalCensusArrow"|
           bedded_metrics=="Midnight Census - Inpatient"|
           bedded_metrics=="Midnight Census - Other Bedded")%>%
  mutate(bedded_metrics2=
           case_when(bedded_metrics=="CountAdmissionsArrow"~ "Admission - All Bedded",
                     bedded_metrics=="CountDischargesArrow"~"Discharges - All Bedded",
                     bedded_metrics=="InpatientCensusNICUArrow"~"Inpatient NICU",
                     bedded_metrics=="InpatientCensusAcuteArrow"~"Inpatient Acute Care",
                     bedded_metrics=="InpatientTotalCensusArrow"~"Inpatient Total",
                     bedded_metrics=="OtherBeddedCensusNICUArrow"~"Other Bedded NICU",
                     bedded_metrics=="OtherBeddedCensusAcuteArrow"~"Other Bedded Acute Care",
                     bedded_metrics=="OtherBeddedTotalCensusArrow"~"Other Bedded Total",
                     bedded_metrics=="TotalCensusArrow"~"Midnight Census - Total",
                     bedded_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     bedded_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  mutate(fv_bed=(sub("_.*", "", fv_bed)))%>%
  arrange(factor(bedded_metrics2,f5))%>%
  rename(fv_bedded_arrow="fv_bed")%>%
  select(fv_bedded_arrow)

fvbedded_metrics_table<-cbind(fv_bed_actuals, fv_bedded_delta, fv_bedded_arrow)
fvbedded_metrics_df<-rbind(c(" ", paste0(format(Sys.Date()-days(1),"%m/%d/%y")),
                           paste0("%","$\\delta$", "from Prior Time Period"), 
                           paste0(format(Sys.Date()-days(1),"%m/%d/%y")),
                           paste0("%","$\\delta$", "from Prior Time Period")), 
                         fvbedded_metrics_table)


#Experience
fv_experience_df<-data.frame(load_dsFVExperience(date2))%>% 
  mutate_at(c("PatientExperienceRatingScore","PatientExperienceRecommendScore"),round_percent)

fv_experience_df<-data.frame(t(fv_experience_df)) %>%select(t.fv_experience_df.)%>%
  rename(fv_campus="t.fv_experience_df.")%>%
  tibble::rownames_to_column(var="experience_metrics")

f8<-c("Patient Experience Daily Comment")

experience_actuals<-fv_experience_df%>%select(experience_metrics,fv_campus)%>%
  filter(experience_metrics=="Comment")%>%
  mutate(experience_metrics2=case_when(experience_metrics=="Comment"~"Patient Experience Daily Comment"))%>%
  arrange(factor(experience_metrics,f8))%>%
  rename(fv_campus_actuals="fv_campus")%>%
  select(experience_metrics2,fv_campus_actuals)

experience_metrics_table<-cbind(experience_actuals)
experience_metrics_df<-rbind(c(" ", paste0(format(Sys.Date()-days(1),"%m/%d/%y"))), experience_metrics_table)

