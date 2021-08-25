library(zoo)
library(tidyverse)
library(kableExtra)
library(fontawesome)

date<-as.Date(Sys.Date()-days(1)) 

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
fvspecialtyclinics_df<-data.frame(load_dsFVSpecialtyClinics(date))%>% 
  select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
         -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
         -SameDayScheduleRateArrowImage,-AverageLagArrowImage)%>% 
  mutate_at(c("Delta_CountArrivedVisits","Delta_NoShowSameDayCancelRate",
              "Delta_CountScheduled","Delta_CountSameDaySchedule","Delta_SameDayScheduleRate",
              "Delta_AverageLag"),percent)%>%
  mutate_at(c("CD_NoShowSameDayCancelRate","CD_SameDayScheduleRate"),round_percent)%>%
  mutate_at(c("CD_CountArrivedVisits","CD_CountSameDaySchedule","CD_CountScheduled"),comma)%>%
  mutate_at(c("CD_CountScheduled", "Delta_CountScheduled"), asterisk)

fvspecialtyclinics_df<-data.frame(t(fvspecialtyclinics_df))%>%
  select(X2,X1)%>%
  rename(fv_campus="X1",metro_fv="X2")%>%
  rownames_to_column(var="specialty_clinic_metrics")

fv1=c("Arrived Visit Volume", "Missed Appointments/Same Day Cancel Rate",
     "Same Day Schedule Count", "Same Day Schedule Rate",
     paste0("Scheduled for ", weekdays(Sys.Date())," (", format(Sys.Date(),"%m/%d/%y"),")"))

fv_campus_actuals<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="CD_CountArrivedVisits"|
         specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"|
         specialty_clinic_metrics=="CD_CountSameDaySchedule"|
         specialty_clinic_metrics=="CD_SameDayScheduleRate"|
         specialty_clinic_metrics=="CD_CountScheduled"|
         specialty_clinic_metrics=="CD_AverageLag")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                                                          format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(fv_campus_actuals="fv_campus")%>%
  select(specialty_clinic_metrics2,fv_campus_actuals)

fv_campus_delta<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="Delta_CountArrivedVisits"|
         specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"|
         specialty_clinic_metrics=="Delta_CountSameDaySchedule"|
         specialty_clinic_metrics=="Delta_SameDayScheduleRate"|
         specialty_clinic_metrics=="Delta_CountScheduled"|
         specialty_clinic_metrics=="Delta_AverageLag")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",weekdays(Sys.Date())," (",
                                                                             format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(fv_campus_delta="fv_campus")%>%
  select(fv_campus_delta)

fv_campus_arrow<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,fv_campus)%>%
  filter(specialty_clinic_metrics=="CountArrivedVisitsArrow"|
         specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"|
         specialty_clinic_metrics=="CountSameDayScheduleArrow"|
         specialty_clinic_metrics=="SameDayScheduleRateArrow"|
         specialty_clinic_metrics=="CountScheduledArrow"|
         specialty_clinic_metrics=="AverageLagArrow")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",  weekdays(Sys.Date())," (",
                                                                             format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(fv_campus=(sub("_.*", "", fv_campus)))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(fv_campus_arrow="fv_campus")%>%
  select(fv_campus_arrow)


metro_fv_actuals<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="CD_CountArrivedVisits"|
         specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"|
         specialty_clinic_metrics=="CD_CountSameDaySchedule"|
         specialty_clinic_metrics=="CD_SameDayScheduleRate"|
         specialty_clinic_metrics=="CD_CountScheduled"|
         specialty_clinic_metrics=="CD_AverageLag")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                                                          format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(metro_fv_actuals="metro_fv")%>%
  select(metro_fv_actuals)

metro_fv_delta<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="Delta_CountArrivedVisits"|
         specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"|
         specialty_clinic_metrics=="Delta_CountSameDaySchedule"|
         specialty_clinic_metrics=="Delta_SameDayScheduleRate"|
         specialty_clinic_metrics=="Delta_CountScheduled"|
         specialty_clinic_metrics=="Delta_AverageLag")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                                                             format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(metro_fv_delta="metro_fv")%>%
  select(metro_fv_delta)

metro_fv_arrow<-fvspecialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_fv)%>%
  filter(specialty_clinic_metrics=="CountArrivedVisitsArrow"|
         specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"|
         specialty_clinic_metrics=="CountSameDayScheduleArrow"|
         specialty_clinic_metrics=="SameDayScheduleRateArrow"|
         specialty_clinic_metrics=="CountScheduledArrow"|
         specialty_clinic_metrics=="AverageLagArrow")%>%
  mutate(specialty_clinic_metrics2=
           case_when(specialty_clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     specialty_clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     specialty_clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     specialty_clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                                                            format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(metro_fv=(sub("_.*", "", metro_fv)))%>%
  arrange(factor(specialty_clinic_metrics2,fv1))%>%
  rename(metro_fv_arrow="metro_fv")%>%
  select(metro_fv_arrow)

fvspecialty_clinic_metrics_table<-cbind(fv_campus_actuals, fv_campus_delta, fv_campus_arrow, 
                                        metro_fv_actuals, metro_fv_delta, metro_fv_arrow)

rm(fv1, fv_campus_actuals, fv_campus_delta, fv_campus_arrow, metro_fv_actuals, metro_fv_delta, metro_fv_arrow)

#CENSUS METRICS
fvcensus_df<-data.frame(load_dsFVCensus(date))%>%
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

fvcensus_df<-data.frame(t(fvcensus_df)) %>%select(t.fvcensus_df.)%>%
  rename(fv_census="t.fvcensus_df.")%>%
  rownames_to_column(var="census_metrics")

fv2<-c("Midnight Census - Inpatient","Inpatient NICU",
      "Inpatient Acute Care", "Inpatient Total", "Midnight Census - Other Bedded", "Other Bedded NICU",
      "Other Bedded Acute Care", "Other Bedded Total",
      "Midnight Census - Total","Admission - All Bedded","Discharges - All Bedded")

fv_census_actuals<-rbind(fvcensus_df, c("Midnight Census - Inpatient"," ", " "), c("Midnight Census - Other Bedded"," ", " "))%>%
  select(census_metrics,fv_census)%>%
  filter(census_metrics=="CD_CountAdmissions"|
         census_metrics=="CD_CountDischarges"|
         census_metrics=="CD_InpatientCensusNICU"|
         census_metrics=="CD_InpatientCensusAcute"|
         census_metrics=="CD_InpatientTotalCensus"|
         census_metrics=="CD_OtherBeddedCensusNICU"|
         census_metrics=="CD_OtherBeddedCensusAcute"|
         census_metrics=="CD_OtherBeddedTotalCensus"|
         census_metrics=="TotalCensus"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
           case_when(census_metrics=="CD_CountAdmissions"~ "Admission - All Bedded",
                     census_metrics=="CD_CountDischarges"~"Discharges - All Bedded",
                     census_metrics=="CD_InpatientCensusNICU"~"Inpatient NICU",
                     census_metrics=="CD_InpatientCensusAcute"~"Inpatient Acute Care",
                     census_metrics=="CD_InpatientTotalCensus"~"Inpatient Total",
                     census_metrics=="CD_OtherBeddedCensusNICU"~"Other Bedded NICU",
                     census_metrics=="CD_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                     census_metrics=="CD_OtherBeddedTotalCensus"~"Other Bedded Total",
                     census_metrics=="TotalCensus"~"Midnight Census - Total",
                     census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"
           ))%>%
  arrange(factor(census_metrics2,fv2))%>%
  rename(fv_census_actuals="fv_census")%>%
  select(census_metrics2,fv_census_actuals)

fv_census_delta<-rbind(fvcensus_df, c("Midnight Census - Inpatient"," ", " "),c("Midnight Census - Other Bedded"," "," "))%>%
  select(census_metrics,fv_census)%>%
  filter(census_metrics=="Delta_CountAdmissions"|
         census_metrics=="Delta_CountDischarges"|
         census_metrics=="Delta_InpatientCensusNICU"|
         census_metrics=="Delta_InpatientCensusAcute"|
         census_metrics=="Delta_InpatientTotalCensus"|
         census_metrics=="Delta_OtherBeddedCensusNICU"|
         census_metrics=="Delta_OtherBeddedCensusAcute"|
         census_metrics=="Delta_OtherBeddedTotalCensus"|
         census_metrics=="Delta_TotalCensus"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
           case_when(census_metrics=="Delta_CountAdmissions"~ "Admission - All Bedded",
                     census_metrics=="Delta_CountDischarges"~"Discharges - All Bedded",
                     census_metrics=="Delta_InpatientCensusNICU"~"Inpatient NICU",
                     census_metrics=="Delta_InpatientCensusAcute"~"Inpatient Acute Care",
                     census_metrics=="Delta_InpatientTotalCensus"~"Inpatient Total",
                     census_metrics=="Delta_OtherBeddedCensusNICU"~"Other Bedded NICU",
                     census_metrics=="Delta_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                     census_metrics=="Delta_OtherBeddedTotalCensus"~"Other Bedded Total",
                     census_metrics=="Delta_TotalCensus"~"Midnight Census - Total",
                     census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  arrange(factor(census_metrics2,fv2))%>%
  rename(fv_census_delta="fv_census")%>%
  select(fv_census_delta)

fv_census_arrow<-rbind(fvcensus_df, c("Midnight Census - Inpatient"," ", " "),c("Midnight Census - Other Bedded"," ", " "))%>%
  select(census_metrics,fv_census)%>%
  filter(census_metrics=="CountAdmissionsArrow"|
         census_metrics=="CountDischargesArrow"|
         census_metrics=="InpatientCensusNICUArrow"|
         census_metrics=="InpatientCensusAcuteArrow"|
         census_metrics=="InpatientTotalCensusArrow"|
         census_metrics=="OtherBeddedCensusNICUArrow"|
         census_metrics=="OtherBeddedCensusAcuteArrow"|
         census_metrics=="OtherBeddedTotalCensusArrow"|
         census_metrics=="TotalCensusArrow"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
           case_when(census_metrics=="CountAdmissionsArrow"~ "Admission - All Bedded",
                     census_metrics=="CountDischargesArrow"~"Discharges - All Bedded",
                     census_metrics=="InpatientCensusNICUArrow"~"Inpatient NICU",
                     census_metrics=="InpatientCensusAcuteArrow"~"Inpatient Acute Care",
                     census_metrics=="InpatientTotalCensusArrow"~"Inpatient Total",
                     census_metrics=="OtherBeddedCensusNICUArrow"~"Other Bedded NICU",
                     census_metrics=="OtherBeddedCensusAcuteArrow"~"Other Bedded Acute Care",
                     census_metrics=="OtherBeddedTotalCensusArrow"~"Other Bedded Total",
                     census_metrics=="TotalCensusArrow"~"Midnight Census - Total",
                     census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  mutate(fv_census=(sub("_.*", "", fv_census)))%>%
  arrange(factor(census_metrics2,fv2))%>%
  rename(fv_census_arrow="fv_census")%>%
  select(fv_census_arrow)

fvcensus_metrics_table<-cbind(fv_census_actuals, fv_census_delta, fv_census_arrow)

rm(fv2, fv_census_actuals, fv_census_delta, fv_census_arrow)

#EXPERIENCE
fv_experience_df<-data.frame(load_dsFVExperience(date))%>% 
  mutate_at(c("PatientExperienceRatingScore","PatientExperienceRecommendScore"),round_percent)

fv_experience_df<-data.frame(t(fv_experience_df)) %>%select(t.fv_experience_df.)%>%
  rename(fv_campus="t.fv_experience_df.")%>%
  rownames_to_column(var="experience_metrics")

fv3<-c("Patient Experience Daily Comment")

experience_actuals<-fv_experience_df%>%select(experience_metrics,fv_campus)%>%
  filter(experience_metrics=="Comment")%>%
  mutate(experience_metrics2=
           case_when(experience_metrics=="Comment"~"Patient Experience Daily Comment"))%>%
  arrange(factor(experience_metrics,fv3))%>%
  rename(fv_campus_actuals="fv_campus")%>%
  select(experience_metrics2,fv_campus_actuals)

experience_metrics_table<-cbind(experience_actuals)

rm(fv3, experience_actuals)

