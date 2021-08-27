
library(zoo)
library(tidyverse)
library(kableExtra)
library(fontawesome)
library(sjmisc)

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

#CLINIC METRICS
clinics_df<-data.frame(load_dsClinics(date))%>% 
  select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
         -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
         -SameDayScheduleRateArrowImage,-AverageLagArrowImage)%>% 
  mutate_at(c("Delta_CountArrivedVisits","Delta_NoShowSameDayCancelRate",
        "Delta_CountScheduled","Delta_CountSameDaySchedule","Delta_SameDayScheduleRate",
        "Delta_AverageLag"),percent)%>%
  mutate_at(c("CD_NoShowSameDayCancelRate","CD_SameDayScheduleRate"),round_percent)%>%
  mutate_at(c("CD_CountArrivedVisits","CD_CountSameDaySchedule","CD_CountScheduled"),comma)%>%
  mutate_at(c("CD_CountScheduled", "Delta_CountScheduled"), asterisk)

clinics_df<-data.frame(t(clinics_df))%>%
  select(X1,X2)%>%
  rename(primary_care="X1",specialty_care="X2")%>%
  rownames_to_column(var="clinic_metrics")

m1=c("Arrived Visit Volume","Missed Appointments/Same Day Cancel Rate","Same Day Schedule Count",
     "Same Day Schedule Rate",paste0("Scheduled for ", weekdays(Sys.Date())," (", 
                                     format(Sys.Date(),"%m/%d/%y"),")"))

primary_care_actuals<-clinics_df%>%
  select(clinic_metrics,primary_care)%>%
  filter(clinic_metrics=="CD_CountArrivedVisits"|
         clinic_metrics=="CD_NoShowSameDayCancelRate"|
         clinic_metrics=="CD_CountSameDaySchedule"|
         clinic_metrics=="CD_SameDayScheduleRate"|
         clinic_metrics=="CD_CountScheduled"|
         clinic_metrics=="CD_AverageLag")%>%
  mutate(clinic_metrics2=
           case_when(clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                     clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                     clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                     clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                 weekdays(Sys.Date())," (",
                                                                 format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(primary_care_actuals="primary_care")%>%
  select(clinic_metrics2,primary_care_actuals)

primary_care_delta<-clinics_df%>%
  select(clinic_metrics,primary_care)%>%
  filter(clinic_metrics=="Delta_CountArrivedVisits"|
         clinic_metrics=="Delta_NoShowSameDayCancelRate"|
         clinic_metrics=="Delta_CountSameDaySchedule"|
         clinic_metrics=="Delta_SameDayScheduleRate"|
         clinic_metrics=="Delta_CountScheduled"|
         clinic_metrics=="Delta_AverageLag")%>%
   mutate(clinic_metrics2=
            case_when(clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                      clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                      clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                      clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                      clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                     weekdays(Sys.Date())," (",
                                                                     format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(primary_care_delta="primary_care")%>%
  select(primary_care_delta)

primary_care_arrow<-clinics_df%>%
  select(clinic_metrics,primary_care)%>%
  filter(clinic_metrics=="CountArrivedVisitsArrow"|clinic_metrics=="NoShowSameDayCancelRateArrow"|
           clinic_metrics=="CountSameDayScheduleArrow"|clinic_metrics=="SameDayScheduleRateArrow"|
           clinic_metrics=="CountScheduledArrow"|clinic_metrics=="AverageLagArrow")%>%
  mutate(clinic_metrics2=
           case_when(clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                   weekdays(Sys.Date())," (",
                                                                   format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(primary_care=(sub("_.*", "", primary_care)))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(primary_care_arrow="primary_care")%>%
  select(primary_care_arrow)

specialty_care_actuals<-clinics_df%>%
  select(clinic_metrics,specialty_care)%>%
  filter(clinic_metrics=="CD_CountArrivedVisits"|
         clinic_metrics=="CD_NoShowSameDayCancelRate"|
         clinic_metrics=="CD_CountSameDaySchedule"|
         clinic_metrics=="CD_SameDayScheduleRate"|
         clinic_metrics=="CD_CountScheduled"|
         clinic_metrics=="CD_AverageLag")%>%
  mutate(clinic_metrics2=
           case_when(clinic_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                     clinic_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     clinic_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                     clinic_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                     clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                weekdays(Sys.Date())," (",
                                                                format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(specialty_care_actuals="specialty_care")%>%
  select(specialty_care_actuals)

specialty_care_delta<-clinics_df%>%
  select(clinic_metrics,specialty_care)%>%
  filter(clinic_metrics=="Delta_CountArrivedVisits"|
         clinic_metrics=="Delta_NoShowSameDayCancelRate"|
         clinic_metrics=="Delta_CountSameDaySchedule"|
         clinic_metrics=="Delta_SameDayScheduleRate"|
         clinic_metrics=="Delta_CountScheduled"|
         clinic_metrics=="Delta_AverageLag")%>%
  mutate(clinic_metrics2=
           case_when(clinic_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                     clinic_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                     clinic_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                     clinic_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                     clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                   weekdays(Sys.Date())," (",
                                                                   format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(specialty_care_delta="specialty_care")%>%
  select(specialty_care_delta)

specialty_care_arrow<-clinics_df%>%
  select(clinic_metrics,specialty_care)%>%
  filter(clinic_metrics=="CountArrivedVisitsArrow"|
         clinic_metrics=="NoShowSameDayCancelRateArrow"|
         clinic_metrics=="CountSameDayScheduleArrow"|
         clinic_metrics=="SameDayScheduleRateArrow"|
         clinic_metrics=="CountScheduledArrow"|
         clinic_metrics=="AverageLagArrow")%>%
  mutate(clinic_metrics2=
           case_when(clinic_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     clinic_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     clinic_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     clinic_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                   weekdays(Sys.Date())," (",
                                                                   format(Sys.Date(),"%m/%d/%y"), ")"),
                     clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(specialty_care=(sub("_.*", "", specialty_care)))%>%
  arrange(factor(clinic_metrics2,m1))%>%
  rename(specialty_care_arrow="specialty_care")%>%
  select(specialty_care_arrow)


clinic_metrics_table<-cbind(primary_care_actuals, primary_care_delta, primary_care_arrow, 
                            specialty_care_actuals, specialty_care_delta, specialty_care_arrow)

rm(m1,primary_care_actuals, primary_care_delta, primary_care_arrow, 
   specialty_care_actuals, specialty_care_delta, specialty_care_arrow)

#SPECIALTY CLINIC BY LOCATION
specialtyclinics_df<-data.frame(load_dsSpecialtyClinics(date))%>% 
    select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
           -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
           -SameDayScheduleRateArrowImage,-AverageLagArrowImage)%>% 
    mutate_at(c("Delta_CountArrivedVisits","Delta_NoShowSameDayCancelRate",
                "Delta_CountScheduled","Delta_CountSameDaySchedule","Delta_SameDayScheduleRate",
                "Delta_AverageLag"),percent)%>%
    mutate_at(c("CD_NoShowSameDayCancelRate","CD_SameDayScheduleRate"),round_percent)%>%
    mutate_at(c("CD_CountArrivedVisits","CD_CountSameDaySchedule","CD_CountScheduled"),comma)%>%
    mutate_at(c("CD_CountScheduled", "Delta_CountScheduled"), asterisk)

specialtyclinics_df<-data.frame(t(specialtyclinics_df))%>%
  select(X1,X2)%>%
  rename(mke_campus="X2",metro_mke="X1")%>%
  rownames_to_column(var="specialty_clinic_metrics")

m2=c("Arrived Visit Volume","Missed Appointments/Same Day Cancel Rate","Same Day Schedule Count",
    "Same Day Schedule Rate",paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                    format(Sys.Date(),"%m/%d/%y"),")"))

mke_campus_actuals<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,mke_campus)%>%
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
                     specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                          weekdays(Sys.Date())," (",
                                                                          format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(mke_campus_actuals="mke_campus")%>%
  select(specialty_clinic_metrics2,mke_campus_actuals)
 
mke_campus_delta<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,mke_campus)%>%
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
                     specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ", 
                                                                             weekdays(Sys.Date())," (",
                                                                             format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(mke_campus_delta="mke_campus")%>%
  select(mke_campus_delta)

mke_campus_arrow<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,mke_campus)%>%
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
                     specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                            weekdays(Sys.Date())," (",
                                                                            format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(mke_campus=(sub("_.*", "", mke_campus)))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(mke_campus_arrow="mke_campus")%>%
  select(mke_campus_arrow)

 
metro_mke_actuals<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_mke)%>%
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
                     specialty_clinic_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                          weekdays(Sys.Date())," (",
                                                                          format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(metro_mke_actuals="metro_mke")%>%
  select(metro_mke_actuals)
 
metro_mke_delta<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_mke)%>%
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
                     specialty_clinic_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                             weekdays(Sys.Date())," (",
                                                                             format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(metro_mke_delta="metro_mke")%>%
  select(metro_mke_delta)

metro_mke_arrow<-specialtyclinics_df%>%
  select(specialty_clinic_metrics,metro_mke)%>%
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
                     specialty_clinic_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                            weekdays(Sys.Date())," (",
                                                                            format(Sys.Date(),"%m/%d/%y"), ")"),
                     specialty_clinic_metrics=="AverageLagArrow"~"Lag Days"))%>%
  arrange(factor(specialty_clinic_metrics2,m2))%>%
  rename(metro_mke_arrow="metro_mke")%>%
  select(metro_mke_arrow)
 
specialty_clinic_metrics_table<-cbind(mke_campus_actuals, mke_campus_delta, mke_campus_arrow, 
                                      metro_mke_actuals, metro_mke_delta, metro_mke_arrow)

rm(m2, mke_campus_actuals, mke_campus_delta, mke_campus_arrow, 
   metro_mke_actuals, metro_mke_delta, metro_mke_arrow)

#COMMUNITY SERVICES
communityservices_df<-data.frame(load_dsCommunityServices(date))%>%
  select(-CountArrivedVisitsArrowImage,-NoShowSameDayCancelRateArrowImage,
         -CountScheduledArrowImage,-CountSameDayScheduleArrowImage,
         -SameDayScheduleRateArrowImage,-AverageLagArrowImage)%>% 
  mutate_at(c("Delta_CountArrivedVisits","Delta_NoShowSameDayCancelRate",
              "Delta_CountScheduled","Delta_CountSameDaySchedule","Delta_SameDayScheduleRate",
              "Delta_AverageLag"),percent)%>%
  mutate_at(c("CD_NoShowSameDayCancelRate","CD_SameDayScheduleRate"),round_percent)%>%
  mutate_at(c("CD_CountArrivedVisits","CD_CountSameDaySchedule","CD_CountScheduled"),comma)%>%
  mutate_at(c("CD_CountScheduled", "Delta_CountScheduled"), asterisk)

communityservices_df<-data.frame(t(communityservices_df))%>%
  select(X2,X1)%>%
  rename(advocacy="X2",counseling="X1")%>%
  rownames_to_column(var="community_services_metrics")

m3=c("Arrived Visit Volume","Missed Appointments/Same Day Cancel Rate","Same Day Schedule Count",
     "Same Day Schedule Rate",paste0("Scheduled for ", weekdays(Sys.Date())," (",
                                     format(Sys.Date(),"%m/%d/%y"),")"))
        
advocacy_actuals<-communityservices_df%>%
  select(community_services_metrics,advocacy)%>%
  filter(community_services_metrics=="CD_CountArrivedVisits"|
         community_services_metrics=="CD_NoShowSameDayCancelRate"|
         community_services_metrics=="CD_CountSameDaySchedule"|
         community_services_metrics=="CD_SameDayScheduleRate"|
         community_services_metrics=="CD_CountScheduled"|
         community_services_metrics=="CD_AverageLag")%>%
  mutate(community_services_metrics2=
                 case_when(community_services_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                           community_services_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                           community_services_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                           community_services_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                           community_services_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                                  weekdays(Sys.Date())," (",
                                                                                  format(Sys.Date(),"%m/%d/%y"), ")"),
                           community_services_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  rename(advocacy_actuals="advocacy")%>%
  select(community_services_metrics2,advocacy_actuals)

advocacy_delta<-communityservices_df%>%
  select(community_services_metrics,advocacy)%>%
  filter(community_services_metrics=="Delta_CountArrivedVisits"|
         community_services_metrics=="Delta_NoShowSameDayCancelRate"|
         community_services_metrics=="Delta_CountSameDaySchedule"|
         community_services_metrics=="Delta_SameDayScheduleRate"|
         community_services_metrics=="Delta_CountScheduled"|
         community_services_metrics=="Delta_AverageLag")%>%
  mutate(community_services_metrics2=
                 case_when(community_services_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                           community_services_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                           community_services_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                           community_services_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                           community_services_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                                  weekdays(Sys.Date())," (",
                                                                                  format(Sys.Date(),"%m/%d/%y"), ")"),
                           community_services_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  rename(advocacy_delta="advocacy")%>%
  select(advocacy_delta)

advocacy_arrow<-communityservices_df%>%
  select(community_services_metrics,advocacy)%>%
  filter(community_services_metrics=="CountArrivedVisitsArrow"|
         community_services_metrics=="NoShowSameDayCancelRateArrow"|
         community_services_metrics=="CountSameDayScheduleArrow"|
         community_services_metrics=="SameDayScheduleRateArrow"|
         community_services_metrics=="CountScheduledArrow"|
         community_services_metrics=="AverageLagArrow")%>%
  mutate(community_services_metrics2=
           case_when(community_services_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     community_services_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     community_services_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     community_services_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     community_services_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                               weekdays(Sys.Date())," (",
                                                                               format(Sys.Date(),"%m/%d/%y"), ")"),
                     community_services_metrics=="AverageLagArrow"~"Lag Days"))%>%
  mutate(advocacy=(sub("_.*", "", advocacy)))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  rename(advocacy_arrow="advocacy")%>%
  select(advocacy_arrow)

counseling_actuals<-communityservices_df%>%
  select(community_services_metrics,counseling)%>%
  filter(community_services_metrics=="CD_CountArrivedVisits"|
         community_services_metrics=="CD_NoShowSameDayCancelRate"|
         community_services_metrics=="CD_CountSameDaySchedule"|
         community_services_metrics=="CD_SameDayScheduleRate"|
         community_services_metrics=="CD_CountScheduled"|
         community_services_metrics=="CD_AverageLag")%>%
  mutate(community_services_metrics2=
                 case_when(community_services_metrics=="CD_CountArrivedVisits"~"Arrived Visit Volume",
                           community_services_metrics=="CD_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                           community_services_metrics=="CD_CountSameDaySchedule"~"Same Day Schedule Count",
                           community_services_metrics=="CD_SameDayScheduleRate"~"Same Day Schedule Rate",
                           community_services_metrics=="CD_CountScheduled"~paste0("Scheduled for ",
                                                                                  weekdays(Sys.Date())," (",
                                                                                  format(Sys.Date(),"%m/%d/%y"), ")"),
                           community_services_metrics=="CD_AverageLag"~"Lag Days"))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  rename(counseling_actuals="counseling")%>%
  select(counseling_actuals)

counseling_delta<-communityservices_df%>%
  select(community_services_metrics,counseling)%>%
  filter(community_services_metrics=="Delta_CountArrivedVisits"|
         community_services_metrics=="Delta_NoShowSameDayCancelRate"|
         community_services_metrics=="Delta_CountSameDaySchedule"|
         community_services_metrics=="Delta_SameDayScheduleRate"|
         community_services_metrics=="Delta_CountScheduled"|
         community_services_metrics=="Delta_AverageLag")%>%
  mutate(community_services_metrics2=
                 case_when(community_services_metrics=="Delta_CountArrivedVisits"~"Arrived Visit Volume",
                           community_services_metrics=="Delta_NoShowSameDayCancelRate"~"Missed Appointments/Same Day Cancel Rate",
                           community_services_metrics=="Delta_CountSameDaySchedule"~"Same Day Schedule Count",
                           community_services_metrics=="Delta_SameDayScheduleRate"~"Same Day Schedule Rate",
                           community_services_metrics=="Delta_CountScheduled"~paste0("Scheduled for ",
                                                                                     weekdays(Sys.Date())," (",
                                                                                     format(Sys.Date(),"%m/%d/%y"), ")"),
                           community_services_metrics=="Delta_AverageLag"~"Lag Days"))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  rename(counseling_delta="counseling")%>%
  select(counseling_delta)

counseling_arrow<-communityservices_df%>%
  select(community_services_metrics,counseling)%>%
  filter(community_services_metrics=="CountArrivedVisitsArrow"|
           community_services_metrics=="NoShowSameDayCancelRateArrow"|
           community_services_metrics=="CountSameDayScheduleArrow"|
           community_services_metrics=="SameDayScheduleRateArrow"|
           community_services_metrics=="CountScheduledArrow"|
           community_services_metrics=="AverageLagArrow")%>%
  mutate(community_services_metrics2=
           case_when(community_services_metrics=="CountArrivedVisitsArrow"~"Arrived Visit Volume",
                     community_services_metrics=="NoShowSameDayCancelRateArrow"~"Missed Appointments/Same Day Cancel Rate",
                     community_services_metrics=="CountSameDayScheduleArrow"~"Same Day Schedule Count",
                     community_services_metrics=="SameDayScheduleRateArrow"~"Same Day Schedule Rate",
                     community_services_metrics=="CountScheduledArrow"~paste0("Scheduled for ",
                                                                               weekdays(Sys.Date())," (",
                                                                               format(Sys.Date(),"%m/%d/%y"), ")"),
                     community_services_metrics=="AverageLagArrow"~"Lag Days"))%>%
  arrange(factor(community_services_metrics2,m3))%>%
  mutate(counseling=(sub("_.*", "", counseling)))%>%
  rename(counseling_arrow="counseling")%>%
  select(counseling_arrow)

community_services_metrics_table<-cbind(advocacy_actuals, advocacy_delta, advocacy_arrow,
                                        counseling_actuals, counseling_delta, counseling_arrow)
rm(m3, advocacy_actuals, advocacy_delta, advocacy_arrow,
   counseling_actuals, counseling_delta, counseling_arrow)

#URGENT EMERGENT METRICS
urgentemergent_df<-data.frame(load_dsUrgentEmergent(date))%>%
  select(-CountVisitsArrowImage,-HigherLevelOfCareRateArrowImage,
         -AverageThroughputArrowImage,-LeftWithoutSeenRateArrowImage)%>% 
  mutate_at(c("Delta_CountVisits","Delta_HigherLevelOfCareRate",
              "Delta_AverageThroughput","Delta_LeftWithoutSeenRate"),percent)%>%
  mutate_at(c("CD_HigherLevelOfCareRate","CD_LeftWithoutSeenRate"),round_percent)%>%
  mutate_at(c("CD_CountVisits"),comma)%>%
  mutate(CD_AverageThroughput=round(CD_AverageThroughput,0))

urgentemergent_df<-data.frame(t(urgentemergent_df))%>%
  select(X1,X2)%>%
  rename(urgent_care="X2",emergency="X1")%>%
  rownames_to_column(var="urgent_emergent_metrics")

m4<-c("CD_CountVisits"~"Visit Volume","CD_HigherLevelOfCareRate"~"Higher Level of Care Rate",
      "CD_AverageThroughput"~"Throughput (minutes)",
      "CD_LeftWithoutSeenRate"~"% Left Without Being Seen")

urgent_care_actuals<-urgentemergent_df%>%
  select(urgent_emergent_metrics,urgent_care)%>%
  filter(urgent_emergent_metrics=="CD_CountVisits"|
         urgent_emergent_metrics=="CD_HigherLevelOfCareRate"|
         urgent_emergent_metrics=="CD_AverageThroughput"|
         urgent_emergent_metrics=="CD_LeftWithoutSeenRate")%>%
  mutate(urgent_emergent_metrics2=
                 case_when(urgent_emergent_metrics=="CD_CountVisits"~"Visit Volume",
                           urgent_emergent_metrics=="CD_HigherLevelOfCareRate"~"Higher Level of Care Rate",
                           urgent_emergent_metrics=="CD_AverageThroughput"~"Throughput (minutes)",
                           urgent_emergent_metrics=="CD_LeftWithoutSeenRate"~"% Left Without Being Seen"))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(urgent_care_actuals="urgent_care")%>%
  select(urgent_emergent_metrics2,urgent_care_actuals)

urgent_care_delta<-urgentemergent_df%>%
  select(urgent_emergent_metrics,urgent_care)%>%
  filter(urgent_emergent_metrics=="Delta_CountVisits"|
         urgent_emergent_metrics=="Delta_HigherLevelOfCareRate"|
         urgent_emergent_metrics=="Delta_AverageThroughput"|
         urgent_emergent_metrics=="Delta_LeftWithoutSeenRate")%>%
  mutate(urgent_emergent_metrics2=
                 case_when(urgent_emergent_metrics=="Delta_CountVisits"~"Visit Volume",
                           urgent_emergent_metrics=="Delta_HigherLevelOfCareRate"~"Higher Level of Care Rate",
                           urgent_emergent_metrics=="Delta_AverageThroughput"~"Throughput (minutes)",
                           urgent_emergent_metrics=="Delta_LeftWithoutSeenRate"~"% Left Without Being Seen"))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(urgent_care_delta="urgent_care")%>%
  select(urgent_care_delta)

urgent_care_arrow<-urgentemergent_df%>%
  select(urgent_emergent_metrics,urgent_care)%>%
  filter(urgent_emergent_metrics=="CountVisitsArrow"|
         urgent_emergent_metrics=="HigherLevelOfCareRateArrow"|
         urgent_emergent_metrics=="AverageThroughputArrow"|
         urgent_emergent_metrics=="LeftWithoutSeenRateArrow")%>%
  mutate(urgent_emergent_metrics2=
           case_when(urgent_emergent_metrics=="CountVisitsArrow"~"Visit Volume",
                     urgent_emergent_metrics=="HigherLevelOfCareRateArrow"~"Higher Level of Care Rate",
                     urgent_emergent_metrics=="AverageThroughputArrow"~"Throughput (minutes)",
                     urgent_emergent_metrics=="LeftWithoutSeenRateArrow"~"% Left Without Being Seen"))%>%
  mutate(urgent_care=(sub("_.*", "", urgent_care)))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(urgent_care_arrow="urgent_care")%>%
  select(urgent_care_arrow)

emergency_actuals<-urgentemergent_df%>%
  select(urgent_emergent_metrics,emergency)%>%
  filter(urgent_emergent_metrics=="CD_CountVisits"|
         urgent_emergent_metrics=="CD_HigherLevelOfCareRate"|
         urgent_emergent_metrics=="CD_AverageThroughput"|
         urgent_emergent_metrics=="CD_LeftWithoutSeenRate")%>%
  mutate(urgent_emergent_metrics2=
                 case_when(urgent_emergent_metrics=="CD_CountVisits"~"Visit Volume",
                           urgent_emergent_metrics=="CD_HigherLevelOfCareRate"~"Higher Level of Care Rate",
                           urgent_emergent_metrics=="CD_AverageThroughput"~"Throughput (minutes)",
                           urgent_emergent_metrics=="CD_LeftWithoutSeenRate"~"% Left Without Being Seen"))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(emergency_actuals="emergency")%>%
  select(emergency_actuals)

emergency_delta<-urgentemergent_df%>%
  select(urgent_emergent_metrics,emergency)%>%
  filter(urgent_emergent_metrics=="Delta_CountVisits"|
         urgent_emergent_metrics=="Delta_HigherLevelOfCareRate"|
         urgent_emergent_metrics=="Delta_AverageThroughput"|
         urgent_emergent_metrics=="Delta_LeftWithoutSeenRate")%>%
  mutate(urgent_emergent_metrics2=
                 case_when(urgent_emergent_metrics=="Delta_CountVisits"~"Visit Volume",
                           urgent_emergent_metrics=="Delta_HigherLevelOfCareRate"~"Higher Level of Care Rate",
                           urgent_emergent_metrics=="Delta_AverageThroughput"~"Throughput (minutes)",
                           urgent_emergent_metrics=="Delta_LeftWithoutSeenRate"~"% Left Without Being Seen"))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(emergency_delta="emergency")%>%
  select(emergency_delta)

emergency_arrow<-urgentemergent_df%>%
  select(urgent_emergent_metrics,emergency)%>%
  filter(urgent_emergent_metrics=="CountVisitsArrow"|
         urgent_emergent_metrics=="HigherLevelOfCareRateArrow"|
         urgent_emergent_metrics=="AverageThroughputArrow"|
         urgent_emergent_metrics=="LeftWithoutSeenRateArrow")%>%
  mutate(urgent_emergent_metrics2=
           case_when(urgent_emergent_metrics=="CountVisitsArrow"~"Visit Volume",
                     urgent_emergent_metrics=="HigherLevelOfCareRateArrow"~"Higher Level of Care Rate",
                     urgent_emergent_metrics=="AverageThroughputArrow"~"Throughput (minutes)",
                     urgent_emergent_metrics=="LeftWithoutSeenRateArrow"~"% Left Without Being Seen"))%>%
  mutate(emergency=(sub("_.*", "", emergency)))%>%
  arrange(factor(urgent_emergent_metrics2,m4))%>%
  rename(emergency_arrow="emergency")%>%
  select(emergency_arrow)

urgent_emergent_metrics_table<-cbind(urgent_care_actuals, urgent_care_delta, urgent_care_arrow,
                                     emergency_actuals, emergency_delta, emergency_arrow)

rm(m4, urgent_care_actuals, urgent_care_delta, urgent_care_arrow,
   emergency_actuals, emergency_delta, emergency_arrow)

#CENSUS METRICS
census_df<-data.frame(load_dsCensus(date))%>%
  select(-CountAdmissionsArrowImage,-CountDischargesArrowImage,
         -InpatientCensusNICUArrowImage,-InpatientCensusPICUArrowImage,
         -InpatientCensusAcuteArrowImage,-InpatientTotalCensusArrowImage, -OtherBeddedCensusNICUArrowImage,
         -OtherBeddedCensusPICUArrowImage,-OtherBeddedCensusAcuteArrowImage,-OtherBeddedTotalCensusArrowImage,
         -TotalCensusArrowImage)%>% 
  mutate_at(c("Delta_CountAdmissions","Delta_CountDischarges","Delta_InpatientCensusNICU",
              "Delta_InpatientCensusPICU","Delta_InpatientCensusAcute","Delta_InpatientTotalCensus",
              "Delta_OtherBeddedCensusNICU","Delta_OtherBeddedCensusPICU","Delta_OtherBeddedCensusAcute",
              "Delta_OtherBeddedTotalCensus","Delta_TotalCensus"),percent)%>%
  mutate_at(c("CD_CountAdmissions","CD_CountDischarges","CD_InpatientCensusNICU",
              "CD_InpatientCensusPICU","CD_InpatientCensusAcute","CD_InpatientTotalCensus",
              "CD_OtherBeddedCensusNICU","CD_OtherBeddedCensusPICU","CD_OtherBeddedCensusAcute",
              "CD_OtherBeddedTotalCensus","TotalCensus"),comma)

census_df<-data.frame(t(census_df))%>%
  select(t.census_df.)%>%
  rename(mke_census="t.census_df.")%>%
  rownames_to_column(var="census_metrics")

m5<-c("Midnight Census - Inpatient","Inpatient NICU", "Inpatient PICU",
      "Inpatient Acute Care", "Inpatient Total", "Midnight Census - Other Bedded", "Other Bedded NICU",
      "Other Bedded PICU", "Other Bedded Acute Care", "Other Bedded Total",
      "Midnight Census - Total","Admission - All Bedded","Discharges - All Bedded")

mke_census_actuals<-rbind(census_df, c("Midnight Census - Inpatient"," ", " "),
                       c("Midnight Census - Other Bedded"," ", " "))%>%
  select(census_metrics,mke_census)%>%
  filter(census_metrics=="CD_CountAdmissions"|
         census_metrics=="CD_CountDischarges"|
         census_metrics=="CD_InpatientCensusNICU"|
         census_metrics=="CD_InpatientCensusPICU"|
         census_metrics=="CD_InpatientCensusAcute"|
         census_metrics=="CD_InpatientTotalCensus"|
         census_metrics=="CD_OtherBeddedCensusNICU"|
         census_metrics=="CD_OtherBeddedCensusPICU"|
         census_metrics=="CD_OtherBeddedCensusAcute"|
         census_metrics=="CD_OtherBeddedTotalCensus"|
         census_metrics=="TotalCensus"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
                 case_when(census_metrics=="CD_CountAdmissions"~ "Admission - All Bedded",
                           census_metrics=="CD_CountDischarges"~"Discharges - All Bedded",
                           census_metrics=="CD_InpatientCensusNICU"~"Inpatient NICU",
                           census_metrics=="CD_InpatientCensusPICU"~"Inpatient PICU",
                           census_metrics=="CD_InpatientCensusAcute"~"Inpatient Acute Care",
                           census_metrics=="CD_InpatientTotalCensus"~"Inpatient Total",
                           census_metrics=="CD_OtherBeddedCensusNICU"~"Other Bedded NICU",
                           census_metrics=="CD_OtherBeddedCensusPICU"~"Other Bedded PICU",
                           census_metrics=="CD_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                           census_metrics=="CD_OtherBeddedTotalCensus"~"Other Bedded Total",
                           census_metrics=="TotalCensus"~"Midnight Census - Total",
                           census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                           census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"
                           ))%>%
  arrange(factor(census_metrics2,m5))%>%
  rename(mke_census_actuals="mke_census")%>%
  select(census_metrics2,mke_census_actuals)

mke_census_delta<-rbind(census_df, c("Midnight Census - Inpatient"," ", " "),
                        c("Midnight Census - Other Bedded"," "," "))%>%
  select(census_metrics,mke_census)%>%
  filter(census_metrics=="Delta_CountAdmissions"|
         census_metrics=="Delta_CountDischarges"|
         census_metrics=="Delta_InpatientCensusNICU"|
         census_metrics=="Delta_InpatientCensusPICU"|
         census_metrics=="Delta_InpatientCensusAcute"|
         census_metrics=="Delta_InpatientTotalCensus"|
         census_metrics=="Delta_OtherBeddedCensusNICU"|
         census_metrics=="Delta_OtherBeddedCensusPICU"|
         census_metrics=="Delta_OtherBeddedCensusAcute"|
         census_metrics=="Delta_OtherBeddedTotalCensus"|
         census_metrics=="Delta_TotalCensus"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
                 case_when(census_metrics=="Delta_CountAdmissions"~ "Admission - All Bedded",
                           census_metrics=="Delta_CountDischarges"~"Discharges - All Bedded",
                           census_metrics=="Delta_InpatientCensusNICU"~"Inpatient NICU",
                           census_metrics=="Delta_InpatientCensusPICU"~"Inpatient PICU",
                           census_metrics=="Delta_InpatientCensusAcute"~"Inpatient Acute Care",
                           census_metrics=="Delta_InpatientTotalCensus"~"Inpatient Total",
                           census_metrics=="Delta_OtherBeddedCensusNICU"~"Other Bedded NICU",
                           census_metrics=="Delta_OtherBeddedCensusPICU"~"Other Bedded PICU",
                           census_metrics=="Delta_OtherBeddedCensusAcute"~"Other Bedded Acute Care",
                           census_metrics=="Delta_OtherBeddedTotalCensus"~"Other Bedded Total",
                           census_metrics=="Delta_TotalCensus"~"Midnight Census - Total",
                           census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                           census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  arrange(factor(census_metrics2,m5))%>%
  rename(mke_census_delta="mke_census")%>%
  select(mke_census_delta)

mke_census_arrow<-rbind(census_df, c("Midnight Census - Inpatient"," ", " "),
                        c("Midnight Census - Other Bedded"," ", " "))%>%
  select(census_metrics,mke_census)%>%
  filter(census_metrics=="CountAdmissionsArrow"|
         census_metrics=="CountDischargesArrow"|
         census_metrics=="InpatientCensusNICUArrow"|
         census_metrics=="InpatientCensusPICUArrow"|
         census_metrics=="InpatientCensusAcuteArrow"|
         census_metrics=="InpatientTotalCensusArrow"|
         census_metrics=="OtherBeddedCensusNICUArrow"|
         census_metrics=="OtherBeddedCensusPICUArrow"|
         census_metrics=="OtherBeddedCensusAcuteArrow"|
         census_metrics=="OtherBeddedTotalCensusArrow"|
         census_metrics=="TotalCensusArrow"|
         census_metrics=="Midnight Census - Inpatient"|
         census_metrics=="Midnight Census - Other Bedded")%>%
  mutate(census_metrics2=
           case_when(census_metrics=="CountAdmissionsArrow"~ "Admission - All Bedded",
                     census_metrics=="CountDischargesArrow"~"Discharges - All Bedded",
                     census_metrics=="InpatientCensusNICUArrow"~"Inpatient NICU",
                     census_metrics=="InpatientCensusPICUArrow"~"Inpatient PICU",
                     census_metrics=="InpatientCensusAcuteArrow"~"Inpatient Acute Care",
                     census_metrics=="InpatientTotalCensusArrow"~"Inpatient Total",
                     census_metrics=="OtherBeddedCensusNICUArrow"~"Other Bedded NICU",
                     census_metrics=="OtherBeddedCensusPICUArrow"~"Other Bedded PICU",
                     census_metrics=="OtherBeddedCensusAcuteArrow"~"Other Bedded Acute Care",
                     census_metrics=="OtherBeddedTotalCensusArrow"~"Other Bedded Total",
                     census_metrics=="TotalCensusArrow"~"Midnight Census - Total",
                     census_metrics=="Midnight Census - Inpatient"~"Midnight Census - Inpatient",
                     census_metrics=="Midnight Census - Other Bedded"~"Midnight Census - Other Bedded"))%>%
  mutate(mke_census=(sub("_.*", "", mke_census)))%>%
  arrange(factor(census_metrics2,m5))%>%
  rename(mke_census_arrow="mke_census")%>%
  select(mke_census_arrow)

census_metrics_table<-cbind(mke_census_actuals, mke_census_delta, mke_census_arrow)

rm(m5, mke_census_actuals, mke_census_delta, mke_census_arrow)

#CURRENT STAFFING
currentstaffing_df<-data.frame(load_dsCurrentStaffing(date))

currentstaffing_df<-data.frame(t(currentstaffing_df))%>%
  select(t.currentstaffing_df.)%>%
  rename(tarp="t.currentstaffing_df.")%>%
  rownames_to_column(var="tarp_summary")

shift_start<-currentstaffing_df%>%
  filter(tarp_summary=="ShiftStartTime")%>%
  mutate(tarp=strptime(tarp, "%Y-%m-%d %H:%M:%OS"))%>%
  mutate(tarp=format(tarp,"%H:%M %p"))

m6<-c(paste0("NICU (as of ", shift_start$tarp, ")"), paste0("PICU (as of ", shift_start$tarp, ")"),
      paste0("Acute (as of ", shift_start$tarp, ")"))

tarp_census<-currentstaffing_df%>%
  filter(tarp_summary=="NICUCensus"|
         tarp_summary=="PICUCensus"|
         tarp_summary=="AcuteCensus")%>%
  mutate(tarp_summary2=
           case_when(tarp_summary=="NICUCensus"~paste0("NICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="PICUCensus"~paste0("PICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="AcuteCensus"~paste0("Acute (as of ", shift_start$tarp, ")")                               
           ))%>%
  arrange(factor(tarp_summary2,m6))%>%
  rename(Census=tarp)%>%
  select(tarp_summary2,Census)

tarp_staffing<-currentstaffing_df%>%
  filter(tarp_summary=="NICUAgreedRNs"|
         tarp_summary=="PICUAgreedRNs"|
         tarp_summary=="AcuteAgreedRNs")%>%
  mutate(tarp_summary2=
           case_when(tarp_summary=="NICUAgreedRNs"~paste0("NICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="PICUAgreedRNs"~paste0("PICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="AcuteAgreedRNs"~paste0("PICU (as of ", shift_start$tarp, ")")))%>%
  arrange(factor(tarp_summary2,m6))%>%
  select(tarp)%>%
  rename(Staffing=tarp)

tarp_variance<-currentstaffing_df%>%
  filter(tarp_summary=="NICURNVariance"|
         tarp_summary=="PICURNVariance"|
         tarp_summary=="AcuteRNVariance")%>%
  mutate(tarp_summary2=
                 case_when(tarp_summary=="NICURNVariance"~paste0("NICU (as of ", shift_start$tarp, ")"),
                           tarp_summary=="PICURNVariance"~paste0("PICU (as of ", shift_start$tarp, ")"),
                           tarp_summary=="AcuteRNVariance"~paste0("PICU (as of ", shift_start$tarp, ")")))%>%
  arrange(factor(tarp_summary2,m6))%>%
  select(tarp)%>%
  rename(RN_Variance=tarp)

tarp_census_color<-currentstaffing_df%>%
  filter(tarp_summary=="NICUCensusStatus"|
         tarp_summary=="PICUCensusStatus"|
         tarp_summary=="AcuteCensusStatus")%>%
  mutate(tarp_summary2=
           case_when(tarp_summary=="NICUCensusStatus"~paste0("NICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="PICUCensusStatus"~paste0("PICU (as of ", shift_start$tarp, ")"),
                     tarp_summary=="AcuteCensusStatus"~paste0("Acute (as of ", shift_start$tarp, ")")                               
           ))%>%
  arrange(factor(tarp_summary2,m6))%>%
  rename(census_color=tarp)%>%
  select(census_color)

tarp_staffing_color<-currentstaffing_df%>%
  filter(tarp_summary=="NICUStaffingStatus"|
         tarp_summary=="PICUStaffingStatus"|
         tarp_summary=="AcuteStaffingStatus")%>%
mutate(tarp_summary2=
         case_when(tarp_summary=="NICUStaffingStatus"~paste0("NICU (as of ", shift_start$tarp, ")"),
                   tarp_summary=="PICUStaffingStatus"~paste0("PICU (as of ", shift_start$tarp, ")"),
                   tarp_summary=="AcuteStaffingStatus"~paste0("Acute (as of ", shift_start$tarp, ")")                               
         ))%>%
  arrange(factor(tarp_summary2,m6))%>%
  rename(staffing_color=tarp)%>%
  select(staffing_color)

tarp_summary_table<-cbind(tarp_census, tarp_census_color,tarp_staffing, tarp_staffing_color,tarp_variance)

rm(m6, tarp_census, tarp_census_color,tarp_staffing, tarp_staffing_color,tarp_variance)

#SURGICAL METRICS
surgery_df<-data.frame(load_dsSurgery(date))%>% 
  select(-CompletedCountArrowImage,-ScheduledCountArrowImage,
         -SameDayCancelCountArrowImage,-SameDayCancelRateArrowImage)%>% 
  mutate_at(c("Delta_CompletedCount","Delta_ScheduledCount",
              "Delta_SameDayCancelCount","Delta_SameDayCancelRate"),percent)%>%
  mutate_at(c("CD_SameDayCancelRate"),percent)%>%
  mutate_at(c("CD_ScheduledCount", "Delta_ScheduledCount"), asterisk)

surgery_df<-data.frame(t(surgery_df))%>%
  select(X2,X1)%>%
  rename(mke_campus="X1",surgicenter="X2")%>%
  rownames_to_column(var="surgery_metrics")

m7<-c("Volume", "Same Day Cancel Cases", paste0("Scheduled for ", 
                                                weekdays(Sys.Date()), " (", 
                                                format(as.Date(Sys.Date()),"%m/%d/%y"), ")"))

mke_campus_actuals<-surgery_df%>%
  select(surgery_metrics,mke_campus)%>%
  filter(surgery_metrics=="CD_CompletedCount"|
         surgery_metrics=="CD_SameDayCancelCount"|
         surgery_metrics=="CD_ScheduledCount")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="CD_CompletedCount"~"Volume",
                     surgery_metrics=="CD_SameDayCancelCount"~"Same Day Cancel Cases",
                     surgery_metrics=="CD_ScheduledCount"~paste0("Scheduled for ", 
                                                                 weekdays(Sys.Date()), " (", 
                                                                 format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(mke_campus_actuals="mke_campus")%>%
  select(surgery_metrics2,mke_campus_actuals)

mke_campus_delta<-surgery_df%>%
  select(surgery_metrics,mke_campus)%>%
  filter(surgery_metrics=="Delta_CompletedCount"|
         surgery_metrics=="Delta_SameDayCancelCount"|
         surgery_metrics=="Delta_ScheduledCount")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="Delta_CompletedCount"~"Volume",
                     surgery_metrics=="Delta_SameDayCancelCount"~"Same Day Cancel Cases",
                     surgery_metrics=="Delta_ScheduledCount"~paste0("Scheduled for ", 
                                                                    weekdays(Sys.Date()), " (", 
                                                                    format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(mke_campus_delta="mke_campus")%>%
  select(mke_campus_delta)

mke_campus_arrow<-surgery_df%>%
  select(surgery_metrics,mke_campus)%>%
  filter(surgery_metrics=="CompletedCountArrow"|
         surgery_metrics=="SameDayCancelCountArrow"|
         surgery_metrics=="ScheduledCountArrow")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="CompletedCountArrow"~"Volume",
                     surgery_metrics=="SameDayCancelCountArrow"~"Same Day Cancel Cases",
                     surgery_metrics=="ScheduledCountArrow"~paste0("Scheduled for ", 
                                                                   weekdays(Sys.Date()), " (",
                                                                   format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  mutate(mke_campus=(sub("_.*", "", mke_campus)))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(mke_campus_arrow="mke_campus")%>%
  select(mke_campus_arrow)

surgicenter_actuals<-surgery_df%>%
  select(surgery_metrics,surgicenter)%>%
  filter(surgery_metrics=="CD_CompletedCount"|
         surgery_metrics=="CD_SameDayCancelCount"|
         surgery_metrics=="CD_ScheduledCount")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="CD_CompletedCount"~"Volume",
                     surgery_metrics=="CD_SameDayCancelCount"~"Same Day Cancel Cases",
                     surgery_metrics=="CD_ScheduledCount"~paste0("Scheduled for ", 
                                                                 weekdays(Sys.Date()), " (",
                                                                 format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(surgicenter_actuals="surgicenter")%>%
  select(surgicenter_actuals)

surgicenter_delta<-surgery_df%>%
  select(surgery_metrics,surgicenter)%>%
  filter(surgery_metrics=="Delta_CompletedCount"|
         surgery_metrics=="Delta_SameDayCancelCount"|
         surgery_metrics=="Delta_ScheduledCount")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="Delta_CompletedCount"~"Volume",
                     surgery_metrics=="Delta_SameDayCancelCount"~"Same Day Cancel Cases",
                     surgery_metrics=="Delta_ScheduledCount"~paste0("Scheduled for ", 
                                                                    weekdays(Sys.Date()), " (", 
                                                                    format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(surgicenter_delta="surgicenter")%>%
  select(surgicenter_delta)

surgicenter_arrow<-surgery_df%>%
  select(surgery_metrics,surgicenter)%>%
  filter(surgery_metrics=="CompletedCountArrow"|
         surgery_metrics=="SameDayCancelCountArrow"|
         surgery_metrics=="ScheduledCountArrow")%>%
  mutate(surgery_metrics2=
           case_when(surgery_metrics=="CompletedCountArrow"~"Volume",
                     surgery_metrics=="SameDayCancelCountArrow"~"Same Day Cancel Cases",
                     surgery_metrics=="ScheduledCountArrow"~paste0("Scheduled for ", 
                                                                   weekdays(Sys.Date()), " (", 
                                                                   format(as.Date(Sys.Date()),"%m/%d/%y"), ")")))%>%
  mutate(surgicenter=(sub("_.*", "", surgicenter)))%>%
  arrange(factor(surgery_metrics2,m7))%>%
  rename(surgicenter_arrow="surgicenter")%>%
  select(surgicenter_arrow)

surgery_metrics_table<-cbind(mke_campus_actuals, mke_campus_delta, mke_campus_arrow,
                             surgicenter_actuals, surgicenter_delta, surgicenter_arrow)

rm(m7, mke_campus_actuals, mke_campus_delta, mke_campus_arrow,
   surgicenter_actuals, surgicenter_delta, surgicenter_arrow)

#SYSTEM METRICS
system_df<-data.frame(load_dsSystem(date))%>% 
  mutate_at(c("PatientExperienceRatingScore","PatientExperienceRecommendScore"),round_percent)%>%
  mutate_at(c("IsolationPercentage"), percent)

system_df<-data.frame(t(system_df))%>%
  select(t.system_df.)%>%
  rename(mke_campus="t.system_df.")%>%
  rownames_to_column(var="system_metrics")

m8<-c("Deaths", "YTD HAC Events", "% Patients in Isolation", "Security Risk Assessment Consult Epic Orders",
      "# of Sitters or Safety Companions", "Patient Experience Daily Comment")

mke_campus_actuals<-system_df%>%
  select(system_metrics,mke_campus)%>%
  filter(system_metrics=="CountDeaths"|
         system_metrics=="YTDHACEvents"|
         system_metrics=="IsolationPercentage"|
         system_metrics=="SecurityRiskAssessmentCount"|
         system_metrics=="SitterCount"|
         system_metrics=="Comment")%>%
  mutate(system_metrics2=
           case_when(system_metrics=="CountDeaths"~"Deaths",
                     system_metrics=="YTDHACEvents"~"YTD HAC Events",
                     system_metrics=="IsolationPercentage"~"% Patients in Isolation",
                     system_metrics=="SecurityRiskAssessmentCount"~"Security Risk Assessment Consult Epic Orders",
                     system_metrics=="SitterCount"~"# of Sitters or Safety Companions",
                     system_metrics=="Comment"~"Patient Experience Daily Comment"))%>%
  arrange(factor(system_metrics2,m8))%>%
  rename(mke_campus_actuals="mke_campus")%>%
  select(system_metrics2,mke_campus_actuals)

system_metrics_table<-cbind(mke_campus_actuals)

rm(m8, mke_campus_actuals)

#OTHER METRICS
passfailcheck_df<-load_dsPassFailCheck(date)
#staffingpassfailcheck_df<-load_dsStaffingPassFailCheck(date)
errormessage<-loadErrorMessage(date)

