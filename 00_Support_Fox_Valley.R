#
# 00_Support.R
#

library(DBI)
library(dplyr)

edw_server <- 'cwwi-edwprod.cwwi.hosted'

#
# Helper function to create a data set with from the passed in SQL statement 
# and database name
getResultSet <- function(sql, dbhost) {
  
  con_string <- paste0("Driver=SQLServer;Server=", dbhost ,";Database=Epic;",
                       "UID=", 
                        Sys.getenv('EDW_USERNAME'), ";PWD=", Sys.getenv('EDW_PASSWORD'),
                       ";Integrated Security=true;",
                       "Trusted_Connection=NTLM")
  
  # create a connection to specified database
  con <- dbConnect(odbc::odbc(), encoding = "latin1",
                   .connection_string=con_string)
  
  results_df <- dbGetQuery(con, sql, immediate=TRUE)
  
  # disconnect from the database
  dbDisconnect(con)
  
  return(results_df)
}


# edw_server <- 'cwwi-edwprod.cwwi.hosted'
# 
# #
# # Helper function to create a data set with from the passed in SQL statement 
# # and database name
# getResultSet <- function(sql, dbhost) {
#   # create a connection to specified database
#   con <- dbConnect(odbc::odbc(), encoding = "latin1",
#                    .connection_string = paste0("Driver={ODBC Driver 17 for SQL Server};server=",
#                                                dbhost, ";trusted_connection=yes"))
#   results_df <- dbGetQuery(con, sql,immediate=TRUE)
# 
#   # disconnect from the database
#   dbDisconnect(con)
# 
#   return(results_df)
# }


#CENSUS
load_dsFVCensus <- function(date) {
  
  sql <- paste0(
    "
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp

/*fields beginning with CD will show the current day selected , fields beginning wtih PD show prior 5 same day of week. for instance prior 5 Mondays*/
  SELECT
SubjectAreaNM	
, DayOfWeekNM
, CD_CountAdmissions	
, PD_CountAdmissions	
, Delta_CountAdmissions
, CASE WHEN Delta_CountAdmissions =0 THEN 'right_grey'
WHEN Delta_CountAdmissions >0 THEN 'up_grey'
ELSE 'down_grey'
END AS CountAdmissionsArrow
, CD_CountDischarges	
, PD_CountDischarges	
, Delta_CountDischarges
, CASE WHEN Delta_CountDischarges =0 THEN 'right_grey'
WHEN Delta_CountDischarges >0 THEN 'up_grey'
ELSE 'down_grey'
END AS CountDischargesArrow
, CD_InpatientCensusNICU	
, PD_InpatientCensusNICU
, Delta_InpatientCensusNICU
, CASE WHEN Delta_InpatientCensusNICU =0 THEN 'right_grey'
WHEN Delta_InpatientCensusNICU >0 THEN 'up_grey'
ELSE 'down_grey'
END AS InpatientCensusNICUArrow
, CD_InpatientCensusPICU	
, PD_InpatientCensusPICU
, Delta_InpatientCensusPICU
, CASE WHEN Delta_InpatientCensusPICU =0 THEN 'right_grey'
WHEN Delta_InpatientCensusPICU >0 THEN 'up_grey'
ELSE 'down_grey'
END AS InpatientCensusPICUArrow
, CD_InpatientCensusAcute
, PD_InpatientCensusAcute
, Delta_InpatientCensusAcute
, CASE WHEN Delta_InpatientCensusAcute =0 THEN 'right_grey'
WHEN Delta_InpatientCensusAcute >0 THEN 'up_grey'
ELSE 'down_grey'
END AS InpatientCensusAcuteArrow
, CD_InpatientTotalCensus
, PD_InpatientTotalCensus
, Delta_InpatientTotalCensus
, CASE WHEN Delta_InpatientTotalCensus =0 THEN 'right_grey'
WHEN Delta_InpatientTotalCensus >0 THEN 'up_grey'
ELSE 'down_grey'
END AS InpatientTotalCensusArrow
, CD_OtherBeddedCensusNICU
, PD_OtherBeddedCensusNICU
, Delta_OtherBeddedCensusNICU
, CASE WHEN Delta_OtherBeddedCensusNICU =0 THEN 'right_grey'
WHEN Delta_OtherBeddedCensusNICU >0 THEN 'up_grey'
ELSE 'down_grey'
END AS OtherBeddedCensusNICUArrow
, CD_OtherBeddedCensusPICU
, PD_OtherBeddedCensusPICU
, Delta_OtherBeddedCensusPICU
, CASE WHEN Delta_OtherBeddedCensusPICU =0 THEN 'right_grey'
WHEN Delta_OtherBeddedCensusPICU >0 THEN 'up_grey'
ELSE 'down_grey'
END AS OtherBeddedCensusPICUArrow
, CD_OtherBeddedCensusAcute
, PD_OtherBeddedCensusAcute
, Delta_OtherBeddedCensusAcute
, CASE WHEN Delta_OtherBeddedCensusAcute =0 THEN 'right_grey'
WHEN Delta_OtherBeddedCensusAcute >0 THEN 'up_grey'
ELSE 'down_grey'
END AS OtherBeddedCensusAcuteArrow
, CD_OtherBeddedTotalCensus
, PD_OtherBeddedTotalCensus
, Delta_OtherBeddedTotalCensus
, CASE WHEN Delta_OtherBeddedTotalCensus =0 THEN 'right_grey'
WHEN Delta_OtherBeddedTotalCensus >0 THEN 'up_grey'
ELSE 'down_grey'
END AS OtherBeddedTotalCensusArrow
, TotalCensus
, Delta_TotalCensus
, CASE WHEN Delta_TotalCensus =0 THEN 'right_grey'
WHEN Delta_TotalCensus >0 THEN 'up_grey'
ELSE 'down_grey'
END AS TotalCensusArrow
INTO #temp
FROM (
  SELECT
  'Fox Valley Campus' AS SubjectAreaNM	
  , DayOfWeekNM
  , CD_CountAdmissions	
  , PD_CountAdmissions	
  , CASE WHEN PD_CountAdmissions = 0 THEN 0
  ELSE (CD_CountAdmissions - PD_CountAdmissions) * 1.0 / PD_CountAdmissions * 1.0
  END AS Delta_CountAdmissions
  , CD_CountDischarges	
  , PD_CountDischarges	
  , CASE WHEN PD_CountDischarges = 0 THEN 0
  ELSE (CD_CountDischarges - PD_CountDischarges) * 1.0 / PD_CountDischarges * 1.0
  END AS Delta_CountDischarges
  , CD_InpatientCensusNICU	
  , PD_InpatientCensusNICU
  , CASE WHEN PD_InpatientCensusNICU = 0 THEN 0
  ELSE (CD_InpatientCensusNICU - PD_InpatientCensusNICU) * 1.0 / PD_InpatientCensusNICU * 1.0
  END AS Delta_InpatientCensusNICU
  , CD_InpatientCensusPICU	
  , PD_InpatientCensusPICU
  , CASE WHEN PD_InpatientCensusPICU = 0 THEN 0
  ELSE (CD_InpatientCensusPICU - PD_InpatientCensusPICU) * 1.0 / PD_InpatientCensusPICU * 1.0
  END AS Delta_InpatientCensusPICU
  , CD_InpatientCensusAcute
  , PD_InpatientCensusAcute
  , CASE WHEN PD_InpatientCensusAcute = 0 THEN 0
  ELSE (CD_InpatientCensusAcute - PD_InpatientCensusAcute) * 1.0 / PD_InpatientCensusAcute * 1.0
  END AS Delta_InpatientCensusAcute
  , CD_InpatientTotalCensus
  , PD_InpatientTotalCensus
  , CASE WHEN PD_InpatientTotalCensus = 0 THEN 0
  ELSE (CD_InpatientTotalCensus - PD_InpatientTotalCensus) * 1.0 / PD_InpatientTotalCensus * 1.0
  END AS Delta_InpatientTotalCensus
  , CD_OtherBeddedCensusNICU
  , PD_OtherBeddedCensusNICU
  , CASE WHEN PD_OtherBeddedCensusNICU = 0 THEN 0
  ELSE (CD_OtherBeddedCensusNICU - PD_OtherBeddedCensusNICU) * 1.0 / PD_OtherBeddedCensusNICU * 1.0
  END AS Delta_OtherBeddedCensusNICU
  , CD_OtherBeddedCensusPICU
  , PD_OtherBeddedCensusPICU
  , CASE WHEN PD_OtherBeddedCensusPICU = 0 THEN 0
  ELSE (CD_OtherBeddedCensusPICU - PD_OtherBeddedCensusPICU) * 1.0 / PD_OtherBeddedCensusPICU * 1.0
  END AS Delta_OtherBeddedCensusPICU
  , CD_OtherBeddedCensusAcute
  , PD_OtherBeddedCensusAcute
  , CASE WHEN PD_OtherBeddedCensusAcute = 0 THEN 0
  ELSE (CD_OtherBeddedCensusAcute - PD_OtherBeddedCensusAcute) * 1.0 / PD_OtherBeddedCensusAcute * 1.0
  END AS Delta_OtherBeddedCensusAcute
  , CD_OtherBeddedTotalCensus
  , PD_OtherBeddedTotalCensus
  , CASE WHEN PD_OtherBeddedTotalCensus = 0 THEN 0
  ELSE (CD_OtherBeddedTotalCensus - PD_OtherBeddedTotalCensus) * 1.0 / PD_OtherBeddedTotalCensus * 1.0
  END AS Delta_OtherBeddedTotalCensus
  , CD_InpatientTotalCensus + CD_OtherBeddedTotalCensus AS TotalCensus
  , CASE WHEN PD_InpatientTotalCensus + PD_OtherBeddedTotalCensus = 0 THEN 0
  ELSE ((CD_InpatientTotalCensus + CD_OtherBeddedTotalCensus) - (PD_InpatientTotalCensus + PD_OtherBeddedTotalCensus)) * 1.0 / (PD_InpatientTotalCensus + PD_OtherBeddedTotalCensus) * 1.0
  END AS Delta_TotalCensus
  FROM (
    SELECT 
    metrics.SubjectAreaNM
    , curdow.DayOfWeekNM
    /*Admissions*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN CountAdmissions ELSE 0 END) AS CD_CountAdmissions
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountAdmissions ELSE 0 END) / 5 AS PD_CountAdmissions  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR
    
    /*Discharges*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN CountDischarges ELSE 0 END) AS CD_CountDischarges
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountDischarges ELSE 0 END) / 5 AS PD_CountDischarges 
    
    /*Inpatient NICU Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN InpatientCensusNICU ELSE 0 END) AS CD_InpatientCensusNICU
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN InpatientCensusNICU ELSE 0 END) / 5 AS PD_InpatientCensusNICU
    
    /*Inpatient PICU Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN InpatientCensusPICU ELSE 0 END) AS CD_InpatientCensusPICU
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN InpatientCensusPICU ELSE 0 END) / 5 AS PD_InpatientCensusPICU
    
    /*Inpatient Acute Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN InpatientCensusAcute ELSE 0 END) AS CD_InpatientCensusAcute
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN InpatientCensusAcute ELSE 0 END) / 5 AS PD_InpatientCensusAcute
    
    /*Inpatient Total Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN InpatientTotalCensus ELSE 0 END) AS CD_InpatientTotalCensus
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN InpatientTotalCensus ELSE 0 END) / 5 AS PD_InpatientTotalCensus
    
    /*Other Bedded NICU Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN OtherBeddedCensusNICU ELSE 0 END) AS CD_OtherBeddedCensusNICU
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN OtherBeddedCensusNICU ELSE 0 END) / 5 AS PD_OtherBeddedCensusNICU
    
    /*Other Bedded PICU Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN OtherBeddedCensusPICU ELSE 0 END) AS CD_OtherBeddedCensusPICU
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN OtherBeddedCensusPICU ELSE 0 END) / 5 AS PD_OtherBeddedCensusPICU
    
    /*Other Bedded Acute Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN OtherBeddedCensusAcute ELSE 0 END) AS CD_OtherBeddedCensusAcute
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN OtherBeddedCensusAcute ELSE 0 END) / 5 AS PD_OtherBeddedCensusAcute
    
    /*Other Bedded Total Census*/
      , SUM(CASE WHEN ContactDTS = '", date, "' THEN OtherBeddedTotalCensus ELSE 0 END) AS CD_OtherBeddedTotalCensus
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN OtherBeddedTotalCensus ELSE 0 END) / 5 AS PD_OtherBeddedTotalCensus
    FROM SAM.CommonMetrics.SummaryDailyBeddedMetrics AS metrics
    INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
    LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT  --JOIN TO DATE DIMENSION AGAIN TO DETERMINE DOW OF ANCHOR DATE
    WHERE SubjectAreaNM = 'Fox Valley'
    AND ((ContactDTS >= '", date, "' AND ContactDTS < DATEADD(dd,1,'", date, "'))
         OR ContactDTS = DATEADD(dd,-7,'", date, "')  --prior 5 days same day of week as anchor date.  ie. prior 5 mondays
         OR ContactDTS = DATEADD(dd,-14,'", date, "')
         OR ContactDTS = DATEADD(dd,-21,'", date, "')
         OR ContactDTS = DATEADD(dd,-28,'", date, "')
         OR ContactDTS = DATEADD(dd,-35,'", date, "'))
    GROUP BY 
    SubjectAreaNM
    , curdow.DayOfWeekNM
  ) AS summary
) AS a


SELECT 
SubjectAreaNM	
, DayOfWeekNM
, CD_CountAdmissions	
, PD_CountAdmissions	
, Delta_CountAdmissions
, CountAdmissionsArrow
, admissions.ImageDSC as CountAdmissionsArrowImage
, CD_CountDischarges	
, PD_CountDischarges	
, Delta_CountDischarges
, CountDischargesArrow
, discharges.ImageDSC as CountDischargesArrowImage
, CD_InpatientCensusNICU	
, PD_InpatientCensusNICU
, Delta_InpatientCensusNICU
, InpatientCensusNICUArrow
, ipnicu.ImageDSC as InpatientCensusNICUArrowImage
, CD_InpatientCensusPICU	
, PD_InpatientCensusPICU
, Delta_InpatientCensusPICU
, InpatientCensusPICUArrow
, ippicu.ImageDSC as InpatientCensusPICUArrowImage
, CD_InpatientCensusAcute
, PD_InpatientCensusAcute
, Delta_InpatientCensusAcute
, InpatientCensusAcuteArrow
, ipacute.ImageDSC as InpatientCensusAcuteArrowImage
, CD_InpatientTotalCensus
, PD_InpatientTotalCensus
, Delta_InpatientTotalCensus
, InpatientTotalCensusArrow
, iptotal.ImageDSC as InpatientTotalCensusArrowImage
, CD_OtherBeddedCensusNICU
, PD_OtherBeddedCensusNICU
, Delta_OtherBeddedCensusNICU
, OtherBeddedCensusNICUArrow
, obnicu.ImageDSC as OtherBeddedCensusNICUArrowImage
, CD_OtherBeddedCensusPICU
, PD_OtherBeddedCensusPICU
, Delta_OtherBeddedCensusPICU
, OtherBeddedCensusPICUArrow
, obpicu.ImageDSC as OtherBeddedCensusPICUArrowImage
, CD_OtherBeddedCensusAcute
, PD_OtherBeddedCensusAcute
, Delta_OtherBeddedCensusAcute
, OtherBeddedCensusAcuteArrow
, obacute.ImageDSC as OtherBeddedCensusAcuteArrowImage
, CD_OtherBeddedTotalCensus
, PD_OtherBeddedTotalCensus
, Delta_OtherBeddedTotalCensus
, OtherBeddedTotalCensusArrow
, obtotal.ImageDSC as OtherBeddedTotalCensusArrowImage
, TotalCensus
, Delta_TotalCensus
, TotalCensusArrow
, total.ImageDSC as TotalCensusArrowImage
FROM #temp AS t
INNER JOIN Epic.CHW.KPIImage AS admissions ON t.CountAdmissionsArrow = admissions.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS discharges ON t.CountDischargesArrow = discharges.ImageNM
INNER JOIN Epic.CHW.KPIImage AS ipnicu ON t.InpatientCensusNICUArrow = ipnicu.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS ippicu ON t.InpatientCensusPICUArrow = ippicu.ImageNM
INNER JOIN Epic.CHW.KPIImage AS ipacute ON t.InpatientCensusAcuteArrow = ipacute.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS iptotal ON t.InpatientTotalCensusArrow = iptotal.ImageNM
INNER JOIN Epic.CHW.KPIImage AS obnicu ON t.OtherBeddedCensusNICUArrow = obnicu.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS obpicu ON t.OtherBeddedCensusPICUArrow = obpicu.ImageNM
INNER JOIN Epic.CHW.KPIImage AS obacute ON t.OtherBeddedCensusAcuteArrow = obacute.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS obtotal ON t.OtherBeddedTotalCensusArrow = obtotal.ImageNM
INNER JOIN Epic.CHW.KPIImage AS total ON t.TotalCensusArrow = total.ImageNM
                      ")
  return(getResultSet(sql, edw_server))
}


#SPECIALTY
load_dsFVSpecialtyClinics <- function(date) {
  
  sql <- paste0(
    "
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp

/*fields beginning with CD will show the current day selected (except for count scheduled which shows the day after), fields beginning wtih PD show prior 5 same day of week. for instance prior 5 Mondays*/
  SELECT 
LocationCategory
, DayOfWeekNM
, NextDayOfWeekNM
, CD_CountArrivedVisits    
, PD_CountArrivedVisits    
, Delta_CountArrivedVisits
, CASE WHEN Delta_CountArrivedVisits =0 THEN 'right_grey'
WHEN Delta_CountArrivedVisits >0 THEN 'up_green'
ELSE 'down_red'
END AS CountArrivedVisitsArrow
, CD_NoShowSameDayCancelRate
, PD_NoShowSameDayCancelRate
, Delta_NoShowSameDayCancelRate
, CASE WHEN Delta_NoShowSameDayCancelRate =0 THEN 'right_grey'
WHEN Delta_NoShowSameDayCancelRate >0 THEN 'up_red'
ELSE 'down_green'
END AS NoShowSameDayCancelRateArrow
, CD_CountSameDaySchedule
, PD_CountSameDaySchedule
, Delta_CountSameDaySchedule
, CASE WHEN Delta_CountSameDaySchedule =0 THEN 'right_grey'
WHEN Delta_CountSameDaySchedule >0 THEN 'up_grey'
ELSE 'down_grey'
END AS CountSameDayScheduleArrow
, CD_SameDayScheduleRate
, PD_SameDayScheduleRate
, Delta_SameDayScheduleRate
, CASE WHEN Delta_SameDayScheduleRate =0 THEN 'right_grey'
WHEN Delta_SameDayScheduleRate >0 THEN 'up_grey'
ELSE 'down_grey'
END AS SameDayScheduleRateArrow
, CD_CountScheduled
, PD_CountScheduled
, Delta_CountScheduled
, CASE WHEN Delta_CountScheduled =0 THEN 'right_grey'
WHEN Delta_CountScheduled >0 THEN 'up_green'
ELSE 'down_red'
END AS CountScheduledArrow
, CD_AverageLag
, PD_AverageLag
, Delta_AverageLag
, CASE WHEN Delta_AverageLag =0 THEN 'right_grey'
WHEN Delta_AverageLag >0 THEN 'up_grey'
ELSE 'down_grey'
END AS AverageLagArrow
INTO #temp
FROM (
  SELECT LocationCategory
  ,DayofWeekNM
  ,NextDayOfWeekNM
  ,CD_CountArrivedVisits
  ,PD_CountArrivedVisits
  ,CASE WHEN PD_CountArrivedVisits = 0 THEN 0
  ELSE (CD_CountArrivedVisits - PD_CountArrivedVisits) * 1.0 / PD_CountArrivedVisits * 1.0
  END AS Delta_CountArrivedVisits
  ,CD_NoShowSameDayCancelRate
  ,PD_NoShowSameDayCancelRate
  ,CD_NoShowSameDayCancelRate - PD_NoShowSameDayCancelRate AS Delta_NoShowSameDayCancelRate
  , CD_CountSameDaySchedule
  , PD_CountSameDaySchedule
  , CASE WHEN PD_CountSameDaySchedule = 0 THEN 0
  ELSE (CD_CountSameDaySchedule - PD_CountSameDaySchedule) * 1.0 / PD_CountSameDaySchedule * 1.0
  END AS Delta_CountSameDaySchedule
  , CD_SameDayScheduleRate  
  , PD_SameDayScheduleRate      
  , CD_SameDayScheduleRate   - PD_SameDayScheduleRate   AS Delta_SameDayScheduleRate   
  ,CD_CountScheduled
  ,PD_CountScheduled
  ,CASE WHEN PD_CountScheduled = 0 THEN 0
  ELSE (CD_CountScheduled - PD_CountScheduled) * 1.0 / PD_CountScheduled * 1.0
  END AS Delta_CountScheduled
  ,CD_AverageLag
  ,PD_AverageLag
  ,CASE WHEN PD_AverageLag = 0 THEN 0
  ELSE (CD_AverageLag - PD_AverageLag) * 1.0 / PD_AverageLag * 1.0
  END AS Delta_AverageLag
  FROM (
    SELECT metrics.LocationCategory
    ,curdow.DayofWeekNM
    ,nextdow.DayofWeekNM AS NextDayOfWeekNM
    ,SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) AS CD_CountArrivedVisits
    ,SUM(CASE WHEN ContactDTS < '", date, "'
         AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits
         ELSE 0
         END) / 5 AS PD_CountArrivedVisits
    ,CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0 
    / SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
    END AS CD_NoShowSameDayCancelRate
    ,CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0 
    / SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
    END AS PD_NoShowSameDayCancelRate
    , SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) AS CD_CountSameDaySchedule
    , SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) / 5 AS PD_CountSameDaySchedule  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR
    , CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) * 1.0
    / SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) * 1.0
    END AS CD_SameDayScheduleRate
    , CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) * 1.0
    / SUM(CASE WHEN ContactDTS <'", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) * 1.0
    END AS PD_SameDayScheduleRate
    ,SUM(CASE WHEN ContactDTS = DATEADD(DD, 1, '", date, "') THEN CountScheduled ELSE 0 END) AS CD_CountScheduled
    ,SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR <> curdow.DayOfWeekIndexNBR THEN CountScheduled ELSE 0 END) / 5 AS PD_CountScheduled
    ,CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN LagNumerator ELSE 0 END) 
    / SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END)
    END AS CD_AverageLag
    ,CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN LagDenominator ELSE 0 END) = 0 THEN 0
    ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN LagNumerator ELSE 0 END) 
    / SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN LagDenominator ELSE 0 END)
    END AS PD_AverageLag
    FROM SAM.CommonMetrics.SummaryDailyClinicMetricsBASE AS metrics
    INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
    LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT
    LEFT JOIN Epic.Reference.DateDimensionBASE AS nextdow ON DATEADD(dd, 1, '", date, "') = nextdow.CalendarDT
    WHERE (
      metrics.LocationCategory IN (
        'Fox Valley Campus'
        ,'Metro Fox Valley'
      )
    )
    AND (metrics.SubjectAreaNM = 'Specialty Care')
    AND (
      metrics.ContactDTS >= '", date, "'
      AND metrics.ContactDTS < DATEADD(dd, 2, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 6, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 7, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 13, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 14, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 20, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 21, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 27, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 28, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 34, '", date, "')
      OR metrics.ContactDTS = DATEADD(dd, - 35, '", date, "')
    )
    GROUP BY metrics.LocationCategory
    ,curdow.DayofWeekNM
    ,nextdow.DayofWeekNM
  ) AS summary
) AS a


SELECT 
LocationCategory
, DayOfWeekNM
, NextDayOfWeekNM
, CD_CountArrivedVisits
, PD_CountArrivedVisits
, Delta_CountArrivedVisits
, CountArrivedVisitsArrow
, arrivedvisits.ImageDSC as CountArrivedVisitsArrowImage
, CD_NoShowSameDayCancelRate
, PD_NoShowSameDayCancelRate
, Delta_NoShowSameDayCancelRate
, NoShowSameDayCancelRateArrow
, noshowsdc.ImageDSC as NoShowSameDayCancelRateArrowImage
, CD_CountScheduled
, PD_CountScheduled
, Delta_CountScheduled
, CountScheduledArrow
, scheduled.ImageDSC as CountScheduledArrowImage
, CD_CountSameDaySchedule
, PD_CountSameDaySchedule
, Delta_CountSameDaySchedule
, CountSameDayScheduleArrow
, samedayschedule.ImageDSC as CountSameDayScheduleArrowImage
, CD_SameDayScheduleRate
, PD_SameDayScheduleRate
, Delta_SameDayScheduleRate
, SameDayScheduleRateArrow
, samedayschedulerate.ImageDSC as SameDayScheduleRateArrowImage
, CD_AverageLag
, PD_AverageLag
, Delta_AverageLag
, AverageLagArrow
, lag.ImageDSC as AverageLagArrowImage
FROM #temp AS t
INNER JOIN Epic.CHW.KPIImage AS arrivedvisits ON t.CountArrivedVisitsArrow = arrivedvisits.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS noshowsdc ON t.NoShowSameDayCancelRateArrow = noshowsdc.ImageNM
INNER JOIN Epic.CHW.KPIImage AS scheduled ON t.CountScheduledArrow = scheduled.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS lag ON t.AverageLagArrow = lag.ImageNM
INNER JOIN Epic.CHW.KPIImage AS samedayschedule	ON t.CountSameDayScheduleArrow = samedayschedule.ImageNM
INNER JOIN Epic.CHW.KPIImage AS samedayschedulerate	ON t.SameDayScheduleRateArrow = samedayschedulerate.ImageNM
")
  return(getResultSet(sql, edw_server))
}

#EXPERIENCE
load_dsFVExperience <- function(date) {
  
  sql <- paste0(
    "SET NOCOUNT ON
SELECT 
CASE WHEN SUM(RatingDenominator) = 0 THEN 0
ELSE SUM(RatingTopBox) * 1.0 / SUM(RatingDenominator) * 1.0
END AS PatientExperienceRatingScore  --for prior 30 days
, CASE WHEN SUM(RecommendDenominator) = 0 THEN 0
ELSE SUM(RecommendTopBox) * 1.0 / SUM(RecommendDenominator) * 1.0
END AS PatientExperienceRecommendScore  --for prior 30 days
, patexpcomment.Comment
FROM Epic.Reference.DateDimension AS dd	--added to account for lack of patient experience data 3/20/20 ksk
LEFT JOIN SAM.CommonMetrics.MetricPatientExperienceBASE AS patexp ON dd.CalendarDT = patexp.ContactDTS
AND SubjectAreaNM = 'Fox Valley'
OUTER APPLY (SELECT Comment
             FROM (
               SELECT com.Date
               , com.Comment
               , ROW_NUMBER() OVER (PARTITION BY DayOfMonth ORDER BY CASE WHEN com.Date = '", date, "' THEN '12/31/2099' ELSE com.Date END DESC) AS RANK
               FROM Idea.RealTime.NRCDailyComments_FoxValleyBASE AS com
               WHERE '", date, "' = com.Date 
               OR (DAY('", date, "') = com.DayOfMonth)
             ) AS comment
             WHERE RANK = 1) AS patexpcomment
WHERE dd.CalendarDT >= DATEADD(DD,-30,'", date, "') AND CalendarDT < DATEADD(DD,1,'", date, "')
--AND SubjectAreaNM = 'Fox Valley'
GROUP BY  patexpcomment.Comment
")
  return(getResultSet(sql, edw_server))
}

#DOWNTIME
load_dsDownTimeMessage <- function(date) {
  sql <- paste0(
    "SET NOCOUNT ON
SELECT
DowntimeMessage
FROM (
  SELECT
  DowntimeMessage
  , ROW_NUMBER() OVER (ORDER BY RANK) AS RANK
  FROM IDEA.DailySystemMetricsDowntimeMessage.DailySystemMetricsDowntimeMessageBASE
  WHERE CAST(GETDATE() AS DATE) >= StartDTS AND CAST(GETDATE() AS DATE) < DATEADD(dd,1,EndDTS)
) AS a
WHERE RANK = 1
")
  return(getResultSet(sql, edw_server))
}

