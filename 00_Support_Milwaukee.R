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



##CLINICS
load_dsClinics <- function(date) {
  sql <- paste0("
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp
SELECT 
  SubjectAreaNM
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
	SELECT
			SubjectAreaNM      
			, DayOfWeekNM
			, NextDayOfWeekNM
			, CD_CountArrivedVisits    
			, PD_CountArrivedVisits    
			, CASE WHEN PD_CountArrivedVisits = 0 THEN 0
				ELSE (CD_CountArrivedVisits - PD_CountArrivedVisits) * 1.0 / PD_CountArrivedVisits * 1.0
				END AS Delta_CountArrivedVisits
			, CD_NoShowSameDayCancelRate      
			, PD_NoShowSameDayCancelRate      
			, CD_NoShowSameDayCancelRate - PD_NoShowSameDayCancelRate AS Delta_NoShowSameDayCancelRate   
			, CD_CountSameDaySchedule
			, PD_CountSameDaySchedule
			, CASE WHEN PD_CountSameDaySchedule = 0 THEN 0
				ELSE (CD_CountSameDaySchedule - PD_CountSameDaySchedule) * 1.0 / PD_CountSameDaySchedule * 1.0
				END AS Delta_CountSameDaySchedule
			, CD_SameDayScheduleRate  
			, PD_SameDayScheduleRate      
			, CD_SameDayScheduleRate   - PD_SameDayScheduleRate   AS Delta_SameDayScheduleRate     
			, CD_CountScheduled  
			, PD_CountScheduled  
			, CASE WHEN PD_CountScheduled = 0 THEN 0
				ELSE (CD_CountScheduled - PD_CountScheduled) * 1.0 / PD_CountScheduled * 1.0
				END AS Delta_CountScheduled
			, CD_AverageLag      
			, PD_AverageLag      
			, CASE WHEN PD_AverageLag = 0 THEN 0
				ELSE (CD_AverageLag - PD_AverageLag) * 1.0 / PD_AverageLag * 1.0
				END AS Delta_AverageLag
	FROM (
			SELECT 
				  SubjectAreaNM
				, curdow.DayOfWeekNM
				, nextdow.DayOfWeekNM AS NextDayOfWeekNM
			/*count arrived/completed*/
				, SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) AS CD_CountArrivedVisits
				, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) / 5 AS PD_CountArrivedVisits  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

			/*no show/same day cancel rate*/
				, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0
								/ SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
						END AS CD_NoShowSameDayCancelRate
				, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0
								/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
						END AS PD_NoShowSameDayCancelRate

			/*count same day schedule*/
				, SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) AS CD_CountSameDaySchedule
				, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) / 5 AS PD_CountSameDaySchedule  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

			/*same day schedule rate*/
				, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) * 1.0
								/ SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) * 1.0
						END AS CD_SameDayScheduleRate
				, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) * 1.0
								/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) * 1.0
						END AS PD_SameDayScheduleRate

			/*count scheduled- note this shows the day after the day selected and the prior 5 days (same day of week)*/
				, SUM(CASE WHEN ContactDTS = DATEADD(DD,1,'", date, "') THEN CountScheduled ELSE 0 END) AS CD_CountScheduled
				, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR <> curdow.DayOfWeekIndexNBR THEN CountScheduled ELSE 0 END) / 5 AS PD_CountScheduled  --COMPARE AGAINST PRIOR 5 SAME DAY (AS TOMORROW)

			/*average lag*/
				, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN LagNumerator ELSE 0 END) 
								/ SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END)
						END AS CD_AverageLag
				, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagDenominator ELSE 0 END) = 0 THEN 0
						ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagNumerator ELSE 0 END)
								/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagDenominator ELSE 0 END)
						END AS PD_AverageLag
			FROM SAM.CommonMetrics.SummaryDailyClinicMetricsBASE AS metrics
			--INNER JOIN SAM.CommonMetrics.RuleDayOfWeekBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
			INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
			LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT  --JOIN TO DATE DIMENSION AGAIN TO DETERMINE DOW OF ANCHOR DATE
			LEFT JOIN Epic.Reference.DateDimensionBASE AS nextdow ON DATEADD(dd,1,'", date, "') = nextdow.CalendarDT
			WHERE 1=1
			AND metrics.LocationCategory IN ('Milwaukee Campus','Metro Milwaukee')
			AND metrics.SubjectAreaNM IN ('Specialty Care','Primary Care')
			AND    ((ContactDTS >= '", date, "' AND ContactDTS < DATEADD(dd,2,'", date, "'))
				OR ContactDTS = DATEADD(dd,-6,'", date, "')     --prior 5 days same day of week as day after anchor date
				OR ContactDTS = DATEADD(dd,-7,'", date, "')  --prior 5 days same day of week as anchor date.  ie. prior 5 mondays
				OR ContactDTS = DATEADD(dd,-13,'", date, "')
				OR ContactDTS = DATEADD(dd,-14,'", date, "')
				OR ContactDTS = DATEADD(dd,-20,'", date, "')
				OR ContactDTS = DATEADD(dd,-21,'", date, "')
				OR ContactDTS = DATEADD(dd,-27,'", date, "')
				OR ContactDTS = DATEADD(dd,-28,'", date, "')
				OR ContactDTS = DATEADD(dd,-34,'", date, "')
				OR ContactDTS = DATEADD(dd,-35,'", date, "'))
			GROUP BY 
					SubjectAreaNM
				, curdow.DayOfWeekNM
				, nextdow.DayOfWeekNM
			) AS summary
	)a


SELECT 
	  SubjectAreaNM
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


##SPECIALTY CLINICS
load_dsSpecialtyClinics <- function(date) {
  sql <- paste0("
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
			,SUM(CASE WHEN ContactDTS <  '", date, "'
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
			, SUM(CASE WHEN ContactDTS = '", date, "'THEN CountSameDaySchedule ELSE 0 END) AS CD_CountSameDaySchedule
			, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) / 5 AS PD_CountSameDaySchedule  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR
			, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
					ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) * 1.0
							/ SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) * 1.0
					END AS CD_SameDayScheduleRate
			, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
					ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) * 1.0
							/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) * 1.0
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
					'Milwaukee Campus'
					,'Metro Milwaukee'
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

#COMMUNITY SERVICES
load_dsCommunityServices<-function(date) {
  sql<-paste0("
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp
/*fields beginning with CD will show the current day selected (except for count scheduled which shows the day after), fields beginning wtih PD show prior 5 same day of week. for instance prior 5 Mondays*/
SELECT 
  SubjectAreaNM
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
	SELECT
		  SubjectAreaNM	
		, DayOfWeekNM
		, NextDayOfWeekNM
		, CD_CountArrivedVisits	
		, PD_CountArrivedVisits	
		, CASE WHEN PD_CountArrivedVisits = 0 THEN 0
			ELSE (CD_CountArrivedVisits - PD_CountArrivedVisits) * 1.0 / PD_CountArrivedVisits * 1.0
			END AS Delta_CountArrivedVisits
		, CD_NoShowSameDayCancelRate	
		, PD_NoShowSameDayCancelRate	
		, CD_NoShowSameDayCancelRate - PD_NoShowSameDayCancelRate AS Delta_NoShowSameDayCancelRate	
		, CD_CountSameDaySchedule
		, PD_CountSameDaySchedule
		, CASE WHEN PD_CountSameDaySchedule = 0 THEN 0
			ELSE (CD_CountSameDaySchedule - PD_CountSameDaySchedule) * 1.0 / PD_CountSameDaySchedule * 1.0
			END AS Delta_CountSameDaySchedule
		, CD_SameDayScheduleRate  
		, PD_SameDayScheduleRate      
		, CD_SameDayScheduleRate   - PD_SameDayScheduleRate   AS Delta_SameDayScheduleRate   
		, CD_CountScheduled	
		, PD_CountScheduled	
		, CASE WHEN PD_CountScheduled = 0 THEN 0
			ELSE (CD_CountScheduled - PD_CountScheduled) * 1.0 / PD_CountScheduled * 1.0
			END AS Delta_CountScheduled
		, CD_AverageLag	
		, PD_AverageLag	
		, CASE WHEN PD_AverageLag = 0 THEN 0
			ELSE (CD_AverageLag - PD_AverageLag) * 1.0 / PD_AverageLag * 1.0
			END AS Delta_AverageLag
	FROM (
		SELECT 
			  SubjectAreaNM
			, curdow.DayOfWeekNM
			, nextdow.DayOfWeekNM AS NextDayOfWeekNM
		/*count arrived/completed*/
			, SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) AS CD_CountArrivedVisits
			, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) / 5 AS PD_CountArrivedVisits  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

		/*no show/same day cancel rate*/
			, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0
					/ SUM(CASE WHEN ContactDTS = '", date, "' THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
				END AS CD_NoShowSameDayCancelRate
			, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancel ELSE 0 END) * 1.0
					/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountNoShowSameDayCancelDenominator ELSE 0 END) * 1.0
				END AS PD_NoShowSameDayCancelRate

		/*same day schedule count*/
			, SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) AS CD_CountSameDaySchedule
			, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) / 5 AS PD_CountSameDaySchedule  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

		/*same day schedule rate*/
			, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
					ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN CountSameDaySchedule ELSE 0 END) * 1.0
							/ SUM(CASE WHEN ContactDTS = '", date, "' THEN CountArrivedVisits ELSE 0 END) * 1.0
					END AS CD_SameDayScheduleRate
			, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) = 0 THEN 0
					ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountSameDaySchedule ELSE 0 END) * 1.0
							/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CountArrivedVisits ELSE 0 END) * 1.0
					END AS PD_SameDayScheduleRate

		/*count scheduled- note this shows the day after the day selected and the prior 5 days (same day of week)*/
			, SUM(CASE WHEN ContactDTS = DATEADD(DD,1,'", date, "') THEN CountScheduled ELSE 0 END) AS CD_CountScheduled
			, SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR <> curdow.DayOfWeekIndexNBR THEN CountScheduled ELSE 0 END) / 5 AS PD_CountScheduled  --COMPARE AGAINST PRIOR 5 SAME DAY (AS TOMORROW)

		/*average lag*/
			, CASE WHEN SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN ContactDTS = '", date, "' THEN LagNumerator ELSE 0 END) 
					/ SUM(CASE WHEN ContactDTS = '", date, "' THEN LagDenominator ELSE 0 END)
				END AS CD_AverageLag
			, CASE WHEN SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagDenominator ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagNumerator ELSE 0 END)
					/ SUM(CASE WHEN ContactDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR  THEN LagDenominator ELSE 0 END)
				END AS PD_AverageLag
		FROM SAM.CommonMetrics.SummaryDailyClinicMetricsBASE AS metrics
		--INNER JOIN SAM.CommonMetrics.RuleDayOfWeekBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
		INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
		LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT  --JOIN TO DATE DIMENSION AGAIN TO DETERMINE DOW OF ANCHOR DATE
		LEFT JOIN Epic.Reference.DateDimensionBASE AS nextdow ON DATEADD(dd,1,'", date, "') = nextdow.CalendarDT
		WHERE 1=1
		AND metrics.LocationCategory IN ('Milwaukee Campus','Metro Milwaukee')
		AND metrics.SubjectAreaNM IN ('Counseling','Advocacy & Protection')
		AND	((ContactDTS >= '", date, "' AND ContactDTS < DATEADD(dd,2,'", date, "'))
			OR ContactDTS = DATEADD(dd,-6,'", date, "')	--prior 5 days same day of week as day after anchor date
			OR ContactDTS = DATEADD(dd,-7,'", date, "')  --prior 5 days same day of week as anchor date.  ie. prior 5 mondays
			OR ContactDTS = DATEADD(dd,-13,'", date, "')
			OR ContactDTS = DATEADD(dd,-14,'", date, "')
			OR ContactDTS = DATEADD(dd,-20,'", date, "')
			OR ContactDTS = DATEADD(dd,-21,'", date, "')
			OR ContactDTS = DATEADD(dd,-27,'", date, "')
			OR ContactDTS = DATEADD(dd,-28,'", date, "')
			OR ContactDTS = DATEADD(dd,-34,'", date, "')
			OR ContactDTS = DATEADD(dd,-35,'", date, "'))
		GROUP BY 
			  SubjectAreaNM
			, curdow.DayOfWeekNM
			, nextdow.DayOfWeekNM
		) AS summary
	) AS a

SELECT 
	  SubjectAreaNM
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

#URGENT EMERGENT
load_dsUrgentEmergent <- function(date) {
  sql <- paste0("
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp
SELECT
	  SubjectAreaNM
	, DayofWeekNM
	, CD_CountVisits
	, PD_CountVisits
	, Delta_CountVisits
	, CASE WHEN Delta_CountVisits =0 THEN 'right_grey'
        WHEN Delta_CountVisits >0 THEN 'up_green'
        ELSE 'down_red'
        END AS CountVisitsArrow
	, CD_HigherLevelOfCareRate
	, PD_HigherLevelOfCareRate
	, Delta_HigherLevelOfCareRate
	, CASE WHEN Delta_HigherLevelOfCareRate =0 THEN 'right_grey'
        WHEN Delta_HigherLevelOfCareRate >0 THEN 'up_red'
        ELSE 'down_green'
        END AS HigherLevelOfCareRateArrow
	, CD_AverageThroughput
	, PD_AverageThroughput
	, Delta_AverageThroughput
	, CASE WHEN Delta_AverageThroughput =0 THEN 'right_grey'
        WHEN Delta_AverageThroughput >0 THEN 'up_red'
        ELSE 'down_green'
        END AS AverageThroughputArrow
	, CD_LeftWithoutSeenRate
	, PD_LeftWithoutSeenRate
	, Delta_LeftWithoutSeenRate
	, CASE WHEN Delta_LeftWithoutSeenRate =0 THEN 'right_grey'
        WHEN Delta_LeftWithoutSeenRate >0 THEN 'up_red'
        ELSE 'down_green'
        END AS LeftWithoutSeenRateArrow
INTO #temp
FROM (
	SELECT SubjectAreaNM
		,DayofWeekNM
		,CD_CountVisits
		,PD_CountVisits
		,CASE 
			WHEN PD_CountVisits = 0
				THEN 0
			ELSE (CD_CountVisits - PD_CountVisits) * 1.0 / PD_CountVisits * 1.0
			END AS Delta_CountVisits
		,CD_HigherLevelOfCareRate
		,PD_HigherLevelOfCareRate
		,CD_HigherLevelOfCareRate - PD_HigherLevelOfCareRate AS Delta_HigherLevelOfCareRate
		,CD_AverageThroughput
		,PD_AverageThroughput
		,CASE 
			WHEN PD_AverageThroughput = 0
				THEN 0
			ELSE (CD_AverageThroughput - PD_AverageThroughput) * 1.0 / PD_AverageThroughput * 1.0
			END AS Delta_AverageThroughput
		,CD_LeftWithoutSeenRate
		,PD_LeftWithoutSeenRate
		,CD_LeftWithoutSeenRate - PD_LeftWithoutSeenRate AS Delta_LeftWithoutSeenRate
	FROM (
		SELECT metrics.SubjectAreaNM
			,curdow.DayofWeekNM
			,SUM(CASE 
					WHEN ContactDTS = '", date, "'
						THEN CountVisits
					ELSE 0
					END) AS CD_CountVisits
			,SUM(CASE 
					WHEN ContactDTS < '", date, "'
						AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
						THEN CountVisits
					ELSE 0
					END) / 5 AS PD_CountVisits
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountVisits
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountHigherLevelOfCareEnc
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountVisits
							ELSE 0
							END) * 1.0
				END AS CD_HigherLevelOfCareRate
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountVisits
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountHigherLevelOfCareEnc
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountVisits
							ELSE 0
							END) * 1.0
				END AS PD_HigherLevelOfCareRate
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN ThroughputDenominator
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN ThroughputNumerator
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN ThroughputDenominator
							ELSE 0
							END) * 1.0
				END AS CD_AverageThroughput
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN ThroughputDenominator
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN ThroughputNumerator
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN ThroughputDenominator
							ELSE 0
							END) * 1.0
				END AS PD_AverageThroughput
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountVisits
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountLeftWithoutSeen
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS = '", date, "'
								THEN CountVisits
							ELSE 0
							END) * 1.0
				END AS CD_LeftWithoutSeenRate
			,CASE 
				WHEN SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountVisits
							ELSE 0
							END) = 0
					THEN 0
				ELSE SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountLeftWithoutSeen
							ELSE 0
							END) * 1.0 / SUM(CASE 
							WHEN ContactDTS < '", date, "'
								AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR
								THEN CountVisits
							ELSE 0
							END) * 1.0
				END AS PD_LeftWithoutSeenRate
		FROM SAM.CommonMetrics.SummaryDailyEmergentUrgentMetrics AS metrics
		INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.ContactDTS = dow.CalendarDT
		LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT
		WHERE (metrics.ContactDTS >= '", date, "')
			AND (metrics.ContactDTS < DATEADD(dd, 1, '", date, "'))
			OR (metrics.ContactDTS = DATEADD(dd, - 7, '", date, "'))
			OR (metrics.ContactDTS = DATEADD(dd, - 14, '", date, "'))
			OR (metrics.ContactDTS = DATEADD(dd, - 21, '", date, "'))
			OR (metrics.ContactDTS = DATEADD(dd, - 28, '", date, "'))
			OR (metrics.ContactDTS = DATEADD(dd, - 35, '", date, "'))
		GROUP BY metrics.SubjectAreaNM
			,curdow.DayofWeekNM
		) AS summary
	) AS a


SELECT 
	  SubjectAreaNM
	, DayofWeekNM
	, CD_CountVisits
	, PD_CountVisits
	, Delta_CountVisits
	, CountVisitsArrow
	, countvisits.ImageDSC as CountVisitsArrowImage
	, CD_HigherLevelOfCareRate
	, PD_HigherLevelOfCareRate
	, Delta_HigherLevelOfCareRate
	, HigherLevelOfCareRateArrow
	, ace.ImageDSC as HigherLevelOfCareRateArrowImage
	, CD_AverageThroughput
	, PD_AverageThroughput
	, Delta_AverageThroughput
	, AverageThroughputArrow
	, throughput.ImageDSC as AverageThroughputArrowImage
	, CD_LeftWithoutSeenRate
	, PD_LeftWithoutSeenRate
	, Delta_LeftWithoutSeenRate
	, LeftWithoutSeenRateArrow
	, lwbs.ImageDSC as LeftWithoutSeenRateArrowImage
FROM #temp AS t
INNER JOIN Epic.CHW.KPIImage AS countvisits ON t.CountVisitsArrow = countvisits.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS ace ON t.HigherLevelOfCareRateArrow = ace.ImageNM
INNER JOIN Epic.CHW.KPIImage AS throughput ON t.AverageThroughputArrow = throughput.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS lwbs ON t.LeftWithoutSeenRateArrow = lwbs.ImageNM
                          
                      ")
  return(getResultSet(sql, edw_server))
}


#CENSUS
load_dsCensus <- function(date) {
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
		  'Milwaukee Campus' AS SubjectAreaNM	
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
		WHERE SubjectAreaNM = 'Milwaukee'
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
  "
  )
  
  return(getResultSet(sql, edw_server)) # connect to the data warehouse and get results
}


#CURRENT STAFFING
load_dsCurrentStaffing <- function(date) {
  sql <- paste0("
SELECT
	  detail.SubjectAreaNM
	, detail.ShiftDTS
	, detail.StartTimeDTS AS ShiftStartTime
	, detail.ShiftNM
	, detail.NICUAgreedRNs
	, CASE WHEN detail.NICUAgreedRNs - detail.NICUDesiredRNs >= -3 THEN 'Green'
			WHEN detail.NICUAgreedRNs - detail.NICUDesiredRNs >= -4 THEN 'Yellow'
			ELSE 'Red'
		END AS NICUStaffingStatus
	, detail.NICUAgreedRNs - detail.NICUDesiredRNs AS NICURNVariance
	, detail.PICUAgreedRNs
	, tarp.PICUStaffingStatus
	, detail.PICUAgreedRNs - detail.PICUDesiredRNs AS PICURNVariance
	, detail.AcuteAgreedRNs
	, tarp.AcuteStaffingStatus
	, detail.AcuteAgreedRNs - detail.AcuteDesiredRNs AS AcuteRNVariance
	, detail.NICUCensus
	, CASE WHEN detail.NICUCensus <= 58 THEN 'Green'
			WHEN detail.NICUCensus <= 64 THEN 'Yellow'
			ELSE 'Red'
		END AS NICUCensusStatus
	, detail.PICUCensus
	, tarp.PICUCensusStatus
	, detail.AcuteCensus
	, tarp.AcuteCensusStatus
FROM (
	SELECT 
			'Milwaukee' AS SubjectAreaNM
		, COALESCE(staff.ShiftDTS, census.ShiftDTS) AS ShiftDTS
		, sh.ShiftNM
		, sh.ShiftID
		, sh.StartTimeDTS
		, sh.FinishTimeDTS
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) = 'NICU' THEN staff.AgreedUponNBR ELSE 0 END) AS NICUAgreedRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) = 'NICU' THEN staff.DesiredNBR ELSE 0 END) AS NICUDesiredRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) = 'NICU' THEN staff.ScheduleID ELSE 0 END) AS NICUScheduledRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('W3','W4','W5') THEN staff.AgreedUponNBR ELSE 0 END) AS PICUAgreedRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('W3','W4','W5') THEN staff.DesiredNBR ELSE 0 END) AS PICUDesiredRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('W3','W4','W5') THEN staff.ScheduleID ELSE 0 END) AS PICUScheduledRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('C4','E5','W7','W9','W10','W11','W12') THEN staff.AgreedUponNBR ELSE 0 END) AS AcuteAgreedRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('C4','E5','W7','W9','W10','W11','W12') THEN staff.DesiredNBR ELSE 0 END) AS AcuteDesiredRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('C4','E5','W7','W9','W10','W11','W12') THEN staff.ScheduleID ELSE 0 END) AS AcuteScheduledRNs
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) = 'NICU' THEN ProjectedCensusNBR ELSE 0 END) AS NICUCensus
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('W3','W4','W5') THEN ProjectedCensusNBR ELSE 0 END) AS PICUCensus
		, SUM(CASE WHEN COALESCE(unit.NameNM, cunit.NameNM) IN ('C4','E5','W7','W9','W10','W11','W12') THEN ProjectedCensusNBR ELSE 0 END) AS AcuteCensus
	FROM MaestroNew.StaffingPlan.RecommendationBASE AS staff
	LEFT JOIN MaestroNew.Maintenance.UnitBASE AS unit ON staff.UnitID = unit.UnitID
	FULL OUTER JOIN MaestroNew.StaffingPlan.CensusBASE AS census ON staff.ShiftID = census.ShiftID
		AND staff.UnitID = census.UnitID
		AND staff.ShiftDTS = census.ShiftDTS
	LEFT JOIN MaestroNew.Maintenance.UnitBASE AS cunit ON census.UnitID = cunit.UnitID
	LEFT JOIN MaestroNew.Maintenance.ShiftBASE AS sh ON COALESCE(staff.ShiftID, census.ShiftID) = sh.ShiftID
	WHERE 1=1
	AND staff.StaffTypeID = '2'  --RNs
	AND (('", date, "' >= DATEADD(dd,-1,CAST(GETDATE() AS DATE))  --if default date (yesterday) is selected, show most recent shift of curent date
			AND COALESCE(staff.ShiftDTS, census.ShiftDTS) = CAST(GETDATE() AS DATE)
			AND CAST(DATEADD(HH,1,GETDATE()) AS TIME) >= CAST(sh.StartTimeDTS AS TIME) AND CAST(DATEADD(HH,1,GETDATE()) AS TIME) <= CAST(sh.FinishTimeDTS AS TIME))		--updated getdate to add 1 hour to account for time change
		OR ('", date, "' < DATEADD(dd,-1,CAST(GETDATE() AS DATE))  --if prior date is selected, show day 1 shift of that date
			AND COALESCE(staff.ShiftDTS, census.ShiftDTS) = '", date, "'
			AND ShiftNM = 'D1 (0700-1100)')
		)
	GROUP BY 
			COALESCE(staff.ShiftDTS, census.ShiftDTS)
		, sh.ShiftNM
		, sh.ShiftID
		, sh.StartTimeDTS
		, sh.FinishTimeDTS
	) AS detail
LEFT JOIN SAM.Maestro.MaestroReportingSummaryMaestroAcuitySumBASE AS tarp ON CAST(detail.ShiftDTS AS DATE) = CAST(tarp.DateDT AS DATE)
	AND detail.ShiftID = tarp.ShiftID
                      ")
  return(getResultSet(sql, edw_server))
}

#SURGERY
load_dsSurgery<-function(date) {
  sql<-paste0("
  SET NOCOUNT ON
IF OBJECT_ID('tempdb..#temp') IS NOT NULL DROP TABLE #temp
/*fields beginning with CD will show the current day selected (except for count scheduled which shows the day after), fields beginning wtih PD show prior 5 same day of week. for instance prior 5 Mondays*/
SELECT
	  SubjectAreaNM	
	, DayOfWeekNM
	, NextDayOfWeekNM
	, CD_CompletedCount
	, PD_CompletedCount
	, Delta_CompletedCount
	, CASE WHEN Delta_CompletedCount =0 THEN 'right_grey'
        WHEN Delta_CompletedCount >0 THEN 'up_green'
        ELSE 'down_red'
        END AS CompletedCountArrow
	, CD_ScheduledCount
	, PD_ScheduledCount
	, Delta_ScheduledCount
	, CASE WHEN Delta_ScheduledCount =0 THEN 'right_grey'
        WHEN Delta_ScheduledCount >0 THEN 'up_green'
        ELSE 'down_red'
        END AS ScheduledCountArrow
	, CD_SameDayCancelCount
	, PD_SameDayCancelCount
	, Delta_SameDayCancelCount
	, CASE WHEN Delta_SameDayCancelCount =0 THEN 'right_grey'
        WHEN Delta_SameDayCancelCount >0 THEN 'up_red'
        ELSE 'down_green'
        END AS SameDayCancelCountArrow
	, CD_SameDayCancelRate	
	, PD_SameDayCancelRate	
	, Delta_SameDayCancelRate
	, CASE WHEN Delta_SameDayCancelRate =0 THEN 'right_grey'
        WHEN Delta_SameDayCancelRate >0 THEN 'up_red'
        ELSE 'down_green'
        END AS SameDayCancelRateArrow
INTO #temp
FROM (
	SELECT
		  LocationCategory AS SubjectAreaNM	
		, DayOfWeekNM
		, NextDayOfWeekNM
		, CD_CompletedCount
		, PD_CompletedCount
		, CASE WHEN PD_CompletedCount = 0 THEN 0
			ELSE (CD_CompletedCount- PD_CompletedCount) * 1.0 / PD_CompletedCount * 1.0
			END AS Delta_CompletedCount
		, CD_ScheduledCount
		, PD_ScheduledCount
		, CASE WHEN PD_ScheduledCount = 0 THEN 0
			ELSE (CD_ScheduledCount - PD_ScheduledCount) * 1.0 / PD_ScheduledCount * 1.0
			END AS Delta_ScheduledCount
		, CD_SameDayCancelCount
		, PD_SameDayCancelCount
		, CASE WHEN PD_SameDayCancelCount = 0 THEN 0
			ELSE (CD_SameDayCancelCount- PD_SameDayCancelCount) * 1.0 / PD_SameDayCancelCount * 1.0
			END AS Delta_SameDayCancelCount
		, CD_SameDayCancelRate	
		, PD_SameDayCancelRate	
		, CD_SameDayCancelRate - PD_SameDayCancelRate AS Delta_SameDayCancelRate
	FROM (
		SELECT 
			  LocationCategory
			, curdow.DayOfWeekNM
			, nextdow.DayOfWeekNM AS NextDayOfWeekNM
		/*count completed*/
			, SUM(CASE WHEN SurgeryDTS = '", date, "' THEN CompletedCount ELSE 0 END) AS CD_CompletedCount
			, SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN CompletedCount ELSE 0 END) / 5 AS PD_CompletedCount  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

		/*count scheduled- note this shows the day after the day selected and the prior 5 days (same day of week)*/
			, SUM(CASE WHEN SurgeryDTS = DATEADD(DD,1,'", date, "') THEN ScheduledCount ELSE 0 END) AS CD_ScheduledCount
			, SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR <> curdow.DayOfWeekIndexNBR THEN ScheduledCount ELSE 0 END) / 5 AS PD_ScheduledCount  --COMPARE AGAINST PRIOR 5 SAME DAY (AS TOMORROW)

		/*count same day cancel*/
			, SUM(CASE WHEN SurgeryDTS = '", date, "' THEN SameDayCancelCount ELSE 0 END) AS CD_SameDayCancelCount
			, SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN SameDayCancelCount ELSE 0 END) / 5 AS PD_SameDayCancelCount  --COMPARE AGAINST 5 SAME DAY OF WEEK PRIOR

		/*same day cancel rate*/
			, CASE WHEN SUM(CASE WHEN SurgeryDTS = '", date, "' THEN ScheduledCount ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN SurgeryDTS = '", date, "' THEN SameDayCancelCount ELSE 0 END) * 1.0
					/ SUM(CASE WHEN SurgeryDTS = '", date, "' THEN ScheduledCount ELSE 0 END) * 1.0
				END AS CD_SameDayCancelRate
			, CASE WHEN SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN ScheduledCount ELSE 0 END) = 0 THEN 0
				ELSE SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN SameDayCancelCount ELSE 0 END) * 1.0
					/ SUM(CASE WHEN SurgeryDTS < '", date, "' AND dow.DayOfWeekIndexNBR = curdow.DayOfWeekIndexNBR THEN ScheduledCount ELSE 0 END) * 1.0
				END AS PD_SameDayCancelRate

		FROM SAM.CommonMetrics.SummaryDailySurgicalMetricsBASE AS metrics
		INNER JOIN Epic.Reference.DateDimensionBASE AS dow ON metrics.SurgeryDTS = dow.CalendarDT
		LEFT JOIN Epic.Reference.DateDimensionBASE AS curdow ON '", date, "' = curdow.CalendarDT  --JOIN TO DATE DIMENSION AGAIN TO DETERMINE DOW OF ANCHOR DATE
		LEFT JOIN Epic.Reference.DateDimensionBASE AS nextdow ON DATEADD(dd,1,'", date, "') = nextdow.CalendarDT
		WHERE 1=1
		and ((SurgeryDTS >= '", date, "' AND SurgeryDTS < DATEADD(dd,2,'", date, "'))
			OR SurgeryDTS = DATEADD(dd,-6,'", date, "')	--prior 5 days same day of week as day after anchor date
			OR SurgeryDTS = DATEADD(dd,-7,'", date, "')  --prior 5 days same day of week as anchor date.  ie. prior 5 mondays
			OR SurgeryDTS = DATEADD(dd,-13,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-14,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-20,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-21,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-27,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-28,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-34,'", date, "')
			OR SurgeryDTS = DATEADD(dd,-35,'", date, "'))
		GROUP BY 
			  LocationCategory
			, curdow.DayOfWeekNM
			, nextdow.DayOfWeekNM
		) AS summary
	) AS a

SELECT 
	  SubjectAreaNM	
	, DayOfWeekNM
	, NextDayOfWeekNM
	, CD_CompletedCount
	, PD_CompletedCount
	, Delta_CompletedCount
	, CompletedCountArrow
	, completed.ImageDSC as CompletedCountArrowImage
	, CD_ScheduledCount
	, PD_ScheduledCount
	, Delta_ScheduledCount
	, ScheduledCountArrow
	, scheduled.ImageDSC as ScheduledCountArrowImage
	, CD_SameDayCancelCount
	, PD_SameDayCancelCount
	, Delta_SameDayCancelCount
	, SameDayCancelCountArrow
	, cancelcount.ImageDSC as SameDayCancelCountArrowImage
	, CD_SameDayCancelRate	
	, PD_SameDayCancelRate	
	, Delta_SameDayCancelRate
	, SameDayCancelRateArrow
	, cancelrate.ImageDSC as SameDayCancelRateArrowImage
FROM #temp AS t
INNER JOIN Epic.CHW.KPIImage AS completed ON t.CompletedCountArrow = completed.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS scheduled ON t.ScheduledCountArrow = scheduled.ImageNM
INNER JOIN Epic.CHW.KPIImage AS cancelcount ON t.SameDayCancelCountArrow = cancelcount.ImageNM 
INNER JOIN Epic.CHW.KPIImage AS cancelrate ON t.SameDayCancelRateArrow = cancelrate.ImageNM 
")
  return(getResultSet(sql, edw_server))
}


#SYSTEM
load_dsSystem<-function(date) {
  sql<-paste0("
	SELECT 
		  SubjectAreaNM
		--, MAX(DATEADD(DD,-30,'", date, "')) AS PatientExperienceStartDT
		, MAX(CASE WHEN ContactDTS = '", date, "' THEN CountDeaths ELSE 0 END) AS CountDeaths
		, SUM(CountHACEvents) AS YTDHACEvents
		, MAX(CASE WHEN ContactDTS = '", date, "' THEN IsolationPercentage ELSE 0 END) AS IsolationPercentage
		, MAX(CASE WHEN ContactDTS = '", date, "' THEN SecurityRiskAssessmentCount ELSE 0 END) AS SecurityRiskAssessmentCount
		, MAX(CASE WHEN ContactDTS = '", date, "' THEN SitterCount ELSE 0 END) AS SitterCount
		, CASE WHEN SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRatingDenominator ELSE 0 END) = 0 THEN 0
			ELSE SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRatingTopBox ELSE 0 END) * 1.0 / SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRatingDenominator ELSE 0 END) * 1.0
			END AS PatientExperienceRatingScore  --Provider/Facility Rating score for prior 30 days
		, CASE WHEN SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRecommendDenominator ELSE 0 END) = 0 THEN 0
			ELSE SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRecommendTopBox ELSE 0 END) * 1.0 / SUM(CASE WHEN ContactDTS > DATEADD(DD,-30,'", date, "') THEN PatientExperienceRecommendDenominator ELSE 0 END) * 1.0
			END AS PatientExperienceRecommendScore  --Would recommend Provider/Facility score for prior 30 days
		, patexpcomment.Comment
	FROM SAM.CommonMetrics.SummaryDailySystemMetricsBASE AS metrics
	OUTER APPLY (SELECT Comment
					FROM (
						SELECT com.Date
							, com.Comment
							, ROW_NUMBER() OVER (PARTITION BY DayOfMonth ORDER BY CASE WHEN com.Date = '", date, "' THEN '12/31/2099' ELSE com.Date END DESC) AS RANK
						FROM IDEA.RealTime.NRCDailyCommentsBASE AS com
						WHERE '", date, "' = com.Date 
							OR (DAY('", date, "') = com.DayOfMonth)
						) AS comment
					WHERE RANK = 1) AS patexpcomment
	WHERE ContactDTS >= DATEADD(YEAR, DATEDIFF(YEAR, 0, '", date, "'), 0) AND ContactDTS < DATEADD(DD,1,'", date, "')
	GROUP BY SubjectAreaNM
		   , patexpcomment.Comment
		  ")
return(getResultSet(sql, edw_server))
}

load_dsPassFailCheck <- function(date) {
  sql <- paste0("SELECT CASE WHEN MAX(EndDTS) < CAST(GETDATE() AS DATE) THEN 'Fail'
	ELSE 'Pass'
	END AS PassFailCheck
    FROM EDWAdmin.CatalystAdminApiV2.ETLBatchHistory AS batch
    WHERE batch.DataMartNM = 'CommonMetrics' 
	AND StatusID = '1'	--SUCCEEDED
                       ")
  return(getResultSet(sql, edw_server))
}


#ERROR MESSAGE
loadErrorMessage<-function(date) {
  sql <- paste0("SELECT
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

#PASS FAIL CHECK
load_dsStaffingPassFailCheck<-function(date) {
  sql<-paste0("--DECLARE @email VARCHAR(1) = '1'

SELECT CASE WHEN @email = '0' THEN 'Pass' --Report Center Version 
		WHEN MAX(staff.ShiftID) IS NULL THEN 'Fail'
		ELSE 'Pass'
		END AS StaffingPassFailCheck
FROM Epic.Reference.DateDimension AS dd 
LEFT JOIN MaestroNew.StaffingPlan.RecommendationBASE AS staff ON dd.CalendarDT = staff.ShiftDTS
	AND staff.StaffTypeID = '2'  --RNs
	AND staff.ShiftID =  1 --D1 (0700-1100)
LEFT JOIN MaestroNew.StaffingPlan.CensusBASE AS census ON staff.ShiftID = census.ShiftID
	AND staff.UnitID = census.UnitID
	AND staff.ShiftDTS = census.ShiftDTS
WHERE dd.CalendarDT = CAST(GETDATE() AS DATE)
             ")
  return(getResultSet(sql, edw_server))
}


