### (1) Install and Load Required Packages ============================================================

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("data.table")
# install.packages("zoo")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("leaflet")
# install.packages("shinyWidgets")
# install.packages("htmlwidgets")
# install.packages(c("readxl","writexl"))
# install.packages("anytime")

# install.packages("htmltools")
# require(htmltools)
# library(htmltools)
# update.packages("htmltools")

library(readxl)
library(writexl)
library(dplyr)
library(data.table)
library(zoo)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(lubridate)
library(tcltk)
library(tidyverse)
library(plotly)
library(knitr)
library(kableExtra)
library(leaflet)
library(shinyWidgets)
library(htmlwidgets)
library(grid)
library(gridExtra)
library(eeptools)
library(ggQC)
library(zipcode)

# # Packages from the process mapping codes [NEED TO BE CLEANED UP]
# install.packages('shinydashboard')
# install.packages('dplyr')
# install.packages('bupaR', dependencies = TRUE)
# install.packages('shiny')
# install.packages('DT')
# intall.packages('DiagrammerR')
# install.packages('shinyalert')
# install.packages('edeaR', dependencies = TRUE)
# install.packages('processmapR')
# install.packages('processmonitR')
# install.packages('processanimateR')
# install.packages('DiagrammeR')
# install.packages('shiny', type='binary')
# install.packages("shinydashboardPlus")
# install.packages("shiny")
# install.packages("leaflet")


library(shinydashboard)
library(dplyr)
library(bupaR)
library(shiny)
library(DT)
library(DiagrammeR)
library(shinyalert)
library(edeaR)
library(processmapR)
library(processmonitR)
library(processanimateR)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(DiagrammeR)
library(shiny)
library(shinydashboardPlus)
library(bupaR)
library(ggplot2)
library(shiny)
library(leaflet)


### (2) Import Data ===================================================================================
### [2.1] Set Working Directory

#wdpath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Coding"
wdpath <- "C:/Users/kweons01/Desktop/Ambulatory Dashboard/Coding"
setwd(wdpath)

### [2.2] Load Data files -----------------------------------------------------------------------------
# MSUS Scheduling Dataset from Hala (this particular file with 15,000+ rows took 3-4 min to be imported - maybe consider other options?)

master.data.raw  <- as.data.frame(read_excel(file.choose()))
data.raw <- master.data.raw
#head(data.raw)

### (3) Pre-process data ==============================================================================
### [3.1] Merge datasets ------------------------------------------------------------------------------

# Create unique identifiers
# Merge horizontally by matching unique identifiers

### [3.2] Subset master.data.raw

# Columns to be included in pre-processed dataset
original.cols <- c("campus","campus specialty...9","Department Name","PROV_NAME_WID",
                   "MRN","PAT_NAME","ZIP_CODE","SEX","BIRTH_DATE","FINCLASS",
                   "APPT_MADE_DTTM","SCHEDULED  APPOINTMENT DATE","PRC_NAME","APPT_LENGTH","Derived Status",
                   "APPT_CANC_DTTM", "CANCEL_REASON_NAME","FPA",
                   "SIGNIN_DTTM","CHECKIN_DTTM",
                   "ROOMED DTTM","ROOM #","VITALS_TAKEN_TM","Provider_Enter_DTTM","Provider_Leave_DTTM",
                   "VISIT_END_DTTM","CHECKOUT_DTTM")
 
# Subset raw data 
data.subset <- data.raw[original.cols]


# [3.3] Change column names -----------------------------------------------------------------------------

new.cols <- c("Campus","Campus.Specialty","Department","Provider",
              "MRN","Patient.Name","Zip.Code","Sex","Birth.Date","Coverage",
              "Appt.Made.DTTM","Appt.DTTM","Appt.Type","Appt.Dur","Appt.Status",
              "Appt.Cancel.DTTM", "Cancel.Reason","FPA",
              "Signin.DTTM","Checkin.DTTM",
              "Roomin.DTTM","ROOM #","Vitals.DTTM","Providerin_DTTM","Providerout_DTTM",
              "Visitend.DTTM","Checkout.DTTM")

colnames(data.subset) <- new.cols

#my.master.list should include all elements used for data pre-processing & analyses: my.master.list[["component name"]] <- component to be added
my.master.list <- list(raw.data.columns = original.cols, new.data.columns = new.cols) 

# [3.5] Formate Date and Time columns --------------------------------------------------------------------

# as.POSIXct(a, format="%Y-%m-%d %H:%M:%S"): timezone = "UTC"
# as.POSIXct(master.data.raw$APPT_ARRIVAL_DTTM * (60*60*24), origin="1899-12-30", tz="UTC")
# tz(master.data.raw$`CHECKIN TO CHECK OUT DATE`)

# Change numerical date and time value to date time format
data.subset$Signin.DTTM <- as.POSIXct(master.data.raw$SIGNIN_DTTM * (60*60*24), origin="1899-12-30", tz="UTC")

# [3.5] Remove duplicates in the data --------------------------------------------------------------------

data.duplicates <- data.subset %>% duplicated()
data.duplicates <- length(data.duplicates[data.duplicates == TRUE]) #count of duplicative records

# New subset data with duplicates removed
data.subset.new <- data.subset %>% distinct()

my.master.list[["processed data"]] <- data.subset.new # Add final pre-processed data to my.master.list

# User notification of duplicative records v(pop-up window)
if(data.duplicates == 0){
  tkmessageBox(title = "NOTE",
               message = "There are no duplicative records founds. Press OK to continue.", icon="info", type="ok")
} else {
  tkmessageBox(title= "NOTE",
               message = print(paste(data.duplicates," duplicative data records removed. Press OK to continue.")))
}



# [3.6] Create additional columns for analysis -------------------------------------------------------------
## Create date-year column
data.subset.new$Appt.DateYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y-%m-%d")

## Create month - year column
data.subset.new$Appt.MonthYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b-%Y")

## Create date column
data.subset.new$Appt.Date <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%m-%d")

## Create year column
data.subset.new$Appt.Year <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y")

## Create month colunm
data.subset.new$Appt.Month <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b")

## Create day of week colunm
data.subset.new$Appt.Day <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%a")

## Create quarter column 
data.subset.new$Appt.Quarter <- quarters(as.Date(data.subset.new$Appt.DTTM))


## Create rounded scheduled appointment times in hour and 30 minute intervals 
data.subset.new$Appt.TM.Hr <- as.ITime(round_date(data.subset.new$Appt.DTTM, "hour"))
data.subset.new$Appt.TM.Hr <- format(strptime(data.subset.new$Appt.TM.Hr ,"%H:%M:%S"),'%H:%M')
data.subset.new$Appt.TM.30m <- as.ITime(round_date(data.subset.new$Appt.DTTM, "30 minutes"))
data.subset.new$Appt.TM.30m  <- format(strptime(data.subset.new$Appt.TM.30m ,"%H:%M:%S"),'%H:%M')

## Create rounded check-in times in hour and 30 minute intervals 
data.subset.new$Checkin.Hr <- as.ITime(round_date(data.subset.new$Checkin.DTTM, "hour"))
data.subset.new$Checkin.Hr <- format(strptime(data.subset.new$Checkin.Hr,"%H:%M:%S"),'%H:%M')
data.subset.new$Checkin.30m <- as.ITime(round_date(data.subset.new$Checkin.DTTM, "30 minutes"))
data.subset.new$Checkin.30m <- format(strptime(data.subset.new$Checkin.30m,"%H:%M:%S"),'%H:%M')

# Create lead days to cancellation column
data.subset.new$Lead.Days <- as.numeric(round(difftime(data.subset.new$Appt.DTTM, data.subset.new$Appt.Cancel.DTTM,  units = "days"),2))

## Unique Identifier
data.subset.new$uniqueId <- paste(data.subset.new$Department,data.subset.new$MRN,data.subset.new$Appt.DTTM)

# (4) Subset data ==============================================================================================
# All data: Arrived, No Show, Canceled, Bumped, Rescheduled
all.data <- data.subset.new 

# Arrived + No Show data: Arrived and No Show
arrivedNoShow.data <- data.subset.new %>%
  filter(Appt.Status %in% c("Arrived","No Show"))
  
# Arrived data: Arrived
arrived.data <- data.subset.new %>%
  filter(Appt.Status %in% c("Arrived"))

## Create cycle time duration (min) - Visitend.DTTM - Checkin.DTTM
arrived.data$cycleTime <- as.numeric(round(difftime(arrived.data$Visitend.DTTM,arrived.data$Checkin.DTTM,units="mins"),1))

## Create Checkin to Rooming duration (min) - Checkin.DTTM - Roomin.DTTM
arrived.data$checkinToRoomin <- as.numeric(round(difftime(arrived.data$Roomin.DTTM,arrived.data$Checkin.DTTM,units="mins"),1))

## Create Provider in to Provider out duration (min) - Providerout.DTTM - Providerin.DTTM
arrived.data$providerinToOut <- as.numeric(round(difftime(arrived.data$Providerout.DTTM,arrived.data$Providerin.DTTM,units="mins"),1))

## Create Visit End to Check out duration (min) - Checkout.DTTM - Visitend.DTTM
arrived.data$visitEndToCheckout <- as.numeric(round(difftime(arrived.data$Checkout.DTTM,arrived.data$Visitend.DTTM,units="mins"),1))

# Canceled data: canceled appointments only
canceled.data <- data.subset.new %>%
  filter(Appt.Status %in% c("Canceled"))

# Bumped data: bumped appointments only
bumped.data <- data.subset.new %>%
  filter(Appt.Status %in% c("Bumped"))

# Save subsetted data to my.master.list
my.master.list[["all data"]] <- all.data
my.master.list[["arrived and noshow data"]] <- arrivedNoShow.data
my.master.list[["arrived data"]] <- arrived.data
my.master.list[["canceled.data"]] <- canceled.data
my.master.list[["bumped.data"]] <- bumped.data


#### ( ) Utilization Data Frame ==============================================================================

# Arrived and No Show Data Pre-processed 

scheduled.data <- arrivedNoShow.data

scheduled.data$Appt.Start <- as.POSIXct(scheduled.data$Appt.TM.Hr, format = "%H:%M")
scheduled.data$Appt.End <- as.POSIXct(scheduled.data$Appt.Start + scheduled.data$Appt.Dur*60, format = "%H:%M")

## Data frame for 30-min interval (including both NOS and ARR) -----------------------------------------------------------------------
# 
# print.POSIXct <- function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))
# 
# time.hour <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "hour"),"%H:%M", tz="GMT")
# time.hour <- time.hour[1:24]
# 
# time.hour.df <- data.frame(matrix(ncol=length(time.hour), nrow=nrow(scheduled.data)))
# 
# colnames(time.hour.df) <- time.hour
# time.hour.df <- cbind(scheduled.data,time.hour.df)
# 
# ### Utilization calculation - Method 1 - While Loop
# c.start <- which(colnames(time.hour.df)=="00:00")
# c.end <- which(colnames(time.hour.df)=="23:00") + 1
# 
# midnight <- data.frame(matrix(ncol=1, nrow=nrow(scheduled.data)))
# colnames(midnight) <- "00:00"
# 
# time.hour.df <- cbind(time.hour.df,midnight)
# 
# i <- 1
# n <- nrow(time.hour.df) + 1
# 
# sleep_for_a_minute <- function() { Sys.sleep(60) }
# start_time <- Sys.time()
# sleep_for_a_minute()
# 
# while(c.start!=c.end){
#  i <- 1
# while(i!=n){
# if(time.hour.df$Appt.Start[i] >= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M") &
#    time.hour.df$Appt.Start[i] < as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
# time.hour.df[i,c.start] <- pmin(time.hour.df$Appt.Dur[i],difftime(as.POSIXct(colnames(time.hour.df)[c.start+1],format = "%H:%M"),
#                                                                   as.POSIXct(colnames(time.hour.df)[c.start],format = "%H:%M"), unit="mins"))
# }else if(time.hour.df$Appt.End[i] >= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M") &
#         time.hour.df$Appt.End[i] < as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
# time.hour.df[i,c.start] <- difftime(time.hour.df$Appt.End[i],as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M"), unit="mins")
# }else if(time.hour.df$Appt.Start[i] >= as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
#  time.hour.df[i,c.start] <- 0
# }else if(time.hour.df$Appt.End[i] <= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M")){
#  time.hour.df[i,c.start] <- 0
# }else{
#  time.hour.df[i,c.start] <- 60
# }
# i <- i+1
# }
# c.start <- c.start+1
# }
# 
# time.hour.df <- time.hour.df[1:length(time.hour.df)-1]
# 
# data.hour.scheduled <- time.hour.df
# 
# data.hour.arrived <- time.hour.df %>%
#   filter(Appt.Status %in% c("Arrived"))
# 
# write_xlsx(data.hour.arrived, "data.hour.arrived.xlsx")
# 
# end_time <- Sys.time()
# 
# processing.time1 <- (end_time - start_time) # 12.7 - 20.7 min to process 34,687 rows + 23 columns (processing time varies each time)
processing.time1

data.hour.arrived <- "data.hour.arrived.csv"
data.hour.arrived <- read_csv(data.hour.arrived)

## Data frame for 30-min interval (including both NOS and ARR) -----------------------------------------------------------------------

# time.30min <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),"%H:%M", tz="GMT")
# time.30min <- time.30min[1:48]
# 
# time.30min.df <- data.frame(matrix(ncol=length(time.30min), nrow=nrow(scheduled.data)))
# 
# colnames(time.30min.df) <- time.30min
# time.30min.df <- cbind(scheduled.data,time.30min.df)
# 
# c.start <- which(colnames(time.30min.df)=="00:00") 
# c.end <- which(colnames(time.30min.df)=="23:30") + 1
# 
# midnight <- data.frame(matrix(ncol=1, nrow=nrow(scheduled.data)))
# colnames(midnight) <- "00:00"
# 
# time.30min.df <- cbind(time.30min.df,midnight)
# 
# i <- 1
# n <- nrow(time.30min.df) + 1
# 
# sleep_for_a_minute <- function() { Sys.sleep(60) }
# start_time <- Sys.time()
# sleep_for_a_minute()
# 
# while(c.start!=c.end){
# i <- 1
# while(i!=n){
# if(time.30min.df$Appt.Start[i] >= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M") &
#  time.30min.df$Appt.Start[i] < as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
# time.30min.df[i,c.start] <- pmin(time.30min.df$Appt.Dur[i],difftime(as.POSIXct(colnames(time.30min.df)[c.start+1],format = "%H:%M"), 
#                                                                       as.POSIXct(colnames(time.30min.df)[c.start],format = "%H:%M"), unit="mins"))
# }else if(time.30min.df$Appt.End[i] >= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M") &
#         time.30min.df$Appt.End[i] < as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
# time.30min.df[i,c.start] <- difftime(time.30min.df$Appt.End[i],as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M"), unit="mins")
# }else if(time.30min.df$Appt.Start[i] >= as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
#  time.30min.df[i,c.start] <- 0
# }else if(time.30min.df$Appt.End[i] <= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M")){
#  time.30min.df[i,c.start] <- 0
# }else{
#  time.30min.df[i,c.start] <- 30
# }
# i <- i+1
# }
# c.start <- c.start+1
# }
# 
# time.30min.df <- time.30min.df[1:length(time.30min.df)-1]
# data.30min.scheduled <- time.30min.df
# data.30min.arrived <- data.30min.scheduled %>%
#   filter(Appt.Status %in% c("Arrived"))
# write_xlsx(time.30min.df, "time.30min.df.xlsx")
# 
# end_time <- Sys.time()
# 
# processing.time2 <- (end_time - start_time) # 26.1 min to process 34,687 rows + 47 columns
processing.time2
time.30min.arrived <- "time.30min.df.csv"
time.30min.arrived <- read_csv(time.30min.arrived)


#### (4) Shiny App ===========================================================================================
# [4.1] Days of week filters ---------------------------------------------------------------------------------
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

# [4.2] Time range filters -----------------------------------------------------------------------------------

timeOptionsHr <- c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",
                    "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                    "20:00","21:00","22:00","23:00")

timeOptions30m <- c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30","04:00","04:30",
                    "05:00","05:30","06:00","06:30","07:00","07:30","08:00","08:30","09:00","09:30",
                    "10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30",
                    "15:00","15:30","16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30",
                    "20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30")

# KPI Filters

KPIvolumeOptions <- c("Appointment Volume","Appointment Status")
KPIschedulingOptions <- c("Booked Rate","Fill Rate")
KPIaccessOptions <- c("New Patient Ratio","Appointment Lead Time","3rd Next Available")
KPIdayOfVisitOptions <- c("Cycle Time","Wait Time")
kpiOptions <- c("Patient Volume","Appointment Status",
                "Booked Rate","Fill Rate",
                "New Patient Ratio","New Patient Wait Time","3rd Next Available",
                "Check-in to Room-in Time","Provider Time")

# Total Days in the Entire Data Set 
daysOfWeek.Table <- 
  data.hour.arrived %>%
  group_by(Appt.Day,Appt.DateYear) %>%
  summarise(count = n()) %>%
  summarise(count = n())

# Empty data frame for day of week by time (hour)
Time <- rep(timeOptionsHr, 7)
Day <- rep(daysOfWeek.options, each = 24)
byDayTime.df <- as.data.frame(cbind(Day,Time))

# Empty data frame for date and time (hour)
dateInData <- length(unique(data.hour.arrived$Appt.DateYear))
Date <- rep(unique(data.hour.arrived$Appt.DateYear), each = 24)
Time <- rep(timeOptionsHr, dateInData)
byDateTime.df <- as.data.frame(cbind(Date,Time))

# Empty data frame for time (hour)
byTime.df <- as.data.frame(timeOptionsHr)
colnames(byTime.df) <- c("Time")

# [4.2] Data filter functions ---------------------------------------------------------------------------------
groupByFilters <- function(dt, campus, specialty, department, provider, mindateRange, maxdateRange, daysofweek){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Provider %in% provider,
                          mindateRange <= Appt.DTTM, maxdateRange >= Appt.DTTM, Appt.Day %in% daysofweek
  )
  return(result)
}

groupByFilters_1 <- function(dt, apptType, insurance){
  result <- dt %>% filter(Appt.Type %in% apptType, Coverage %in% insurance)
  return(result)
}

# PLACEHOLDER FOR DAY OF VISIT ANALYSIS ------------------------------------------------------------------------
library(edeaR)
ex_patients <- "example patient flow observation.csv"
ex_patients <- read.csv(ex_patients, stringsAsFactors = TRUE)

ex_patients$registration_type <- factor(ex_patients$registration_type, labels = c("complete","start")) # Converting the activity status to factor values
ex_patients$time <- ymd_hms(ex_patients$time) # Converting the timestamps

ex_patients <- eventlog(eventlog =  ex_patients,
                        case_id = "patient",
                        activity_id = "handling",
                        activity_instance_id = "handling_id",
                        lifecycle_id = "registration_type",
                        timestamp = "time",
                        resource_id = "employee",
                        order = ".order")

