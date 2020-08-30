### (0) Install and Load Required Packages ============================================================

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

### (1) Set aesthetics theme -----------------------------------------------------------------------------

# Color Functions for Graphs
theme_set(theme_minimal())

# Mount Sinai corporate colors 
MountSinai_colors <- c(
  `dark purple`  = "#212070",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#7f7f7f",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#5753d0",
  `med pink`     = "#f75dbe",
  `med blue`     = "#5cd3ff",
  `med grey`     = "#a5a7a5",
  `light purple` = "#c7c6ef",
  `light pink`   = "#fcc9e9",
  `light blue`   = "#c9f0ff",
  `light grey`   = "#dddedd"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `main`  = MountSinai_cols("dark purple","dark pink","dark blue","dark grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot insetead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# ggplot themes

library(dplyr)
library(extrafont)
font_import()
loadfonts(device = "win")
windowsFonts()

# ggplot theme functions
theme_new_line <- function(base_size = 12,
                           base_family = "Calibri",
                           base_line_size = base_size / 170,
                           base_rect_size = base_size / 170) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(1.5),
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(1.25),
        face = "italic",
        hjust = 0,
        margin = margin(0, 0, 10, 0)
      ),
      axis.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(0.75)
      ),
      axis.text = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255),
        size = rel(0.75)
      ),
      axis.text.x = element_text(angle = 45,
                                 hjust = 0.5),
      axis.line = element_line(color = rgb(25, 43, 65, maxColorValue = 255)),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      
      complete = TRUE
    )
}


theme_new_line <- function(base_size = 12,
                           base_family = "Calibri",
                           base_line_size = base_size / 170,
                           base_rect_size = base_size / 170) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 20,
        margin = margin(0, 0, 30, 0)
      ),
      legend.position = "top",
      legend.text = element_text(size = "12"),
      legend.direction = "horizontal",
      legend.key.size = unit(1.0, "cm"),
      legend.title = element_blank(),
      axis.title = element_text(size = "14"),
      axis.text = element_text(size = "14"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(
        angle = 90,
        hjust = 0.5,
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(margin = margin(l = 5, r = 5)),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 0.3, colour = "black"),
      plot.margin = margin(30, 30, 30, 30)
    )
}

theme_new1 <- function(base_size = 12,
                       base_family = "Calibri",
                       base_line_size = base_size / 170,
                       base_rect_size = base_size / 170) {
  theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 20,
        margin = margin(0, 0, 30, 0)
      ),
      legend.position = "top",
      legend.text = element_text(size = "12"),
      legend.direction = "horizontal",
      legend.key.size = unit(1.0, "cm"),
      legend.title = element_blank(),
      axis.title = element_text(size = "14"),
      axis.text = element_text(size = "14"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5)),
      axis.text.x = element_text(
        angle = 90,
        hjust = 0.5,
        margin = margin(t = 10)
      ),
      axis.text.y = element_text(margin = margin(l = 5, r = 5)),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(size = 0.3, colour = "black"),
      plot.margin = margin(30, 30, 30, 30)
    )
}










### (2) Import Data ----------------------------------------------------------------------------------

# Set Working Directory
#wdpath <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Ambulatory Dashboard/Coding"
wdpath <- "C:/Users/kweons01/Desktop/Ambulatory Dashboard/Coding"
setwd(wdpath)

# Load Data Files 
#master.data.old <- read_excel("Master_Data.xlsx", col_names = TRUE, na = c("", "NA"))
master.data.new <- read_excel("MV DM PATIENT ACCESS sample.xlsx", col_names = TRUE, na = c("", "NA"))

# Load Reference Files
site_ref <-  read_excel("Department Site Crosswalk 8-24-2020.xlsx", col_names = TRUE, na = c("", "NA"))

# 
# old_cols <- as.data.frame(colnames(master.data.old))
# old_cols$in_old <- "Yes"
# 
# new_cols <- as.data.frame(colnames(master.data.new))
# new_cols$in_old <- old_cols$in_old[match(new_cols$`colnames(master.data.new)`, old_cols$`colnames(master.data.old)`)]
# 
# #write_xlsx(new_cols, "new fields.xlsx")
# 
# cols_inc <- new_cols %>% filter(in_old == "Yes")
# cols_inc <- as.vector(cols_inc$`colnames(master.data.new)`)
# 
# data_raw <- master.data.new[,cols_inc]

data.raw <- master.data.new

# Crosswalk Campus to Site by Department Name
data.raw$campus_new <- site_ref$`Site Final`[match(data.raw$DEPARTMENT_NAME,site_ref$`Dept Name`)]

### (3) Pre-process data ----------------------------------------------------------------------------------

# Data fields incldued for analysis 
original.cols <- c("campus_new","DEPT_SPECIALTY_NAME","DEPARTMENT_NAME","PROV_NAME_WID",
                   "MRN","PAT_NAME","ZIP_CODE","SEX","BIRTH_DATE","FINCLASS",
                   "APPT_MADE_DTTM","APPT_DTTM","PRC_NAME","APPT_LENGTH","DERIVED_STATUS_DESC",
                   "APPT_CANC_DTTM", "CANCEL_REASON_NAME","FPA",
                   "SIGNIN_DTTM","PAGED_DTTM","CHECKIN_DTTM","ARVL_LIST_REMOVE_DTTM",
                   "ROOMED_DTTM","FIRST_ROOM_ASSIGN_DTTM","VITALS_TAKEN_TM",
                   "PHYS_ENTER_DTTM","Provider_Leave_DTTM",
                   "VISIT_END_DTTM","CHECKOUT_DTTM",
                   "TIME_IN_ROOM_MINUTES","CYCLE_TIME_MINUTES")

# Subset raw data 
data.subset <- data.raw[original.cols]

# Rename data fields (columns) 
new.cols <- c("Campus","Campus.Specialty","Department","Provider",
              "MRN","Patient.Name","Zip.Code","Sex","Birth.Date","Coverage",
              "Appt.Made.DTTM","Appt.DTTM","Appt.Type","Appt.Dur","Appt.Status",
              "Appt.Cancel.DTTM", "Cancel.Reason","FPA",
              "Signin.DTTM","Paged.DTTM","Checkin.DTTM","Arrival.remove.DTTM",
              "Roomin.DTTM","Room.assigned.DTTM","Vitals.DTTM",
              "Providerin_DTTM","Providerout_DTTM",
              "Visitend.DTTM","Checkout.DTTM",
              "Time.in.room","Cycle.time")

colnames(data.subset) <- new.cols

# Format Date and Time Columns
dttm.cols <- c("Birth.Date","Appt.Made.DTTM","Appt.DTTM","Appt.Cancel.DTTM",
               "Checkin.DTTM","Arrival.remove.DTTM","Roomin.DTTM","Room.assigned.DTTM",
               "Vitals.DTTM","Providerin_DTTM","Providerout_DTTM",
               "Visitend.DTTM","Checkout.DTTM")

dttm <- function(x) {
  as.POSIXct(x,format="%d-%b-%Y %H:%M:%S",tz=Sys.timezone())
}

data.subset$Birth.Date <- dttm(data.subset$Birth.Date)
data.subset$Appt.Made.DTTM <- dttm(data.subset$Appt.Made.DTTM)
data.subset$Appt.DTTM <- dttm(data.subset$Appt.DTTM)
data.subset$Appt.Cancel.DTTM <- dttm(data.subset$Appt.Cancel.DTTM)
data.subset$Checkin.DTTM <- dttm(data.subset$Checkin.DTTM)
data.subset$Arrival.remove.DTTM <- dttm(data.subset$Arrival.remove.DTTM)
data.subset$Roomin.DTTM <- dttm(data.subset$Roomin.DTTM)
data.subset$Room.assigned.DTTM <- dttm(data.subset$Room.assigned.DTTM)
data.subset$Vitals.DTTM <- dttm(data.subset$Vitals.DTTM)
data.subset$Providerin_DTTM <- dttm(data.subset$Providerin_DTTM)
data.subset$Providerout_DTTM <- dttm(data.subset$Providerout_DTTM)
data.subset$Visitend.DTTM <- dttm(data.subset$Visitend.DTTM)
data.subset$Checkout.DTTM <- dttm(data.subset$Checkout.DTTM)

# Notify and remove duplicates in data 
data.duplicates <- data.subset %>% duplicated()
data.duplicates <- length(data.duplicates[data.duplicates == TRUE]) ## Count of duplicated records
data.subset.new <- data.subset %>% distinct() ## New data set with duplicates removed

if(data.duplicates == 0){
  tkmessageBox(title = "NOTE",
               message = "There are no duplicative records founds. Press OK to continue.", icon="info", type="ok")
} else {
  tkmessageBox(title= "NOTE",
               message = print(paste(data.duplicates," duplicative data records removed. Press OK to continue.")))
}

# Create additional columns for analysis 
data.subset.new$Appt.DateYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y-%m-%d") ## Create date-year column
data.subset.new$Appt.MonthYear <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b-%Y") ## Create month - year column
data.subset.new$Appt.Date <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%m-%d") ## Create date column
data.subset.new$Appt.Year <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%Y") ## Create year column
data.subset.new$Appt.Month <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%b") ## Create month colunm
data.subset.new$Appt.Day <- format(as.Date(data.subset.new$Appt.DTTM, format="%m/%d/%Y"), "%a") ## Create day of week colunm
data.subset.new$Appt.Quarter <- quarters(as.Date(data.subset.new$Appt.DTTM)) ## Create quarter column 
data.subset.new$Appt.TM.Hr <- format(strptime(as.ITime(round_date(data.subset.new$Appt.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by hour 
data.subset.new$Appt.TM.30m <- format(strptime(as.ITime(round_date(data.subset.new$Appt.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Appt time rounded by 30-min
data.subset.new$Checkin.Hr <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "hour")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by hour 
data.subset.new$Checkin.30m <- format(strptime(as.ITime(round_date(data.subset.new$Checkin.DTTM, "30 minutes")), "%H:%M:%S"),'%H:%M') ## Checkin time rounded by 30-min
data.subset.new$Lead.Days <- as.numeric(round(difftime(data.subset.new$Appt.DTTM, data.subset.new$Appt.Cancel.DTTM,  units = "days"),2)) ## Lead days for appt cancellation 
data.subset.new$uniqueId <- paste(data.subset.new$Department,data.subset.new$MRN,data.subset.new$Appt.DTTM) ## Unique ID 
data.subset.new$cycleTime <- as.numeric(round(difftime(data.subset.new$Visitend.DTTM,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Visitend (min)
data.subset.new$checkinToRoomin <- as.numeric(round(difftime(data.subset.new$Roomin.DTTM,data.subset.new$Checkin.DTTM,units="mins"),1)) ## Checkin to Roomin (min)
data.subset.new$providerinToOut <- as.numeric(round(difftime(data.subset.new$Providerout.DTTM,data.subset.new$Providerin.DTTM,units="mins"),1)) ## Provider in to out (min)
data.subset.new$visitEndToCheckout <- as.numeric(round(difftime(data.subset.new$Checkout.DTTM,data.subset.new$Visitend.DTTM,units="mins"),1)) ## Visitend to Checkout (min)

data.subset.new <- as.data.frame(data.subset.new)


### (4) Data Subset -----------------------------------------------------------------------------------------------------

all.data <- data.subset.new ## All data: Arrived, No Show, Canceled, Bumped, Rescheduled
arrivedNoShow.data <- data.subset.new %>% filter(Appt.Status %in% c("Arrived","No Show")) ## Arrived + No Show data: Arrived and No Show
arrived.data <- data.subset.new %>% filter(Appt.Status %in% c("Arrived")) ## Arrived data: Arrived
canceled.data <- data.subset.new %>% filter(Appt.Status %in% c("Canceled")) ## Canceled data: canceled appointments only
bumped.data <- data.subset.new %>% filter(Appt.Status %in% c("Bumped")) ## Bumped data: bumped appointments only

### (5) Pre-processing Space Utilization Dataframe ----------------------------------------------------------------------

scheduled.data <- arrivedNoShow.data ## All appts scheduled 
scheduled.data$Appt.Start <- as.POSIXct(scheduled.data$Appt.TM.Hr, format = "%H:%M")
scheduled.data$Appt.End <- as.POSIXct(scheduled.data$Appt.Start + scheduled.data$Appt.Dur*60, format = "%H:%M")

# 1. Scheduled utilization dataframe by hour interval 
#print.POSIXct <- function(x,...)print(format(x,"%Y-%m-%d %H:%M:%S"))

time.hour <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "hour"),"%H:%M", tz="GMT")
time.hour <- time.hour[1:24]
time.hour.df <- data.frame(matrix(ncol=length(time.hour), nrow=nrow(scheduled.data)))
colnames(time.hour.df) <- time.hour
time.hour.df <- cbind(scheduled.data,time.hour.df) ## Scheduled data + columns by hour 

c.start <- which(colnames(time.hour.df)=="00:00")
c.end <- which(colnames(time.hour.df)=="23:00") + 1

midnight <- data.frame(matrix(ncol=1, nrow=nrow(scheduled.data)))
colnames(midnight) <- "00:00"

time.hour.df <- cbind(time.hour.df,midnight)

i <- 1
n <- nrow(time.hour.df) + 1

sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()
sleep_for_a_minute()

while(c.start!=c.end){
  i <- 1
  while(i!=n){
    if(time.hour.df$Appt.Start[i] >= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M") &
       time.hour.df$Appt.Start[i] < as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
      time.hour.df[i,c.start] <- pmin(time.hour.df$Appt.Dur[i],difftime(as.POSIXct(colnames(time.hour.df)[c.start+1],format = "%H:%M"),
                                                                        as.POSIXct(colnames(time.hour.df)[c.start],format = "%H:%M"), unit="mins"))
      }else if(time.hour.df$Appt.End[i] >= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M") &
               time.hour.df$Appt.End[i] < as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
        time.hour.df[i,c.start] <- difftime(time.hour.df$Appt.End[i],as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M"), unit="mins")
        }else if(time.hour.df$Appt.Start[i] >= as.POSIXct(colnames(time.hour.df)[c.start+1], format = "%H:%M")){
          time.hour.df[i,c.start] <- 0
          }else if(time.hour.df$Appt.End[i] <= as.POSIXct(colnames(time.hour.df)[c.start], format = "%H:%M")){
            time.hour.df[i,c.start] <- 0
          }else{time.hour.df[i,c.start] <- 60
            }
    i <- i+1
  }
  c.start <- c.start+1
  }

time.hour.df <- time.hour.df[1:length(time.hour.df)-1]

data.hour.scheduled <- time.hour.df

data.hour.arrived <- time.hour.df %>% filter(Appt.Status %in% c("Arrived")) ## Utilization by hour data for arrived appts only 

write_xlsx(data.hour.scheduled, "scheduled_util_by_hour.xlsx")
write_xlsx(data.hour.arrived, "actual_util_by_hour.xlsx")

end_time <- Sys.time()

processing.time1 <- (end_time - start_time) # 12.7 - 20.7 min to process 34,687 rows + 23 columns (processing time varies each time)
processing.time1

# 2. Scheduled utilization dataframe by 30-min interval 

time.30min <- format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "30 min"),"%H:%M", tz="GMT")
time.30min <- time.30min[1:48]

time.30min.df <- data.frame(matrix(ncol=length(time.30min), nrow=nrow(scheduled.data)))

colnames(time.30min.df) <- time.30min
time.30min.df <- cbind(scheduled.data,time.30min.df)

c.start <- which(colnames(time.30min.df)=="00:00")
c.end <- which(colnames(time.30min.df)=="23:30") + 1

midnight <- data.frame(matrix(ncol=1, nrow=nrow(scheduled.data)))
colnames(midnight) <- "00:00"

time.30min.df <- cbind(time.30min.df,midnight)

i <- 1
n <- nrow(time.30min.df) + 1

sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()
sleep_for_a_minute()

while(c.start!=c.end){
  i <- 1
  while(i!=n){
    if(time.30min.df$Appt.Start[i] >= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M") &
       time.30min.df$Appt.Start[i] < as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
      time.30min.df[i,c.start] <- pmin(time.30min.df$Appt.Dur[i],difftime(as.POSIXct(colnames(time.30min.df)[c.start+1],format = "%H:%M"),
                                                                          as.POSIXct(colnames(time.30min.df)[c.start],format = "%H:%M"), unit="mins"))
      }else if(time.30min.df$Appt.End[i] >= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M") &
               time.30min.df$Appt.End[i] < as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
        time.30min.df[i,c.start] <- difftime(time.30min.df$Appt.End[i],as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M"), unit="mins")
        }else if(time.30min.df$Appt.Start[i] >= as.POSIXct(colnames(time.30min.df)[c.start+1], format = "%H:%M")){
          time.30min.df[i,c.start] <- 0
          }else if(time.30min.df$Appt.End[i] <= as.POSIXct(colnames(time.30min.df)[c.start], format = "%H:%M")){
            time.30min.df[i,c.start] <- 0
            }else{
              time.30min.df[i,c.start] <- 30
              }
    i <- i+1}
  c.start <- c.start+1
  }

time.30min.df <- time.30min.df[1:length(time.30min.df)-1]

data.30min.scheduled <- time.30min.df
data.30min.arrived <- data.30min.scheduled %>% filter(Appt.Status %in% c("Arrived"))

write_xlsx(data.hour.scheduled, "scheduled_util_by_30min.xlsx")
write_xlsx(data.hour.arrived, "actual_util_by_30min.xlsx")

end_time <- Sys.time()

processing.time2 <- (end_time - start_time) # 26.1 min to process 34,687 rows + 47 columns
processing.time2

# # Import calculated utilization dataframes
# data.hour.arrived <- read_excel("actual_util_by_hour.xlsx", col_names = TRUE, na = c("", "NA"))
# data.hour.scheduled <- read_excel("scheduled_util_by_hour.xlsx", col_names = TRUE, na = c("", "NA"))
# data.30min.arrived <- read_excel("actual_util_by_30min.xlsx", col_names = TRUE, na = c("", "NA"))
# data.30min.scheduled <- read_excel("scheduled_util_by_30min.xlsx", col_names = TRUE, na = c("", "NA"))


### (6) Shiny App Components Set-up -------------------------------------------------------------------------------

# Mater Filters 
daysOfWeek.options <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") ## Days of Week Filter

timeOptionsHr <- c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00",
                   "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                   "20:00","21:00","22:00","23:00") ## Time Range by Hour Filter

timeOptions30m <- c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30","04:00","04:30",
                    "05:00","05:30","06:00","06:30","07:00","07:30","08:00","08:30","09:00","09:30",
                    "10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30",
                    "15:00","15:30","16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30",
                    "20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30") ## Time Range by 30min Filter

# KPI Filters
KPIvolumeOptions <- c("Appointment Volume","Appointment Status")
KPIschedulingOptions <- c("Booked Rate","Fill Rate")
KPIaccessOptions <- c("New Patient Ratio","Appointment Lead Time","3rd Next Available")
KPIdayOfVisitOptions <- c("Cycle Time","Wait Time")
kpiOptions <- c("Patient Volume","Appointment Status",
                "Booked Rate","Fill Rate",
                "New Patient Ratio","New Patient Wait Time","3rd Next Available",
                "Check-in to Room-in Time","Provider Time")

# Reference dataframes, vectors, etc.
daysOfWeek.Table <- data.hour.arrived %>% group_by(Appt.Day,Appt.DateYear) %>% summarise(count = n()) %>% summarise(count = n()) ## Total Days in the Entire Data Set 

Time <- rep(timeOptionsHr, 7)
Day <- rep(daysOfWeek.options, each = 24)
byDayTime.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (hour)

dateInData <- length(unique(data.hour.arrived$Appt.DateYear))
Date <- rep(unique(data.hour.arrived$Appt.DateYear), each = 24)
Time <- rep(timeOptionsHr, dateInData)
byDateTime.df <- as.data.frame(cbind(Date,Time)) ## Empty data frame for date and time (hour)

byTime.df <- as.data.frame(timeOptionsHr)
colnames(byTime.df) <- c("Time") ## Empty data frame for time (hour)


# (7) Data Reactive functions ---------------------------------------------------------------------------------

groupByFilters <- function(dt, campus, specialty, department, provider, mindateRange, maxdateRange, daysofweek){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Provider %in% provider,
                          mindateRange <= Appt.DTTM, maxdateRange >= Appt.DTTM, Appt.Day %in% daysofweek)
  return(result)
}

groupByFilters_1 <- function(dt, apptType, insurance){
  result <- dt %>% filter(Appt.Type %in% apptType, Coverage %in% insurance)
  return(result)
}

# (8) PLACEHOLDER FOR DAY OF VISIT ANALYSIS ------------------------------------------------------------------------
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

