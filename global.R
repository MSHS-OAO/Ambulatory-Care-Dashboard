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
# 
# # install.packages("htmltools")
# # require(htmltools)
# # library(htmltools)
# # update.packages("htmltools")
# 
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
# install.packages("ggforce")
# install.packages("packcircles")
# install.packages("treemapify")
# install.packages("treemap")
# install.packages("tis")
# install.packages("vroom")
# install.packages("lubridate")
# install.packages("plyr")
# install.packages("sjmisc")
# install.packages("shinyBS")
# install.packages("shinyscreenshot")
# install.packages("reactable")
# devtools::install_github("ropensci/plotly")

devtools::install_github("haozhu233/kableExtra", upgrade = "never")

suppressMessages({
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  #library(zipcode)
  library(utils)
  library(scales)
  library(chron)
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
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(janitor)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(feather)
  library(zipcodeR)
  library(formattable)
  library(shinyjs)
  library(janitor)
  library(patchwork)
  library(pryr)
  library(reactable)
  library(devtools)
})




#detach("package:kableExtra", unload = TRUE)
#library(kableExtra)

# ### (0) Maximize R Memory Size 
memory.limit(size = 8000000)

### (1) Set aesthetics theme -----------------------------------------------------------------------------

# Color Functions for Graphs =====================================
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
  
  `dark`  = MountSinai_cols("dark purple","dark grey",
                            "yellow","med pink","dark pink","dark blue",
                            "med purple","med grey","med blue"),
  
  `main`  = MountSinai_cols("dark purple","dark grey","dark pink","dark blue","med purple","med pink","med blue","med grey"),
  
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

# ggplot theme function s====================================

# font_import()
# loadfonts(device = "win")
# windowsFonts()


graph_theme <- function(legend_pos) {
  theme(
    plot.title = element_text(hjust=0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
    plot.caption = element_text(size = 12, face = "italic"),
    legend.position = legend_pos,
    #legend.title = element_text(size = "14"),
    legend.title = element_blank(),
    legend.text = element_text(size = "14"),
    strip.text = element_text(size=14),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 16, angle=50, hjust=1),
    axis.text.y = element_text(size = 14),
    axis.line.x = element_blank())#,
  #plot.margin = margin(0,80,0,80))
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
      strip.text = element_text(size = 16),
      legend.position = "top",
      legend.text = element_text(size = "14"),
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


table_theme <- function(){
  theme(
    panel.grid.minor = element_line(size = 0.3, colour = "black"),
    panel.grid.major = element_blank(),
    axis.title.x = element_text(size = 14, angle = 0, colour = "black", face= "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14, colour = "black", face= "bold"),
    legend.position = "none",
    plot.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size=0.5),
    axis.line.x = element_line(colour = "black", size=0.5),
    plot.margin=unit(c(-0.5,1,1,1), "cm"))
}




### (2) Import Data ----------------------------------------------------------------------------------

wdpath <- here::here()
#wdpath <- "C:/Users/kweons01/Desktop/IP Demand Modeling Desktop/Ambulatory-Care-Dashboard-Publish"

setwd(wdpath)


### (4) Data Subset -----------------------------------------------------------------------------------------------------

### RStudio COnnect Data Read In

# ### New Location with Updated Data
# historical.data <- readRDS("/data/Ambulatory/Data/historical_data.rds")
# slot.data.subset <- readRDS("/data/Ambulatory/Data/slot_data.rds")
# holid <- as.data.frame(read_feather("/data/Ambulatory/Data/holid.feather"))
# utilization.data <- readRDS("/data/Ambulatory/Data/utilization_data.rds")
# population.data_filtered  <- readRDS("/data/Ambulatory/Data/population_data.rds")
# filter_path <- "/data/Ambulatory/Filters"

# historical.data <- readRDS("C:/Users/villea04/Documents/Process_Amb/Data/historical_data.rds")
historical.data <- readRDS(paste0(wdpath,"/Data/historical_data.rds")) ## Filter out historical data only
slot.data.subset <- readRDS(paste0(wdpath,"/Data/slot_data_subset.rds"))
holid <- readRDS(paste0(wdpath,"/Data/holid.rds"))
utilization.data <- readRDS(paste0(wdpath,"/Data/utilization_data.rds"))
population.data_filtered <- readRDS(paste0(wdpath,"/Data/population_data_filtered.rds"))
filter_path <- paste0(wdpath, "/Filters")

max_date <- max(historical.data$Appt.DateYear)

historical.data["Appt.Source.New"][historical.data["Appt.Source.New"] == "MyChart"] <- "My MountSinai/ MyChart"
historical.data["Appt.Source.New"][historical.data["Appt.Source"] == "APP, FINDADOC [MSHSFAD]"] <- "FindADoc"
## Slot datasets
# past.slot.data <- slot.data.subset %>% filter(Appt.DTTM <= max_date, Appt.DTTM >= max_date - 365)
# future.slot.data <- slot.data.subset %>% filter(Appt.DTTM > max_date, Appt.DTTM <= max_date + 90)
# rm(slot.data.subset)

setDT(utilization.data)
setDT(slot.data.subset)
setDT(historical.data)
kpi.all.data <- historical.data[Appt.DTTM >= max_date - 3*365]
rm(historical.data)




# ## KPI Rows DataTable
kpi.arrivedNoShow.data.rows <- kpi.all.data[Appt.Status %in% c("Arrived","No Show"), which = TRUE]
kpi.arrived.data.rows <- kpi.all.data[Appt.Status %in% c("Arrived"), which = TRUE]
kpi.canceled.bumped.data.rows <- kpi.all.data[Appt.Status %in% c("Canceled","Bumped"), which = TRUE]
kpi.canceled.data.rows <- kpi.all.data[Appt.Status %in% c("Canceled"), which = TRUE]
kpi.bumped.data.rows <- kpi.all.data[Appt.Status %in% c("Bumped"), which = TRUE]


# ## Other datasets Rows DataTable
all.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730, which = TRUE]

arrived.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 & 
                                    Appt.Status %in% c("Arrived"), which = TRUE]

canceled.bumped.rescheduled.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 &
                                                        Appt.Status %in% c("Canceled","Bumped","Rescheduled"), which = TRUE]

canceled.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 & 
                                     Appt.Status %in% c("Canceled"), which = TRUE]

bumped.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 &
                                   Appt.Status %in% c("Bumped"), which = TRUE]

rescheduled.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 &
                                        Appt.Status %in% c("Rescheduled"), which = TRUE]

sameDay.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 &
                               Appt.Status %in% c("Canceled","Bumped","Rescheduled") &
                               Lead.Days == 0, which = TRUE]

noshow.data.rows <- kpi.all.data[Appt.DTTM >= max_date - 730 &
                                   Appt.Status %in% c("No Show"),
                                 which = TRUE
]

noshow.data.rows <- c(sameDay.rows, noshow.data.rows)

arrivedNoShow.data.rows <-  c(noshow.data.rows, arrived.data.rows)

past.slot.data.rows <- slot.data.subset[Appt.DateYear <= max_date, which = TRUE]

future.slot.data.rows <- slot.data.subset[Appt.DateYear > max_date, which = TRUE]

all.slot.rows <- c(past.slot.data.rows,future.slot.data.rows)

scheduled.utilization.data.rows <- utilization.data[util.type == "scheduled", which = TRUE]

arrived.utilization.data.rows <- utilization.data[util.type == "actual", which = TRUE]




## KPI datasets
# kpi.all.data <- historical.data %>% filter(Appt.DTTM >= max_date - 3*365) ## All data: Arrived, No Show, Canceled, Bumped, Rescheduled
# kpi.arrivedNoShow.data <- kpi.all.data %>% filter(Appt.Status %in% c("Arrived","No Show"))  ## Arrived + No Show data: Arrived and No Show
# kpi.arrived.data <- kpi.arrivedNoShow.data %>% filter(Appt.Status %in% c("Arrived")) ## Arrived data: Arrived
# kpi.canceled.bumped.data <- kpi.all.data %>% filter(Appt.Status %in% c("Canceled","Bumped")) ## Arrived data: Arrived
# kpi.canceled.data <- kpi.canceled.bumped.data %>% filter(Appt.Status %in% c("Canceled")) ## Canceled data: canceled appointments only
# kpi.bumped.data <-kpi.canceled.bumped.data %>% filter(Appt.Status %in% c("Bumped")) ## Bumped data: bumped appointments only



## Other datasets
# all.data <- historical.data %>% filter(Appt.DTTM >= max_date - 365) ## All data: Arrived, No Show, Canceled, Bumped, Rescheduled
# arrived.data <- all.data %>% filter(Appt.Status %in% c("Arrived")) ## Arrived data: Arrived
# canceled.bumped.rescheduled.data <- all.data %>% filter(Appt.Status %in% c("Canceled","Bumped","Rescheduled")) ## Canceled data: canceled appointments only
# canceled.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Canceled")) ## Canceled data: canceled appointments only
# bumped.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Bumped")) ## Bumped data: bumped appointments only
# rescheduled.data <- canceled.bumped.rescheduled.data %>% filter(Appt.Status %in% c("Rescheduled")) ## Bumped data: bumped appointments only
# sameDay <- canceled.bumped.rescheduled.data %>% filter(Lead.Days == 0) # Same day canceled, rescheduled, bumped appts
# noShow.data <- all.data %>% filter(Appt.Status %in% c("No Show")) ## Arrived + No Show data: Arrived and No Show
# noShow.data <- rbind(noShow.data,sameDay) # No Shows + Same day canceled, bumped, rescheduled
# arrivedNoShow.data <- rbind(arrived.data,noShow.data) ## Arrived + No Show data: Arrived and No Show

kpi.all.data <- as.data.frame(kpi.all.data)
slot.data.subset <- as.data.frame(slot.data.subset)
utilization.data <- as.data.frame(utilization.data)


### (5) Pre-processing Space Utilization Dataframe --------------------------------------------------------------------------------------
# Filter utilization data in last 60 days

#Combine Utilization Data
# timeOptionsHr_filter <- c("07:00","08:00","09:00",
#                           "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
#                           "20:00") ## Time Range by Hour Filter
# utilization.data <- rbind(data.hour.scheduled, data.hour.arrived)
# utilization.data <- utilization.data %>%
#   select(Campus, Campus.Specialty, Department, Resource, Provider,
#          Visit.Method, Appt.Type,
#          Appt.DateYear, Appt.MonthYear, Appt.Year, Appt.Week, Appt.Day, Appt.TM.Hr, holiday, sum, util.type,
#          timeOptionsHr_filter)
# 
# saveRDS(utilization.data, "new_utilization_data.rds")
# 
# arrived.utilization.data <- rbind(data.hour.scheduled %>% filter(Appt.Status == "Arrived"), data.hour.arrived)


### Zip Code Analysis--------------------------------------------------------------------------------------

# zipcode_ref <- read_csv(here::here("Oncology System Data - Zip Code Groupings 4.13.2021.csv"))
# zipcode_ref <- zipcode_ref[1:(length(zipcode_ref)-7)]
# zipcode_ref$`Zip Code Layer: A`[which(zipcode_ref$`Zip Code Layer: A` == "Long island")] <- "Long Island"
# 
# zipcode <- read_csv(here::here("zipcode_data.csv"))
# 
# population.data <- arrived.data
# population.data$new_zip <- normalize_zip(population.data$Zip.Code)
# population.data <- merge(population.data, zipcode_ref, by.x="new_zip", by.y="Zip Code", all.x = TRUE)
# 
# population.data <- merge(population.data, zipcode, by.x="new_zip", by.y="zip", all.x = TRUE)
# 
# population.data$`Zip Code Layer: A`[(is.na(population.data$`Zip Code Layer: A`) & 
#                                        (!is.na(population.data$state) | population.data$state != "NY"))] <- "Out of NYS"
# population.data <- population.data %>%
#   mutate(`Zip Code Layer: B` = ifelse(`Zip Code Layer: A` == "Out of NYS" & is.na(`Zip Code Layer: B`),
#                                       ifelse(state == "NJ", "New Jersey",
#                                              ifelse(state == "CT", "Connecticut",
#                                                     ifelse(state == "FL", "Florida",
#                                                            ifelse(state == "PA", "Pennsylvania", "Other")))), `Zip Code Layer: B`))
# 
# 
# population.data_filtered <- population.data %>% filter(!is.na(`Zip Code Layer: A`))

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

timeOptionsHr_filter <- c("07:00","08:00","09:00",
                          "10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00",
                          "20:00") ## Time Range by Hour Filter

timeOptions30m_filter <- c("07:00","07:30","08:00","08:30","09:00","09:30",
                           "10:00","10:30","11:00","11:30","12:00","12:30","13:00","13:30","14:00","14:30",
                           "15:00","15:30","16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30","20:00") ## Time Range by 30min Filter

monthOptions <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

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
Time <- rep(timeOptionsHr, 7)
Day <- rep(daysOfWeek.options, each = 24)
byDayTime.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (hour)

dateInData <- length(unique(utilization.data[arrived.utilization.data.rows,]$Appt.DateYear))
Date <- rep(unique(utilization.data[arrived.utilization.data.rows,]$Appt.DateYear), each = 24)
Time <- rep(timeOptionsHr, dateInData)
byDateTime.df <- as.data.frame(cbind(Date,Time)) ## Empty data frame for date and time (hour)

Time <- rep(timeOptions30m, 7)
Day <- rep(daysOfWeek.options, each = 48)
byDayTime30m.df <- as.data.frame(cbind(Day,Time)) ## Empty data frame for day of week by time (30-min)

dateInData <- length(unique(utilization.data[arrived.utilization.data.rows,]$Appt.DateYear))
Date <- rep(unique(utilization.data[arrived.utilization.data.rows,]$Appt.DateYear), each = 24)
Time <- rep(timeOptionsHr, dateInData)
byDateTime.df <- as.data.frame(cbind(Date,Time)) ## Empty data frame for date and time (30-min)

byTime.df <- as.data.frame(timeOptionsHr)
colnames(byTime.df) <- c("Time") ## Empty data frame for time (hour)

byTime30.df <- as.data.frame(timeOptions30m)
colnames(byTime30.df) <- c("Time") ## Empty data frame for time (hour)


# (7) Data Reactive functions ---------------------------------------------------------------------------------

## Filtered Scheduling Data
groupByFilters <- function(dt, campus, specialty, department, resource, provider, visitMethod, visitType, mindateRange, maxdateRange, daysofweek, holidays){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Resource %in% resource, Provider %in% provider,
                          Visit.Method %in% visitMethod, Appt.Type %in% visitType, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  return(result)
}




## Filtered No Show Data
# groupByFilters_1 <- function(dt, apptType, insurance){
#   result <- dt %>% filter(Appt.Type %in% apptType, Coverage %in% insurance)
#   # na_result <- dt[is.na(dt$Coverage),]
#   # result <- rbind(result,na_result)
#   return(result)
# }

groupByFilters_1 <- function(dt, apptType, insurance){
  result <- dt %>% filter(Coverage %in% insurance)
  if(apptType == "New"){
    result <- result %>% filter(New.PT3 == TRUE)
  }
  else if(apptType == "Established"){
    result <- result %>% filter(New.PT3 == FALSE)
  }
  else{
    result
  }
  # na_result <- dt[is.na(dt$Coverage),]
  # result <- rbind(result,na_result)
  return(result)
}

## Filtered Utilization Data
groupByFilters_2 <- function(dt, campus, specialty, department, resource, provider, visitMethod, visitType, mindateRange, maxdateRange, daysofweek, holidays, type){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Resource %in% resource, Provider %in% provider, 
                          Visit.Method %in% visitMethod, Appt.Type %in% visitType, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays, util.type %in% type)
  return(result)
}

## Filtered by Appt.Type Data
groupByFilters_3 <- function(dt, apptType){
  result <- dt %>% filter(New.PT3 == FALSE, Appt.Type %in% apptType)
  return(result)
}


## Filtered Slot Data
groupByFilters_4 <- function(dt, campus, specialty, department, resource, provider, visitMethod, mindateRange, maxdateRange, daysofweek, holidays){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Resource %in% resource, Provider %in% provider,
                          Visit.Method %in% visitMethod, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  return(result)
}



## Filtered Slot Data Test
groupByFilters_4_Test <- function(dt, campus, specialty, department, resource, visitMethod, mindateRange, maxdateRange, daysofweek, holidays){
  result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Resource %in% resource,
                          Visit.Method %in% visitMethod, 
                          mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
  return(result)
}


## Unique Patients Functions  -----------------------------------------------------------------
uniquePts_df_system <- function(data){
  
  result <- data %>%
    arrange(MRN, Appt.DTTM) %>% group_by(MRN) %>% mutate(uniqueSystem = row_number()) %>% ungroup() %>%
    filter(uniqueSystem == 1)
  
  return(result)
}

# Function for Value Boxes ------------------------------------------------------------------
valueBoxSpark <- function(value, title, subtitle, sparkobj = NULL, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      h4(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      em(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

arrived_first_date <- min((kpi.all.data[arrived.data.rows,])$Appt.DTTM)
arrived_last_date <- max((kpi.all.data[arrived.data.rows,])$Appt.DTTM)

enableBookmarking(store = "server")
