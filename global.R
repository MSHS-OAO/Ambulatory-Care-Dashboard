
## Access Tab


suppressMessages({
  library(readxl)
  library(writexl)
  library(plyr)
  library(pool)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(DBI)
  library(odbc)
  library(dbplyr)
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
  library(glue)
})



#detach("package:kableExtra", unload = TRUE)
#library(kableExtra)

# ### (0) Maximize R Memory Size 
#memory.limit(size = 8000000)

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


# connect to SQL
# con <- dbConnect(odbc(),
#                  Driver = "Oracle 21_5 ODBC driver",
#                  Trusted_Connection = "True",
#                  uid = 'aghaer01',
#                  pwd = "5VWtKW*yxf" )
# 

poolcon <- dbPool(drv = odbc::odbc(),
                  dsn = "OAO Cloud DB",
                  username= 'aghaer01',
                  password = "5VWtKW*yxf")


#con <- dbConnect(odbc(), "OAO Cloud DB")



access_tbl <- tbl(poolcon, "ACCESS_SQL")

holid <- tbl(poolcon, "HOLIDAYS")


  
max_date <- Sys.Date()-1
min_date <- max_date - 730



max_arrived <- glue("Select max(APPT_MADE_DTTM) AS maxDATE FROM(Select APPT_MADE_DTTM, APPT_STATUS FROM ACCESS_SQL WHERE APPT_STATUS IN 'Arrived')")
max_arrived <- dbGetQuery(poolcon, max_arrived)
max_arrived <- as.Date(max_arrived$MAXDATE, format="%Y-%m-%d")

# max_arrived <- access_tbl %>% select(APPT_MADE_DTTM, APPT_STATUS )%>% filter(APPT_STATUS=="Arrived" ) %>% collect()
# max_arrived <- max(max_arrived$APPT_MADE_DTTM)


date_format <- "YYYY-MM-DD HH24:MI:SS"




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





## Filtered Access Data
groupByFilters_Access <- function(dt, campus, specialty, department, resource, provider, visitMethod, visitType, mindateRange, maxdateRange, daysofweek, holidays){
  format <- "YYYY-MM-DD HH24:MI:SS"
  daysofweek <- toupper(daysofweek)
  
  result <- dt %>% filter(CAMPUS %in% campus, 
                          CAMPUS_SPECIALTY %in% specialty, 
                          DEPARTMENT %in% department, 
                          RESOURCES %in% resource, 
                          PROVIDER %in% provider,
                          VISIT_METHOD %in% visitMethod, 
                          APPT_TYPE %in% visitType, 
                          TO_DATE(mindateRange, format) <= APPT_MADE_DTTM, 
                          TO_DATE(maxdateRange, format) >= APPT_MADE_DTTM, 
                          APPT_DAY %in% daysofweek#, 
                          #!HOLIDAY %in% holidays
                          ) #%>% collect()
  #return(result)
}


arrived_access_subset <- access_tbl %>% filter(APPT_STATUS %in% c("Rescheduled", "Scheduled", "Arrived") & 
                                                 TO_DATE(min_date, date_format) <= APPT_DTTM) #%>% show_query()

dataAll_access_subset <- access_tbl %>% filter(TO_DATE(min_date, date_format) <= APPT_DTTM)


data_arrived_noshow_subset <- access_tbl %>% filter(TO_DATE(min_date, date_format) <= APPT_DTTM  & 
                                                      TO_DATE(max_arrived, date_format) >= APPT_DTTM &
                                                      APPT_STATUS %in% c("Arrived" , "No Show") |
                                                      (LEAD_DAYS  == 0 & APPT_STATUS %in% c("Canceled", "Bumped", "Rescheduled"))) #%>% show_query()


