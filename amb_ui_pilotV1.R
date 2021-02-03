default_campus <- "MSUS"
default_specialty <- sort(unique(historical.data[historical.data$Campus %in% "MSUS", "Campus.Specialty"]))
default_departments <- sort(unique(historical.data[historical.data$Campus %in% "MSUS" &
                                                     historical.data$Campus.Specialty %in% default_specialty, "Department"])) 
default_resource_type <- c("Provider","Resource")
default_provider <- sort(unique(historical.data[
  historical.data$Campus %in% default_campus &
    historical.data$Campus.Specialty %in% default_specialty &
    historical.data$Department %in% default_departments & 
    historical.data$Resource %in% default_resource_type, "Provider"]))


ui <- dashboardPage(
  
  ### UI start-----------------------------------------------------------
  dashboardHeader(title = "Amb Care Analytics Tool",
                  titleWidth = 250),
  dashboardSidebar(
    # Customize dashboard color scheme: title bar = .logo & .navbar; side bar = .main-sidebar; background = .content-wrapper
    tags$head(tags$style(HTML('.logo {
                              background-color: #221f72 !important;
                              }
                              .navbar {
                              background-color: #221f72 !important;
                              }
                              
                              .content-wrapper {
                              background-color: white !important;
                              }'
                              
    ))),
    
    # Overwrite fixed height of dashboard sidebar
    tags$head(tags$style(HTML('.content-wrapper { height: 6000px !important;}'))),
    
    width = 200,
    
    sidebarMenu(id = "sbm",
                menuItem("KPIs", tabName = "KPIs", icon = icon("tachometer-alt")),
                menuItem("Site Overview", tabName = "systemOverview", icon = icon("hospital-symbol"),
                         menuSubItem("Site Overview", tabName = "system"),
                         menuSubItem("Site Comparison", tabName = "systemComparison")),
                menuItem("Practice Overview", tabName = "profile", icon = icon("hospital")),
                menuItem("Provider Overview", tabName = "provider", icon = icon("user-md")),
                menuItem("Population", tabName = "population", icon = icon("users")),
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar")),
                menuItem("Scheduling", tabName = "scheduling", icon = icon("calendar-day"),
                         menuSubItem("Scheduled/Arrived", tabName = "arrived"),
                         menuSubItem("No Shows/Overbooks", tabName = "noshows"),
                         menuSubItem("Bumps/Cancellations", tabName = "cancellations")),
                menuItem("Utilization", tabName = "utilization", icon = icon("percent")),
                menuItem("Access", tabName = "access", icon = icon("location-arrow"),
                         menuSubItem("New Patients", tabName = "newPatients"),
                         # menuSubItem("Upcoming Demand", tabName = "upcomingDemand"),
                         menuSubItem("Slot Usage", tabName = "slotUsage")),
                menuItem("Day of Visit", tabName = "day", icon = icon("hand-holding-medical"),
                         menuSubItem("Cycle Time", tabName = "cycleTime"),
                         menuSubItem("Room-in Time", tabName = "roomInTime")),
                menuItem("Data", tabName = "data", icon = icon("table")),
                menuItem("Help", tabName = "help", icon = icon("question-circle"),
                         menuSubItem("About", tabName = "helpabout", icon = icon("info")),
                         menuSubItem("Analysis Methods", tabName = "helpanalysis", icon = icon("file-excel")))
    ) # Close sidebarMenu
    
  ), # Close dashboardSidebar
  
  dashboardBody(
    
    # box "status" color for Mount Sinai Purple
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#221f72
    }
    .box.box-solid.box-primary{
    border-bottom-color:#ffffff;
    border-left-color:#ffffff;
    border-right-color:#ffffff;
    border-top-color:#ffffff;
    }
                    ")),
    
    # valueBox "yellow" color for Mount Sinai Light Grey
    tags$style(".small-box.bg-yellow { background-color: 	#dddedd !important; color: #000000 !important; }"),
    # valueBox "purple" color for Mount Sinai Dark Purple
    tags$style(".small-box.bg-purple { background-color: 	#212070 !important; color: #ffffff !important; }"),
    # valueBox "fuchsia" color for Mount Sinai Dark Pink
    tags$style(".small-box.bg-fuchsia { background-color: 	#d80b8c !important; color: #ffffff !important; }"),
    # valueBox "aqua" color for Mount Sinai Dark Blue
    tags$style(".small-box.bg-aqua { background-color: 	#00aeef !important; color: #ffffff !important; }"),
    
    
    # Top align plot outputs
    tags$head(tags$style(".top-align { vertical-align: top;}  ")),
    
    tabItems(
      # System Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "system",
              column(10,
                     div("Site Overview: Analysis by Campus", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     fluidRow(
                       column(3,
                              boxPlus(
                                title = "Summary", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(infoBoxOutput("siteTotalPts", width =12)),
                                fluidRow(infoBoxOutput("siteNewPtRatio", width=12)),
                                fluidRow(infoBoxOutput("siteTotalProvs", width=12)),
                                fluidRow(infoBoxOutput("sitePtsPerProv", width=12)))),
                       column(9, 
                              boxPlus(
                                title = "Specialties", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("siteSpecialties", height = "520px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c")
                                            ))),
                     column(12, 
                            boxPlus(
                              title = "Appointment Wait Time", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "median1",
                                label = "Median", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteWaitTime", height="800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                             )),
                     column(12, 
                            boxPlus(
                              title = "Working FTE", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("siteWorkingFTE", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), br(),
                              plotOutput("sitePtsPerFTE", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ))
              )),
      
      # System Comparison Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "systemComparison",
              column(10,
                     div("Site Comparison: Analysis by Specialty", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     column(12, 
                            boxPlus(
                              title = "Patient Volume", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty1",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonPts", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ),
                            boxPlus(
                              title = "New Patients", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty2",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNewPtRatio", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), br(),
                              materialSwitch(
                                inputId = "bySpecialty3",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNewPtWaitTime", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ),
                            boxPlus(
                              title = "Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty4",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonBookedRate", height="900px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), br(),
                              materialSwitch(
                                inputId = "bySpecialty5",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNoShow", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ),
                            boxPlus(
                              title = "Cycle Times", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty6",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonCycleTime", height="900px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")),
                            boxPlus(
                              title = "Working FTE", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty7",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonWorkingFTE", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), br(),
                              materialSwitch(
                                inputId = "bySpecialty8",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonPtsPerFTE", height="550px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ))
                     
              )),
      
      # Practice Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "profile",
              column(10,
                     div("Practice Overview", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_profile"),
                     column(8,
                            boxPlus(
                              title = "Volume", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                valueBoxOutput("uniquePts", width=3),
                                valueBoxOutput("totalVisits", width=3),
                                valueBoxOutput("avgVisitsPt", width=3),
                                valueBoxOutput("avgVisitsDay", width=3)),
                              plotOutput("avgPtArrival", height = "450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ),
                            boxPlus(
                              title = "Day of Visit", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                column(4,
                                       fluidRow(valueBoxOutput("avgCycleTime", width = 12)), 
                                       fluidRow(valueBoxOutput("medCycleTime", width = 12))),
                                column(8,
                                       plotOutput("cycleTimeBoxPlot") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")
                                       )), 
                              hr(),
                              fluidRow(
                                column(4,
                                       fluidRow(valueBoxOutput("avgCheckinToRoomin", width = 12)), 
                                       fluidRow(valueBoxOutput("medCheckinToRoomin", width = 12))),
                                column(8,
                                       plotOutput("checkInRoomInBoxPlot") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")
                                       )))
                     ),
                     column(4,
                            boxPlus(
                              title = "Access", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(infoBoxOutput("newPtRatio", width =12)),
                              fluidRow(infoBoxOutput("newApptWaitTime", width=12)),
                              fluidRow(infoBoxOutput("newNoShow", width=12))
                            ),
                            boxPlus(
                              title = "Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(plotOutput("fillRate", height = "350px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")),
                              fluidRow(plotOutput("apptStatus", height = "650px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c"))
                            ))
              )),
      
      # Provider Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "provider",
              column(10,
                     div("Provider Overview", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_profile"),
                     column(12,
                            boxPlus(
                              title = "Key Metrics", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                valueBoxOutput("vbox"),
                                valueBoxOutput("vbox2"),
                                valueBoxOutput("vbox3")),
                              fluidRow(
                                valueBoxOutput("vbox4"),
                                valueBoxOutput("vbox5"),
                                valueBoxOutput("vbox6")),
                              fluidRow(
                                valueBoxOutput("vbox7"),
                                valueBoxOutput("vbox8"),
                                valueBoxOutput("vbox9"))
                            )),
                     column(5,
                            boxPlus(
                              title = "Appointment Status", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provApptStatusPie", height = "420px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")),
                            boxPlus(
                              title = "Coverage and No Show", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provCoveragePie", height = "420px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"))),
                     column(7,
                            boxPlus(
                              title = "Daily Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              radioButtons("provSchedulingChoice", label = NULL, inline=T,
                                           choices = list("Arrived " = 1, "No Show " = 2, "Overbooks " = 3, "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                              plotOutput("provDailySchedule", height = "890px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              )),
                     column(12,
                            boxPlus(
                              title = "Slot Usage", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provSlotUsagesAvg") %>% 
                                withSpinner(type = 5, color = "#d80b8c")))
              )),
      
      tabItem(tabName = "KPIs",
              # KPIs Tab --------------------------------------------------------------------------------------------------------------------
              column(10,
                     div("KPIs", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_KPIs"),
                     column(2,
                            box(
                              title = NULL,
                              width = 12,
                              solidHeader = FALSE,
                              radioButtons("kpiTrend", label = h4("Compare KPIs by:"),
                                           choices = list("Historical Trend" = 1, "Seasonality" = 2), 
                                           selected = 1)),
                            box(
                              title = NULL,
                              width = 12,
                              solidHeader = FALSE,
                              radioButtons("kpiFreq", label = h4("Display KPIs by:"),
                                           choices = list("Year" = 1, "Quarter" = 2, "Month" = 3, "Day" = 4), 
                                           selected = 1))
                     ),
                     column(10,
                            boxPlus(
                              title = "Volume KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiVolumeGraph", height="450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                            ),
                            boxPlus(
                              title = "Scheduling KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiApptStatusGraph", height="800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                            ),
                            boxPlus(
                              title = "Access KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiNewWaitTimeGraph", height="450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                            ),
                            boxPlus(
                              title = "Day of Visit KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiCycleTimeGraph", height="450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), br(),
                              plotOutput("kpiWaitTimeGraph", height="450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                            )
                     )
                     
              )),
      
      # Population Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "population",
              column(10,
                     div("Population", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     column(12,
                            boxPlus(
                              title = "Patient Gender and Age Group", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                column(6, offset = 3, plotOutput("sex_breakdown", height = "300px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c"))),
                              fluidRow(
                                plotOutput("pop_breakdown", width = "100%", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c")))
                     ),
                     column(12,
                            boxPlus(
                              title = "Insurance Types", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              column(8,girafeOutput(outputId = "ins_breakdown", height = "600px")),
                              column(4, tableOutput("ins_breakdown_tb")))
                     ),
                     column(12,
                            boxPlus(
                              title = "Geographical Analysis", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              leafletOutput("population1", height = "800px")))
              )),
      
      # Volume Tab -----------------------------------------------------------------------------------------------------------
      tabItem(tabName = "volume",
              column(10,
                     div("Volume", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     boxPlus(
                       title = "Volume Over Time", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       highchartOutput("volume1")),
                     boxPlus(
                       title = "Monthly Volume", width =12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("volume2") %>% 
                         withSpinner(type = 5, color = "#d80b8c"),
                       plotOutput("volume4") %>% 
                         withSpinner(type = 5, color = "#d80b8c"),
                       tableOutput("volume4.1")),
                     boxPlus(
                       title = "Daily Volume", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       column(2,""),
                       column(8,
                              plotOutput("volume3") %>% 
                                withSpinner(type = 5, color = "#d80b8c"),
                              plotOutput("volume5") %>% 
                                withSpinner(type = 5, color = "#d80b8c"), 
                              tableOutput("volume5.1")),
                       column(2,))
              )),
      
      # Scheduling Tab -------------------------------------------------------------------------------------------------------
      tabItem(tabName = "arrived",
              column(10,
                     div("Scheduling | Scheduled/Arrived", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "Scheduled Patients", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("scheduledPts", height = "600px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"))
                     ),
                     fluidRow(
                       boxPlus(
                         title = "Arrived Patients", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("arrivedPts", height = "600px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"))
                     ))),
      
      tabItem(tabName = "noshows",
              column(10,
                     div("Scheduling | No Shows/Overbooks", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "No Show Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(
                           column(3, uiOutput("apptTypeControl")),
                           column(3, uiOutput("insuranceControl")),
                           column(3, valueBoxOutput("avgDailyNoShow_Count", width = 12)),
                           column(3, valueBoxOutput("avgDailyNoShow_Perc", width = 12))),
                         h5("No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")
                       ),
                       boxPlus(
                         title = "No Shows by Time of Day", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           column(6, plotOutput("avgNoShowCount", height="800px") %>% 
                                    withSpinner(type = 5, color = "#d80b8c")),
                           column(6, plotOutput("avgNoShowPercent", height = "800px") %>% 
                                    withSpinner(type = 5, color = "#d80b8c")))
                       ),
                       boxPlus(
                         title = "No Shows by Lead Days to Appointment", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         plotOutput("noShowLeadDays", height = "600px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"))
                     )
              )),
      
      tabItem(tabName = "cancellations",
              column(10,
                     div("Scheduling | Bumps/Cancellations", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     boxPlus(
                       title = "Bumped/Canceled/Recheduled Summary", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                       fluidRow(valueBoxOutput("totalBumpedCanceledRescheduledBox", width = 3),
                                valueBoxOutput("totalBumpedBox", width = 3), 
                                valueBoxOutput("totalCanceledBox", width = 3),
                                valueBoxOutput("totalRescheduledBox", width = 3)),
                       fluidRow(valueBoxOutput("avgDailyBumpedCanceledRescheduledBox", width = 3),
                                valueBoxOutput("avgDailyBumpedBox", width = 3), 
                                valueBoxOutput("avgDailyCanceledBox", width = 3),
                                valueBoxOutput("avgDailyRescheduledBox", width = 3))),
                     fluidRow(
                       column(5,
                              boxPlus(
                                title = "Same-Day Bumped/Canceled/Rescheduled", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("sameDayBumpedCanceledRescheduled", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"))),
                       column(7,
                              boxPlus(
                                title = "Top 10 Bumped Reasons", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("bumpedReasonsLeadDays", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c")))),
                     fluidRow(
                       column(5,
                              boxPlus(
                                title = "Bumped/Canceled/Rescheduled Lead Days", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("bumpedCanceledRescheduledLeadDays", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"))),
                       column(7,
                              boxPlus(
                                title = "Top 10 Canceled Reasons", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("canceledReasonsLeadDays", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"))))
              )),
      
      
      #        #textOutput("practiceName_utilization"),
      #        # fluidRow(
      #        #   boxPlus(
      #        #     title = "Cancellations", width = 12, status = "primary",
      #        #     solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #        #     column(6, plotOutput("canceledLeadDays")),
      #        #     column(6, "placeholder for cancellation reason graph"))
      #        # ),
      #        fluidRow(
      #          boxPlus(
      #            title = "Bumps", width = 12, status = "primary",
      #            solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #            column(6, plotOutput("bumpedLeadDays")),
      #            column(6, "placeholder for bumped reason graph"))
      #        )
      # )),
      
      # Utilization Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "utilization",
              column(10,
                     div("Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     column(12,
                            boxPlus(
                              title = "Utilization Overview", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              br(),
                              column(12,
                                     column(3, 
                                            box(title = NULL, width = 12, solidHeader = FALSE,
                                                radioGroupButtons(
                                                  inputId = "utilType",
                                                  label = h4("Analysis based on:"),
                                                  size = "lg",
                                                  choices = list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "arrived"),
                                                  checkIcon = list(
                                                    yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                    no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                                                hr(),
                                                h5("SCHEDULED: Utilization of all arrived appointments based on scheduled appointment start and end time."),
                                                h5("ACTUAL: Utilization of all arrived appointments based on actual appointment start and end time."))),
                                     column(9,
                                            fluidRow(
                                              column(4,
                                                     box(title = NULL, width = 12, solidHeader = FALSE,
                                                         sliderInput("setRooms", label = h4("Set Rooms Available:"), min = 1, max = 24, value = 8))),
                                              column(4,
                                                     valueBoxOutput("roomStat1", width=12)),
                                              # fluidRow(valueBoxOutput("roomStat1", width=12)),
                                              # fluidRow(valueBoxOutput("roomStat2", width=12))),
                                              column(4,
                                                     valueBoxOutput("avgRoomsRequired", width=12))),
                                            fluidRow(
                                              column(4,
                                                     box(title = NULL, width = 12, solidHeader = FALSE,
                                                         sliderInput("setHours", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                              column(4,
                                                     valueBoxOutput("avgScheduledUtilization", width=12)),
                                              column(4,
                                                     valueBoxOutput("avgUtilization", width=12)))))),
                            boxPlus(
                              title = "Space Utilization", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tabBox(
                                title = NULL,
                                id = "tabset1", width = "100%", height = "1000px",
                                tabPanel("Average",
                                         plotOutput("spaceUtil", height = "900px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c")),
                                tabPanel("Percentiles",
                                         plotOutput("spaceUtilPerc", height = "900px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c")))),
                            boxPlus(
                              title = "Space Required", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tabBox(
                                title = NULL,
                                id = "tabset2", width = "100%", height = "1000px",
                                tabPanel("Average",
                                         plotOutput("spaceUsed", height = "900px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c")),
                                tabPanel("Percentiles",
                                         plotOutput("spaceUsedPerc", height = "900px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c"))))
                            
                            
                            
                            
                            
                     )) 
      ),
      
      # Access Tab ------------------------------------------------------------------------------------------------------------
      # tabItem(tabName = "access"),
      tabItem(tabName = "newPatients",
              column(10,
                     div("Access | New Patients", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "New Patient Visit Ratio", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         tabBox(
                           title = NULL,
                           id = "tabset4", width = "100%",
                           tabPanel("Total", 
                                    plotOutput("newPtRatioByDept", height = "550px") %>% 
                                      withSpinner(type = 5, color = "#d80b8c")),
                           tabPanel("By Provider",
                                    "*Select Fewer Providers for Better Visibility",
                                    plotOutput("newPtRatioByProv", height = "550px") %>% 
                                      withSpinner(type = 5, color = "#d80b8c"))))),
                     fluidRow(
                       boxPlus(
                         title = "New Patient Wait Time", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         tabBox(
                           title = NULL,
                           id = "tabset5", width = "100%",
                           tabPanel("Total", 
                                    plotOutput("newPtWaitTimeByDept", height = "550px") %>% 
                                      withSpinner(type = 5, color = "#d80b8c")),
                           tabPanel("By Provider",
                                    "*Select Fewer Providers for Better Visibility",
                                    plotOutput("newPtWaitTimeByProv", height = "550px") %>% 
                                      withSpinner(type = 5, color = "#d80b8c"))))),
                     fluidRow(
                       boxPlus(
                         title = "New Patient Source", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("newPtApptSourceByDept", height = "550px") %>% 
                           withSpinner(type = 5, color = "#d80b8c")))
              )),
      
      # tabItem(tabName = "upcomingDemand",
      #         column(10,
      #                div("Access | Upcoming Demand", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
      #                tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
      #                #textOutput("practiceName_utilization"),
      #                fluidRow(
      #                  boxPlus(
      #                    title = "Daily Slot Booked Rate (%) - Upcoming 2 Weeks", width = 12, status = "primary",
      #                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                    materialSwitch(
      #                      inputId = "byProvider2",
      #                      label = "By Provider", 
      #                      right = TRUE,
      #                      status = "primary"),
      #                    #div(style='height:800px; overflow-y: scroll',  plotOutput("demandWeeksGraph", height = "1500px"))
      #                    plotOutput("demandWeeksGraph", height = "900px"),
      #                    br()
      #                    #tableOutput("slotUsageTb")
      #                  )),
      #                
      #                fluidRow(
      #                  boxPlus(
      #                    title = "Daily Slot Booked Rate (%) - Upcoming 3 Months", width = 12, status = "primary",
      #                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                    materialSwitch(
      #                      inputId = "byProvider3",
      #                      label = "By Provider", 
      #                      right = TRUE,
      #                      status = "primary"),
      #                    plotOutput("demandMonthsGraph", height = "900px"), class = 'top-align',
      #                    br()
      #                    #tableOutput("slotUsageTb")
      #                  ))
      #         )),
      
      
      tabItem(tabName = "slotUsage",
              column(10,
                     div("Access | Slot Usage", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "Booked vs. Filled Rate", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         radioButtons("slotUsageChoice", label = NULL, inline=T,
                                      choices = list("Available Hours " = 1, "Booked Hours " = 2, "Filled Hours " = 3, 
                                                     "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                         "*Select Fewer Providers for Better Visibility",
                         plotOutput("slotUsageGraph", height = "800px") %>% 
                           withSpinner(type = 5, color = "#d80b8c"),
                         br(),
                         materialSwitch(
                           inputId = "byProvider2",
                           label = "By Provider",
                           right = TRUE,
                           status = "primary"),
                         tableOutput("slotUsageTb")))
                     
              )),
      
      # tabItem(tabName = "slotUsage",
      #         column(10,
      #                div("Access | Slot Usage", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
      #                tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
      #                #textOutput("practiceName_utilization"),
      #                fluidRow(
      #                  boxPlus(
      #                    title = "Booked vs. Filled Rate", width = 12, status = "primary",
      #                    solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                    materialSwitch(
      #                      inputId = "byProvider1",
      #                      label = "By Provider", 
      #                      right = TRUE,
      #                      status = "primary"),
      #                    plotOutput("slotUsageGraph", height = "800px"),
      #                    br(),
      #                    tableOutput("slotUsageTb")))
      #                
      #         )),
      
      
      # Day of Visit Tab ------------------------------------------------------------------------------------------------------------
      
      tabItem(tabName = "cycleTime",
              column(10,
                     div("Check-in to Visit-end Time", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     column(12,
                            boxPlus(
                              title = "Cycle Time Summary", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              br(),
                              fluidRow(column(4, uiOutput("apptTypeControl2")),
                                       column(4, valueBoxOutput("cycleTimeCompNew", width = 12)),
                                       column(4, valueBoxOutput("cycleTimeCompOther", width = 12)))),
                            boxPlus(
                              title = "Cycle Time by Appointment Type", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(column(12, plotOutput("cycleTimeTrend", height = "600px") %>% 
                                                withSpinner(type = 5, color = "#d80b8c"))),
                              hr(),
                              fluidRow(
                                column(6, plotOutput("newCycleTimeBoxPlot", height = "500px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")),
                                column(6, plotOutput("establishedCycleTimeBoxPlot", height = "500px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")))),
                            boxPlus(
                              title = "Cycle Time by Provider and Appointment Type", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("newCycleTimeByProv", height = "800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"),
                              plotOutput("establishedCycleTimeByProv", height = "800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"))
                     ))
      ),
      
      tabItem(tabName = "roomInTime",
              column(10,
                     div("Check-in to Room-in Time", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     column(12,
                            boxPlus(
                              title = "Room-in Time Summary", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              br(),
                              fluidRow(column(4, uiOutput("apptTypeControl3")),
                                       column(4, valueBoxOutput("roomInTimeCompNew", width = 12)),
                                       column(4, valueBoxOutput("roomInTimeCompOther", width = 12)))),
                            boxPlus(
                              title = "Room-in Time by Appointment Type", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(column(12, plotOutput("roomInTimeTrend", height = "600px") %>% 
                                                withSpinner(type = 5, color = "#d80b8c"))),
                              hr(),
                              fluidRow(
                                column(6, plotOutput("newRoomInTimeBoxPlot", height = "500px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")),
                                column(6, plotOutput("establishedRoomInTimeBoxPlot", height = "500px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c")))),
                            boxPlus(
                              title = "Room-in Time by Provider and Appointment Type", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("newRoomInTimeByProv", height = "800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"),
                              plotOutput("establishedRoomInTimeByProv", height = "800px") %>% 
                                withSpinner(type = 5, color = "#d80b8c"))
                     ))
      ),
      
      # tabItem(tabName = "day",
      #         column(10,
      #                div("Patient Flow", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
      #                tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
      #                #textOutput("practiceName_utilization"),
      #                column(12,
      #                       boxPlus(
      #                         title = "Value vs. Non-Value Added Times", width = 12, status = "primary",
      #                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                         fluidRow(
      #                           valueBoxOutput("avgCycleTime2", width=4),
      #                           valueBoxOutput("avgValueAdded", width=4),
      #                           valueBoxOutput("avgNonValueAdded", width=4)),
      #                         fluidRow(
      #                           plotOutput("cycleTimeDis", height = "400px"))
      #                       ),
      #                       boxPlus(
      #                         title = "Paient Flow and Frequency", width = 12, status = "primary",
      #                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                         tabBox(
      #                           title = NULL,
      #                           id = "tabset2", width = "100%",
      #                           tabPanel("Count", 
      #                                    grVizOutput("vsm_freqCount")),
      #                           tabPanel("Percent",
      #                                    grVizOutput("vsm_freqPerc")))),
      #                       boxPlus(
      #                         title = "Paient Flow and Cycle Times", width = 12, status = "primary",
      #                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
      #                         tabBox(
      #                           title = NULL,
      #                           id = "tabset3", width = "100%",
      #                           tabPanel("Average", 
      #                                    grVizOutput("vsm_durAvg")),
      #                           tabPanel("Median",
      #                                    grVizOutput("vsm_durMed"))))
      #                       
      #                ))
      # ),
      
      # Data Tab ---------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "data",
              DT::dataTableOutput(outputId = "dTableAll"))
    ),
    
    
    # Conditional Filters ------------------------------------------------------------------------------------------------------
    
    conditionalPanel(
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
      
      column(2,
           fluidRow(
             column(2, offset = 1,
                dropdownButton(
                  circle = F,
                  inline = T,
                  icon = icon("download"), 
                  width = "300px",
                  size = "sm",
                 br(), br(),
                 box(
                   title = "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                   width = 12,
                   solidHeader = FALSE,
                   actionButton("download", "Download", width="200px"),
                   #bsTooltip("download", "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                             #"top", options = list(container = "body"))
                  )
                )
             ),
             column(2, offset = 1,
                    actionButton("download1",
                                 label = icon("download")),
                    bsTooltip("download1", "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                                "bottom", options = list(container = "body"))
                    
             )
          ),
            br(),
             box(
               title = "Select Campus:",
               width = 12,
               solidHeader = FALSE,
               pickerInput("selectedCampus",label=NULL,
                           choices=sort(unique(historical.data$Campus)),
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Campuses", 
                             dropupAuto = FALSE),
                           selected = "MSUS")),
             
             box(
               title = "Select Specialty:",
               width = 12,
               solidHeader = FALSE,
               pickerInput("selectedSpecialty",label=NULL,
                           choices=default_specialty,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1",
                             countSelectedText = "{0}/{1} Specialties",
                             dropupAuto = FALSE),
                           selected = default_specialty)),
             box(
               title = "Select Department:",
               width = 12,
               solidHeader = FALSE,
               pickerInput("selectedDepartment",label=NULL,
                           choices=default_departments,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1",
                             countSelectedText = "{0}/{1} Departments",
                             dropupAuto = FALSE),
                           selected = default_departments)),
             box(
               title = "Select Resource Type:",
               width = 12,
               solidHeader = FALSE,
               checkboxGroupButtons(
                 inputId = "selectedResource",
                 label = NULL, 
                 choices = c("Provider","Resource"),
                 justified = TRUE,
                 checkIcon = list(
                   yes = icon("ok", lib = "glyphicon")),
                 selected = c("Provider","Resource"))
             ),
             box(
               title = "Select Provider:",
               width = 12, 
               solidHeader = FALSE, 
               pickerInput("selectedProvider",label=NULL,
                           choices=default_provider,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Providers", 
                             dropupAuto = FALSE),
                           selected = default_provider)),
             box(
               title = "Select Visit Type:",
               width = 12,
               solidHeader = FALSE,
               pickerInput("selectedVisitMethod",label=NULL,
                           choices=sort(unique(historical.data$Visit.Method)),
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Visit Types", 
                             dropupAuto = FALSE),
                           selected = unique(historical.data$Visit.Method)))
      )),
    
    
    # conditionalPanel(
    #   condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
    #   input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
    #   input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='day'",
    #   column(2,
    #          box(
    #            title = "Select Visit Type:",
    #            width = 12,
    #            solidHeader = FALSE,
    #            pickerInput("selectedVisitMethod",label=NULL,
    #                        choices=sort(unique(historical.data$Visit.Method)),
    #                        multiple=TRUE,
    #                        options = pickerOptions(
    #                          liveSearch = TRUE,
    #                          actionsBox = TRUE,
    #                          dropupAuto = FALSE),
    #                        selected = unique(historical.data$Visit.Method)))
    #   )),
    
    
    conditionalPanel(
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='access' |
      input.sbm=='newPatients' | input.sbm=='slotUsage' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
      column(2,
             uiOutput("dateRangeControl"),
             uiOutput("daysOfWeekControl")
      )),
    
    conditionalPanel(
      condition = "input.sbm=='KPIs'",
      column(2,
             uiOutput("dateRangeControlKpi"),
             uiOutput("daysOfWeekControlKpi")
      )),
    
    conditionalPanel(
      condition = "input.sbm=='utilization'",
      column(2,
             uiOutput("dateRangeControlUtil"),
             uiOutput("daysOfWeekControlUtil")
      )),
    
    
    # Future Slot 
    conditionalPanel(
      condition = "input.sbm=='upcomingDemand'",
      column(2,
             uiOutput("dateRangeControlFutureSlot"),
             uiOutput("daysOfWeekControlFutureSlot")
      )),
    
    # # Past Slot 
    # conditionalPanel(
    #   condition = "input.sbm=='slotUsage'",
    #   column(2,
    #          uiOutput("dateRangeControlPastSlot"),
    #          uiOutput("daysOfWeekControlPastSlot")
    #   )),
    
    conditionalPanel(
      condition = "input.sbm=='KPIs' | input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | input.sbm=='slotUsage' |
      input.sbm=='newPatients' | input.sbm=='slotUsage' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
      column(2,
             box(
               title = "Select Holidays to Exclude:",
               width = 12,
               solidHeader = FALSE,
               pickerInput("excludeHolidays",label=NULL,
                           choices= unique(holid$holiday),
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             dropupAuto = FALSE),
                           selected = unique(holid$holiday)))
      ))
    
    
  ) # Close daashboardBody
) # Close DashboardPage

# Run ShinyApp ===============================================================================================
#shinyApp(ui, server)
