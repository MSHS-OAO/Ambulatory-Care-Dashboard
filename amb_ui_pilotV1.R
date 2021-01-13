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
                # menuItem("Day of Visit", tabName = "day", icon = icon("hand-holding-medical")),
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
                                plotOutput("siteSpecialties", height = "520px")))),
                     column(12, 
                            boxPlus(
                              title = "Appointment Wait Time", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "median1",
                                label = "Median", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteWaitTime", height="800px"))),
                     column(12, 
                            boxPlus(
                              title = "Working FTE", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("siteWorkingFTE", height="550px"), br(),
                              plotOutput("sitePtsPerFTE", height="550px")))
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
                              plotOutput("siteComparisonPts", height="550px")),
                            boxPlus(
                              title = "New Patients", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty2",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNewPtRatio", height="550px"), br(),
                              materialSwitch(
                                inputId = "bySpecialty3",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNewPtWaitTime", height="550px")),
                            boxPlus(
                              title = "Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty4",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonBookedRate", height="900px"), br(),
                              materialSwitch(
                                inputId = "bySpecialty5",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonNoShow", height="550px")),
                            boxPlus(
                              title = "Cycle Times", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty6",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonCycleTime", height="900px")),
                            boxPlus(
                              title = "Working FTE", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              materialSwitch(
                                inputId = "bySpecialty7",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonWorkingFTE", height="550px"), br(),
                              materialSwitch(
                                inputId = "bySpecialty8",
                                label = "By week", 
                                right = TRUE,
                                status = "primary"),
                              plotOutput("siteComparisonPtsPerFTE", height="550px")))
                     
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
                              plotOutput("avgPtArrival", height = "450px")
                            ),
                            boxPlus(
                              title = "Day of Visit", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(valueBoxOutput("avgCycleTime", width=6), 
                                       valueBoxOutput("avgCheckinToRoomin", width=6)), hr(),
                              fluidRow(valueBoxOutput("medCycleTime", width=6),
                                       valueBoxOutput("medCheckinToRoomin", width=6)))
                            # fluidRow(valueBoxOutput("avgProviderTime", width=6),
                            #          valueBoxOutput("avgCheckoutTime", width=6)))
                     ),
                     column(4,
                            boxPlus(
                              title = "Access", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(infoBoxOutput("newPtRatio", width =12)),
                              fluidRow(infoBoxOutput("thirdDays", width=12)),
                              fluidRow(infoBoxOutput("apptWaitTime", width=12))
                            ),
                            boxPlus(
                              title = "Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(plotOutput("fillRate", height = "200px")),
                              fluidRow(plotOutput("apptStatus", height = "350px"))
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
                              title = "Appt Status", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provApptStatusPie", height = "420px")),
                            boxPlus(
                              title = "Coverage and No Show", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provCoveragePie", height = "420px"))),
                     column(7,
                            boxPlus(
                              title = "Daily Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              radioButtons("provSchedulingChoice", label = NULL, inline=T,
                                           choices = list("Arrived " = 1, "No Show " = 2, "Overbooks " = 3, "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                              plotOutput("provDailySchedule", height = "890px"))),
                     column(12,
                            boxPlus(
                              title = "Slot Usage", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("provSlotUsagesAvg")))
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
                              plotOutput("kpiVolumeGraph", height="450px")
                            ),
                            boxPlus(
                              title = "Scheduling KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiApptStatusGraph", height="800px")
                            ),
                            boxPlus(
                              title = "Access KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiNewWaitTimeGraph", height="450px")
                            ),
                            boxPlus(
                              title = "Day of Visit KPIs", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiCycleTimeGraph", height="450px"), br(),
                              plotOutput("kpiWaitTimeGraph", height="450px")
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
                                column(6, offset = 3, plotOutput("sex_breakdown", height = "300px"))),
                              fluidRow(
                                plotOutput("pop_breakdown", width = "100%", height = "500px")))
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
                       plotOutput("volume2"),
                       plotOutput("volume4"),
                       tableOutput("volume4.1")),
                     boxPlus(
                       title = "Daily Volume", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       column(2,""),
                       column(8,
                              plotOutput("volume3"),
                              plotOutput("volume5"), 
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
                         plotOutput("scheduledPts", height = "500px"))
                     ),
                     fluidRow(
                       boxPlus(
                         title = "Arrived Patients", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("arrivedPts", height = "500px"))
                     ))),
      
      tabItem(tabName = "noshows",
              column(10,
                     div("Scheduling | No Shows/Overbooks", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "No Shows by Time of Day", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         fluidRow(
                           column(3, uiOutput("apptTypeControl")),
                           column(3, uiOutput("insuranceControl")),
                           column(3, valueBoxOutput("avgDailyNoShow_Count", width = 12)),
                           column(3, valueBoxOutput("avgDailyNoShow_Perc", width = 12))),
                         fluidRow(
                           column(6, plotOutput("avgNoShowCount", height="800px")),
                           column(6, plotOutput("avgNoShowPercent", height = "800px")))
                       ),
                       boxPlus(
                         title = "No Shows by Lead Days to Appointment", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         plotOutput("noShowLeadDays", height = "600px"))
                     )
              )),
      
      tabItem(tabName = "cancellations",
              column(10,
                     div("Scheduling | Bumps/Cancellations", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     fluidRow(
                       boxPlus(
                         title = "Total Bumped/Canceled/Recheduled", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE, br(),
                         valueBoxOutput("totalCanceledBumpedBox", width = 4), 
                         valueBoxOutput("totalCanceledBox", width = 4),
                         valueBoxOutput("totalBumpedBox", width = 4))),
                     column(5,
                            boxPlus(
                              title = "Bump/Cancellation Lead Days", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("canceledBumpedLeadDays", height = "980px"))),
                     column(7,
                            boxPlus(
                              title = "Top 10 Canceled Reasons", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("canceledReasonsLeadDays", height = "450px")),
                            boxPlus(
                              title = "Top 10 Bumped Reasons", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("bumpedReasonsLeadDays", height = "450px")))
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
                                                  choices = c("SCHEDULED time and duration", "ACTUAL time and duration"),
                                                  checkIcon = list(
                                                    yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                    no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))))),
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
                                         plotOutput("spaceUtil", height = "900px")),
                                tabPanel("Percentiles",
                                         plotOutput("spaceUtilPerc", height = "900px")))),
                            boxPlus(
                              title = "Space Required", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tabBox(
                                title = NULL,
                                id = "tabset2", width = "100%", height = "1000px",
                                tabPanel("Average",
                                         plotOutput("spaceUsed", height = "900px")),
                                tabPanel("Percentiles",
                                         plotOutput("spaceUsedPerc", height = "900px"))))
                            
                            
                            
                            
                            
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
                                    plotOutput("newPtRatioByDept", height = "550px")),
                           tabPanel("By Provider",
                                    "*Select Fewer Providers for Better Visibility",
                                    plotOutput("newPtRatioByProv", height = "550px"))))),
                     fluidRow(
                       boxPlus(
                         title = "New Patient Wait Time", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         tabBox(
                           title = NULL,
                           id = "tabset5", width = "100%",
                           tabPanel("Total", 
                                    plotOutput("newPtWaitTimeByDept", height = "550px")),
                           tabPanel("By Provider",
                                    "*Select Fewer Providers for Better Visibility",
                                    plotOutput("newPtWaitTimeByProv", height = "550px"))))),
                     fluidRow(
                       boxPlus(
                         title = "New Patient Source", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("newPtApptSourceByDept", height = "550px")))
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
                         plotOutput("slotUsageGraph", height = "800px"),
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
      tabItem(tabName = "day",
              column(10,
                     div("Patient Flow", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), hr(),
                     #textOutput("practiceName_utilization"),
                     column(12,
                            boxPlus(
                              title = "Value vs. Non-Value Added Times", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                valueBoxOutput("avgCycleTime2", width=4),
                                valueBoxOutput("avgValueAdded", width=4),
                                valueBoxOutput("avgNonValueAdded", width=4)),
                              fluidRow(
                                plotOutput("cycleTimeDis", height = "400px"))
                            ),
                            boxPlus(
                              title = "Paient Flow and Frequency", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tabBox(
                                title = NULL,
                                id = "tabset2", width = "100%",
                                tabPanel("Count", 
                                         grVizOutput("vsm_freqCount")),
                                tabPanel("Percent",
                                         grVizOutput("vsm_freqPerc")))),
                            boxPlus(
                              title = "Paient Flow and Cycle Times", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tabBox(
                                title = NULL,
                                id = "tabset3", width = "100%",
                                tabPanel("Average", 
                                         grVizOutput("vsm_durAvg")),
                                tabPanel("Median",
                                         grVizOutput("vsm_durMed"))))
                            
                     ))
      ),
      
      # Data Tab ---------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "data",
              DT::dataTableOutput(outputId = "dTableAll"))
    ),
    
    
    # Conditional Filters ------------------------------------------------------------------------------------------------------
    
    
    conditionalPanel(
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='day'",
      column(2,
             br(), br(),
             box(
               title = "Download",
               width = 12,
               solidHeader = FALSE,
               actionButton("download", "Download"),
               bsTooltip("download", "Creates a PNG file with all visible graphs on this page. Use the minimize or close buttons to hide unwanted graphs",
                         "right", options = list(container = "body"))
               
             ),
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
             uiOutput("specialtyControl"),
             uiOutput("departmentControl"),
             uiOutput("resourceControl"),
             uiOutput("providerControl"),
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
      input.sbm=='newPatients' | input.sbm=='slotUsage' | input.sbm=='day'",
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
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='day' | 
      input.sbm=='KPIs' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage'",
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

