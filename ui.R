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

default_visit_method <- sort(unique(historical.data[
  historical.data$Campus %in% default_campus &
    historical.data$Campus.Specialty %in% default_specialty &
    historical.data$Department %in% default_departments & 
    historical.data$Resource %in% default_resource_type &
    historical.data$Provider %in% default_provider, "Visit.Method"]))

default_PRC_name <- sort(unique(historical.data[
  historical.data$Campus %in% default_campus &
    historical.data$Campus.Specialty %in% default_specialty &
    historical.data$Department %in% default_departments & 
    historical.data$Resource %in% default_resource_type &
    historical.data$Provider %in% default_provider &
    historical.data$Visit.Method %in% default_visit_method, "Appt.Type"]))

dateRangeKpi_start = min(kpi.arrived.data$Appt.DateYear) 
dateRangeKpi_end = max(kpi.arrived.data$Appt.DateYear)
dateRangeKpi_min = min(arrived.data$Appt.DateYear) 
dateRangeKpi_max = max(arrived.data$Appt.DateYear)


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
    #tags$head(tags$style(HTML('.content-wrapper { height: 6000px !important;}'))),
    
    width = 200,
    
    sidebarMenu(id = "sbm",
                menuItem("KPIs", tabName = "KPIs", icon = icon("tachometer-alt")),
                menuItem("Site Overview", tabName = "systemOverview", icon = icon("hospital-symbol"),
                         menuSubItem("Site Overview", tabName = "system"),
                         menuSubItem("Site Comparison", tabName = "systemComparison")),
                menuItem("Practice Overview", tabName = "profile", icon = icon("hospital")),
                menuItem("Provider Overview", tabName = "provider", icon = icon("user-md")),
                menuItem("Utilization", tabName = "utilization", icon = icon("percent"))
    ) # Close sidebarMenu
    
  ), # Close dashboardSidebar
  
  dashboardBody(
    # tags$head(tags$style(
    #   HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    # )),
    fluidPage(
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
              column(11,
                     div("Site Overview: Analysis by Campus", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_siteOverview"),
                     tags$head(tags$style("#practiceName_siteOverview{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                     fluidRow(
                       column(3,
                              boxPlus(
                                title = "Summary", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(infoBoxOutput("siteTotalPts", width =12) %>%
                                           withSpinner(type = 5, color = "#d80b8c")),
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
                             ))
                     # column(12, 
                     #        boxPlus(
                     #          title = "Working FTE", width = 12, status = "primary",
                     #          solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                     #          plotOutput("siteWorkingFTE", height="550px") %>% 
                     #            withSpinner(type = 5, color = "#d80b8c"), br(),
                     #          plotOutput("sitePtsPerFTE", height="550px") %>% 
                     #            withSpinner(type = 5, color = "#d80b8c")
                     #          ))
              )),
      
      # System Comparison Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "systemComparison",
              column(11,
                     div("Site Comparison: Analysis by Specialty", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_siteComp"),
                     tags$head(tags$style("#practiceName_siteComp{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
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
                                withSpinner(type = 5, color = "#d80b8c"))
                            # boxPlus(
                            #   title = "Working FTE", width = 12, status = "primary",
                            #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                            #   materialSwitch(
                            #     inputId = "bySpecialty7",
                            #     label = "By week", 
                            #     right = TRUE,
                            #     status = "primary"),
                            #   plotOutput("siteComparisonWorkingFTE", height="550px") %>% 
                            #     withSpinner(type = 5, color = "#d80b8c"), br(),
                            #   materialSwitch(
                            #     inputId = "bySpecialty8",
                            #     label = "By week", 
                            #     right = TRUE,
                            #     status = "primary"),
                            #   plotOutput("siteComparisonPtsPerFTE", height="550px") %>% 
                            #     withSpinner(type = 5, color = "#d80b8c")
                            #   )
                            )
                     
              )),
      
      # Practice Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "profile",
              column(11,
                     div("Practice Overview", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_practice"),
                     tags$head(tags$style("#practiceName_practice{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                     fluidRow(
                       column(12,
                              boxPlus(
                                title = "Volume", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(
                                  valueBoxOutput("uniquePts", width=3),
                                  valueBoxOutput("totalVisits", width=3),
                                  valueBoxOutput("avgVisitsPt", width=3),
                                  valueBoxOutput("avgVisitsDay", width=3)),
                                plotOutput("avgPtArrival", height = "500px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c")
                              ))
                     ),
                     fluidRow(
                       column(8, 
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
                                fluidRow(infoBoxOutput("newPtRatio", width =12) %>% 
                                           withSpinner(type = 5, color = "#d80b8c")),
                                fluidRow(infoBoxOutput("newApptWaitTime", width=12)),
                                fluidRow(infoBoxOutput("newNoShow", width=12))),
                              boxPlus(
                                title = "Scheduling", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(tableOutput("fillRate_tb") %>% 
                                           withSpinner(type = 5, color = "#d80b8c"))))))
      ), # Close Practice Overview Tab
                     
      
      # Provider Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "provider",
              column(11,
                     div("Provider Overview", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_provider"),
                     tags$head(tags$style("#practiceName_provider{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                     column(12,
                            boxPlus(
                              title = "Key Metrics", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                valueBoxOutput("vbox1") %>%
                                  withSpinner(type = 5, color = "#d80b8c"),
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
                            # boxPlus(
                            #   title = "Slot Usage", width = 12, status = "primary",
                            #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                            #   plotOutput("provSlotUsagesAvg", height = "300px") %>% 
                            #     withSpinner(type = 5, color = "#d80b8c")),
                            # boxPlus(
                            #   title = "Appointment Status", width = 12, status = "primary",
                            #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                            #   plotOutput("provApptStatusPie", height = "300px") %>% 
                            #     withSpinner(type = 5, color = "#d80b8c")),
                            boxPlus(
                              title = "Coverage and No Show", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              tableOutput("provCoverage") %>%
                                withSpinner(type = 5, color = "#d80b8c"))
                            ),
                     column(7,
                            boxPlus(
                              title = "Daily Scheduling", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              radioButtons("provSchedulingChoice", label = NULL, inline=T,
                                           choices = list("Arrived " = 1, "No Show " = 2, "Overbooks " = 3, "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                              plotOutput("provDailySchedule", height = "890px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                              ))
              )),
      
      tabItem(tabName = "KPIs",
              # KPIs Tab --------------------------------------------------------------------------------------------------------------------
              column(11,
                     div("KPIs", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_KPIs"),
                     tags$head(tags$style("#practiceName_KPIs{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                     column(2,
                            box(
                              title = NULL,
                              width = 12,
                              height = "200px",
                              solidHeader = FALSE,
                              radioButtons("kpiTrend", label = h4("Compare KPIs by:"),
                                           choices = list("Historical Trend" = 1, "Seasonality" = 2), 
                                           selected = 1)),
                            box(
                              title = NULL,
                              width = 12,
                              height = "200px",
                              solidHeader = FALSE,
                              radioButtons("kpiFreq", label = h4("Display KPIs by:"),
                                           choices = list("Year" = 1, "Quarter" = 2, "Month" = 3, "Day" = 4), 
                                           selected = 1))
                     ),
                     column(10,
                            boxPlus(
                              title = "Volume KPIs", width = 12, status = "primary", height = "500px",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("kpiVolumeGraph", height="450px") %>% 
                                withSpinner(type = 5, color = "#d80b8c")
                            ),
                            boxPlus(
                              title = "Scheduling KPIs", width = 12, status = "primary", height = "850px",
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
                              column(12,
                                plotOutput("kpiCycleTimeGraph", height="450px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"),hr(),
                                plotOutput("kpiWaitTimeGraph", height="450px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"))
                            )
                     )
                     
              )),
      # Utilization Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "utilization",
              column(11,
                     div("Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     textOutput("practiceName_utilization"),
                     tags$head(tags$style("#practiceName_utilization{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                     column(12,
                            boxPlus(
                              title = "Utilization Overview", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              br(),
                              column(12,
                                     column(3,
                                            util_choices <- list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "arrived"),
                                            box(title = NULL, width = 12, solidHeader = FALSE,
                                                radioGroupButtons(
                                                  inputId = "utilType",
                                                  label = h4("Analysis based on:"),
                                                  size = "lg",
                                                  choices = util_choices,
                                                  selected = util_choices[1],
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
                                                     valueBoxOutput("roomStat1", width=12) %>%
                                                       withSpinner(type = 5, color = "#d80b8c")),
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
      )
    ), #Close Tab Items

    
    
    # Conditional Filters ------------------------------------------------------------------------------------------------------
    # Formatting Buttons
    tags$head(tags$style(HTML("#dropdownbutton1 {color: #212070;}"))),
    tags$head(
      tags$style(HTML("
                  #download1 {
                    background: #fff;
                    color: #212070;
                    padding: 8px 15px;
                    font-size: 24px;
                    font-family: inherit;
                    height: 54px;
                    width: 54px;
                    line-height: 44px;
                    outline: none;
                    box-shadow: 0 2px 5px 0 rgba(0,0,0,.18), 0 1px 5px 0 rgba(0,0,0,.15);
                    border-radius: 50%;
                    border-color: transparent;}"))),
    br(),
    br(),
    br(),
    br(),
    dropdown(
    conditionalPanel(
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
            br(),
             box(
               title = "Select Campus:",
               width = 12,
               height = "100px",
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
               height = "100px",
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
               height = "100px",
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
               height = "100px",
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
               height = "100px",
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
               title = "Select Visit Method:",
               width = 12,
               height = "100px",
               solidHeader = FALSE,
               pickerInput("selectedVisitMethod",label=NULL,
                           choices=default_visit_method,
                           multiple=TRUE,
                           options = pickerOptions(
                             liveSearch = TRUE,
                             actionsBox = TRUE,
                             selectedTextFormat = "count > 1", 
                             countSelectedText = "{0}/{1} Visit Methods", 
                             dropupAuto = FALSE),
                           selected = default_visit_method)),
      box(
        title = "Select Visit Type:",
        width = 12,
        height = "100px",
        solidHeader = FALSE,
        pickerInput("selectedPRCName",label=NULL,
                    choices=default_PRC_name,
                    multiple=TRUE,
                    options = pickerOptions(
                      liveSearch = TRUE,
                      actionsBox = TRUE,
                      selectedTextFormat = "count > 1", 
                      countSelectedText = "{0}/{1} Visit Types", 
                      dropupAuto = FALSE),
                    selected = default_PRC_name))
      ),
    
    
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
             box(
               title = "Select Date Range:",
               width = 12, 
               height = "100px",
               solidHeader = FALSE, 
               dateRangeInput("dateRange", label = NULL,
                              start = dateRangeKpi_min, end = dateRangeKpi_max,
                              min = dateRangeKpi_min, max = dateRangeKpi_max)),
             box(
               title = "Select Days of Week:",
               width = 12, 
               solidHeader = FALSE, 
               selectInput("daysOfWeek",label = NULL,
                           choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                           multiple=TRUE, selectize=TRUE))
      ),
    
    conditionalPanel(
      condition = "input.sbm=='KPIs'",
             box(
               title = "Select Date Range:",
               width = 12, 
               solidHeader = FALSE, 
               dateRangeInput("dateRangeKpi", label = NULL,
                              start = dateRangeKpi_start, end = dateRangeKpi_end,
                              min = dateRangeKpi_min, max = dateRangeKpi_max)),
             box(
               title = "Select Days of Week:",
               width = 12, 
               solidHeader = FALSE, 
               selectInput("daysOfWeekKpi",label = NULL,
                           choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                           multiple=TRUE, selectize=TRUE))
      ),
    
    conditionalPanel(
      condition = "input.sbm=='utilization'",
             #uiOutput("dateRangeControlUtil"),
             #uiOutput("daysOfWeekControlUtil")
      box(
        title = "Select Date Range:",
        width = 12, 
        solidHeader = FALSE, 
        dateRangeInput("dateRangeUtil", label = NULL,
                       start = min(data.hour.arrived$Appt.DateYear), end = max(data.hour.arrived$Appt.DateYear),
                       min = min(arrived.data$Appt.DateYear), max = max(arrived.data$Appt.DateYear))),
      
      box(
        title = "Select Days of Week:",
        width = 12, 
        solidHeader = FALSE, 
        selectInput("daysOfWeekUtil",label = NULL,
                    choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                    multiple=TRUE, selectize=TRUE))
      ),
    
    
    # Future Slot 
    conditionalPanel(
      condition = "input.sbm=='upcomingDemand'",
             uiOutput("dateRangeControlFutureSlot"),
             uiOutput("daysOfWeekControlFutureSlot")
      ),
    
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
      ),
    style = "material-circle", size = "lg", right = TRUE, status = "default",
    icon = icon("filter"), width = "300px",
    tooltip = tooltipOptions(title = "Set additional filters for graphs/tables."),
    inputId = "dropdownbutton1"
    ), #Close dropdown
    
    conditionalPanel(
      condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='upcomingDemand' | input.sbm=='slotUsage' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
      
      # br(),
      # dropdown(
      #   box(
      #     title = "Change height:",
      #     width = 12,
      #     height = "150px",
      #     solidHeader = FALSE,
      #     sliderInput(
      #       inputId = "plotHeight",
      #       label = NULL,
      #       value = 650, min = 450, max = 2000,
      #       ticks = FALSE
      #     ),
      #     fluidRow(
      #       column(2, offset = 4,
      #              actionButton("resetheight", "Reset")
      #       )
      #     )
      #   ),
      #   # numericInput("height", "height", 300),
      # 
      #   style = "material-circle", size = "lg", right = TRUE, status = "default",
      #   icon = icon("gear"), width = "300px",
      # 
      #   tooltip = tooltipOptions(title = "Format graphs."),
      #   inputId = "dropdownheight"
      # 
      # ), # Close Drop Down Button
      br(),
      actionButton("download1",
                   label = icon("download")),
      bsTooltip("download1", "Download (PNG) current tab.",
                "right", options = list(container = "body")
      )
    )
    
    ) #Close Fluid Page
  ) # Close daashboardBody
)# Close DashboardPage

# Run ShinyApp ===============================================================================================
#shinyApp(ui, server)
