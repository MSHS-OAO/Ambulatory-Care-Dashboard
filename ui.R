default_campus <- "MSUS"
default_campus_choices <- historical.data %>% select(CAMPUS) %>% mutate(CAMPUS = unique(CAMPUS)) %>% collect()
default_campus_choices <- sort(default_campus_choices$CAMPUS, na.last = T)

default_specialty_choices <-  historical.data %>% filter(CAMPUS %in% default_campus) %>% select( CAMPUS_SPECIALTY)  %>%
  mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
default_specialty_choices <- sort(default_specialty_choices$CAMPUS_SPECIALTY, na.last = T)

default_specialty <- "Allergy"


default_departments <-  historical.data %>% filter(CAMPUS %in% default_campus & 
                                                      CAMPUS_SPECIALTY %in% default_specialty) %>% select( DEPARTMENT)  %>%
  mutate(DEPARTMENT= unique(DEPARTMENT)) %>% collect()
default_departments <- sort(default_departments$DEPARTMENT, na.last = T)


default_resource_type <- c("Provider","Resource")

default_provider <-   historical.data %>% filter(CAMPUS %in% default_campus & 
                                                     CAMPUS_SPECIALTY %in% default_specialty& 
                                                     DEPARTMENT %in% default_departments ) %>% 
  select(PROVIDER)  %>% 
  mutate(PROVIDER= unique(PROVIDER)) %>% collect()
default_provider <- sort(default_provider$PROVIDER, na.last = T)


default_visit_method <-    historical.data %>% filter(CAMPUS %in% default_campus & 
                                                  CAMPUS_SPECIALTY %in% default_specialty & 
                                                  DEPARTMENT %in% default_departments &
                                                  PROVIDER %in% default_provider) %>% 
  select( VISIT_METHOD)  %>% 
  mutate(VISIT_METHOD= unique(VISIT_METHOD)) %>% collect()
default_visit_method <- sort(default_visit_method$VISIT_METHOD, na.last = T)



default_PRC_name <-  historical.data %>% filter(CAMPUS %in% default_campus & 
                                            CAMPUS_SPECIALTY %in% default_specialty & 
                                            DEPARTMENT %in% default_departments &
                                            PROVIDER %in% default_provider &
                                            VISIT_METHOD %in% default_visit_method) %>% 
  select(APPT_TYPE )  %>% 
  mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
default_PRC_name <- sort(default_PRC_name$APPT_TYPE, na.last = T) 


# util_date_start = min(utilization.data$Appt.DateYear)
# util_date_end = max(utilization.data$Appt.DateYear)

# util_date_start <- utilization.data %>% select(APPT_DATE_YEAR) %>% summarise(start = min(APPT_DATE_YEAR, na.rm = T)) %>% collect()
# util_date_start <- format(util_date_start$start, "%Y-%m-%d")
# 
# util_date_end <- utilization.data %>% select(APPT_DATE_YEAR) %>% summarise(start = max(APPT_DATE_YEAR, na.rm = T)) %>% collect()
# util_date_end <- format(util_date_end$start, "%Y-%m-%d")

 

dateRange_max <- max_date_arrived



# dateRange_min <- glue("Select min(APPT_DTTM) AS minDate FROM AMBULATORY_ACCESS WHERE APPT_STATUS = 'Arrived'")
# dateRange_min <- dbGetQuery(poolcon, dateRange_min)
# dateRange_min <- as.Date(dateRange_min$MINDATE, format="%Y-%m-%d")
dateRange_min <- "2021-01-01"
dateRange_min <- as.Date(dateRange_min, format="%Y-%m-%d")

util_date_start <- dateRange_min
util_date_end <- dateRange_max

dateRange_start <-  dateRange_min

dateRangeKpi_start = dateRange_min 
dateRangeKpi_end = dateRange_max
dateRangeKpi_min = dateRange_min
dateRangeKpi_max = dateRange_max

# dateRangeSlot_start <- glue("Select min(APPT_DTTM) AS minDate FROM AMBULATORY_SLOT")
# dateRangeSlot_start <- dbGetQuery(poolcon, dateRangeSlot_start)
# dateRangeSlot_start <- as.Date(dateRangeSlot_start$MINDATE, format="%Y-%m-%d")
dateRangeSlot_start <- dateRange_min
  
# dateRangeSlot_end <- glue("Select max(APPT_DTTM) AS maxDate FROM AMBULATORY_SLOT")
# dateRangeSlot_end <- dbGetQuery(poolcon, dateRangeSlot_end)
# dateRangeSlot_end <- as.Date(dateRangeSlot_end$MAXDATE, format="%Y-%m-%d")
dateRangeSlot_end <- dateRange_max
  
dateRangepop_max <- glue("Select max(APPT_DTTM) AS maxDate FROM AMBULATORY_POPULATION")
dateRangepop_max <- dbGetQuery(poolcon, dateRangepop_max)
dateRangepop_max <- as.Date(dateRangepop_max$MAXDATE, format="%Y-%m-%d")

# dateRangepop_min <- glue("Select min(APPT_DTTM) AS minDate FROM AMBULATORY_POPULATION")
# dateRangepop_min <- dbGetQuery(poolcon, dateRangepop_min)
# dateRangepop_min <- as.Date(dateRangepop_min$MINDATE, format="%Y-%m-%d")
dateRangepop_min <- dateRange_min


header <-   dashboardHeader(title = HTML("Ambulatory Analytics Tool"),
                            disable = FALSE,
                            titleWidth = 400,
                            tags$li(class = "dropdown", actionButton("download1",
                                                                     label = icon("download")
                            )
                            ),
                            
                            tags$li(class = "dropdown",
                                    dropdown(
                                      box(
                                        title = "Bookmark Current Page:",
                                        width = 12,
                                        height = "200px",
                                        solidHeader = FALSE,
                                        h5("For naming your bookmarks please follow: 'SITE_FIRSTNAME_LASTNAME_DESC'"),#, style = "font-size:12px;"), br(),
                                        textInput("filter_name", label = NULL),
                                        actionButton("save_filters", "CLICK TO SAVE", width = "80%")
                                      ), br(), br(), br(), br(), br(), br(), br(), br(),
                                      br(), br(),
                                      style = "material-circle", size = "lg", right = TRUE, status = "default",
                                      icon = icon("save"), width = "300px",
                                      inputId = "dropdownbutton4"
                                    )
                            ),

                            tags$li(class = "dropdown", dropdown(box(title = "Retrieve Previously Saved Bookmark:",
                                                                     width = 12,
                                                                     height = "100px",
                                                                     solidHeader = FALSE,
                                                                     pickerInput("filter_list", choices = NULL, multiple = TRUE,
                                                                                 selected = NULL, options = pickerOptions(maxOptions = 1)
                                                                     ),
                                                                     actionButton("update_filters1", "CLICK TO UPDATE", width = "80%")
                            ), br(), br(), br(), br(), br(), br(),
                            br(), br(),
                            # actionButton("remove_filters", "CLICK TO REMOVE", width = "80%"), br(), br(),
                            style = "material-circle", size = "lg", right = TRUE, status = "default",
                            icon = icon("star"), width = "300px",
                            tooltip = tooltipOptions(title = "Set additional filters for graphs/tables."),
                            inputId = "dropdownbutton3"
                            ),
                            )

                            )
                            
#)



header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <-  tags$a(href='https://peak.mountsinai.org/',
                                              tags$img(src='Sinai_logo_white.png',height='100%',width='30%'))



ui <- dashboardPage(
  
  ### UI start-----------------------------------------------------------
  
  header,
  
  dashboardSidebar(
    tags$head(tags$style(HTML("
    .bttn-material-circle.bttn-default {
      color: #fff:
      background: #212070:
    
    }
                    "))),
    
    tags$head(tags$style(HTML("
    .logo {
    image-rendering: -webkit-optimize-contrast;
    
    }
                    "))),
    #Customize dashboard color scheme: title bar = .logo & .navbar; side bar = .main-sidebar; background = .content-wrapper
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
                menuItem("Home", tabName = "homepage", icon= icon("home")),
                br(),
                #menuItem("Comparison", tabName = "comparison",
                tags$div("Key Metrics",
                         style= "
                 font-size: 20px;
                 text-align: center;
                 margin: 0;
                 background: rgba(255, 255, 255, 0);
                 color: #FFFFFF"),
                tags$hr(style="border-color: #FFFFFF; margin-top: 10px;"),
                menuItem("Access", tabName = "newPatients", icon = icon("plus-circle")),
                menuItem("Cycle Time", tabName = "cycleTime", icon = icon("stopwatch"),
                         menuSubItem("Cycle Time", tabName = "cycleTime"),
                         menuSubItem("Room-in Time", tabName = "roomInTime")
                ),
                menuItem("Population", tabName = "population", icon = icon("users")),
                menuItem("Scheduling", tabName = "scheduling", icon = icon("calendar-day"),
                         menuSubItem("Scheduled/Arrived", tabName = "arrived"),
                         menuSubItem("No Shows/Overbooks", tabName = "noshows"),
                         menuSubItem("Bumps/Cancellations", tabName = "cancellations")
                ),
                menuItem("Booked/Filled Rate", tabName = "slotManagement", icon = icon("tachometer-alt")),
                menuItem("Utilization", tabName = "utilization", icon = icon("percent")),
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar")),
                
                br(),
                tags$div("Analysis",
                         style= "
                 font-size: 20px;
                 text-align: center;
                 margin: 0;
                 background: rgba(255, 255, 255, 0);
                 color: #FFFFFF"),
                tags$hr(style="border-color: #FFFFFF; margin-top: 10px;"),
                menuItem("Comparison", tabName = "Comparison"),
                
                menuItem("Trending", tabName = "KPIs"),
                br(),
                tags$div("Operational Improvement",
                         style= "
                 font-size: 16px;
                 text-align: center;
                 margin: 0;
                 background: rgba(255, 255, 255, 0);
                 color: #FFFFFF"),
                tags$hr(style="border-color: #FFFFFF; margin-top: 10px;"),
                menuItem("Schedule Optimization", tabName = "optimization")
                
                
    ) # Close sidebarMenu
    
  ), # Close dashboardSidebar
  
  dashboardBody(
    ##Tabset pills html
    tags$style(HTML("
        .nav-tabs-custom > .nav > li[class=active] > a {
           background-color: #d80b8c;
           color: #FFF;
           border-top-color: #d80b8c;
        }")),
    
    tags$style(HTML("
        .nav-tabs-custom {
          font-size: 15px;
          border-top-color: #d80b8c;
        }")),
    
    
    # tags$head(tags$style(
    #   HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    # )),
    fluidPage(
      useShinyalert(),
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
      
      # box "status" color for Mount Sinai Grey
      tags$style(HTML("
    .box.box-solid.box-warning>.box-header {
    color:#000000;
    background:#e5e6e5
    }
    .box.box-solid.box-warning{
    border-bottom-color:#e5e6e5;
    border-left-color:#e5e6e5;
    border-right-color:#e5e6e5;
    border-top-color:#e5e6e5;
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
      
      
      tags$style(HTML("
    #update_filters {
    position: absolute;
    left: 25px
    
    }
                    ")),
      
      tags$style(HTML("
    #update_filters1 {
    position: absolute;
    left: 25px
    
    }
                    ")),
      tags$style(HTML("
    #remove_filters {
    position: absolute;
    left: 25px
    
    }
                    ")),
      
      tags$style(HTML("
    #save_filters {
    position: absolute;
    left: 25px
    
    }
                    ")),
      
      
      # tags$style(HTML("
      #                 #home_description {
      #                 background-color: #212070
      #                 }")
      #            ),
      
      # Top align plot outputs
      tags$head(tags$style(".top-align { vertical-align: top;}  ")),
      
      tabItems(
        tabItem(tabName = "comparison",
                h3("This tab is still being built")
        ),
        # HomePage ------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "homepage",
                column(12,
                       # setBackgroundImage(
                       #   src = "MS_RGB_Vrtl_test.png", shinydashboard = TRUE
                       # ),
                       div("Ambulatory Analytics Tool", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$div(id = "home_text",
                                HTML("<p>Version: 1.0 <br> Last Updated: 2/10/2022</p>")
                       ),
                       tags$head(tags$style("#home_text{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 15px; margin-top: -0.2em; margin-bottom: -4em; margin-left: 20px}")), 
                       br(), br(),
                       tags$image(src = "Sinai_logo_color.png", height = "200px", width = "300px", alt = "Something went wrong", deleteFiles = FALSE),
                       fluidRow(
                         column(4,
                                box(
                                  title = p("About this Tool", style = "font-size:34px; font-weight:bold"), width = 12,  height = "400px", status = "warning", solidHeader = TRUE,
                                  p("- Ambulatory Analytics Tool is a system-wide analytics tool that 
                                aims to provide analytical solutions to help", strong("drive data-driven decisions and operational improvements."), style = "font-size:22px"),
                                  p("- Ambulatory Analytics Tool provides", strong("current state assessment and historical trending"), "of key operational metrics.", style = "font-size:22px")
                                )),
                         
                         column(4,
                                tags$div(id = "home_data",
                                         box(
                                           title = p("Data Sources", style = "font-size:34px; font-weight:bold"), width = 12, height = "400px", status = "warning", solidHeader = TRUE,
                                           p("Ambulatory Analytics Tool is developed based on the following data from EPIC Clarity:", style = "font-size:22px; font-weight: bold"),
                                           p("1. ", strong("Scheduling Data"), " provides scheduling details on arrived, bumped, canceled, no show, and rescheduled appointments.", style = "font-size:22px"),
                                           p("2. ", strong("Slot Availability Data"), " provides slot level details inlcuding booked and filled hours and slots.", style = "font-size:22px"),
                                         ))),
                         
                         column(4,
                                tags$div(id = "home_mapping",
                                         box(
                                           title = p("Reference Files", style = "font-size:34px; font-weight:bold"), width = 12, height = "400px", status = "warning", solidHeader = TRUE,
                                           p("- Metrics Overview:", style = "font-size:22px; font-weight: bold"),
                                           a(href = "Mappings/Metrics_Overview.pdf",target='blank', 'Click to View', download = 'Ambulatory Analysis Tool - Metric Overview.pdf', style = "font-size:22px"),
                                           p("- Metric Definitions and Analysis Methodology:", style = "font-size:20px; font-weight: bold"),
                                           a(href = "Mappings/Analysis Methodology.xlsx",target='blank', 'Click to Download', download = 'Ambulatory Analysis Tool - Metric definitions.xlsx', style = "font-size:22px")
                                         )))
                       ),
                       
                       fluidRow(
                         column(6,
                                tags$div(id = "home_video",
                                         box(
                                           title = p("How to Use this Tool", style = "font-size:34px; font-weight:bold"), width = 12, status = "warning", solidHeader = TRUE,
                                           shiny::tags$video(src = "Quick_HowtoUse_Amb Care Tool_Intro.mp4", type = "video/mp4",  autoplay = 'autoplay',
                                                             controls = 'controls', width = "700px", height = "550px",
                                                             style="display: block; margin-left: auto; margin-right: auto"),
                                           p("For any technical issues, please reach out to OAO@mssm.edu.", 
                                             style = "font-size:18px; font-style: italic")))),
                         column(6,
                                tags$div(id = "home_use_case",
                                         box(
                                           title = p("Targeted Use", style = "font-size:34px; font-weight:bold"), width = 12, status = "warning", solidHeader = TRUE,
                                           p("Data-driven insights and findings obtained from this tool should be used in conjunction with the learnings from the Ambulatory Academy to achieve operational improvements.", 
                                             style = "font-size:20px; font-weight: bold; font-style: italic"),
                                           img(src = "Use_Case.png", width = "700px", style="display: block; margin-left: auto; margin-right: auto"))))
                       )
                       
                       
                )),
        
        # System Overview Tab ------------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "system",
                column(11,
                       div("Site Overview: Analysis by Campus", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       textOutput("practiceName_siteOverview"),
                       tags$head(tags$style("#practiceName_siteOverview{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")),
                       hr(),
                       fluidRow(
                         column(4,
                                boxPlus(
                                  title = "Summary", width = 12, status = "primary",
                                  solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                  fluidRow(infoBoxOutput("siteTotalPts", width =12) %>%
                                             withSpinner(type = 5, color = "#d80b8c")),
                                  fluidRow(infoBoxOutput("siteNoShowPts", width=12)),
                                  fluidRow(infoBoxOutput("siteNewPtRatio", width=12)),
                                  fluidRow(infoBoxOutput("siteTotalProvs", width=12)),
                                  fluidRow(infoBoxOutput("sitePtsPerProv", width=12)))),
                         column(8,
                                boxPlus(
                                  title = "Volume by Specialty", width = 12, status = "primary",
                                  solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                  plotOutput("siteSpecialties", height = "580px") %>%
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
                                plotOutput("siteComparisonPts", height="700px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Total Arrived Patients by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:180%; font-weight:bold"),
                                reactableOutput("siteComparisonPtsTb")
                              ),
                              boxPlus(
                                title = "Scheduling", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                materialSwitch(
                                  inputId = "bySpecialty4",
                                  label = "By week", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("siteComparisonBookedRate", height="1100px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Average Booked vs. Filled Rate (%) by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:180%; font-weight:bold"),
                                reactableOutput("siteComparisonBookedRateTb"), hr(),
                                materialSwitch(
                                  inputId = "bySpecialty5",
                                  label = "By week", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("siteComparisonNoShow", height="700px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Avg No Show Rate by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:180%; font-weight:bold"),
                                reactableOutput("siteComparisonNoShowTb")
                                #dataTableOutput("Testtable")
                              ),
                              boxPlus(
                                title = "New Patients", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                materialSwitch(
                                  inputId = "bySpecialty2",
                                  label = "By week", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("siteComparisonNewPtRatio", height="700px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("New Patient Ratio by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:180%; font-weight:bold"),
                                reactableOutput("siteComparisonNewPtRatioTb"), hr(),
                                materialSwitch(
                                  inputId = "bySpecialty3",
                                  label = "By week", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("siteComparisonNewPtWaitTime", height="700px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Median Wait Time to New Appointment by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:180%; font-weight:bold"),
                                reactableOutput("siteComparisonNewPtWaitTimeTb")
                              ),
                              boxPlus(
                                title = "Cycle Times", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                materialSwitch(
                                  inputId = "bySpecialty6",
                                  label = "By week", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("siteComparisonMedianCheckInCycleTime", height="700px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Median Check-in to Visit-end Time by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:150%; font-weight:bold"),
                                reactableOutput("siteComparisonMedianCheckInCycleTimeTb"), hr(),
                                plotOutput("siteComparisonMedianCycleTime", height="600px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"), br(),
                                div("Median Check-in to Room-in Time by Site and Specialty", style = "text-align: center; 
                                  background-color:white; color:black; font-size:150%; font-weight:bold"),
                                reactableOutput("siteComparisonMedianCycleTimeTb"))
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
                       ),
                       boxPlus(
                         title = "New Patient Access", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(infoBoxOutput("newPtRatio", width =4) %>% 
                                    withSpinner(type = 5, color = "#d80b8c"),
                                  infoBoxOutput("newApptWaitTime", width=4),
                                  infoBoxOutput("newNoShow", width=4))),
                       fluidRow(
                         column(6,
                                boxPlus(
                                  title = "Booked and Filled Rates", width = 12, status = "primary",
                                  solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                  plotOutput("pracBookedFilledRate") %>% 
                                    withSpinner(type = 5, color = "#d80b8c"))),
                         column(6,
                                boxPlus(
                                  title = "Scheduling", width = 12, status = "primary",
                                  solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                  plotOutput("pracApptStatus") %>% 
                                    withSpinner(type = 5, color = "#d80b8c")))),
                       boxPlus(
                         title = "Cycle Times", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           column(4,
                                  fluidRow(valueBoxOutput("avgCycleTime", width = 12)), 
                                  fluidRow(valueBoxOutput("medCycleTime", width = 12))),
                           column(8,
                                  plotOutput("cycleTimeBoxPlot") %>% 
                                    withSpinner(type = 5, color = "#d80b8c"))), hr(),
                         fluidRow(
                           column(4,
                                  fluidRow(valueBoxOutput("avgCheckinToRoomin", width = 12)), 
                                  fluidRow(valueBoxOutput("medCheckinToRoomin", width = 12))),
                           column(8,
                                  plotOutput("checkInRoomInBoxPlot") %>% 
                                    withSpinner(type = 5, color = "#d80b8c"))))
                )
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
                                  valueBoxOutput("vbox6")))),
                       column(6,
                              boxPlus(
                                title = "Scheduling Status", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(
                                  valueBoxOutput("provScheduledAppts", width = 6),
                                  valueBoxOutput("provIncompleteAppts", width = 6)),
                                hr(),
                                fluidRow(plotOutput("provNoShowPie", height = "200px") %>%
                                           withSpinner(type = 5, color = "#d80b8c")),
                                tags$em("*Incomplete Appts = No Show + Same-day Bumped/Canceled/Rescheduled Appts"),
                                br(), br()), 
                              boxPlus(
                                title = "Daily Scheduling", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                radioButtons("provSchedulingChoice", label = NULL, inline=T,
                                             choices = list("Arrived " = 1, "No Show " = 2, "Overbooks " = 3, "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                                plotOutput("provDailySchedule", height = "890px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c")
                              )),
                       column(6,
                              boxPlus(
                                title = "Bumps", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(
                                  valueBoxOutput("provBumps", width = 6),
                                  valueBoxOutput("provBumpsPerc", width = 6)),
                                hr(),
                                fluidRow(plotOutput("provBumpLeadDays", height = "200px") %>%
                                           withSpinner(type = 5, color = "#d80b8c")),
                                br(), br()), 
                              boxPlus(
                                title = "No Shows", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(
                                  valueBoxOutput("provNoShow", width = 6),
                                  valueBoxOutput("provNoShowPerc", width = 6)),
                                hr(),
                                fluidRow(tableOutput("provCoverage") %>%
                                           withSpinner(type = 5, color = "#d80b8c")),
                                tags$em("*No Show Rate = No Show / (Arrived + No Show)"))
                              
                              # # boxPlus(
                              # #   title = "Slot Usage", width = 12, status = "primary",
                              # #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              # #   plotOutput("provSlotUsagesAvg", height = "300px") %>% 
                              # #     withSpinner(type = 5, color = "#d80b8c")),
                              # boxPlus(
                              #   title = "Scheduling Activity", width = 12, status = "primary",
                              #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              #   plotOutput("provApptStatusPie", height = "300px") %>%
                              #     withSpinner(type = 5, color = "#d80b8c")),
                              # boxPlus(
                              #   title = "Coverage and No Show", width = 12, status = "primary",
                              #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              #   tableOutput("provCoverage") %>%
                              #     withSpinner(type = 5, color = "#d80b8c"))
                       )
                       
                       
                )),
        
        tabItem(tabName = "KPIs",
                # KPIs Tab --------------------------------------------------------------------------------------------------------------------
                column(11,
                       div("KPIs", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       textOutput("practiceName_KPIs"),
                       #textOutput("kpis_mem"),
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
                                title = "Cycle Time KPIs", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                column(12,
                                       plotOutput("kpiCycleTimeGraph", height="450px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c"),hr(),
                                       plotOutput("kpiWaitTimeGraph", height="450px") %>% 
                                         withSpinner(type = 5, color = "#d80b8c"))
                              )
                       )
                       
                )),
        # # Population Tab ------------------------------------------------------------------------------------------------------
        tabItem(tabName = "population",
                column(11,
                       div("Population", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("practiceName_population"),
                       tags$head(tags$style("#practiceName_population{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       # column(12,
                       #        boxPlus(
                       #          title = "Patient Gender and Age Group", width = 12, status = "primary",
                       #          solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       #          fluidRow(
                       #            column(6, offset = 3, plotOutput("sex_breakdown", height = "300px") %>%
                       #                     withSpinner(type = 5, color = "#d80b8c"))),
                       #          fluidRow(
                       #            plotOutput("pop_breakdown", width = "100%", height = "500px") %>%
                       #              withSpinner(type = 5, color = "#d80b8c")))
                       # ),
                       column(12,
                              boxPlus(
                                title = "Payer Mix", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                column(2),
                                column(8, tableOutput("ins_breakdown_tb") %>%
                                         withSpinner(type = 5, color = "#d80b8c")))
                       ),
                       column(12,
                              boxPlus(
                                title = "Geographical Analysis", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                fluidRow(
                                  column(4, tableOutput("zipCode_tb") %>%
                                           withSpinner(type = 5, color = "#d80b8c")),
                                  column(8, leafletOutput("population1", height = "800px") %>%
                                           withSpinner(type = 5, color = "#d80b8c")))))
                )),
        
        # Volume Tab -----------------------------------------------------------------------------------------------------------
        tabItem(tabName = "volume",
                column(11,
                       div("Volume", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("practiceName_volume"),
                       tags$head(tags$style("#practiceName_volume{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       #profvis_ui("profiler"),
                       # fluidRow(
                       #   column(4,
                       #          box(
                       #            title = "Select Visit Type:",
                       #            width = 12,
                       #            height = "110px",
                       #            solidHeader = FALSE, 
                       #            radioGroupButtons(
                       #              inputId = "annualVolSummary",
                       #              label = NULL,
                       #              choices =c("Total", "IN PERSON", "VIDEO_TELEHEALTH VISITS", "TELEPHONE VISITS"),
                       #              selected = c("Total"),
                       #              justified = TRUE,
                       #              checkIcon = list(yes = icon("ok", lib = "glyphicon"))
                       #            ))
                       #   )),
                       # fluidRow(
                       #   column(12, offset = 1,
                       #          tableOutput("trend_visitstable"))
                       # ),
                       boxPlus(
                         title = "Monthly Volume", width =12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotOutput("volume2", height = '500px') %>%
                           withSpinner(type = 5, color = "#d80b8c"),
                         plotOutput("volume4", height = '500px') %>%
                           withSpinner(type = 5, color = "#d80b8c")),
                       #tableOutput("volume4.1")),
                       boxPlus(
                         title = "Daily Volume", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(2,""),
                         column(8,
                                plotOutput("volume3") %>%
                                  withSpinner(type = 5, color = "#d80b8c"),
                                plotOutput("volume5") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         #tableOutput("volume5.1")),
                         column(2,)),
                       boxPlus(
                         title = "Volume Over Time", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         plotlyOutput("volume1", height = 700) %>%
                           withSpinner(type = 5, color = "#d80b8c"))
                )),
        # Scheduling Tab -------------------------------------------------------------------------------------------------------
        tabItem(tabName = "arrived",
                column(11,
                       div("Scheduling | Scheduled/Arrived", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("scheduled_arrived"),
                       tags$head(tags$style("#scheduled_arrived{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       fluidRow(
                         boxPlus(
                           title = "Scheduling Summary", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           column(3,
                                  br(),
                                  valueBoxOutput("scheduledAppts", width = 12),
                                  valueBoxOutput("incompleteAppts", width = 12),
                                  tags$em("*Incomplete Appts = No Show + Same-day Bumped/Canceled/Rescheduled Appts")),
                           column(9,
                                  plotOutput("schedulingStatusSummary") %>% 
                                    withSpinner(type = 5, color = "#d80b8c")))
                       ),
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
                           plotOutput("arrivedPts", height = "700px") %>% 
                             withSpinner(type = 5, color = "#d80b8c"))
                       ))),
        
        tabItem(tabName = "noshows",
                column(11,
                       div("Scheduling | No Shows/Overbooks", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("scheduled_noshow"),
                       tags$head(tags$style("#scheduled_noshow{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       fluidRow(
                         boxPlus(
                           title = "No Show Summary", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           br(),
                           fluidRow(
                             column(3, uiOutput("apptTypeControl")),
                             column(3, uiOutput("insuranceControl")),
                             column(3, valueBoxOutput("avgDailyNoShow_Count", width = 12) %>%
                                      withSpinner(type = 5, color = "#d80b8c")),
                             column(3, valueBoxOutput("avgDailyNoShow_Perc", width = 12))),
                           tags$em("*No Show Rate = No Show / (Arrived + No Show)")
                           # h5("No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")
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
                           title = "No Shows by Wait Time to Appointment", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           br(),
                           plotOutput("noShowLeadDays", height = "600px") %>% 
                             withSpinner(type = 5, color = "#d80b8c"))
                       )
                )),
        
        ## Bumps/Cancellations Tab 
        tabItem(tabName = "cancellations",
                column(11,
                       div("Scheduling | Bumps/Cancellations", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("scheduled_cancel"),
                       tags$head(tags$style("#scheduled_cancel{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}"))), hr(),
                column(11,
                       boxPlus(
                         title = "Summary", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         fluidRow(
                           valueBoxOutput("avgDailyBumpedBox", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgDailyCanceledBox", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c"),
                           valueBoxOutput("avgDailyRescheduledBox", width = 4) %>%
                             withSpinner(type = 5, color = "#d80b8c")), hr(),
                         column(4, 
                                plotOutput("avgBumpsCancRescRate", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(4,
                                plotOutput("leadDaysBumpsCancResc", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(4,
                                plotOutput("sameDayBumpedCanceledRescheduled", height = "450px") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))),
                       boxPlus(
                         title = "Top Reasons to Bumps and Cancellations", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         br(),
                         materialSwitch(
                           inputId = "percent",
                           label = "Percent (%)", 
                           right = TRUE,
                           status = "primary"), br(),
                         column(6, 
                                plotOutput("reasonsBumps", height = "700px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                         column(6, 
                                plotOutput("reasonsCanc", height = "700px") %>%
                                  withSpinner(type = 5, color = "#d80b8c"))
                         
                       )
                )
                
        ), # Close Bumps/Cancellations Tab
        
        
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
                                              #util_choices <- list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "arrived"),
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  radioGroupButtons(
                                                    inputId = "utilType",
                                                    label = h4("Analysis based on:"),
                                                    size = "lg",
                                                    choices = list("SCHEDULED time and duration" = "scheduled", "ACTUAL time and duration" = "actual"),
                                                    # choices = util_choices,
                                                    # selected = util_choices[1],
                                                    checkIcon = list(
                                                      yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                      no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                                                  hr(),
                                                  h5("SCHEDULED: Utilization of all arrived appointments based on scheduled appointment start and end time."),
                                                  h5("ACTUAL: Utilization of all arrived appointments based on actual appointment start and end time."),
                                                  h5("Scheduled time includes an adjustment factor of 20% (every visit duration increased by 20%) to account for room turnover times and any non-physician contact time spent in rooms."))),
                                       column(3,
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  sliderInput("setRooms", label = h4("Set Rooms Available:"), min = 1, max = 24, value = 8)),
                                              box(title = NULL, width = 12, solidHeader = FALSE,
                                                  sliderInput("setHours", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                       column(6,
                                              tableOutput("roomStat1")%>% 
                                                withSpinner(type = 5, color = "#d80b8c"))
                                              
                                              
                                              # valueBoxOutput("roomStat1", width=12) %>%
                                              #   withSpinner(type = 5, color = "#d80b8c"),
                                              #   valueBoxOutput("avgUtilization", width=12),
                                              #   valueBoxOutput("maxUtilization", width=12),
                                              #   valueBoxOutput("maxRoomsRequired", width=12))
                                       
                                       
                                       # column(9,
                                       #        fluidRow(
                                       #          column(4,
                                       #                 box(title = NULL, width = 12, solidHeader = FALSE,
                                       #                     sliderInput("setRooms", label = h4("Set Rooms Available:"), min = 1, max = 24, value = 8))),
                                       #          column(4,
                                       #                 valueBoxOutput("roomStat1", width=12) %>%
                                       #                   withSpinner(type = 5, color = "#d80b8c")),
                                       #          column(4,
                                       #                 valueBoxOutput("avgRoomsRequired", width=12))),
                                       #        fluidRow(
                                       #          column(4,
                                       #                 box(title = NULL, width = 12, solidHeader = FALSE,
                                       #                     sliderInput("setHours", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                       #          column(4,
                                       #                 valueBoxOutput("avgScheduledUtilization", width=12)),
                                       #          column(4,
                                       #                 valueBoxOutput("avgUtilization", width=12))))
                                       
                                )),
                              boxPlus(
                                title = "Space Utilization", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                tabBox(
                                  title = NULL, type = "pills",
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
                                  title = NULL, type = "pills",
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
                column(11,
                       div("Access | New Patients", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("newpatients"),
                       tags$head(tags$style("#newpatients{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),                    
                       fluidRow(
                         boxPlus(
                           title = "New Patient Visit Ratio", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           tabBox(
                             title = NULL, type = "pills",
                             id = "tabset4", width = "100%",
                             tabPanel("Total", 
                                      plotOutput("newPtRatioByDept", height = "550px") %>% 
                                        withSpinner(type = 5, color = "#d80b8c")),
                             tabPanel("By Provider",
                                      "*Select fewer providers for better visibility",
                                      plotOutput("newPtRatioByProv", height = "550px") %>% 
                                        withSpinner(type = 5, color = "#d80b8c"))))),
                       fluidRow(
                         boxPlus(
                           title = "New Patient Wait Time", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           tabBox(
                             title = NULL, type = "pills",
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
        
        
        tabItem(tabName = "slotManagement",
                column(11,
                       div("Access | Slot Management", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("slot_usage"),
                       tags$head(tags$style("#slot_usage{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),                    
                       fluidRow(
                         boxPlus(
                           title = "Slot Management", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           materialSwitch(
                             inputId = "byRate",
                             label = "By Rate",
                             right = TRUE,
                             status = "primary"),
                           plotlyOutput("slotManageGraph", height = "800px") %>%
                             withSpinner(type = 5, color = "#d80b8c"))),
                       fluidRow(
                         # boxPlus(
                         #   title = "Booked vs. Filled Rate", width = 12, status = "primary",
                         #   solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         # radioButtons("slotUsageChoice", label = NULL, inline=T,
                         #              choices = list("Available Hours " = 1, "Booked Hours " = 2, "Filled Hours " = 3, 
                         #                             "Booked Rate (%) " = 4, "Filled Rate (%) " = 5), selected = 1),
                         # h3("*Select fewer providers for better visibility"),
                         # plotOutput("slotUsageGraph", height = "800px") %>% 
                         #   withSpinner(type = 5, color = "#d80b8c"),
                         # br(),
                         boxPlus(
                           title = "Slot Management Summary Table", width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                           materialSwitch(
                             inputId = "byProvider2",
                             label = "By Provider",
                             right = TRUE,
                             status = "primary"),
                           br(),
                           DT::dataTableOutput("slotUsageTb") %>% 
                             withSpinner(type = 5, color = "#d80b8c")))
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
                column(11,
                       div("Check-in to Visit-end Time", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("cycle_time"),
                       tags$head(tags$style("#cycle_time{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),                    
                       column(12,
                              boxPlus(
                                title = "Cycle Time Summary", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                "*Visit-end Time is the minimum of Visit-end Time and Check-out",
                                br(),
                                fluidRow(column(4, uiOutput("apptTypeControl2")),
                                         column(4, valueBoxOutput("cycleTimeCompNew", width = 12) %>%
                                                  withSpinner(type = 5, color = "#d80b8c")),
                                         column(4, valueBoxOutput("cycleTimeCompOther", width = 12)))),
                              boxPlus(
                                title = "Cycle Time by Visit Type", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                "*Select fewer visit types for better visibility",
                                br(),
                                fluidRow(
                                  column(6, plotOutput("newCycleTimeBoxPlot", height = "500px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c")),
                                  column(6, plotOutput("establishedCycleTimeBoxPlot", height = "500px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c"))),
                                hr(),
                                fluidRow(column(12, plotOutput("cycleTimeTrend", height = "600px") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")))
                                ),
                              boxPlus(
                                title = "Cycle Time by Time of Day", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                materialSwitch(
                                  inputId = "median2",
                                  label = "Median", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("cycleTimeByHour", height = "700px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                              boxPlus(
                                title = "Cycle Time by Provider and Visit Type", width = 12, status = "primary", enable_dropdown = TRUE, dropdown_menu = dropdownItemList(dropdownItem(url = "https://mtsinai.sharepoint.com/:i:/s/MSHSAmbulatoryCareAnalyticsTool/EUcZvHOZbixGl0-bweS36zsBpwy3yX0b7NTpKeTH3yb7DQ?e=Yvff2b", name = "Reading a Boxplot")), dropdown_icon = "question",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                "*Select fewer providers for better visibility",
                                plotOutput("newCycleTimeByProv", height = "800px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"),
                                plotOutput("establishedCycleTimeByProv", height = "800px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"))
                       ))
        ),
        
        tabItem(tabName = "roomInTime",
                column(11,
                       div("Check-in to Room-in Time", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       textOutput("room_time"),
                       tags$head(tags$style("#room_time{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),                    
                       column(12,
                              boxPlus(
                                title = "Room-in Time Summary", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                br(),
                                fluidRow(column(4, uiOutput("apptTypeControl3")),
                                         column(4, valueBoxOutput("roomInTimeCompNew", width = 12) %>%
                                                  withSpinner(type = 5, color = "#d80b8c")),
                                         column(4, valueBoxOutput("roomInTimeCompOther", width = 12)))),
                              boxPlus(
                                title = "Room-in Time by Visit Type", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                "*Select fewer visit types for better visibility",
                                br(),
                                fluidRow(
                                  column(6, plotOutput("newRoomInTimeBoxPlot", height = "500px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c")),
                                  column(6, plotOutput("establishedRoomInTimeBoxPlot", height = "500px") %>% 
                                           withSpinner(type = 5, color = "#d80b8c"))),
                                hr(),
                                fluidRow(column(12, plotOutput("roomInTimeTrend", height = "600px") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")))
                                ),
                              boxPlus(
                                title = "Room-in Time by Time of Day", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                materialSwitch(
                                  inputId = "median3",
                                  label = "Median", 
                                  right = TRUE,
                                  status = "primary"),
                                plotOutput("roomInTimeByHour", height = "700px") %>%
                                  withSpinner(type = 5, color = "#d80b8c")),
                              boxPlus(
                                title = "Room-in Time by Provider and Visit Type", width = 12, status = "primary", enable_dropdown = TRUE, dropdown_menu = dropdownItemList(dropdownItem(url = "https://mtsinai.sharepoint.com/:i:/s/MSHSAmbulatoryCareAnalyticsTool/EUcZvHOZbixGl0-bweS36zsBpwy3yX0b7NTpKeTH3yb7DQ?e=Yvff2b", name = "Reading a Boxplot")), dropdown_icon = "question",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                "*Select fewer providers for better visibility",
                                plotOutput("newRoomInTimeByProv", height = "800px") %>% 
                                  withSpinner(type = 5, color = "#d80b8c"),
                                plotOutput("establishedRoomInTimeByProv", height = "650px") %>% 
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
                div("Data", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"), 
                hr(),
                column(11,
                       DT::dataTableOutput(outputId = "dTableAll") %>%
                         withSpinner(type = 5, color = "#d80b8c")
                )
        ),
        
        # Comparison ---------------------------------------------------------------------------------------------------------------
        
        tabItem(tabName = "Comparison",
                column(11,
                       div("Comparison", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       textOutput("practiceName_vol_comp"),
                       #textOutput("kpis_mem"),
                       tags$head(tags$style("#practiceName_vol_comp{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       
                       
                       column(2,
                              box(
                                title = NULL,
                                width = 12,
                                height = "200px",
                                solidHeader = FALSE,
                                radioButtons("compare_filters", label = h4("Compare by:"),
                                             # choices = list("Provider" = "Provider", "Specialty" = "Campus.Specialty", "Department" = "Department"),
                                             choices = list("Specialty" = "CAMPUS_SPECIALTY", "Department" = "DEPARTMENT", "Provider" = "PROVIDER"),
                                             selected = "CAMPUS_SPECIALTY")),
                              box(
                                title = NULL,
                                width = 12,
                                height = "200px",
                                solidHeader = FALSE,
                                radioButtons("breakdown_filters", label = h4("Breakdown by:"),
                                             choices = list("Visit Method" = "VISIT_METHOD", "Visit Type" = "APPT_TYPE",  "New vs. Established Patients*" = "NEW_PT2"),
                                             selected = "VISIT_METHOD"),
                                h6("*New Patients defined by CPT codes"))
                              
                       ),
                       
                       column(10,
                              tabBox(title = NULL, id = "tabset7", width = "100%", type = 'pills',      
                                     tabPanel("Volume",
                                              boxPlus(
                                                title = "Volume Comparison", width = 12, status = "primary", 
                                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                                                h3(uiOutput(("vol_day_title"))),
                                                DTOutput("volume_comparison_tb_day") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c"),
                                                hr(),
                                                h3(uiOutput(("vol_month_title"))),
                                                DTOutput("volume_comparison_tb_month") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")
                                              )
                                     ),
                                     tabPanel("New Patient Ratio",
                                              boxPlus(
                                                title = "New Patient Ratio", width = 12, status = "primary", 
                                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                                # h2(uiOutput(("npr_day_title"))),
                                                # br(),
                                                # DTOutput("new_patient_ratio_day") %>% 
                                                #   withSpinner(type = 5, color = "#d80b8c"),
                                                h3(uiOutput(("npr_month_title"))),
                                                DTOutput("new_patient_ratio_month") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")
                                              )
                                     ),
                                     tabPanel("New Patient Wait Time",
                                              boxPlus(
                                                title = "New Patient Wait Time", width = 12, status = "primary", 
                                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                                # h2(uiOutput(("new_wait_day_title"))),
                                                # br(),
                                                # DTOutput("new_patient_lead_time_day") %>% 
                                                #   withSpinner(type = 5, color = "#d80b8c"),
                                                h3(uiOutput(("new_wait_month_title"))),
                                                DTOutput("new_patient_lead_time_month") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")
                                              )
                                     ),
                                     # tabPanel("No Show %",
                                     #          boxPlus(
                                     #            title = "No Show %", width = 12, status = "primary", 
                                     #            solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                     #            h3(uiOutput(("no_show_comp_title_daily"))),
                                     #            DTOutput("no_show_comp_daily") %>%
                                     #              withSpinner(type = 5, color = "#d80b8c"),
                                     #            hr(),
                                     #            h3(uiOutput(("no_show_comp_title"))),
                                     #            DTOutput("no_show_comp_month") %>% 
                                     #              withSpinner(type = 5, color = "#d80b8c")
                                     #          )
                                     #),
                                     tabPanel("Booked/Filled Rate",
                                              boxPlus(
                                                title = "Filled Rate", width = 12, status = "primary", 
                                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                                                h3(uiOutput(("new_patient_fill_rate_day_title"))),
                                                DTOutput("new_patient_fill_rate_day") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c"),
                                                hr(),
                                                h3(uiOutput(("new_patient_fill_rate_month_title"))),
                                                DTOutput("new_patient_fill_rate_month") %>% 
                                                  withSpinner(type = 5, color = "#d80b8c")
                                              )
                                     )
                              )
                              
                       )
                )
        ),
        
        tabItem(tabName = "optimization",
                column(11,
                       div("Scheduling Template Optimization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       textOutput("practiceName_opt_comp"),
                       #textOutput("kpis_mem"),
                       tags$head(tags$style("#practiceName_opt_comp{color:#7f7f7f; font-family:Calibri; font-style: italic; font-size: 22px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 20px}")), hr(),
                       
                       
                       column(2,
                              box(
                                title = NULL,
                                width = 12,
                                height = "200px",
                                solidHeader = FALSE,
                                radioButtons("compare_filters_opt", label = h4("Compare by:"),
                                             choices = list("Specialty" = "Campus.Specialty", "Department" = "Department", "Provider" = "Provider"),
                                             selected = "Campus.Specialty")),
                              
                              
                       ),
                       
                       column(10,
                              box(title = NULL, id = "tabset8", width = "100%", type = 'pills',      
                                     boxPlus(
                                       title = "Metrics Comparison", width = 12, status = "primary", 
                                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE, 
                                       #h3(uiOutput(("opt_day_title"))),
                                       #DTOutput("opt_comparison_tb") %>% 
                                       tableOutput("opt_comparison_tb_kable") %>%
                                         withSpinner(type = 5, color = "#d80b8c")
                                       #hr()
                                    
                                       
                                     )
                                     
                                     
                              )
                       )
                ) # close column
                
        ) # close tabItem
        
        
      ), #Close Tab Items
      
      
      tags$head(tags$style(HTML("#dropdownheight {color: #212070;}"))),
      tags$head(tags$style(HTML("#dropdownheightkpi {color: #212070;}"))),
      # Conditional Filters ------------------------------------------------------------------------------------------------------
      conditionalPanel(
        condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows' | input.sbm=='utilization' | input.sbm=='access' | input.sbm=='cancellations' | 
      input.sbm=='newPatients' | input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime' | input.sbm=='data' | input.sbm == 'Comparison'| input.sbm == 'optimization'",
        
        # Formatting Buttons
        tags$head(tags$style(HTML("#dropdownboxplot {color: #fff;}"))),
        tags$head(tags$style(HTML("#dropdownbutton1 {color: #fff;}"))),
        tags$head(tags$style(HTML("#dropdownbutton3 {color: #fff;
                                                   background-color: #212070;
                                                   box-shadow: none;
                                                }"))),

        tags$head(tags$style(HTML("#dropdownbutton4 {color: #fff;
                                                   background-color: #212070;
                                                   box-shadow: none;
                                                }"))),
        
        tags$head(tags$style(HTML(".bttn-material-circle.bttn-default {background-color: #212070}"))),
        tags$head(
          tags$style(HTML("
                  #download1 {
                    background: #212070;
                    color: #fff;
                    padding: 8px 15px;
                    font-size: 24px;
                    font-family: inherit;
                    height: 54px;
                    width: 54px;
                    line-height: 44px;
                    outline: none;
                    box-shadow: none;
                    border-radius: 50%;
                    border-color: transparent;}"))),
        tags$head(tags$style(HTML("#update_filters {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px}"))),
        tags$head(tags$style(HTML("#update_filters1 {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px}"))),
        tags$head(tags$style(HTML("#remove_filters {background-color: #221f72;
                                                color: #FFFFFF;
                                                font-size: 18px}"))),
        
        tags$head(tags$style(HTML("#save_filters {background-color: #d80b8c;
                                                color: #FFFFFF;
                                                font-size: 18px}"))),   
        
        
        br(),
        br(),
        br(),
        br(),
        br(),
        column(1,
               dropdown(
                 conditionalPanel(
                   condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
        input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
        input.sbm=='newPatients' | input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime'| input.sbm=='data'| input.sbm == 'Comparison' | input.sbm == 'optimization'",
                   br(),
                   actionButton("update_filters", "CLICK TO UPDATE", width = "80%"),
                   br(),
                   br(),
                   box(
                     title = "Select Campus:",
                     width = 12,
                     height = "100px",
                     solidHeader = FALSE,
                     pickerInput("selectedCampus",label=NULL,
                                 choices=default_campus_choices,
                                 multiple=FALSE,
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
                                 choices=default_specialty_choices,
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
                 #                        choices=sort(unique(kpi.all.data$Visit.Method)),
                 #                        multiple=TRUE,
                 #                        options = pickerOptions(
                 #                          liveSearch = TRUE,
                 #                          actionsBox = TRUE,
                 #                          dropupAuto = FALSE),
                 #                        selected = unique(kpi.all.data$Visit.Method)))
                 #   )),
                 
                 
                 conditionalPanel(
                   condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='volume' | input.sbm=='scheduling' |
        input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='access' |
        input.sbm=='newPatients' | input.sbm=='cycleTime' | input.sbm=='roomInTime'| input.sbm=='data'| input.sbm == 'Comparison'| input.sbm == 'optimization'",
                   box(
                     title = "Select Date Range:",
                     width = 12, 
                     height = "100px",
                     solidHeader = FALSE, 
                     dateRangeInput("dateRange", label = NULL,
                                    start = dateRange_start, end = dateRange_max,
                                    min = dateRange_min, max = dateRange_max)),
                   box(
                     title = "Select Days of Week:",
                     width = 12, 
                     solidHeader = FALSE, 
                     selectInput("daysOfWeek",label = NULL,
                                 choices=daysOfWeek.options, selected = daysOfWeek.options,
                                 multiple=TRUE, selectize=TRUE))
                 ),
                 
                 conditionalPanel(
                   condition = "input.sbm=='population'",
                   box(
                     title = "Select Date Range:",
                     width = 12,
                     height = "100px",
                     solidHeader = FALSE,
                     dateRangeInput("dateRangepop", label = NULL,
                                    start = dateRangepop_min, end = dateRangepop_max,
                                    min = dateRangepop_min, max = dateRangepop_max)),
                   box(
                     title = "Select Days of Week:",
                     width = 12,
                     solidHeader = FALSE,
                     selectInput("daysOfWeek",label = NULL,
                                 choices=daysOfWeek.options, selected = daysOfWeek.options,
                                 multiple=TRUE, selectize=TRUE))
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.sbm=='slotManagement'",
                   box(
                     title = "Select Date Range:",
                     width = 12, 
                     solidHeader = FALSE, 
                     dateRangeInput("dateRangeslot", label = NULL,
                                    start = dateRangeSlot_start, end = dateRangeSlot_end,
                                    min = dateRangeSlot_start, max = dateRangeSlot_end)),
                   
                   
                   box(
                     title = "Select Days of Week:",
                     width = 12, 
                     solidHeader = FALSE, 
                     selectInput("daysOfWeekslot",label = NULL,
                                 choices = daysOfWeek.options, selected = daysOfWeek.options,
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
                                 choices=daysOfWeek.options, selected = daysOfWeek.options,
                                 multiple=TRUE, selectize=TRUE))
                 ),
                 
                 conditionalPanel(
                   condition = "input.sbm=='utilization'",
                   box(
                     title = "Select Date Range:",
                     width = 12, 
                     solidHeader = FALSE, 
                     dateRangeInput("dateRangeUtil", label = NULL,
                                    start = util_date_start, end = util_date_end,
                                    min = util_date_start, max = util_date_end)),
                   
                   box(
                     title = "Select Days of Week:",
                     width = 12, 
                     solidHeader = FALSE, 
                     selectInput("daysOfWeekUtil",label = NULL,
                                 choices = daysOfWeek.options, selected = daysOfWeek.options,
                                 multiple=TRUE, selectize=TRUE))
                 ),
                 
                 
                 # Future Slot 
                 # conditionalPanel(
                 #   condition = "input.sbm=='upcomingDemand'",
                 #          uiOutput("dateRangeControlFutureSlot"),
                 #          uiOutput("daysOfWeekControlFutureSlot")
                 #   ),
                 # 
                 # # Past Slot 
                 # conditionalPanel(
                 #   condition = "input.sbm=='slotUsage'",
                 #   column(2,
                 #          uiOutput("dateRangeControlPastSlot"),
                 #          uiOutput("daysOfWeekControlPastSlot")
                 #   )),
                 
                 conditionalPanel(
                   condition = "input.sbm=='KPIs' | input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
        input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | input.sbm=='newPatients' | 
        input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
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
        ) #Close column
        
      ), #Close conditional Panel
      
      conditionalPanel(
        condition = "input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
      input.sbm=='newPatients' | input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
        
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
        
        # conditionalPanel(
        #   condition = "input.sbm=='system' | input.sbm=='profile' | input.sbm=='volume' | input.sbm=='scheduling' |
        # input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
        # input.sbm=='newPatients' | input.sbm=='cycleTime' | input.sbm=='roomInTime'| input.sbm=='data'",
        # dropdown(
        #   box(
        #     title = "Change Width:",
        #     width = 12,
        #     height = "150px",
        #     solidHeader = FALSE,
        #     sliderInput(
        #       inputId = "plotWidth",
        #       label = NULL, 
        #       value = 1350, min = 200, max = 1450,
        #       ticks = FALSE
        #     ),
        #     fluidRow(
        #       column(2, offset = 4,
        #              actionButton("resetwidth", "Reset")
        #       )
        #     )
        #   ),
        #   
        #   # numericInput("height", "height", 300),
        #   
        #   style = "material-circle", size = "lg", right = TRUE, status = "default",
        #   icon = icon("gear"), width = "300px",
        #   
        #   tooltip = tooltipOptions(title = "Format graphs."),
        #   inputId = "dropdownheight"
        #   
        # ), # Close Drop Down Button
        # ),
        # conditionalPanel(
        #   condition = "input.sbm=='KPIs'",
        #     dropdown(
        #       box(
        #         title = "Change Width:",
        #         width = 12,
        #         height = "150px",
        #         solidHeader = FALSE,
        #         sliderInput(
        #           inputId = "plotWidthkpi",
        #           label = NULL, 
        #           value = 1100, min = 200, max = 1100,
        #           ticks = FALSE
        #         ),
        #         fluidRow(
        #           column(2, offset = 4,
        #                  actionButton("resetwidthkpi", "Reset")
        #           )
        #         )
        #       ),
        #       
        #       # numericInput("height", "height", 300),
        #       
        #       style = "material-circle", size = "lg", right = TRUE, status = "default",
        #       icon = icon("gear"), width = "300px",
        #       
        #       tooltip = tooltipOptions(title = "Format graphs."),
        #       inputId = "dropdownheightkpi"
        #       
        #     ), # Close Drop Down Button
        # ),
        br(),
        # actionButton("download1",
        #              label = icon("download")),
        
        # bsTooltip("dropdownbutton3", "Load previously saved bookmarks.",
        #           "left", options = list(container = "body")
        # ),
        # bsTooltip("dropdownbutton4", "Bookmark global filters.",
        #           "left", options = list(container = "body")
        # ),
        bsTooltip("download1", "Download (PNG) current tab.",
                  "left", options = list(container = "body")
        ),
        bsTooltip("update_filters", "Select filters and press the button to update the tool",
                  "top", options = list(container = "body")
        )
      ),
      
      conditionalPanel(
        condition = "input.sbm == 'cycleTime' | input.sbm == 'roomInTime'",
          br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        column(1,
          dropdown(
            box(
              title = NULL,
              width = 20,
              height = "400px",
              solidHeader = FALSE,
              tags$img(src='Boxplot.png',height='100%',width='100%')
            ),
            style = "material-circle", size = "lg", right = TRUE, status = "default",
            icon = icon("info"), width = "300px",
            
            tooltip = tooltipOptions(title = "Click for additional info on how to interpret boxplots."),
            inputId = "dropdownboxplot"
          )
        )
      )
      
    ) #Close Fluid Page
  ) # Close daashboardBody
)# Close DashboardPage

# Run ShinyApp ===============================================================================================
#shinyApp(ui, server)
