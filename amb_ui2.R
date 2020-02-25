ui <- dashboardPage(
  ### UI start-----------------------------------------------------------
  dashboardHeader(title = "Operational Dashboard",
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
                              }
                              '
                              #.main-sidebar {
                              #ackground-color: #d9d9d9 !important;
                              #}'
                              
    ))),
    
    # Overwrite fixed height of dashboard sidebar
    tags$head(tags$style(HTML('.content-wrapper { height: 3000px !important;}'))),
    
    width = 200,
    
    sidebarMenu(id = "sbm",
                menuItem("Practice Overview", tabName = "profile", icon = icon("hospital")),
                menuItem("KPIs", tabName = "KPIs", icon = icon("chart-line")),
                menuItem("Population", tabName = "population", icon = icon("users")),
                menuItem("Volume", tabName = "volume", icon = icon("chart-bar")),
                menuItem("Scheduling", tabName = "scheduling", icon = icon("clock"),
                         menuSubItem("Scheduled/Arrived", tabName = "arrived"),
                         menuSubItem("No Shows/Overbooks", tabName = "noshows"),
                         menuSubItem("Cancellations/Bumps", tabName = "cancellations")),
                menuItem("Utilization", tabName = "utilization", icon = icon("percent")),
                menuItem("Access", tabName = "access", icon = icon("location-arrow")),
                menuItem("Day of Visit", tabName = "day", icon = icon("walking")),
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
    
    # valueBox "yellow" color for Mount Sinai Grey
    tags$style(".small-box.bg-yellow { background-color: 	#d9d9d9 !important; color: #000000 !important; }"),
    
    tabItems(
      # Practice Overview Tab ------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "profile",
              column(10,
                     div("Practice Overview", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
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
                       plotOutput("avgPtArrival")
                       ),
                     boxPlus(
                       title = "Day of Visit", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       fluidRow(valueBoxOutput("avgCycleTime", width=3),
                                valueBoxOutput("avgCheckinToRoomin", width=3),
                                valueBoxOutput("avgProviderTime", width=3),
                                valueBoxOutput("avgCheckoutTime", width=3)))
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
                       fluidRow(plotOutput("fillRate", height = "150px")),
                       fluidRow(plotOutput("apptStatus", height = "250px"))
                     ))
              )),
      
      tabItem(tabName = "KPIs",
              # KPIs Tab --------------------------------------------------------------------------------------------------------------------
                column(10,
                       div("KPIs", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                       tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                       #textOutput("practiceName_KPIs"),
                       column(2,
                              # box(
                              #   title = NULL,
                              #   width = 12,
                              #   solidHeader = FALSE,
                              #   checkboxGroupInput(
                              #     inputId = "selectedKPIs", label = h4("Select KPIs:"),
                              #     choices = kpiOptions,
                              #     selected = "Volume")),
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
                                plotOutput("kpiVolumeGraph")
                              ),
                              boxPlus(
                                title = "Scheduling KPIs", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("kpiApptStatusGraph", height="800px")
                              ),
                              boxPlus(
                                title = "Access KPIs", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE
                              ),
                              boxPlus(
                                title = "Day of Visit KPIs", width = 12, status = "primary",
                                solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                                plotOutput("kpiWaitTimeGraph")
                              )
                              )
  
              )),
      
      # Population Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "population",
              column(10,
                     boxPlus(
                       title = "Population Analysis", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       box(
                         width = 12, solidHeader = FALSE,
                         leafletOutput("population1", width = "80%", height = "800px"))))
              ),
      
      # Volume Tab -----------------------------------------------------------------------------------------------------------
      tabItem(tabName = "volume",
              column(10,
                     div("Volume", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName1{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                     boxPlus(
                       title = "Volume Over Time", width = 12, status = "primary",
                       solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                       plotOutput("volume1")),
                     column(6,
                            boxPlus(
                              title = "Monthly Volume", width =12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("volume2"),
                              plotOutput("volume4"),
                              tableOutput("volume4.1"))),
                     column(6,
                            boxPlus(
                              title = "Daily Volume", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              plotOutput("volume3"),
                              plotOutput("volume5"),
                              tableOutput("volume5.1")))
              )),
      
      # Scheduling Tab -------------------------------------------------------------------------------------------------------
      tabItem(tabName = "arrived",
              column(10,
                     div("Scheduling | Scheduled/Arrived", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
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
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
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
                           column(6, plotOutput("avgNoShowCount", height="600px")),
                           column(6, plotOutput("avgNoShowPercent", height = "600px")))
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
                     div("Scheduling | Cancellations/Bumps", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                     #textOutput("practiceName_utilization"),
                     fluidRow(
                       boxPlus(
                         title = "Cancellations", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(6, plotOutput("canceledLeadDays")),
                         column(6, "placeholder for cancellation reason graph"))
                       ),
                     fluidRow(
                       boxPlus(
                         title = "Bumps", width = 12, status = "primary",
                         solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                         column(6, plotOutput("bumpedLeadDays")),
                         column(6, "placeholder for bumped reason graph"))
                     )
                     )),
      
      # Utilization Tab ------------------------------------------------------------------------------------------------------
      tabItem(tabName = "utilization",
              column(10,
                     div("Utilization", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                     #textOutput("practiceName_utilization"),
                     column(12,
                            boxPlus(
                              title = "Utilization Overview", width = 12, status = "primary",
                              solidHeader = TRUE, collapsible = TRUE, closable = TRUE,
                              fluidRow(
                                br(),
                                column(3, box(title = NULL, width = 12, solidHeader = FALSE,
                                              sliderInput("setHours", label = h4("Set Daily Open Hours:"), min = 1, max = 24, value = 8))),
                                column(3, box(title = NULL, width = 12, solidHeader = FALSE, 
                                              sliderInput("setRooms", label = h4("Set Rooms Available:"), min = 1, max = 50, value = 5))),
                                column(3, valueBoxOutput("avgRoomsRequired", width=12)),
                                column(3, valueBoxOutput("avgUtilization", width=12))),
                              hr(),
                                tabBox(
                                  title = NULL,
                                  id = "tabset1", width = "100%", height = "1000px",
                                  tabPanel("Average", 
                                           plotOutput("spaceUsed", height = "500px"),
                                           plotOutput("spaceUtil", height = "500px")),
                                  tabPanel("Percentiles",
                                           plotOutput("spaceUsedPerc", height = "500px"),
                                           plotOutput("spaceUtilPerc", height = "500px")
                                           ))
                              ))
                     )),
      
      # Access Tab ------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "access"),
      
      # Day of Visit Tab ------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "day",
              column(10,
                     div("Patient Flow", style = "color:	#221f72; font-family:Calibri; font-weight:bold; font-size:34px; margin-left: 20px"),
                     tags$style("#practiceName{color:black; font-family:Calibri; font-style: italic; font-size: 20px; margin-top: -0.5em; margin-bottom: 0.5em; margin-left: 20px}"),
                     #textOutput("practiceName_utilization"),
                     column(12,
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
    
    # Master Filter Panel ---------------------------------------------------------------------------------------------------------------------
    conditionalPanel(
      condition = "input.sbm=='profile' | input.sbm=='KPIs' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
      input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | input.sbm=='day'",
      column(2,
             br(), br(),
             box(
               title = "Select Campus:",
               width = 12,
               solidHeader = FALSE,
               selectInput("selectedCampus", label = NULL,
                           choices=sort(unique(data.subset.new$Campus)),
                           multiple=TRUE, selectize=TRUE,
                           selected = "MOUNT SINAI HOSPITAL")),
             uiOutput("specialtyControl"),
             uiOutput("departmentControl"),
             uiOutput("providerControl"),
             uiOutput("dateRangeControl"),
             uiOutput("daysOfWeekControl")
      ))
    

    
  ) # Close daashboardBody
) # Close DashboardPage

# Run ShinyApp ===============================================================================================
shinyApp(ui, server)

install.packages("echarts4r")
install.packages("echarts4r.assets")
library(echarts4r)
library(echarts4r.assets)

library(gridExtra)
library(grid)
df22 <- data.frame(
  x = sort(LETTERS[1:5], decreasing = TRUE),
  y = sort(sample(20:80,5))
)

df22 %>% 
  e_charts(x) %>% 
  e_pictorial(y, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("westeros") %>%
  e_title("People Icons") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  
  remotes::install_github("rstudio/shiny@rc-v1.3.2")
remotes::install_github("cpsievert/QCA")
# restart R sesssion
QCA::runGUI()

