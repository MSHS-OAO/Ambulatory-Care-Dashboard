# Holiday choices

holiday_choices <- holid %>% select(HOLIDAY) %>% mutate(HOLIDAY =unique(HOLIDAY)) %>% collect()

# Default choices for access tab 
access_campus_choices <-  access_tbl %>% select(CAMPUS) %>% mutate(CAMPUS  = unique(CAMPUS)) %>% collect()
access_campus_choices <- sort(access_campus_choices$CAMPUS)


access_specialty_choices <-  access_tbl %>% filter(CAMPUS %in% "MSUS") %>% select( CAMPUS_SPECIALTY)  %>%
     mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
access_specialty_choices <- sort(access_specialty_choices$CAMPUS_SPECIALTY)
  
  
access_department_choices <-  access_tbl %>% filter(CAMPUS %in% "MSUS" & 
                                        CAMPUS_SPECIALTY %in% access_specialty_choices) %>% select( DEPARTMENT)  %>%
                                         mutate(DEPARTMENT= unique(DEPARTMENT)) %>% collect()
access_department_choices <- sort(access_department_choices$DEPARTMENT )
  
   
access_provider_choices <-   access_tbl %>% filter(CAMPUS %in% "MSUS" & 
                                               CAMPUS_SPECIALTY %in% access_specialty_choices & 
                                                 DEPARTMENT %in% access_department_choices ) %>% 
                                                   select(PROVIDER)  %>% 
                                                    mutate(PROVIDER= unique(PROVIDER)) %>% collect()


access_provider_choices <- sort(access_provider_choices$PROVIDER)


access_visit_method <-    access_tbl %>% filter(CAMPUS %in% "MSUS" & 
                                             CAMPUS_SPECIALTY %in% access_specialty_choices & 
                                              DEPARTMENT %in% access_department_choices &
                                                PROVIDER %in% access_provider_choices) %>% 
                                                 select( VISIT_METHOD)  %>% 
                                                mutate(VISIT_METHOD= unique(VISIT_METHOD)) %>% collect()
  
access_visit_method <- sort(access_visit_method$VISIT_METHOD)
 

access_PRC_name <-  access_tbl %>% filter(CAMPUS %in% "MSUS" & 
                                       CAMPUS_SPECIALTY %in% access_specialty_choices & 
                                       DEPARTMENT %in% access_department_choices &
                                       PROVIDER %in% access_provider_choices &
                                       VISIT_METHOD %in% access_visit_method) %>% 
                                        select(APPT_TYPE )  %>% 
                                         mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
  

access_PRC_name <- sort(access_PRC_name$APPT_TYPE)  
  


# access_dateRange <-  access_tbl %>% select(APPT_MADE_DTTM , APPT_DTTM)  %>% collect()
# access_dateRange <- access_dateRange %>% filter(APPT_DTTM >= max_date - 730)
# 
# 
# access_dateRange_min <- min(as.Date(access_dateRange$APPT_MADE_DTTM, format="%Y-%m-%d"))
# access_dateRange_max <- max(as.Date(access_dateRange$APPT_MADE_DTTM, format="%Y-%m-%d"))


access_dateRange_min <- glue("Select min(APPT_MADE_DTTM) AS minDate FROM ACCESS_SQL")
access_dateRange_min <- dbGetQuery(con, access_dateRange_min)
access_dateRange_min <- as.Date(access_dateRange_min$MINDATE, format="%Y-%m-%d")

access_dateRange_max <- glue("Select max(APPT_MADE_DTTM) AS maxDate FROM ACCESS_SQL")
access_dateRange_max <- dbGetQuery(con, access_dateRange_max)
access_dateRange_max <- as.Date(access_dateRange_max$MAXDATE, format="%Y-%m-%d")

access_dateRange_start <-  access_dateRange_min




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
                menuItem("Release", tabName = "release", icon= icon("info")),
                menuItem("Q&A", tabName = "q&a", icon= icon("question")),
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
                 #menuItem("Access", tabName = "newPatients", icon = icon("plus-circle")),
                 menuItem("Access", tabName = "access", icon = icon("plus-circle"))
                
                
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
    #access_update_filters {
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
        
        
        # Access Tab ------------------------------------------------------------------------------------------------------------
        tabItem(tabName = "access",
                #tabItem(tabName = "newPatients",
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
                )
                )
        
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
          
          tags$head(tags$style(HTML("#access_update_filters {background-color: #d80b8c;
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
                   
                   # For access tab
                   conditionalPanel(
                     condition = "input.sbm=='access' ",
                     br(),
                     actionButton("access_update_filters", "CLICK TO UPDATE", width = "80%"),
                     br(),
                     br(),
                     box(
                       title = "Select Campus:",
                       width = 12,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("selectedCampus_access",label=NULL,
                                   choices=access_campus_choices,
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
                       pickerInput("selectedSpecialty_access",label=NULL,
                                   choices=access_specialty_choices,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1",
                                     countSelectedText = "{0}/{1} Specialties",
                                     dropupAuto = FALSE),
                                   selected = access_specialty_choices[1])),
                     box(
                       title = "Select Department:",
                       width = 12,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("selectedDepartment_access",label=NULL,
                                   choices=access_department_choices,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1",
                                     countSelectedText = "{0}/{1} Departments",
                                     dropupAuto = FALSE),
                                   selected =access_department_choices[1])),
                     box(
                       title = "Select Resource Type:",
                       width = 12,
                       height = "100px",
                       solidHeader = FALSE,
                       checkboxGroupButtons(
                         inputId = "selectedResource_access",
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
                       pickerInput("selectedProvider_access",label=NULL,
                                   choices=access_provider_choices,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1", 
                                     countSelectedText = "{0}/{1} Providers", 
                                     dropupAuto = FALSE),
                                   selected = access_provider_choices)),
                     box(
                       title = "Select Visit Method:",
                       width = 12,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("selectedVisitMethod_access",label=NULL,
                                   choices=access_visit_method,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1", 
                                     countSelectedText = "{0}/{1} Visit Methods", 
                                     dropupAuto = FALSE),
                                   selected = access_visit_method)),
                     box(
                       title = "Select Visit Type:",
                       width = 12,
                       height = "100px",
                       solidHeader = FALSE,
                       pickerInput("selectedPRCName_access",label=NULL,
                                   choices=access_PRC_name,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     selectedTextFormat = "count > 1", 
                                     countSelectedText = "{0}/{1} Visit Types", 
                                     dropupAuto = FALSE),
                                   selected = access_PRC_name))
                   ),
                   
                   conditionalPanel(
                     condition = "input.sbm=='access'",
                     box(
                       title = "Select Date Range:",
                       width = 12, 
                       height = "100px",
                       solidHeader = FALSE, 
                       dateRangeInput("dateRange_access", label = NULL,
                                      start = access_dateRange_start, end = access_dateRange_max,
                                      min = access_dateRange_min, max = access_dateRange_max)),
                     box(
                       title = "Select Days of Week:",
                       width = 12, 
                       solidHeader = FALSE, 
                       selectInput("daysOfWeek_access",label = NULL,
                                   choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                                   multiple=TRUE, selectize=TRUE))
                   ),
                   

              
                   
                   conditionalPanel(
                     condition = "input.sbm=='KPIs' | input.sbm=='system' | input.sbm=='systemComparison' | input.sbm=='profile' | input.sbm=='provider' | input.sbm=='population' | input.sbm=='volume' | input.sbm=='scheduling' |
        input.sbm=='arrived' | input.sbm=='noshows'| input.sbm=='cancellations' | input.sbm=='utilization' | input.sbm=='access' | 
        input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
                     box(
                       title = "Select Holidays to Exclude:",
                       width = 12,
                       solidHeader = FALSE,
                       pickerInput("excludeHolidays",label=NULL,
                                   choices= holiday_choices ,
                                   multiple=TRUE,
                                   options = pickerOptions(
                                     liveSearch = TRUE,
                                     actionsBox = TRUE,
                                     dropupAuto = FALSE),
                                   selected = holiday_choices))
                     
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
       input.sbm=='slotManagement' | input.sbm=='cycleTime' | input.sbm=='roomInTime'",
          
        
    
          br(),
          
          bsTooltip("download1", "Download (PNG) current tab.",
                    "left", options = list(container = "body")
          ),
          bsTooltip("update_filters", "Select filters and press the button to update the tool",
                    "top", options = list(container = "body")
          ),
          bsTooltip("access_update_filters", "Select filters and press the button to update the tool",
                    "top", options = list(container = "body")
          )
        )
      
        
      ) #Close Fluid Page
    ) # Close daashboardBody
  )# Close DashboardPage
  
