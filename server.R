server <- function(input, output, session) {
  
  
  observeEvent(input$update_filters,{print(is.null(input$update_filters))})
  
  user <- reactive({
    #session$user
    "Armando"
  })
  
  filter_choices <- eventReactive(input$save_filters,{
    user <- user()
    filter_path <- paste0(filter_path, "/", user)
    file_path_sans_ext(list.files(path = filter_path, pattern = "*.csv"))
  })
  
  
  observeEvent(input$remove_filters,{
    if(is.null(input$filter_list)){
      shinyalert("No preset selected.", type = "error")
      #showNotification("Please provider a name", duration = 5, type = "error")
    }else{
      
      user <- user()
      filter_path_full <- paste0(filter_path, "/", user,"/",input$filter_list,".csv")
      filter_path_half <- paste0(filter_path, "/", user)
      if (file.exists(filter_path_full)) {
        #Delete file if it exists
        file.remove(filter_path_full)
      }
      
      filter_choices <- file_path_sans_ext(list.files(path = filter_path_half, pattern = "*.csv"))
      updatePickerInput(session, "filter_list", choices = filter_choices)
    }
    
    print(input$filter_list)
    
    
    
  })
  
  observeEvent(input$sbm,{
    user <- user()
    filter_path_full <- paste0(filter_path, "/", user)
    dir.create(file.path(filter_path, user), showWarnings = FALSE)
    filter_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
    updatePickerInput(session, "filter_list", choices = filter_choices)
  }, once = TRUE)
  
  observeEvent(input$save_filters,{
    user <- user()
    
    if(input$filter_name == ""){
      shinyalert("Please provide a name.", type = "error")
      #showNotification("Please provider a name", duration = 5, type = "error")
    }else{
      updateTextInput(session, "filter_name", value = "")
      print(input$filter_name)
      filter_path_full <- paste0(filter_path, "/", user)
      
      filter_df <- mapply(c, input$filter_name, input$selectedCampus, input$selectedSpecialty,
                          input$selectedDepartment, input$selectedResource,
                          input$selectedProvider, input$selectedVisitMethod,
                          input$selectedPRCName, input$daysOfWeek,
                          SIMPLIFY = TRUE)
      filter_df <- as.data.frame(t(filter_df), row.names = FALSE)
      colnames(filter_df) <- c("Name", "Campus", "Specialty", "Department", "Resource",
                               "Provider", "Visit Method", "Visit Type", "Days")
      write.csv(filter_df, here::here(paste0(filter_path_full, "/" , input$filter_name, ".csv")), row.names = FALSE)
      
      filter_list_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
      
      updatePickerInput(session,
                        inputId = "filter_list",
                        choices = filter_list_choices
      )
      
      filter_df
    }
    
  })
  
  
  
  observeEvent(input$filter_list, {
    user <- user()
    
    filter_path <- paste0(filter_path, "/", user, "/", input$filter_list, ".csv")
    filter_df <- read_csv(filter_path)
    
    campus_selected <- unique(filter_df$Campus)
    specialty_selected <- unique(filter_df$Specialty)
    department_selected <- unique(filter_df$Department)
    resource_selected <- unique(filter_df$Resource)
    provider_selected <- unique(filter_df$Provider)
    visitMethod_selected <- unique(filter_df$`Visit Method`)
    visitType_selected <- unique(filter_df$`Visit Type`)
    day_selected <- unique(filter_df$Days)
    
    specialty_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% campus_selected, "Campus.Specialty"]))
    
    department_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% campus_selected &
                                                     kpi.all.data$Campus.Specialty %in% specialty_selected, "Department"]), na.last = TRUE)
    
    provider_choices <- sort(unique(kpi.all.data[
      kpi.all.data$Campus %in% campus_selected &
        kpi.all.data$Campus.Specialty %in% specialty_selected &
        kpi.all.data$Department %in% department_choices &
        kpi.all.data$Resource %in% resource_selected, "Provider"]), na.last = TRUE)
    
    visitMethod_choices <- sort(unique(kpi.all.data[
      kpi.all.data$Campus %in% campus_selected &
        kpi.all.data$Campus.Specialty %in% specialty_selected &
        kpi.all.data$Department %in% department_choices &
        kpi.all.data$Resource %in% resource_selected &
        kpi.all.data$Provider %in% provider_choices, "Visit.Method"]), na.last = TRUE)
    
    visitType_choices <- sort(unique(kpi.all.data[
      kpi.all.data$Campus %in% campus_selected &
        kpi.all.data$Campus.Specialty %in% specialty_selected &
        kpi.all.data$Department %in% department_choices &
        kpi.all.data$Resource %in% resource_selected &
        kpi.all.data$Provider %in% provider_choices &
        kpi.all.data$Visit.Method %in% visitMethod_choices, "Appt.Type"]), na.last = TRUE)
    
    
    
    updatePickerInput(session,
                      inputId = "selectedCampus",
                      selected = campus_selected
    )
    
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = specialty_choices,
                      selected = specialty_selected
    )
    
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = department_choices,
                      selected = department_selected
    )
    
    updateCheckboxGroupButtons(session,
                               inputId = "selectedResource",
                               choices = c("Provider","Resource"),
                               checkIcon = list(
                                 yes = icon("ok", lib = "glyphicon")),
                               selected = resource_selected
    )
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_selected
    )
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visitMethod_choices,
                      selected = visitMethod_selected
    )
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = visitType_choices,
                      selected = visitType_selected
    )
    
    updatePickerInput(session,
                      inputId = "daysOfWeek",
                      choices = daysOfWeek.options,
                      selected = day_selected
    )
    
    
  })
  
  observeEvent(input$update_filters1,{
    user <- user()
    filter_path_full <- paste0(filter_path, "/", user)
    dir.create(file.path(filter_path, user), showWarnings = FALSE)
    filter_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
    updatePickerInput(session, "filter_list", choices = filter_choices)
    
  })
  
  
  observeEvent(input$selectedCampus,{
    if(is.null(input$filter_list)){
      specialty_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% input$selectedCampus, "Campus.Specialty"]))
      updatePickerInput(session,
                        inputId = "selectedSpecialty",
                        choices = specialty_choices,
                        selected = specialty_choices
      )
    }
    
    
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedSpecialty,{
    if(is.null(input$filter_list)){
      department_choices <- sort(unique(kpi.all.data[
        kpi.all.data$Campus %in% input$selectedCampus &
          kpi.all.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))
      
      updatePickerInput(session,
                        inputId = "selectedDepartment",
                        choices = department_choices,
                        selected = department_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedDepartment,{
    if(is.null(input$filter_list)){
      provider_choices <- sort(unique(kpi.all.data[
        kpi.all.data$Campus %in% input$selectedCampus &
          kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
          kpi.all.data$Department %in% input$selectedDepartment &
          kpi.all.data$Resource %in% input$selectedResource, "Provider"]))
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedResource,{
    if(is.null(input$filter_list)){
      provider_choices <- sort(unique(kpi.all.data[
        kpi.all.data$Campus %in% input$selectedCampus &
          kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
          kpi.all.data$Department %in% input$selectedDepartment &
          kpi.all.data$Resource %in% input$selectedResource, "Provider"]))
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedProvider, {
    if(is.null(input$filter_list)){
      visit_choices <- sort(unique(kpi.all.data[
        kpi.all.data$Campus %in% input$selectedCampus &
          kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
          kpi.all.data$Department %in% input$selectedDepartment &
          kpi.all.data$Resource %in% input$selectedResource &
          kpi.all.data$Provider %in% input$selectedProvider, "Visit.Method"]))
      
      updatePickerInput(session,
                        inputId = "selectedVisitMethod",
                        choices = visit_choices,
                        selected = visit_choices
      )
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(list(input$selectedVisitMethod,input$selectedProvider), {
    if(is.null(input$filter_list)){
      prc_choices <- sort(unique(kpi.all.data[
        kpi.all.data$Campus %in% input$selectedCampus &
          kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
          kpi.all.data$Department %in% input$selectedDepartment &
          kpi.all.data$Resource %in% input$selectedResource &
          kpi.all.data$Provider %in% input$selectedProvider &
          kpi.all.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
      
      updatePickerInput(session,
                        inputId = "selectedPRCName",
                        choices = prc_choices,
                        selected = prc_choices
      )
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  output$specialtyControl <- renderUI({
    
    box(
      title = "Select Specialty:",
      width = 12,
      solidHeader = FALSE,
      pickerInput("selectedSpecialty",label=NULL,
                  choices=sort(unique(kpi.all.data[kpi.all.data$Campus %in% input$selectedCampus, "Campus.Specialty"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1",
                    countSelectedText = "{0}/{1} Specialties",
                    dropupAuto = FALSE),
                  selected = sort(unique((kpi.all.data %>% filter(Campus == "MSUS"))$Campus.Specialty))))
  })
  output$departmentControl <- renderUI({
    
    box(
      title = "Select Department:",
      width = 12, 
      solidHeader = FALSE, 
      pickerInput("selectedDepartment",label=NULL,
                  choices=sort(unique(kpi.all.data[
                    kpi.all.data$Campus %in% input$selectedCampus &
                      kpi.all.data$Campus.Specialty %in% input$selectedSpecialty, "Department"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Departments",
                    dropupAuto = FALSE),
                  selected = sort(unique((kpi.all.data %>% filter(Campus == "MSUS"))$Department))))
  })
  
  output$resourceControl <- renderUI({
    
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
    )
    
  })
  
  output$dateRangeControl <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRange", label = NULL,
                     start = min((kpi.all.data[arrived.data.rows,])$Appt.DateYear), end = max((kpi.all.data[arrived.data.rows,])$Appt.DateYear),
                     min = min((kpi.all.data[arrived.data.rows,])$Appt.DateYear), max = max((kpi.all.data[arrived.data.rows,])$Appt.DateYear)))
  })
  
  output$daysOfWeekControl <- renderUI({
    
    box(
      title = "Select Days of Week:",
      width = 12, 
      solidHeader = FALSE, 
      selectInput("daysOfWeek",label = NULL,
                  choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                  multiple=TRUE, selectize=TRUE))
  })
  
  
  output$dateRangeControlUtil <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRangeUtil", label = NULL,
                     start = min(data.hour.arrived$Appt.DateYear), end = max(data.hour.arrived$Appt.DateYear),
                     min = min((kpi.all.data[arrived.data.rows,])$Appt.DateYear), max = max((kpi.all.data[arrived.data.rows,])$Appt.DateYear)))
  })
  
  output$daysOfWeekControlUtil <- renderUI({
    
    box(
      title = "Select Days of Week:",
      width = 12, 
      solidHeader = FALSE, 
      selectInput("daysOfWeekUtil",label = NULL,
                  choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                  multiple=TRUE, selectize=TRUE))
  })
  
  # ## Slot Data - Date Range Filters
  # output$dateRangeControlPastSlot <- renderUI({
  #   
  #   box(
  #     title = "Select Date Range:",
  #     width = 12, 
  #     solidHeader = FALSE, 
  #     dateRangeInput("dateRangePastSlot", label = NULL,
  #                    start = min(past.slot.data$Date), end = max(past.slot.data$Date)))
  # })
  # 
  # output$daysOfWeekControlPastSlot <- renderUI({
  #   
  #   box(
  #     title = "Select Days of Week:",
  #     width = 12, 
  #     solidHeader = FALSE, 
  #     selectInput("daysOfWeekPastSlot",label = NULL,
  #                 choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
  #                 multiple=TRUE, selectize=TRUE))
  # })
  
  output$dateRangeControlFutureSlot <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRangeFutureSlot", label = NULL,
                     start = min(future.slot.data$Appt.DateYear), end = max(future.slot.data$Appt.DateYear),
                     min = min((kpi.all.data[arrived.data.rows,])$Appt.DateYear), max = max((kpi.all.data[arrived.data.rows,])$Appt.DateYear)))
  })
  
  output$daysOfWeekControlFutureSlot <- renderUI({
    
    box(
      title = "Select Days of Week:",
      width = 12, 
      solidHeader = FALSE, 
      selectInput("daysOfWeekFutureSlot",label = NULL,
                  choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                  multiple=TRUE, selectize=TRUE))
  })
  
  ### (2) Prepare datasets for analysis ===============================================================================================
  # [2.1] All pre-processed data for non-kpi tabs --------------------------------------------------------------------------------------
  dataAll <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[all.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShow <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[arrivedNoShow.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrived <- eventReactive(list(input$update_filters,input$update_filters1),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[arrived.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  dataNoShow <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[noshow.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedRescheduled<- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[canceled.bumped.rescheduled.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceled<- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[canceled.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumped<- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[bumped.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataRescheduled<- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[rescheduled.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] All pre-processed data for kpi tabs --------------------------------------------------------------------------------------
  dataAllKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShowKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[kpi.arrivedNoShow.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[kpi.arrived.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[kpi.canceled.bumped.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[kpi.canceled.data.rows,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumpedKpi <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(kpi.all.data[kpi.bumped.data,],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  # [2.2] All pre-processed data for utilization tabs --------------------------------------------------------------------------------------
  
  dataUtilization <- eventReactive(list(input$update_filters,input$utilType,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_2(utilization.data,
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, input$selectedPRCName, 
                     input$dateRange[1], input$dateRange[2], input$daysOfWeekUtil, input$excludeHolidays, input$utilType)
  }) 
  
  # dataScheduledUtilization <- reactive({
  #   groupByFilters_2(utilization.data[scheduled.utilization.data.rows,],
  #                    input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                    input$selectedVisitMethod, input$selectedPRCName, 
  #                    input$dateRange[1], nput$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$utilType)
  # }) 
  # 
  # dataArrivedUtilization <- reactive({
  #   groupByFilters_2(utilization.data[arrived.utilization.data.rows,],
  #                    input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                    input$selectedVisitMethod, input$selectedPRCName, 
  #                    input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$utilType)
  # }) 
  
  # dataHourScheduled <- reactive({
  #   groupByFilters(data.hour.scheduled,
  #                  input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                  input$selectedVisitMethod, input$selectedPRCName, 
  #                  input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # }) 
  # 
  # dataHourArrived <- reactive({
  #   groupByFilters(data.hour.arrived,
  #                  input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                  input$selectedVisitMethod, input$selectedPRCName, 
  #                  input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # }) 
  
  # [2.3] All pre-processed data for access tabs --------------------------------------------------------------------------------------
  dataFutureSlot <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_4(slot.data.subset[future.slot.data.rows,],
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, 
                     input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  dataPastSlot <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_4(slot.data.subset[past.slot.data.rows,],
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod,
                     input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  
  dataAllSlot <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_4(slot.data.subset[all.slot.rows,],
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod,
                     input$dateRangeslot[1], input$dateRangeslot[2], input$daysOfWeekslot, input$excludeHolidays)
  }) 
  
  dataAllSlot_comp <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_4(slot.data.subset[all.slot.rows,],
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod,
                     input$dateRange[1], input$dateRange[2], input$daysOfWeekslot, input$excludeHolidays)
  }) 
  
  
  
  # [2.4] Arrived Population Data --------------------------------------------------------------------------------------
  
  dataArrivedPop <- eventReactive(list(input$update_filters,input$update_filters1),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(population.data_filtered,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangepop[1], input$dateRangepop[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  ### (3) Dashboard Layout ============================================================================================================
  ### [3.1] Title of  Dashboard -------------------------------------------------------------------------------------------------------
  # Site Overview Tab -------------------------------------------------------------------------------------------------
  output$siteTotalPts <- renderValueBox({
    valueBox(
      prettyNum(round(length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark = ','),
      subtitle = tags$p("Avg. Patients per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$siteNoShowPts <- renderValueBox({
    
    valueBox(
      prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / 
                        length(unique((dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear)),0), big.mark = ","),
      subtitle = tags$p("Avg. No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$siteNewPtRatio <- renderValueBox({
    
    valueBox(
      paste0(round((nrow(dataArrived() %>% filter(New.PT3 == TRUE)) / nrow(dataArrived()))*100),"%"),
      subtitle = tags$p("New Patient Ratio", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$siteTotalProvs <- renderValueBox({
    
    valueBox(
      prettyNum(round(mean((dataArrived() %>% group_by(Appt.DateYear, Provider) %>% dplyr::summarise(n()) 
                            %>% group_by(Appt.DateYear) %>% dplyr::summarise(total = n()))$total)), big.mark = ','),
      subtitle = tags$p("Avg. Unique Providers Available per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$sitePtsPerProv <- renderValueBox({
    
    valueBox(
      prettyNum(round((length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear)))/
                        (mean((dataArrived() %>% group_by(Appt.DateYear, Provider) %>% dplyr::summarise(n()) 
                               %>% group_by(Appt.DateYear) %>% dplyr::summarise(total = n()))$total))), big.mark = ','),
      subtitle = tags$p("Avg. Patients per Provider per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  
  output$siteSpecialties <- renderPlot({
    data <- dataArrived()
    
    # data <- arrived.data %>% filter(Campus == "MSUS")
    
    specialty.count <- data %>% group_by(Campus.Specialty) %>% dplyr::summarise(depts = n())
    
    ggplot(specialty.count, aes(area = depts, fill = depts, label = paste0(Campus.Specialty," (",prettyNum(depts,big.mark = ','),")"))) +
      geom_treemap() +
      geom_treemap_text(grow = F, reflow = T, colour = "black", fontface = "bold",
                        place = "centre", family = "Calibri", size = 16) +
      scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "grey50", name = "Total Unique Depts") +
      labs(x = NULL, y = NULL, fill = NULL, 
           title = "Total Arrived Patients by Specialty",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))) +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
            legend.position = "none",
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  
  output$siteWaitTime <- renderPlot({
    data <- dataAll() %>% filter(New.PT3 == TRUE)
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS") %>% filter(New.PT3 == TRUE)
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0, !is.na(New.PT3)) %>%
      group_by(Campus.Specialty) %>%
      dplyr::summarise(avgWaitTime = round(mean(wait.time)),
                       medWaitTime = round(median(wait.time)))
    
    if(input$median1 == TRUE){ # Median Wait Time
      
      ggplot(waitTime, 
             aes(reorder(Campus.Specialty, -medWaitTime), medWaitTime))+
        geom_bar(stat="identity", width = 0.8, fill = "#221f72") +
        geom_hline(aes(yintercept = 14), color = "red", linetype="dashed", size = 1)+
        geom_text(aes(nrow(waitTime)*(0.9),14, label = "New Patient Wait Time \nTarget: 14 days", 
                      vjust = -1, hjust = 0, size = 16, color = "red"))+        scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.2), expand = c(0,0))+
        labs(x=NULL, y="Days",
             title = "Median Wait Time to New Appointment by Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")
      
    }else{ # Average Wait Time 
      
      ggplot(waitTime, 
             aes(reorder(Campus.Specialty, -avgWaitTime), avgWaitTime))+
        geom_bar(stat="identity", width = 0.8, fill = "#221f72") +
        geom_hline(aes(yintercept = 14), color = "red", linetype="dashed", size = 1)+
        geom_text(aes(nrow(waitTime)*(0.9),14, label = "New Patient Wait Time \nTarget: 14 days", 
                      vjust = -1, hjust = 0, size = 16, color = "red"))+
        scale_y_continuous(limits=c(0,max(waitTime$avgWaitTime)*1.2), expand = c(0,0))+
        labs(x=NULL, y="Days",
             title = "Average Wait Time to New Appointment by Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")
      
    }
  })
  
  # output$siteWorkingFTE <- renderPlot({
  #   
  #   data <- dataPastSlot()
  #   # data <- past.slot.data %>% filter(Campus == "MSUS")
  #   
  #   summary <- data %>%
  #     group_by(Campus.Specialty, Appt.DateYear) %>%
  #     dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60,0)) %>%
  #     gather(variable, value, 3) %>%
  #     group_by(Campus.Specialty, variable) %>%
  #     dplyr::summarise(avgHrs = mean(value),
  #                      avgFTE = avgHrs/7.5*7.5)
  #   
  #   
  #   ggplot(summary, aes(x=Campus.Specialty)) +
  #     geom_bar(aes(y=avgHrs),stat="identity", width = 0.8, fill="#d80b8c")+
  #     scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"), limits=c(0,max(summary$avgHrs)*1.3))+
  #     labs(x=NULL, y="Hours\n",
  #          title = "Daily Avg Provider Hours and FTE Available by Specialty",
  #          subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #          caption = "1 FTE = 7.5 patient care hours per day")+
  #     theme_bw() + graph_theme(legend_pos="none") + theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
  #                                                         strip.background = element_rect(fill="#dddedd"))
  #   
  # })
  
  
  # output$sitePtsPerFTE <- renderPlot({
  #   data <- dataPastSlot()
  #   # data <- past.slot.data %>% filter(Campus == "MSUS")
  #   
  #   summary <- data %>%
  #     group_by(Campus.Specialty) %>%
  #     dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60,0),
  #                      `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
  #     mutate(avgFTE = `Available Hours`/7.5,
  #            avgPtsPerFTE = `Arrived Slots`/avgFTE)
  #   
  #   summary$avgPtsPerFTE[which(!is.finite(summary$avgPtsPerFTE))] <- 0
  #   
  #   ggplot(summary, aes(x=Campus.Specialty, y=avgPtsPerFTE)) +
  #     geom_bar(stat="identity", width = 0.8, fill="#00aeef")+
  #     scale_y_continuous(limits=c(0,max(summary$avgPtsPerFTE)*1.3))+
  #     labs(x=NULL, y="Patientss\n",
  #          title = "Daily Avg Patients Arrived per Provider FTE",
  #          subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #          caption = "1 FTE = 7.5 patient care hours per day")+
  #     theme_bw()+ graph_theme(legend_pos="none") + theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"), strip.background = element_rect(fill="#dddedd"))
  # })
  # 
  # Site Comparison Tab -------------------------------------------------------------------------------------------------
  
  output$siteComparisonPts <- renderPlot({
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,] %>% filter(Campus == "MSUS") %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    if(input$bySpecialty1 == TRUE) {
      
      arrivedPts.tb <- arrivedPts %>% group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Total Patients Arrived` = n())
      
      ggplot(arrivedPts.tb, aes(Appt.Week, `Total Patients Arrived`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Patients",
             title = "Total Arrived Patients by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(arrivedPts.tb$`Total Patients Arrived`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
    } else{
      
      arrivedPts.tb <- arrivedPts %>% group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Total Patients Arrived` = n())
      
      ggplot(arrivedPts.tb, aes(Appt.MonthYear, `Total Patients Arrived`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Patients",
             title = "Total Arrived Patients by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(arrivedPts.tb$`Total Patients Arrived`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") +theme(legend.title = element_blank())
    }
    
  })
  
  output$siteComparisonPtsTb <- renderReactable({
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty == "Cardiology")
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    if(input$bySpecialty1 == TRUE) {
      
      table <- arrivedPts %>% 
        group_by(Campus, Campus.Specialty, Appt.Week) %>% 
        dplyr::summarise(total = n()) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "total")) %>%
        pivot_wider(names_from = Appt.Week,
                    values_from = total,
                    values_fill = 0) 
      
      col_names <- c(colnames(table),c("Total Arrived Patients"))
      table <- transform(table, `Total Arrived Patients` = round(rowSums(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table[table == "NA%"] <- "-"      
      
    } else{
      
      table <- arrivedPts %>% 
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>% 
        dplyr::summarise(total = n()) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "total")) %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = total,
                    values_fill = 0) 
      
      col_names <- c(colnames(table),c("Total Arrived Patients"))
      table <- transform(table, `Total Arrived Patients` = round(rowSums(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table[table == "NA%"] <- "-"
      
    }
    
    # bar_chart <- function(label, width = "100%", height = "16px", fill = "#00aeef", background = NULL, align="left") {
    #   bar <- div(style = list(background = fill, width = width, height = height))
    #   chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    #   div(style = list(display = "flex", alignItems = "center"), label, chart)
    # }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
                #   Total = colDef(name = "Total", align = "left", cell = function(value) {
                #     width <- paste0(value / max(table$Total) * 100, "%")
                #     bar_chart(value, width = width)
                #   }
                # ),
                # Trend = colDef(cell = function(value, index) {
                #   sparkline(sparkline$trend[[index]])
                # })
                
              )
    )
    
  })
  
  output$siteComparisonNewPtRatio <- renderPlot({
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- arrived.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    
    if(input$bySpecialty2 == TRUE) {
      
      # Total Arrived Patients
      arrivedPts.tb <- arrivedPts %>% group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Total Patients Arrived` = n())
      # Total Arrived New Patients
      arrivedNewPts.tb <- arrivedPts %>% filter(New.PT3 == TRUE) %>% group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Total New Patients Arrived` = n())
      
      newPts <- merge(arrivedPts.tb, arrivedNewPts.tb, all.x=TRUE)
      newPts[is.na(newPts)] <- 0
      
      newPts$newRatio <- round(newPts$`Total New Patients Arrived` / newPts$`Total Patients Arrived`,2)
      
      ggplot(newPts, aes(Appt.Week, newRatio, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Patients",
             title = "New Patient Ratio by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(labels = percent_format(), limits=c(0,1))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
      
    } else{
      
      # Total Arrived Patients
      arrivedPts.tb <- arrivedPts %>% group_by(siteSpecialty, Appt.MonthYear ) %>% dplyr::summarise(`Total Patients Arrived` = n())
      # Total Arrived New Patients
      arrivedNewPts.tb <- arrivedPts %>% filter(New.PT3 == TRUE) %>% group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Total New Patients Arrived` = n())
      
      newPts <- merge(arrivedPts.tb, arrivedNewPts.tb, all.x=TRUE)
      newPts[is.na(newPts)] <- 0
      
      newPts$newRatio <- round(newPts$`Total New Patients Arrived` / newPts$`Total Patients Arrived`,2)
      
      ggplot(newPts, aes(Appt.MonthYear, newRatio, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Patients",
             title = "New Patient Ratio by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
  })
  
  output$siteComparisonNewPtRatioTb <- renderReactable({
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty == "Cardiology")
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    if(input$bySpecialty2 == TRUE) {
      
      # Total Arrived Patients
      arrivedPts.tb <- arrivedPts %>% group_by(Campus, Campus.Specialty, Appt.Week) %>% dplyr::summarise(`Total Patients Arrived` = n())
      # Total Arrived New Patients
      arrivedNewPts.tb <- arrivedPts %>% filter(New.PT3 == TRUE) %>% group_by(Campus, Campus.Specialty, Appt.Week) %>% dplyr::summarise(`Total New Patients Arrived` = n())
      
      newPts <- merge(arrivedPts.tb, arrivedNewPts.tb, all.x=TRUE)
      newPts[is.na(newPts)] <- 0
      
      newPts$newRatio <- round(newPts$`Total New Patients Arrived` / newPts$`Total Patients Arrived`, 2)*100
      
      table <- newPts %>% 
        select(Campus, Campus.Specialty, Appt.Week, newRatio) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "ratio"))
      
      table <- dcast(table, Campus + Specialty ~ Appt.Week)
      col_names <- c(colnames(table),c("Avg. New Patient Ratio"))
      table <- transform(table, `Avg. New Patient Ratio` = round(rowMeans(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
      
    } else{
      
      # Total Arrived Patients
      arrivedPts.tb <- arrivedPts %>% group_by(Campus, Campus.Specialty, Appt.MonthYear) %>% dplyr::summarise(`Total Patients Arrived` = n())
      # Total Arrived New Patients
      arrivedNewPts.tb <- arrivedPts %>% filter(New.PT3 == TRUE) %>% group_by(Campus, Campus.Specialty, Appt.MonthYear) %>% dplyr::summarise(`Total New Patients Arrived` = n())
      
      newPts <- merge(arrivedPts.tb, arrivedNewPts.tb, all.x=TRUE)
      newPts[is.na(newPts)] <- 0
      
      newPts$newRatio <- round(newPts$`Total New Patients Arrived` / newPts$`Total Patients Arrived`, 2)*100
      
      table <- newPts %>% 
        select(Campus, Campus.Specialty, Appt.MonthYear, newRatio) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "ratio"))
      
      table <- dcast(table, Campus + Specialty ~ Appt.MonthYear)
      
      col_names <- c(colnames(table),c("Avg. New Patient Ratio"))
      table <- transform(table, `Avg. New Patient Ratio` = round(rowMeans(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
    
  })
  
  
  output$siteComparisonNewPtWaitTime <- renderPlot({
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- arrived.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    # Median New Patient Wait Time 
    newWaitTime <- arrivedPts %>% filter(New.PT3 == TRUE) %>% 
      mutate(wait.time = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
      filter(wait.time >= 0)
    
    if(input$bySpecialty3 == TRUE) {
      
      newWaitTime.tb <- newWaitTime %>% 
        group_by(siteSpecialty, Appt.Week) %>%
        dplyr::summarise(`Median New Wait Time` = round(median(wait.time)))
      
      
      ggplot(newWaitTime.tb, aes(Appt.Week, `Median New Wait Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Days",
             title = "Median Wait Time to New Appointment by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(newWaitTime.tb$`Median New Wait Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
      
    } else{
      
      newWaitTime.tb <- newWaitTime %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>%
        dplyr::summarise(`Median New Wait Time` = round(median(wait.time)))
      
      
      ggplot(newWaitTime.tb, aes(Appt.MonthYear, `Median New Wait Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Days",
             title = "Median Wait Time to New Appointment by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(newWaitTime.tb$`Median New Wait Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
      
    }
  })
  
  output$siteComparisonNewPtWaitTimeTb <- renderReactable({
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,] %>% filter(Campus == "MSUS") 
    
    # Median New Patient Wait Time 
    newWaitTime <- arrivedPts %>% filter(New.PT3 == TRUE) %>% 
      mutate(wait.time = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
      filter(wait.time >= 0)
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    
    if(input$bySpecialty3 == TRUE) {
      
      newWaitTime.tb <- newWaitTime %>% 
        group_by(Campus, Campus.Specialty, Appt.Week) %>%
        dplyr::summarise(waitTime = round(median(wait.time))) 
      
      table <-  newWaitTime.tb %>% 
        select(Campus, Campus.Specialty, Appt.Week, waitTime) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "waitTime")) %>%
        pivot_wider(names_from = Appt.Week, values_from = waitTime)
      table$`Median Wait Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," days"))
      # table$`Median Wait Time` <- paste0(table$`Median Wait Time`," days")
      table[table == "NA days"] <- "-"
      
      
      
    } else{
      
      newWaitTime.tb <- newWaitTime %>% 
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>%
        dplyr::summarise(waitTime = round(median(wait.time))) 
      
      table <-  newWaitTime.tb %>% 
        select(Campus, Campus.Specialty, Appt.MonthYear, waitTime) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "waitTime")) %>%
        pivot_wider(names_from = Appt.MonthYear, values_from = waitTime)
      table$`Median Wait Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," days"))
      # table$`Median Wait Time` <- paste0(table$`Median Wait Time`," days")
      table[table == "NA days"] <- "-"
      
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
    
  })
  
  
  output$siteComparisonNoShow <- renderPlot({
    # Scheduling Arrived and No Show Data
    arrivedNoShowPts <- dataArrivedNoShow()
    # arrivedNoShowPts <- arrivedNoShow.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedNoShowPts$siteSpecialty <- paste0(arrivedNoShowPts$Campus," - ",arrivedNoShowPts$Campus.Specialty)
    
    if(input$bySpecialty5 == TRUE) {
      
      # No Show Rate
      noShows <- arrivedNoShowPts %>%
        group_by(siteSpecialty, Appt.Week, Appt.Status) %>%
        dplyr::summarise(Total = n()) %>%
        spread(Appt.Status, Total)
      
      noShows[is.na(noShows)] <- 0
      
      noShows$`No Show Perc` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)
      
      ggplot(noShows, aes(Appt.Week, `No Show Perc`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        #scale_y_continuous(labels = percent_format(), limits = c(0,1))+
        scale_y_continuous(expand = c(0, 0), limits = c(0,max(noShows$`No Show Perc`)*1.2),
                           labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Average No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
      
    } else{
      
      # No Show Rate
      noShows <- arrivedNoShowPts %>%
        group_by(siteSpecialty, Appt.MonthYear, Appt.Status) %>%
        dplyr::summarise(Total = n()) %>%
        spread(Appt.Status, Total)
      
      noShows[is.na(noShows)] <- 0
      
      noShows$`No Show Perc` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)
      
      ggplot(noShows, aes(Appt.MonthYear, `No Show Perc`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(expand = c(0, 0), limits = c(0,max(noShows$`No Show Perc`)*1.2)
                           ,labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Average No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
    
  })
  
  output$siteComparisonNoShowTb <- renderReactable({
    
    # Scheduling Arrived and No Show Data
    arrivedNoShowPts <- dataArrivedNoShow()
    # arrivedNoShowPts <- kpi.all.data[arrivedNoShow.data.rows,] 
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    if(input$bySpecialty5 == TRUE) {
      
      # No Show Rate
      noShows <- arrivedNoShowPts %>%
        group_by(Campus, Campus.Specialty, Appt.Week, Appt.Status) %>%
        dplyr::summarise(Total = n()) %>%
        spread(Appt.Status, Total)
      
      noShows[is.na(noShows)] <- 0
      
      noShows$`noShow` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)*100
      
      table <-  noShows %>% 
        select(Campus, Campus.Specialty, Appt.Week, noShow) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "noShow")) %>%
        pivot_wider(names_from = Appt.Week, 
                    values_from = noShow)
      col_names <- c(colnames(table),c("Avg. No Show Rate"))
      table <- transform(table, `Avg. No Show Rate` = round(rowMeans(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
      
    } else{
      
      # No Show Rate
      noShows <- arrivedNoShowPts %>%
        group_by(Campus, Campus.Specialty, Appt.MonthYear, Appt.Status) %>%
        dplyr::summarise(Total = n()) %>%
        spread(Appt.Status, Total)
      
      noShows[is.na(noShows)] <- 0
      
      noShows$`noShow` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)*100
      
      table <-  noShows %>% 
        select(Campus, Campus.Specialty, Appt.MonthYear, noShow) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "noShow")) %>%
        pivot_wider(names_from = Appt.MonthYear, 
                    values_from = noShow)
      col_names <- c(colnames(table),c("Avg. New Patient Ratio"))
      table <- transform(table, `Avg. New Patient Ratio` = round(rowMeans(table[,3:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
      
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
  })
  
  # testdataset <- reactive({
  #   slotData <- dataPastSlot()
  #   slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
  #   bookedFilledRate <- slotData %>%
  #     group_by(siteSpecialty, Appt.Week) %>%
  #     dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
  #                      `Booked Hours` = round(sum(BOOKED_MINUTES),0),
  #                      `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
  #                      `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
  #                      `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
  #     mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
  #            `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
  #     gather(variable, value, 3:9)
  #   
  #   bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
  # })
  # 
  # output$Testtable <- renderDataTable(testdataset(), filter = "top")
  
  output$siteComparisonBookedRate <- renderPlot({
    # Slot Data
    slotData <- dataPastSlot()
    # slotData <- slot.data.subset[past.slot.data.rows,]
    # slotData <- slotData %>% filter(Campus == "MSUS")
    
    if(input$bySpecialty4 == TRUE) {
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(siteSpecialty, Appt.Week) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2),
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2)) %>%
        gather(variable, value, 3:7)
      
      bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
      
      ggplot(bookedFilledRate, aes(Appt.Week, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_wrap(variable~., dir ="v")+
        scale_y_continuous(expand = c(0, 0), limits = c(0,max(bookedFilledRate$value)*1.2),
                           labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y=NULL,
             title = "Average Booked vs. Filled Rate (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
    } else{
      
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(siteSpecialty, Appt.MonthYear) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2),
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2)) %>%
        gather(variable, value, 3:7)
      
      bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
      
      bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
      
      ggplot(bookedFilledRate, aes(Appt.MonthYear, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_wrap(variable~., dir = "v")+
        scale_y_continuous(expand = c(0, 0), limits = c(0,max(bookedFilledRate$value)*1.2),
                           labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y=NULL,
             title = "Average Booked vs. Filled Rate (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(bookedFilledRate$value)*1.2),labels=scales::percent_format(accuracy = 1))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
      
    }
    
  })
  
  output$siteComparisonBookedRateTb <- renderReactable({
    
    # Slot Data
    slotData <- dataPastSlot()
    # slotData <- slot.data.subset[past.slot.data.rows,]
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    
    if(input$bySpecialty4 == TRUE) {
      
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(Campus, Campus.Specialty, Appt.Week) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2)*100,
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2)*100) %>%
        select(Campus, Campus.Specialty, Appt.Week, `Booked Rate`, `Filled Rate`) %>%
        gather(variable, value, 4:5) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "Status", "value")) 
      
      table <- bookedFilledRate %>%
        pivot_wider(names_from = Appt.Week,
                    values_from = value) %>%
        arrange(Campus, Specialty) 
      col_names <- c(colnames(table),c("Avg. Rate"))
      table <- transform(table, `Avg. Rate` = round(rowMeans(table[,4:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
      table[table == "Inf%"] <- "-"
      table[table == "NaN%"] <- "-"
      
      
      
    } else{
      
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2)*100,
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2)*100) %>%
        select(Campus, Campus.Specialty, Appt.MonthYear, `Booked Rate`, `Filled Rate`) %>%
        gather(variable, value, 4:5) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "Status", "value")) 
      
      table <- bookedFilledRate %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = value) %>%
        arrange(Campus, Specialty)
      col_names <- c(colnames(table),c("Avg. Rate"))
      table <- transform(table, `Avg. Rate` = round(rowMeans(table[,4:length(table)], na.rm = TRUE)))
      colnames(table) <- col_names
      table <- table %>% mutate_if(is.numeric, ~paste0(.,"%"), )
      table[table == "NA%"] <- "-"
      table[table == "Inf%"] <- "-"
      table[table == "NaN%"] <- "-"
      
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
  })
  
  
  output$siteComparisonMedianCheckInCycleTime <- renderPlot({
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- arrived.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    cycleTimes.tb <- arrivedPts %>% filter(cycleTime > 0) %>% 
      group_by(siteSpecialty, Appt.MonthYear, Appt.Week, Appt.DateYear) %>% dplyr::summarise(cycleTime = round(median(cycleTime)))
    
    if(input$bySpecialty6 == TRUE) {
      
      # Check in to Visit End 
      cycleTimes <- cycleTimes.tb %>% 
        group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Median Cycle Time` = round(median(cycleTime)))
      
      ggplot(cycleTimes, aes(Appt.Week, `Median Cycle Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Check-in to Visit End Time (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(cycleTimes$`Median Cycle Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
    } else{
      
      # Check in to Visit End 
      cycleTimes <- cycleTimes.tb %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Median Cycle Time` = round(median(cycleTime)))
      
      ggplot(cycleTimes, aes(Appt.MonthYear, `Median Cycle Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Check-in to Visit End Time (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(cycleTimes$`Median Cycle Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
  })
  
  output$siteComparisonMedianCheckInCycleTimeTb <- renderReactable({
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,]
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    if(input$bySpecialty6 == TRUE) {
      
      table <- arrivedPts %>% filter(cycleTime > 0) %>% 
        group_by(Campus, Campus.Specialty, Appt.Week) %>% 
        dplyr::summarise(cycleTime = round(median(cycleTime))) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "cycleTime")) %>%
        pivot_wider(names_from = Appt.Week,
                    values_from = cycleTime)
      table$`Median Cycle Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," min"))
      table[table == "NA min"] <- "-"
      
    } else{
      
      
      table <- arrivedPts %>% filter(cycleTime > 0) %>% 
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>% 
        dplyr::summarise(cycleTime = round(median(cycleTime))) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "cycleTime")) %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = cycleTime)
      table$`Median Cycle Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," min"))
      table[table == "NA min"] <- "-"
      
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
  })
  
  
  output$siteComparisonMedianCycleTime <- renderPlot({
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- arrived.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    cycleTimes.tb <- arrivedPts %>% filter(cycleTime > 0) %>% 
      group_by(siteSpecialty, Appt.MonthYear, Appt.Week, Appt.DateYear) %>% dplyr::summarise(cycleTime = round(median(cycleTime)))
    
    # Check in to Room in  
    roomedTime.tb <- arrivedPts %>% filter(checkinToRoomin >= 0) %>% 
      group_by(siteSpecialty, Appt.MonthYear, Appt.Week, Appt.DateYear) %>% dplyr::summarise(checkinToRoomin = round(median(checkinToRoomin)))
    
    if(input$bySpecialty6 == TRUE) {
      
      # Check in to Visit End 
      cycleTimes <- cycleTimes.tb %>% 
        group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Median Cycle Time` = round(median(cycleTime)))
      
      # Check in to Room in  
      roomedTime <- roomedTime.tb %>% 
        group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Median Check-in to Room-in Time` = round(median(checkinToRoomin)))
      
      ggplot(roomedTime, aes(Appt.Week, `Median Check-in to Room-in Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Check-in to Room-in Time (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(roomedTime$`Median Check-in to Room-in Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
    } else{
      
      # Check in to Visit End 
      cycleTimes <- cycleTimes.tb %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Median Cycle Time` = round(median(cycleTime)))
      
      # Check in to Room in  
      roomedTime <- roomedTime.tb %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Median Check-in to Room-in Time` = round(median(checkinToRoomin)))
      
      ggplot(roomedTime, aes(Appt.MonthYear, `Median Check-in to Room-in Time`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Check-in to Room-in Time (Min) by Site and Specialty",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(roomedTime$`Median Check-in to Room-in Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
  })
  
  output$siteComparisonMedianCycleTimeTb <- renderReactable({
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- kpi.all.data[arrived.data.rows,] %>% filter(Campus == "MSUS") %>% filter(Campus.Specialty == "Cardiology")
    
    sticky_style <- list(position = "sticky", left = 0, background = "#d80b8c", color = "white", 
                         fontWeight = "bold", zIndex = 1,
                         borderRight = "1px solid #eee")
    
    if(input$bySpecialty6 == TRUE) {
      
      # Check in to Room in  
      table <- arrivedPts %>% filter(checkinToRoomin >= 0) %>% 
        group_by(Campus, Campus.Specialty, Appt.Week) %>% 
        dplyr::summarise(checkinToRoomin = round(median(checkinToRoomin))) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.Week", "checkinToRoomin")) %>%
        pivot_wider(names_from = Appt.Week,
                    values_from = checkinToRoomin)
      table$`Median Room-in Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," min"))
      table[table == "NA min"] <- "-"
      
    } else{
      
      # Check in to Room in  
      table <- arrivedPts %>% filter(checkinToRoomin >= 0) %>% 
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>% 
        dplyr::summarise(checkinToRoomin = round(median(checkinToRoomin))) %>%
        `colnames<-` (c("Campus", "Specialty", "Appt.MonthYear", "checkinToRoomin")) %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = checkinToRoomin)
      table$`Median Room-in Time` <- apply(table[,3:length(table)], 1, median, na.rm=T)
      table <- table %>% mutate_if(is.numeric, ~paste0(.," min"))
      table[table == "NA min"] <- "-"
      
    }
    
    reactable(table,
              style = list(fontFamily = "Calibri"),
              striped = TRUE,
              highlight = TRUE,
              pagination = FALSE,
              bordered = TRUE,
              height = 300,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 120,
                headerStyle = list(background = "#d80b8c",
                                   color = "white",
                                   fontWeight = "bold")
              ),
              columns = list(
                Campus = colDef(
                  style = sticky_style,
                  headerStyle = sticky_style
                )
              )
    )
  })
  
  # output$siteComparisonWorkingFTE <- renderPlot({
  #   # Slot Data
  #   slotData <- dataPastSlot()
  #   # slotData <- past.slot.data %>% filter(Campus == "MSUS")
  #   slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
  #   
  #   
  #   if(input$bySpecialty7 == TRUE) {
  #     # Total Provider Hours Available
  #     provAvail <- slotData %>%
  #       group_by(siteSpecialty, Appt.Week) %>%
  #       dplyr::summarise(avgHrs = round(sum(AVAIL_MINUTES)/60,0)) %>%
  #       mutate(avgFTE = avgHrs/7.5*7.5)
  #     
  #     
  #     ggplot(provAvail, aes(x=Appt.Week, y=avgHrs, group=siteSpecialty, col=siteSpecialty)) +
  #       geom_line()+
  #       geom_point(size=2)+
  #       scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"))+
  #       scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
  #       labs(x=NULL, y="Hours\n",
  #            title = "Daily Avg Provider Hours and FTE Available by Specialty",
  #            subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #            caption = "1 FTE = 7.5 patient care hours per day")+
  #       theme_new_line()+
  #       theme_bw()+
  #       graph_theme("bottom")+
  #       scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
  #   } else{
  #     
  #     # Total Provider Hours Available
  #     provAvail <- slotData %>%
  #       group_by(siteSpecialty, Appt.MonthYear) %>%
  #       dplyr::summarise(avgHrs = round(sum(AVAIL_MINUTES)/60,0)) %>%
  #       mutate(avgFTE = avgHrs/7.5*7.5)
  #     
  #     
  #     ggplot(provAvail, aes(x=Appt.MonthYear, y=avgHrs, group=siteSpecialty, col=siteSpecialty)) +
  #       geom_line()+
  #       geom_point(size=2)+
  #       scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"))+
  #       scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
  #       labs(x=NULL, y="Hours\n",
  #            title = "Daily Avg Provider Hours and FTE Available by Specialty",
  #            subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #            caption = "1 FTE = 7.5 patient care hours per day")+
  #       theme_new_line()+
  #       theme_bw()+
  #       graph_theme("bottom")
  #     
  #   }
  # })
  
  # output$siteComparisonPtsPerFTE <- renderPlot({
  #   
  #   # Slot Data
  #   slotData <- dataPastSlot()
  #   # slotData <- past.slot.data %>% filter(Campus == "MSUS")
  #   slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
  #   
  #   if(input$bySpecialty8 == TRUE) {
  #     summary <- slotData %>%
  #       group_by(siteSpecialty, Appt.Week) %>%
  #       dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60),
  #                        `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
  #       mutate(avgFTE = `Available Hours`/7.5,
  #              avgPtsPerFTE = `Arrived Slots`/avgFTE)
  #     
  #     ggplot(summary, aes(x=Appt.Week, y=avgPtsPerFTE, group=siteSpecialty, col=siteSpecialty)) +
  #       geom_line()+
  #       geom_point(size=2)+
  #       scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
  #       labs(x=NULL, y="Patients per Provider FTE\n",
  #            title = "Daily Avg Arrived Patients per Provider FTE by Site and Specialty",
  #            subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #            caption = "1 FTE = 7.5 patient care hours per day")+
  #       theme_new_line()+
  #       theme_bw()+
  #       graph_theme("bottom")+
  #       scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
  #   } else{
  #     
  #     summary <- slotData %>%
  #       group_by(siteSpecialty, Appt.MonthYear) %>%
  #       dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60),
  #                        `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
  #       mutate(avgFTE = `Available Hours`/7.5,
  #              avgPtsPerFTE = `Arrived Slots`/avgFTE)
  #     
  #     ggplot(summary, aes(x=Appt.MonthYear, y=avgPtsPerFTE, group=siteSpecialty, col=siteSpecialty)) +
  #       geom_line()+
  #       geom_point(size=2)+
  #       scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
  #       labs(x=NULL, y="Patients per Provider FTE\n",
  #            title = "Daily Avg Arrived Patients per Provider FTE by Site and Specialty",
  #            subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #            caption = "1 FTE = 7.5 patient care hours per day")+
  #       theme_new_line()+
  #       theme_bw()+
  #       graph_theme("bottom")
  #     
  #   }
  #   
  # })
  
  
  # Date Range Header --------------------------------------------------------------------------
  #observeEvent(input$sbm,{
  output$kpis_mem <- renderText({
    session$clientData[["output_kpiVolumeGraph_width"]]
    #mem_used()/1000000000
    #object_size(dataArrivedKpi())
  })
  #})
  
  
  output$practiceName_vol_comp <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_KPIs <- renderText({
    paste0("Based on data from ", input$dateRangeKpi[1]," to ", input$dateRangeKpi[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_siteOverview <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_siteComp <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_practice <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_provider <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_volume <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_population <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_utilization <- renderText({
    paste0("Based on data from ", input$dateRangeUtil[1]," to ", input$dateRangeUtil[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$scheduled_arrived <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$scheduled_noshow <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$scheduled_cancel <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$newpatients <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$slot_usage <- renderText({
    paste0("Based on data from ", input$dateRangeslot[1]," to ", input$dateRangeslot[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '), " as of ", max(dataAll()$Appt.DateYear) + 1)
  })
  
  output$cycle_time <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$room_time <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  
  ### (4) Analysis Output ==============================================================================================================
  ### Practice Overview Tab ------------------------------------------------------------------------------------------------------------
  ### Volume Section
  output$uniquePts <- renderValueBox({
    valueBox(
      prettyNum(length((unique(dataArrived()$MRN))), big.mark = ','),
      subtitle = tags$p("Total Unique Patients", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$totalVisits <- renderValueBox({
    valueBox(
      prettyNum(length((unique(dataArrived()$uniqueId))), big.mark = ','), 
      subtitle = tags$p("Total Visits", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgVisitsPt <- renderValueBox({
    valueBox(
      prettyNum(round(length((unique(dataArrived()$uniqueId))) / length((unique(dataArrived()$MRN))),1), big.mark = ','),
      subtitle = tags$p("Avg. Visits per Patient", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgVisitsDay <- renderValueBox({
    valueBox(
      prettyNum(round(length((unique(dataArrived()$uniqueId))) / length((unique(dataArrived()$Appt.DateYear)))), big.mark = ','),
      subtitle = tags$p("Avg. Visits per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ### Average Daily Patients by Scheduled vs. Check-in Times (1-hour interval)
  
  output$avgPtArrival <- renderPlot({
    data.arrivedNoShow <- dataArrivedNoShow()
    # data.arrivedNoShow <- arrivedNoShow.data
    
    data.arrived <- dataArrived()
    # data.arrived <- arrived.data
    
    ptsScheduled <- data.arrivedNoShow %>%
      group_by(Appt.TM.Hr) %>%
      summarise(Total = n_distinct(uniqueId)) %>%
      mutate(Scheduled = round(Total / length(unique(data.arrived$Appt.DateYear))))
    
    ptsArrived <- data.arrived %>%
      group_by(Appt.TM.Hr) %>%
      summarise(Total = n_distinct(uniqueId)) %>%
      mutate(Arrived = round(Total / length(unique(data.arrived$Appt.DateYear))))
    
    ptsByHour <- as.data.frame(timeOptionsHr) 
    ptsByHour <- merge(ptsByHour,ptsScheduled, by.x = "timeOptionsHr", by.y = "Appt.TM.Hr", all.x = TRUE)  
    ptsByHour <- merge(ptsByHour, ptsArrived, by.x = "timeOptionsHr", by.y = "Appt.TM.Hr", all.x = TRUE)
    ptsByHour[is.na(ptsByHour)] <- 0
    
    names(ptsByHour) <- c("Time","Total Scheduled","Scheduled","Total Arrived","Arrived")
    avgPtsByHour <- ptsByHour[,c("Time","Scheduled","Arrived")]
    
    avgPtsByHour <- reshape2::melt(avgPtsByHour, id="Time", measure = c("Scheduled","Arrived"))
    
    avgPtsByHour <- avgPtsByHour %>% filter(Time %in% timeOptionsHr_filter)
    
    # Scheduled vs. Actual Arrival in Hour Interval 
    
    ggplot(avgPtsByHour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(aes(linetype=variable), size=1.2)+
      scale_linetype_manual(values=c("dashed","solid"))+
      scale_color_manual(values=c("maroon1","midnightblue"))+
      labs(x=NULL, y="Patients",
           title = "Average Scheduled vs. Arrived Patients",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "Scheduled = Arrived + No Show Patients")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top") + theme(legend.title = element_blank())
    
  })
  
  ### Access Section
  
  output$newPtRatio <- renderInfoBox({
    infoBox(
      title = tags$p("Arrived New Patient Ratio", style = "font-size: 150%;"), subtitle = NULL, 
      value = tags$p(paste0(round((nrow(dataArrived() %>% filter(New.PT3 == TRUE)) / nrow(dataArrived()))*100),"%"), style = "font-size: 200%;"), icon = icon("user")
    )
  })
  
  output$newNoShow <- renderInfoBox({
    infoBox(
      title = tags$p("Avg. New Patient No Show %", style = "font-size: 150%;"), subtitle = NULL,
      value = tags$p(paste0(round(nrow(dataArrivedNoShow() %>% filter(New.PT3 == TRUE) %>% filter(Appt.Status == "No Show"))/ 
                                    nrow(dataArrivedNoShow() %>% filter(New.PT3 == TRUE) %>% filter(Appt.Status %in% c("Arrived", "No Show"))),2)*100,"%"), style = "font-size: 200%;"), icon = icon("user-times")
    )
  })
  
  output$newApptWaitTime <- renderInfoBox({
    infoBox(
      title = tags$p("Avg. Wait Time to New Appointment", style = "font-size: 150%;"), subtitle = NULL, 
      value = tags$p(paste0(round(mean((dataAll() %>% filter(New.PT3 == TRUE) %>%
                                          mutate(wait.time = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
                                          filter(!is.na(wait.time)) %>% filter(wait.time >= 0))$wait.time))," days"), style = "font-size: 200%;"), icon = icon("clock")
    )
  })
  
  ## Booked and Filled Rate
  output$pracBookedFilledRate <- renderPlot({
    # Booked and Filled Rate
    data <- dataPastSlot()
    # data <- slot.data.subset[past.slot.data.rows,]
    
    daily.booked <- data.frame(variable = c("Booked Rate", "Filled Rate"),
                               value = c(round((sum(data$`Booked Hours`)/sum(data$`Available Hours`)),2),
                                         round((sum(data$`Arrived Hours`/sum(data$`Available Hours`))),2)))
    
    ggplot(daily.booked, aes(reorder(variable, -value), value, fill=variable)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(daily.booked$value))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Booked and Filled Rate",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=paste0(round(value*100),"%")), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  output$pracApptStatus <- renderPlot({
    
    data <- dataNoShow()
    # data <- kpi.all.data[noshow.data.rows,]
    
    # sameDay <- data %>%
    #   group_by(Appt.Status) %>%
    #   summarise(value = round(n()/length(unique(kpi.all.data[arrived.data.rows,]$Appt.DateYear)))) %>%
    #   arrange(desc(value)) 
    
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = round(n()/length(unique(dataArrived()$Appt.DateYear)))) %>%
      arrange(desc(value)) 
    
    ggplot(sameDay, aes(reorder(Appt.Status, -value), value, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(sameDay$value))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Average Daily No Shows and Same-day \nBumped/Canceled/Rescheduled Appointments",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=value), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # ### Scheduling Section
  # output$fillRate_tb <- function(){
  #   # Booked and Filled Rate
  #   data <- dataPastSlot()
  #   # data <- past.slot.data
  # 
  #     daily.booked <- data %>%
  #     dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
  #                      `Booked Hours` = round(sum(BOOKED_MINUTES),0),
  #                      `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
  #                      `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
  #                      `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
  #     mutate(`Booked Rate` = paste0(round((`Booked Hours`/`Available Hours`),2)*100, "%"),
  #            `Filled Rate` = paste0(round((`Arrived Hours`/`Available Hours`),2)*100, "%")) %>%
  #     gather(variable, value, 1:7) %>%
  #     filter(variable %in% c("Booked Rate","Filled Rate")) %>%
  #     select(variable, value)
  # 
  #   
  #   # Scheduling Activity 
  #     
  #     data2 <- dataAll()
  #     # data2 <- all.data
  #     
  #     apptsCanceled <- data2 %>% 
  #       group_by(Appt.Status) %>%
  #       summarise(value = n()) %>%
  #       arrange(desc(value)) %>%
  #       mutate(percent = paste0(round((value/sum(value))*100),"%")) %>%
  #       select(Appt.Status, percent) %>%
  #       `colnames<-` (c("variable", "value"))
  #     
  #     # apptsCanceled$percent[apptsCanceled$Status == "No Show"] <- round((apptsCanceled$Total[apptsCanceled$Status == "No Show"] / (apptsCanceled$Total[apptsCanceled$Status == "Arrived"] + apptsCanceled$Total[apptsCanceled$Status == "No Show"])),2)
  #     
  #     scheduling_tb <- bind_rows(daily.booked, apptsCanceled)
  #     
  #     
  #     kable(scheduling_tb, col.names = NULL) %>%
  #       pack_rows("Slot Usage", 1, 2, label_row_css = "background-color: #fcc9e9;") %>%
  #       pack_rows("Scheduling Activity", 3, nrow(scheduling_tb), label_row_css = "background-color: #fcc9e9;") %>%
  #       kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE,
  #                     position = "center", row_label_position = "l", font_size = 18) %>%
  #       column_spec(1,  background = "#d80b8c", color = "white")
  # }
  
  
  ### Day of Visit Section
  
  output$avgCycleTime <- renderValueBox({
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(cycleTime > 0))$cycleTime, na.rm = TRUE))," min"),
      title = toupper("Avg. Check-in to Visit-End"),
      sparkobj = NULL,
      info = "Sample size (N) is the total number of arrived patients included in this analysis.\nPatients must have both check-in and check-out times > 0 min.",
      icon = NULL,
      subtitle = paste0("*Sample Size = ",round(nrow(dataArrived() %>% filter(!is.na(cycleTime) | cycleTime > 0))/nrow(dataArrived()),2)*100,"% of Total Arrived Patients"),
      width = 6,
      color = "yellow",
      href = NULL
    )
    
  })
  
  output$medCycleTime <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(median((dataArrived() %>% filter(cycleTime > 0))$cycleTime, na.rm = TRUE))," min"),
      title = toupper("Median Check-in to Visit-End"),
      sparkobj = NULL,
      info = "Sample size (N) is the total number of arrived patients included in this analysis.\nPatients must have both check-in and check-out times > 0 min.",
      icon = NULL,
      subtitle = paste0("*Sample Size = ",round(nrow(dataArrived() %>% filter(!is.na(cycleTime) | cycleTime > 0))/nrow(dataArrived()),2)*100,"% of Total Arrived Patients"),
      width = 6,
      color = "yellow",
      href = NULL
    )
    
  })
  
  output$cycleTimeBoxPlot <- renderPlot({
    
    data <- dataArrived() %>% filter(cycleTime > 0)
    # data <- arrived.data %>% filter(cycleTime > 0, Campus == "MSUS")
    
    ggplot(data, aes(x=cycleTime)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 22,
                     color="#212070", fill="#c7c6ef") +
      labs(title = "Distribution of Check-in to Visit-end Time", 
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  
  output$avgCheckinToRoomin <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0))$checkinToRoomin, na.rm = TRUE))," min"),
      title = toupper("Avg. Check-in to Room-in"),
      sparkobj = NULL,
      info = "Sample size (N) is the total number of arrived patients included in this analysis.\nPatients must have both check-in and room-in times >= 0.",
      icon = NULL,
      subtitle = paste0("*Sample Size = ",round(nrow(dataArrived() %>% filter(!is.na(checkinToRoomin) | checkinToRoomin >= 0))/nrow(dataArrived()),2)*100,"% of Total Arrived Patients"),
      width = 6,
      color = "yellow",
      href = NULL
    )
    
  })
  
  output$medCheckinToRoomin <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(median((dataArrived() %>% filter(checkinToRoomin >= 0))$checkinToRoomin, na.rm = TRUE))," min"),
      title = toupper("Median Check-in to Room-in"),
      sparkobj = NULL,
      info = "Sample size (N) is the total number of arrived patients included in this analysis.\nPatients must have both check-in and room-in times >= 0.",
      icon = NULL,
      subtitle = paste0("*Sample Size = ",round(nrow(dataArrived() %>% filter(is.na(checkinToRoomin) | checkinToRoomin <= 0))/nrow(dataArrived()),2)*100,"% of Total Arrived Patients"),
      width = 6,
      color = "yellow",
      href = NULL
    )
    
  })
  
  output$checkInRoomInBoxPlot <- renderPlot({
    data <- dataArrived() %>% filter(checkinToRoomin >= 0)
    # data <- arrived.data %>% filter(checkinToRoomin >= 0)
    
    ggplot(data, aes(x=checkinToRoomin)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 14,
                     color="#212070", fill="#c7c6ef") +
      labs(title = "Distribution of Check-in to Room-in Time", 
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      scale_x_continuous(breaks = seq(0, 210, 30), lim = c(0, 210))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  # output$avgProviderTime <- renderValueBox({
  #   
  #   # valueBox(
  #   #   paste0(round(mean(dataArrived()$providerinToOut, na.rm = TRUE),1)," min"),
  #   #   subtitle = tags$p("Avg Provider Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   # )
  #   # 
  #   valueBoxSpark(
  #     value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0))$checkinToRoomin, na.rm = TRUE))," | ",
  #                     round(median((dataArrived() %>% filter(checkinToRoomin >= 0))$checkinToRoomin, na.rm = TRUE))," min"),
  #     title = toupper("Avg | Median Check-in to Room-in"),
  #     sparkobj = NULL,
  #     info = "Sample size (N) is the total number of arrived patients included in this analysis.\nPatients must have both check-in and room-in times >= 0.",
  #     icon = NULL,
  #     subtitle = paste0("N =  ",round(nrow(dataArrived() %>% filter(is.na(checkinToRoomin) | checkinToRoomin <= 0))/nrow(dataArrived())),"% of Total Arrived Patients"),
  #     width = 6,
  #     color = "yellow",
  #     href = NULL
  #   )
  #   
  # })
  # 
  # output$avgCheckoutTime <- renderValueBox({
  #   
  #   valueBox(
  #     paste0(round(mean(dataArrived()$visitEndToCheckout, na.rm = TRUE),1)," min"),
  #     subtitle = tags$p("Avg Check-out Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   )
  #   
  # })
  
  # Provider Overview Tab ------------------------------------------------------------------------------------------------------------
  ## Value Boxes for Key Metrics
  
  output$vbox1 <- renderValueBox({
    
    valueBox(
      prettyNum(round(length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark=','),
      subtitle = tags$p("Avg. Patients Seen per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox2 <- renderValueBox({
    
    valueBox(
      paste0(round(nrow(dataArrived() %>% filter(New.PT3 == TRUE) %>% distinct(uniqueId)) / nrow(dataArrived()), 2)*100, "%"),
      subtitle = tags$p("New Patient Ratio (%)", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox3 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round(median((dataAll() %>% filter(New.PT3 == TRUE, Wait.Time >= 0))$Wait.Time)), big.mark=','), " days"),
      subtitle = tags$p("Median New Patient Wait Time", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox4 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round((sum(dataPastSlot()$`Available Hours`) / length(unique(dataPastSlot()$Appt.DateYear)))), big.mark=','), " hrs"),
      subtitle = tags$p("Avg. Hrs Available per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox5 <- renderValueBox({
    
    valueBox(
      # round(((sum(dataPastSlot()$`Available Hours`)*60) / length(unique(dataPastSlot()$Appt.DateYear))) /
      #         (length(unique(dataArrived()$uniqueId)) / length(unique(dataArrived()$Appt.DateYear))), 1),
      paste0(round(mean(dataArrivedNoShow()$Appt.Dur))," min"),
      subtitle = tags$p("Avg. Scheduled Duration per Visit", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox6 <- renderValueBox({
    
    valueBox(
      prettyNum(round(nrow(dataBumped()) / length(unique(dataArrived()$Appt.DateYear))), big.mark=','),
      subtitle = tags$p("Avg. Same-day Bumps per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  
  output$provDailySchedule <- renderPlot({
    
    if(input$provSchedulingChoice == 1) { # Arrived
      
      data <- dataArrived()
      # data <- arrived.data %>% filter(Campus == "MSUS")
      # nrow(data)
      
      daily.arrived <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(Total = n())  
      
      sum(daily.arrived$Total)
      
      days <- unique(data[,c("Appt.Day","Appt.DateYear")])
      days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
      
      daily.arrived$days <- days$days[match(daily.arrived$Appt.Day, days$Appt.Day)]
      daily.arrived$avgArrived <- round(daily.arrived$Total/daily.arrived$days,0)
      
      daily.arrived.df <- byDayTime.df %>% filter(Day %in% unique(daily.arrived$Appt.Day))
      daily.arrived.df <- merge(daily.arrived.df, daily.arrived, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.arrived.df <- daily.arrived.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.arrived.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=avgArrived), colour = "black", size=0.5)+
        labs(x=NULL, y=NULL,
             title = "Average Daily Patients Arrived",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#5753d0", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Patients Arrived")+
        scale_x_discrete(position = "top")+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 14),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.key.size = unit(1,"cm"),
              legend.text = element_text(size="12"),
              axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
              axis.text.y = element_text(color= "black", margin = margin(r=15)),
              axis.text = element_text(size="14"),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        geom_text(aes(label= ifelse(is.na(avgArrived),"",avgArrived)), color="black", size=6)
      
    } else if(input$provSchedulingChoice == 2) { # No Show
      
      data <- dataArrivedNoShow() %>% filter(Appt.Status %in% c("No Show"))
      
      daily.arrivedNoShow <- data %>%
        group_by(Appt.Day, Appt.TM.Hr, Appt.Status) %>%
        dplyr::summarise(Total = n()) %>%
        spread(Appt.Status, Total)
      
      daily.arrivedNoShow$`No Show`[is.na(daily.arrivedNoShow$`No Show`)] <- 0
      
      days <- unique(data[,c("Appt.Day","Appt.DateYear")])
      days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
      
      daily.arrivedNoShow$days <- days$days[match(daily.arrivedNoShow$Appt.Day, days$Appt.Day)]
      daily.arrivedNoShow$avgNoShow <- round(daily.arrivedNoShow$`No Show`/daily.arrivedNoShow$days,0)
      
      daily.arrivedNoShow.df <- byDayTime.df %>% filter(Day %in% unique(daily.arrivedNoShow$Appt.Day))
      daily.arrivedNoShow.df <- merge(daily.arrivedNoShow.df, daily.arrivedNoShow, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.arrivedNoShow.df <- daily.arrivedNoShow.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.arrivedNoShow.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=avgNoShow), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Show Patients")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily No Show Patients",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 14),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.key.size = unit(1,"cm"),
              legend.text = element_text(size="12"),
              axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
              axis.text.y = element_text(color= "black", margin = margin(r=15)),
              axis.text = element_text(size="14"),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        geom_text(aes(label= ifelse(is.na(avgNoShow),"",avgNoShow)), color="black", size=6)
      
    } else if(input$provSchedulingChoice == 3) { # Overbooks
      
      data <- dataArrivedNoShow()
      
      daily.overbooks <- data %>%
        group_by(Provider, Appt.DateYear, Time, Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(total = n()) %>%
        filter(total > 1) %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(total = sum(total))
      
      days <- unique(data[,c("Appt.Day","Appt.DateYear")])
      days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
      
      daily.overbooks$days <- days$days[match(daily.overbooks$Appt.Day, days$Appt.Day)]
      daily.overbooks$avgOverbooks <- round(daily.overbooks$total/daily.overbooks$days,0)
      
      daily.overbooks.df <- byDayTime.df %>% filter(Day %in% unique(daily.overbooks$Appt.Day))
      daily.overbooks.df <- merge(daily.overbooks.df, daily.overbooks, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.overbooks.df$avgOverbooks[is.na(daily.overbooks.df$avgOverbooks)] <- 0
      
      daily.overbooks.df <- daily.overbooks.df %>% filter(Time %in% timeOptionsHr_filter)
      
      
      ggplot(daily.overbooks.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=avgOverbooks), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "#63be7b", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Overbooks*")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily Overbooks",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
             caption = "All Arrived/No Show Appointments Scehduled at same times.")+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 14),
              plot.caption = element_text(size = 12, face = "italic"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.key.size = unit(1,"cm"),
              legend.text = element_text(size="12"),
              axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
              axis.text.y = element_text(color= "black", margin = margin(r=15)),
              axis.text = element_text(size="14"),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        geom_text(aes(label= ifelse(is.na(avgOverbooks),"",avgOverbooks)), color="black", size=6)
      
    } else if(input$provSchedulingChoice == 4) { # Booked Rate (%)
      
      data <- dataPastSlot()
      # data <- slot.data.subset[past.slot.data.rows,] %>% filter(Campus == "MSUS")
      
      daily.booked <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2) * 100,
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2) * 100) 
      
      daily.booked.df <- byDayTime.df %>% filter(Day %in% unique(daily.booked$Appt.Day))
      daily.booked.df <- merge(daily.booked.df, daily.booked, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.booked.df <- daily.booked.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.booked.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=`Booked Rate`), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Booked Rate (%)")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily Booked Rate",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 14),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.key.size = unit(1,"cm"),
              legend.text = element_text(size="12"),
              axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
              axis.text.y = element_text(color= "black", margin = margin(r=15)),
              axis.text = element_text(size="14"),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        geom_text(aes(label= ifelse(is.na(`Booked Rate`),"",paste0(`Booked Rate`,"%"))), color="black", size=6)
      
    } else { # Filled Rate (%)
      
      data <- dataPastSlot()
      # data <- past.slot.data %>% filter(Campus == "MSUS")
      
      daily.filled <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(`Available Hours` = sum(`Available Hours`),
                  `Booked Hours` = sum(`Booked Hours`),
                  `Arrived Hours` = sum(`Arrived Hours`)) %>%
        mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2) * 100,
               `Filled Rate` = round(`Arrived Hours`/`Available Hours`, 2) * 100) 
      
      daily.filled.df <- byDayTime.df %>% filter(Day %in% unique(daily.filled$Appt.Day))
      daily.filled.df <- merge(daily.filled.df, daily.filled, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.filled.df <- daily.filled.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.filled.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=`Filled Rate`), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Filled Rate (%)")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily Filled Rate",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 14),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.key.size = unit(1,"cm"),
              legend.text = element_text(size="12"),
              axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
              axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
              axis.text.y = element_text(color= "black", margin = margin(r=15)),
              axis.text = element_text(size="14"),
              panel.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank())+
        geom_text(aes(label= ifelse(is.na(`Filled Rate`),"",paste0(`Filled Rate`,"%"))), color="black", size=6)
      
    }
    
  })
  
  # Average Daily Appts Scheduled
  output$provScheduledAppts <- renderValueBox({
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      prettyNum(round(nrow(dataArrivedNoShow())/length(unique(dataArrivedNoShow()$Appt.DateYear))), big.mark = ","),
      subtitle = tags$p("Avg. Appointments Scheduled per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Average Daily Incomplete Appts
  output$provIncompleteAppts <- renderValueBox({
    
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      paste0(prettyNum(round(nrow(dataArrivedNoShow() %>% filter(Appt.Status != "Arrived")) / 
                               nrow(dataArrivedNoShow()), 2)*100, big.mark = ","),"%"),
      subtitle = tags$p("% of Incomplete Appointments", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Total No Shows per Day
  output$provNoShow <- renderValueBox({
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique((dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear)),0), big.mark = ","),
      subtitle = tags$p("Average No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "fuchsia"
    )
    
  })
  
  
  # % No Shows per Day
  output$provNoShowPerc <- renderValueBox({
    valueBox(
      paste0(round((nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show")))) / 
                     (nrow(dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived", "No Show")))), 2) *100, "%"),
      subtitle = tags$p("No Show Rate (%)", style = "font-size: 130%;"), icon = NULL, color = "fuchsia"
    )
    
  })
  
  
  output$provNoShowPie <- renderPlot({
    
    data <- dataNoShow()
    # data <- kpi.all.data[noshow.data.rows,]
    
    noShows <- data %>%
      group_by(Appt.Status) %>%
      summarise(total = n()) %>%
      mutate(avg = round(total/length(unique((dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear))),
             name = "appt.status")
    
    noShows$Appt.Status[which(noShows$Appt.Status == "Bumped")] <- "Same-day Bumped"
    noShows$Appt.Status[which(noShows$Appt.Status == "Canceled")] <- "Same-day Canceled"
    noShows$Appt.Status[which(noShows$Appt.Status == "Rescheduled")] <- "Same-day Rescheduled"
    
    ggplot(noShows, aes(x = name, y = avg, fill = Appt.Status))+
      geom_col()+
      geom_text(aes(label = prettyNum(avg, big.mark = ",")),
                position = position_stack(vjust = 0.5), color="white", fontface="bold", size = 7)+
      # stat_summary(fun.y = sum, vjust = 1, hjust =0, aes(label=ifelse(..y.. == 0,"",..y..), group =name), geom="text", color="black",
      #              size=7, fontface="bold.italic")+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      # scale_y_continuous(limits=c(0,sum(noShows$avg)*1.2))+
      labs(x=NULL, y=NULL,
           title = paste0("Average Daily Incomplete Appointments Breakdown"),
           subtitle = paste0("Total per Day: ", prettyNum(sum(noShows$avg), big.mark = ','),
                             "\nBased on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = "14"),
        strip.text = element_text(size=14),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle=0, hjust=0.5),
        axis.text.y = element_blank(),
        axis.line.x = element_blank())+
      coord_flip()+
      guides(colour = guide_legend(nrow = 1))
    
  })
  
  
  output$provCoverage <- function(){
    
    data <- dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- arrivedNoShow.data
    
    major <- data %>% 
      group_by(Coverage) %>%
      dplyr::summarise(Total = n()) %>% 
      arrange(desc(Total)) %>% 
      mutate(percent = round((Total / sum(Total))*100)) %>%
      filter(percent >= 5) %>%
      mutate(percent = paste0(percent, "%"))# Filter out coverage
    
    noShow <- data %>% 
      group_by(Coverage, Appt.Status) %>%
      dplyr::summarise(Total = n()) %>%
      spread(Appt.Status, Total) %>%
      mutate(noShow = round((`No Show`/ (Arrived + `No Show`))*100)) 
    
    # noShow$noShow <- color_tile("white", "red")(noShow$noShow)
    noShow$noShow <- paste0(noShow$noShow, "%")
    
    major$noShow <- noShow$noShow[match(major$Coverage, noShow$Coverage)]
    major$Coverage[is.na(major$Coverage)] <- "Unknown"
    
    kable(major %>% select(Coverage, percent, noShow),
          col.names = c("Coverage Type", "% of Total Patients Scheduled \n(Arrived + No Show)", "No Show %")) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE,
                    position = "center", row_label_position = "l", font_size = 18) %>%
      row_spec(0, background = "#d80b8c", color = "white") %>%
      column_spec(1, width = "40%", bold = T) %>%
      column_spec(2, width = "30%", bold = T) %>%
      column_spec(3, width = "15%", color = "red", bold = T)
    
  }
  
  
  
  # Total Bumps per Day
  output$provBumps <- renderValueBox({
    
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("Bumped"))) / length(unique((dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear)),0), big.mark = ","),
      subtitle = tags$p("Average Same-day Bumps per Day", style = "font-size: 130%;"), icon = NULL, color = "aqua"
    )
    
  })
  
  # % No Shows per Day
  output$provBumpsPerc <- renderValueBox({
    
    valueBox(
      paste0(round((nrow(dataNoShow() %>% filter(Appt.Status %in% c("Bumps")))) / 
                     (nrow(dataArrivedNoShow())), 2) *100, "%"),
      subtitle = tags$p("Bumped Rate (%)", style = "font-size: 130%;"), icon = NULL, color = "aqua"
    )
    
  })
  
  ## Lead Days to Bumps
  output$provBumpLeadDays <- renderPlot({
    
    data <- dataBumped()
    # data <- bumped.data
    
    lead.days.df <- data %>%
      filter(Lead.Days >= 0) %>%
      mutate(leadDays = ifelse(Lead.Days> 14, "> 14 days", 
                               ifelse(Lead.Days <= 14 & Lead.Days >= 8, "8-14 days",
                                      ifelse(Lead.Days < 8 & Lead.Days >= 1, "1-7 days",
                                             ifelse(Lead.Days < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Appt.Status) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Status) %>%
      mutate(perc = total/sum(total))
    
    
    ggplot(lead.days.df, aes(x=Appt.Status, y=perc, fill=factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days"))))+
      geom_col()+
      geom_text(aes(label = paste0(round(perc, 2)*100, "%")),
                position = position_stack(vjust = 0.5), color="white", fontface="bold", size=7)+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      labs(x=NULL, y=NULL,
           title = "% of Bumps by Lead Days to Appt Cancellation",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = "14"),
        strip.text = element_text(size=14),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle=0, hjust=0.5),
        axis.text.y = element_blank(),
        axis.line.x = element_blank())+
      coord_flip()+
      guides(colour = guide_legend(nrow = 1))
    
  })
  
  
  # output$provNoShowPie <- renderPlot({
  #   
  #   data <- no
  #   apptsCanceled <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status), FUN=NROW)
  #   names(apptsCanceled) <- c("Status","Total")
  #   
  #   apptsCanceled <- apptsCanceled %>% filter(Status %in% c("Rescheduled","Canceled","No Show","Bumped","Scheduled","Arrived")) %>%
  #     mutate(Percent = as.numeric(round((Total / sum(Total)),2)))
  #   
  #   apptsCanceled$percent[apptsCanceled$Status == "No Show"] <- round((apptsCanceled$Total[apptsCanceled$Status == "No Show"] / (apptsCanceled$Total[apptsCanceled$Status == "Arrived"] + apptsCanceled$Total[apptsCanceled$Status == "No Show"])),2)
  #   
  #   
  #   ggplot(apptsCanceled, aes(reorder(Status, Percent),Percent, fill=Status)) +
  #     geom_bar(stat="identity", width = 0.8) +
  #     scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$Percent))*1.5))+
  #     scale_fill_manual(values=MountSinai_pal("all")(10))+
  #     labs(x=NULL, y=NULL, title = NULL,
  #          plot.title = "Appointment Status Breakdown",
  #          subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
  #          caption = "% No Show = No Show / No Show and Arrived\n
  #          Based breakdown of all appointments by appointment status")+
  #     coord_flip()+
  #     theme_new_line()+
  #     theme(
  #       plot.subtitle = element_text(hjust=0.5, size = 14),
  #       plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
  #       legend.position = "none",
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_text(size = "16"),
  #       axis.line.x = element_blank())+
  #     geom_text(aes(label=paste0(round(Percent*100),"%")), hjust = -.5, color="black", fontface="bold",
  #               position = position_dodge(1), size=5)
  #   
  # })
  
  
  
  # output$provApptStatusPie <- renderPlot({
  # 
  #   data <- dataAll()
  #   # data <- all.data
  # 
  #   apptsCanceled <- data %>%
  #     group_by(Appt.Status) %>%
  #     summarise(value = n()) %>%
  #     arrange(desc(value)) %>%
  #     mutate(percent = round((value/sum(value)), 2)) %>%
  #     select(Appt.Status, percent) 
  # 
  #   ggplot(apptsCanceled, aes(reorder(Appt.Status, -percent), percent, fill=Appt.Status)) +
  #     geom_bar(stat="identity", width = 0.8) +
  #     scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$percent))*1.5))+
  #     scale_fill_manual(values=MountSinai_pal("all")(10))+
  #     labs(x=NULL, y=NULL, 
  #          title = "Scheduling Activity Breakdown")+
  #     theme_new_line()+
  #     theme_bw()+
  #     graph_theme("none")+
  #     geom_text(aes(label=paste0(round(percent*100),"%")), hjust = 0.5, vjust = -1, color="black", fontface="bold",
  #               position = position_dodge(1), size=5)
  # 
  # })
  
  # 
  # output$provSlotUsagesAvg <- renderPlot({
  #   
  #   data <- dataPastSlot()
  #   # data <- past.slot.data %>% filter(Campus == "MSUS")
  #   
  #   # slot.usage <- data %>%
  #   #   dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
  #   #                    `Booked Hours` = round(sum(BOOKED_MINUTES),0),
  #   #                    `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
  #   #                    `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
  #   #                    `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
  #   #   mutate(`Slot Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
  #   #          `Slot Filled Rate` = round((`Arrived Hours`/`Available Hours`),2),
  #   #          `Slot No Show Rate` = round((`No Show Hours`/`Booked Hours`), 2),
  #   #          `Slot Canceled Rate` = round((`No Show Hours`/`Booked Hours`), 2)) %>%
  #   #   gather(variable, value, 1:7)
  #   # 
  #   
  #   slot.usage <- data %>%
  #     dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
  #                      `Booked Hours` = round(sum(BOOKED_MINUTES),0),
  #                      `Arrived Hours` = round(sum(ARRIVED_MINUTES),0)) %>%
  #     mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`),2),
  #            `Filled Rate (%)` = round((`Arrived Hours`/`Available Hours`),2),
  #            `Unused Rate (%)` = 1 - `Filled Rate (%)`) %>%
  #     gather(variable, value, 1:6) %>%
  #     group_by(variable) %>%
  #     filter(variable %in% c("Unused Rate (%)","Filled Rate (%)","Booked Rate (%)"))
  #   
  #   ggplot(slot.usage, aes(x=factor(x=variable, level= c("Unused Rate (%)","Filled Rate (%)","Booked Rate (%)")), y=value, fill=variable)) +
  #     geom_bar(stat="identity", width=0.8, color="white") +
  #     scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(slot.usage$value)*1.2)) +
  #     labs(x = NULL, y = NULL,
  #          title = "Average Booked vs. Filled Rate",
  #          subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
  #     scale_fill_manual(values = c("#212070","#d80b8c","#dddedd")) +
  #     theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
  #           plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
  #           legend.position = "none",
  #           axis.title.y = element_blank(),
  #           axis.title.x =  element_text(color= "black"),
  #           axis.text.x = element_text(size = "14"),
  #           axis.text.y = element_blank(),
  #           panel.background = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.grid.major = element_blank(),
  #           plot.margin = margin(30,30,30,30)) +
  #     geom_text(aes(label= paste0(value*100,"%")), hjust=0.5, vjust=-1, color="black", size=6) 
  #   # annotate(geom="label",x=.7,y=dept.booked.avg,label="Avg. Dept\nBooked Rate", fill = "white", color = "#212070", size=5) +
  #   # annotate(geom="label",x=1.3,y=dept.filled.avg,label="Avg. Dept\nFilled Rate", fill = "white", color = "#d80b8c", size=5)
  #   
  # })
  
  
  
  ### KPIs Tab ------------------------------------------------------------------------------------------------------------------------
  # Volume KPI ====================================================================
  output$kpiVolumeGraph <- renderPlot({
    kpiVolumeData <- aggregate(dataArrivedKpi()$uniqueId, by=list(dataArrivedKpi()$Appt.Year,dataArrivedKpi()$Appt.Quarter,
                                                                  dataArrivedKpi()$Appt.Month, dataArrivedKpi()$Appt.Date, dataArrivedKpi()$Appt.MonthYear, dataArrivedKpi()$Appt.DateYear), FUN=NROW)
    
    # arrived.data <- kpi.all.data[arrived.data.rows,]
    # kpiVolumeData <- aggregate(arrived.data$uniqueId, by=list(arrived.data$Appt.Year,arrived.data$Appt.Quarter,
    #                                                          arrived.data$Appt.Month, arrived.data$Appt.Date, arrived.data$Appt.MonthYear, arrived.data$Appt.DateYear), FUN=NROW)
    
    colnames(kpiVolumeData) <- c("Year","Quarter","Month","Date","YearMonth","DateYear","Volume")
    kpiVolumeData$DateYear <-as.Date(kpiVolumeData$DateYear, "%Y-%m-%d")
    
    kpiVolumeDataYear <- kpiVolumeData %>% group_by(Year) %>% dplyr::summarise(Total = round(sum(Volume)))
    kpiVolumeDataQuarter <- kpiVolumeData %>% group_by(Year, Quarter) %>% dplyr::summarise(Total = round(sum(Volume)))
    kpiVolumeDataMonth <- kpiVolumeData %>% group_by(Year, Month, YearMonth) %>% dplyr::summarise(Total = round(sum(Volume))) %>% arrange(YearMonth)
    
    if(input$kpiTrend ==1){
      if(input$kpiFreq == 1){ #Year
        ggplot(kpiVolumeDataYear, aes(x=Year, y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Patients",
               title = "Historical Trend of Patient Volume by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataYear$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none") + theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(kpiVolumeDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Patients",
               title = "Historical Trend of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataQuarter$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else if(input$kpiFreq == 3){ # Month
        # kpiVolumeDataMonth$YearMonth <- as.yearmon(kpiVolumeDataMonth$YearMonth, "%Y-%m")
        # kpiVolumeDataMonth$YearMonth <- lapply(kpiVolumeDataMonth$YearMonth, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')})
        # kpiVolumeDataMonth$YearMonth <- paste(sapply(strsplit(kpiVolumeDataMonth$YearMonth, "\\s+"), rev), collapse= '-')
        
        
        ggplot(kpiVolumeDataMonth, aes(x=YearMonth, y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          geom_smooth(method='lm', col = "red", se=FALSE, size=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Historical Trend of Patient Volume by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataMonth$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none") 
        
      } else { # Day
        ggplot(kpiVolumeData, aes(x=DateYear, y=Volume,group=1)) +
          geom_line(color="midnightblue") +
          geom_smooth(method='lm', col = "red", se=FALSE, size=0.5)+
          labs(x = NULL, y = "Patients",
               title = "Historical Trend of Patient Volume by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeData$Volume)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
      
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(kpiVolumeDataYear %>% mutate(Label = "Year"), aes(x=Label, y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataYear$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top") + theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(kpiVolumeDataQuarter, aes(x=Quarter, y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataQuarter$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(kpiVolumeDataMonth, aes(x = factor(x=Month, level= monthOptions), y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataMonth$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(kpiVolumeData, aes(x=as.Date(DateYear,"%m-%d"), y=Volume, col=Year,group=Year)) +
          geom_line() +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeData$Volume)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.title.x = element_blank()) +
          scale_color_MountSinai("main")+
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  # Appt Status KPI ========================================================================
  output$kpiApptStatusGraph <- renderPlot({
    statusData <- dataAll() %>% 
      group_by(Appt.Year, Appt.Quarter, Appt.Month, Appt.Date, Appt.Status, Appt.MonthYear, Appt.DateYear) %>%
      summarise(total = n()) %>%
      `colnames<-` (c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count"))
    
    # statusData <- kpi.all.data[all.data.rows,] %>%
    #   group_by(Appt.Year, Appt.Quarter, Appt.Month, Appt.Date, Appt.Status, Appt.MonthYear, Appt.DateYear) %>%
    #   summarise(total = n()) %>%
    #   `colnames<-` (c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count"))
    
    statusDataYear <- statusData %>% group_by(Year,Status) %>% dplyr::summarise(Total = round(sum(Count)))
    statusDataYear <- reshape2::dcast(statusDataYear, Year ~ Status)
    statusDataYear[is.na(statusDataYear)] <- 0
    statusDataYear <- statusDataYear %>%
      mutate(Cancelled = Canceled / rowSums(statusDataYear[,2:6])) %>%
      mutate(Bumped = Bumped / rowSums(statusDataYear[,2:6])) %>%
      mutate(NoShow = `No Show` / rowSums(statusDataYear[,c("Arrived","No Show")])) %>%
      dplyr::select(c("Year","Cancelled","Bumped","NoShow"))
    statusDataYear[is.na(statusDataYear)] <- 0
    statusDataYear <- reshape2::melt(statusDataYear, id.vars = c("Year"))
    
    statusDataQuarter <- statusData %>% group_by(Year, Quarter, Status) %>% dplyr::summarise(Total = round(sum(Count)))
    statusDataQuarter <- reshape2::dcast(statusDataQuarter, Year + Quarter ~ Status)
    statusDataQuarter[is.na(statusDataQuarter)] <- 0
    statusDataQuarter <- statusDataQuarter %>%
      mutate(Cancelled = Canceled / rowSums(statusDataQuarter[,3:7])) %>%
      mutate(Bumped = Bumped / rowSums(statusDataQuarter[,3:7])) %>%
      mutate(NoShow = `No Show` / rowSums(statusDataQuarter[,c("Arrived","No Show")])) %>%
      dplyr::select(c("Year","Quarter","Cancelled","Bumped","NoShow"))
    statusDataQuarter[is.na(statusDataQuarter)] <- 0
    statusDataQuarter <- reshape2::melt(statusDataQuarter, id.vars = c("Year","Quarter"))
    
    statusDataMonth <- statusData %>% group_by(Year, Month, YearMonth, Status) %>% dplyr::summarise(Total = round(sum(Count)))
    statusDataMonth <- reshape2::dcast(statusDataMonth, Year + Month + YearMonth ~ Status)
    statusDataMonth[is.na(statusDataMonth)] <- 0
    statusDataMonth <- statusDataMonth %>%
      mutate(Cancelled = Canceled / rowSums(statusDataMonth[,4:length(statusDataMonth)])) %>%
      mutate(Bumped = Bumped / rowSums(statusDataMonth[,4:length(statusDataMonth)])) %>%
      mutate(NoShow = `No Show` / rowSums(statusDataMonth[,c("Arrived","No Show")])) %>%
      dplyr::select(c("Year","Month","YearMonth","Cancelled","Bumped","NoShow"))
    statusDataMonth[is.na(statusDataMonth)] <- 0
    statusDataMonth <- reshape2::melt(statusDataMonth, id.vars = c("Year","Month","YearMonth"))
    
    statusDataDay <- statusData %>% group_by(Year, Date, DateYear, Status) %>% dplyr::summarise(Total = round(sum(Count)))
    statusDataDay <- reshape2::dcast(statusDataDay, Year + Date + DateYear ~ Status)
    statusDataDay[is.na(statusDataDay)] <- 0
    statusDataDay <- statusDataDay %>%
      mutate(Cancelled = Canceled / rowSums(statusDataDay[,4:length(statusDataDay)])) %>%
      mutate(Bumped = Bumped / rowSums(statusDataDay[,4:length(statusDataDay)])) %>%
      mutate(NoShow = `No Show` / rowSums(statusDataDay[,c("Arrived","No Show")])) %>%
      dplyr::select(c("Year","Date","DateYear","Cancelled","Bumped","NoShow"))
    statusDataDay[is.na(statusDataDay)] <- 0
    statusDataDay <- reshape2::melt(statusDataDay, id.vars = c("Year","Date","DateYear"))
    
    if(input$kpiTrend ==1){
      if(input$kpiFreq == 1){ #Year
        ggplot(statusDataYear, aes(x=Year, y=value,col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          facet_wrap(variable~., dir = "v", scales = "free")+
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(statusDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=YearMonth, y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_color_MountSinai("main")
        
      } else { # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%Y-%m-%d"), y=value, col=variable, group=variable)) +
          geom_line() +
          labs(x = NULL, y = NULL,  
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_color_MountSinai("main")+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else {
      if(input$kpiFreq == 1){ # Year
        ggplot(statusDataYear %>% mutate(Label = "Year"), aes(x=Label, y=value, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter
        ggplot(statusDataQuarter, aes(x=Quarter, y=value, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=factor(Month, level = monthOptions), y=value, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%m-%d"), y=value, col=Year,group=Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., dir = "v", scales = "free")+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main")+
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
        
      }
    }
    
  })
  
  
  # Access KPI ========================================================================
  ## Avg New Wait Time
  output$kpiNewWaitTimeGraph <- renderPlot({
    data <- dataAllKpi() 
    # data <- kpi.all.data %>% filter(Campus == "MSUS")
    
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"), 2))
    data <- data %>% filter(New.PT3 == TRUE) %>% filter(wait.time >= 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE)))
        ggplot(data_filter, aes(x=Appt.Year, y=mean, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Average Wait Time to New Appointment by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>%
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Average Wait Time to New Appointment by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none") 
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.MonthYear) %>% 
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.MonthYear,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days", 
               title = "Average Wait Time to New Appointment by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else { # Day
        data_filter <- data %>% group_by(Appt.Year, Appt.Date) %>%
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE)))
        data_filter$DateYear <- as.Date(with(data_filter, paste(Appt.Year, Appt.Date,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean, group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Average Wait Time to New Appointment by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
          mutate(Label = "Year")
        ggplot(data_filter, aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days", 
               title = "Average Wait Time to New Appointment by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>% 
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter")
        ggplot(data_filter, aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days", 
               title = "Average Wait Time to New Appointment by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.Year, Appt.Month) %>% 
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
          mutate(Label = "Month") #%>%
        #rename(Appt.Year = Year)
        
        ggplot(data_filter, aes(x = factor(Appt.Month, level = monthOptions), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days",
               title = "Average Wait Time to New Appointment by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main") +
          scale_fill_manual(name = "Year")
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(Appt.DateYear, Appt.Year) %>%
          dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>%
          mutate(Label = "Date")
        ggplot(data_filter, aes(x = Appt.DateYear, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = "Days",
               title = "Average Wait Time to New Appointment by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          coord_cartesian(clip = 'off') +
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+
          scale_color_MountSinai("main") +
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  
  # Day of Visit KPI ========================================================================
  ## Avg Check-in to Visit-end
  output$kpiCycleTimeGraph <- renderPlot({
    data <- dataArrivedKpi() %>% filter(cycleTime > 0)
    # data <- kpi.arrived.data %>% filter(cycleTime > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE)))
        ggplot(data_filter, aes(x=Appt.Year, y=mean, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Visit-End Time (Min.) by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Visit-End Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.MonthYear) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.MonthYear,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Visit-End Time (Min.) by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else { # Day
        data_filter <- data %>% group_by(Appt.Year, Appt.Date) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE)))
        data_filter$DateYear <- as.Date(with(data_filter, paste(Appt.Year, Appt.Date,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean,group=1)) +
          #ggplot(data_filter, aes(x=interaction(Appt.Year,as.Date(Appt.Date, format="%Y-%m-%d"),lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Time (min)",  
               title = "Average Check-in to Visit-End Time (Min.) by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
          mutate(Label = "Year")
        ggplot(data_filter, aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)",  
               title = "Average Check-in to Visit-End Time (Min.) by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter")
        ggplot(data_filter, aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Visit-End Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.Year, Appt.Month) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
          mutate(Label = "Month")
        ggplot(data_filter, aes(x = factor(Appt.Month, level = monthOptions), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)",
               title = "Average Check-in to Visit-End Time (Min.) by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(Appt.Year, Appt.DateYear) %>% 
          dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
          mutate(Label = "Date")
        ggplot(data_filter, aes(x = Appt.DateYear, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Visit-End Time (Min.) by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main") +
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
        
      }
    }
    
  })
  
  ## Check-in to Room-in Wait Time
  output$kpiWaitTimeGraph <- renderPlot({
    data <- dataArrivedKpi() %>% filter(checkinToRoomin > 0)
    # data <- kpi.arrived.data %>% filter(checkinToRoomin > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE)))
        ggplot(data_filter, aes(x=Appt.Year, y=mean, group=1)) +
          #stat_summary(fun.y="mean", geom="line")+
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.MonthYear) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE)))
        ggplot(data_filter, aes(x=interaction(Appt.MonthYear,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else { # Day
        data_filter <- data %>% group_by(Appt.Year, Appt.Date) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE)))
        data_filter$DateYear <- as.Date(with(data_filter, paste(Appt.Year, Appt.Date,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
          mutate(Label = "Year")
        ggplot(data_filter, aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(Appt.Year, Appt.Quarter) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter")
        ggplot(data_filter, aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(Appt.Year, Appt.Month) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
          mutate(Label = "Month")
        ggplot(data_filter, aes(x = factor(Appt.Month, level = monthOptions), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(Appt.Year, Appt.DateYear) %>% 
          dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
          mutate(Label = "Date")
        ggplot(data_filter, aes(x = Appt.DateYear, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time (Min.) by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main") + 
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  ### Scheduling Tab -------------------------------------------------------------------------------------------------------------------
  # Scheduling Summary
  # Average Daily Appts Scheduled
  output$scheduledAppts <- renderValueBox({
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      prettyNum(round(nrow(dataArrivedNoShow())/length(unique(dataArrivedNoShow()$Appt.DateYear))), big.mark = ","),
      subtitle = tags$p("Avg. Appointments Scheduled per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Average Daily Incomplete Appts
  output$incompleteAppts <- renderValueBox({
    
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      prettyNum(round(nrow(dataArrivedNoShow() %>% filter(Appt.Status != "Arrived")) / 
                        nrow(dataArrivedNoShow()), 2)*100, big.mark = ","),
      subtitle = tags$p("% of Incomplete Appointments", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$schedulingStatusSummary <- renderPlot({
    
    data <- dataArrivedNoShow()
    # data <- kpi.all.data[arrivedNoShow.data.rows,]
    
    # sameDay <- data %>%
    #   group_by(Appt.Status) %>%
    #   summarise(value = round(n()/length(unique(kpi.all.data[arrived.data.rows,]$Appt.DateYear)))) %>%
    #   arrange(desc(value)) 
    
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = round(n()/length(unique(dataArrived()$Appt.DateYear)))) %>%
      arrange(desc(value)) 
    
    sameDay$Appt.Status <- as.character(sameDay$Appt.Status)
    
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Bumped")] <- "Same-day Bumped"
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Canceled")] <- "Same-day Canceled"
    sameDay$Appt.Status[which(sameDay$Appt.Status == "Rescheduled")] <- "Same-day Rescheduled"
    
    ggplot(sameDay, aes(reorder(Appt.Status, -value), value, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(sameDay$value))*1.3))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Average Daily No Shows and Same-day \nBumped/Canceled/Rescheduled Appointments",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=prettyNum(value, big.mark = ',')), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # Scheduled Patients
  output$scheduledPts <- renderPlot({
    data <- dataArrivedNoShow()
    # data <- kpi.all.data[arrivedNoShow.data.rows,]
    
    data <- data %>%
      group_by(Appt.DateYear, Appt.TM.Hr, Appt.Status) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.TM.Hr, Appt.Status) %>%
      dplyr::summarise(avg = round(mean(total)))
    
    data <- dcast(data, Appt.TM.Hr ~ Appt.Status, sum)
    
    byTime.df <- as.data.frame(byTime.df[which(byTime.df$Time %in% unique(data$Appt.TM.Hr)),])
    colnames(byTime.df) <- "Time"
    
    data <- as.data.frame(merge(byTime.df,data, by.x = c("Time"), by.y = c("Appt.TM.Hr"), all.x = TRUE, all.y = TRUE))
    
    #data[is.na(data)] <- 0
    data <- reshape2::melt(data, id=c("Time"))
    
    data$variable <- as.character(data$variable)
    
    data$variable[which(data$variable == "Bumped")] <- "Same-day Bumped"
    data$variable[which(data$variable == "Canceled")] <- "Same-day Canceled"
    data$variable[which(data$variable == "Rescheduled")] <- "Same-day Rescheduled"
    
    data <- data %>% filter(Time %in% timeOptionsHr_filter)
    
    #data <- data %>%
    # arrange(desc(variable))
    
    totals <- data %>%
      group_by(Time) %>%
      summarize(value = sum(value))
    
    
    ggplot(data, aes(x=Time, y=value, fill=factor(variable, levels=c("Same-day Bumped", 
                                                                     "Same-day Canceled", 
                                                                     "Same-day Rescheduled", 
                                                                     "No Show","Arrived"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai(reverse = TRUE)+
      labs(x = NULL, y = "Pateints",
           title = "Average Patients Arrived",
           subtitle = paste0("Based on scheduled appointment time from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      scale_y_continuous(expand = c(0, 0), limits = c(0,max(totals$value)*1.5))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(legend.title = element_blank())+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=ifelse(value < max(value)*0.1," ",value)), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Time), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  # Arrived Patients 
  output$arrivedPts <- renderPlot({
    data <- dataArrived()
    # data <- arrived.data
    
    arrived <- data %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(avg = round(mean(total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(arrived$Day)),]
    
    arrived <- as.data.frame(merge(byDayTime.df,arrived, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))
    arrived[is.na(arrived)] <- 0
    
    arrived <- arrived %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- 
      ggplot(arrived, aes(x=Time, y=avg, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      scale_y_continuous(expand = c(0, 0), limits = c(0,max(arrived$avg)*1.2))+
      labs(x = NULL, y = "Pateints",
           title = "Average Patients Arrived* by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*Based on scheduled appointment time.")+
      scale_color_MountSinai("main")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(legend.title = element_blank())+
      guides(colour = guide_legend(nrow = 1))
    
    
    table <- 
      ggplot(arrived, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=avg), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(arrived$avg)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Patients\nArrived")+
      scale_x_discrete(position = "bottom")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg,1))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Reactive Filters for Scheduling Tab: Appointment Type & Insurance 
  output$apptTypeControl <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType", label=h4("Select Appointment Type:"),
                  choices = sort(unique(dataAll()$Appt.Type)),
                  #choices = sort(unique(dataAll()$Appt.Type), na.last = TRUE),
                  # choices=sort(unique(dataAll()$Appt.Type)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1",
                    countSelectedText = "{0}/{1} Appointment Types",
                    dropupAuto = FALSE),
                  selected = unique(dataAll()$Appt.Type)
                  #selected = sort(unique(dataAll()$Appt.Type), na.last = TRUE)
      )
    )
  })
  
  output$insuranceControl <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedInsurance", label=h4("Select Insurance Type:"),
                  #choices = sort(unique(dataAll()$Coverage)),
                  choices = sort(unique(dataAll()$Coverage), na.last = TRUE),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1",
                    countSelectedText = "{0}/{1} Insurance Types",
                    dropupAuto = FALSE),
                  #selected = unique(dataAll()$Coverage)
                  selected = sort(unique(dataAll()$Coverage), na.last = TRUE)
      )
    )
  })
  
  # Arrived No Show Data with Additional Filters (Appointment Type and Insurance)
  dataArrivedNoShow_1 <- reactive({
    data <- dataArrivedNoShow()
    data[,c("Coverage")][is.na(data[,c("Coverage")])] <- "NA"
    groupByFilters_1(data %>% filter(Appt.Status %in% c("Arrived", "No Show")),
                     input$selectedApptType, input$selectedInsurance)
  })
  
  dataNoShow_1 <- reactive({
    data <- dataNoShow()
    data[,c("Coverage")][is.na(data[,c("Coverage")])] <- "NA"
    groupByFilters_1(data %>% filter(Appt.Status %in% c("No Show")),
                     input$selectedApptType, input$selectedInsurance
    )
  })
  
  # Total No Shows per Day
  output$avgDailyNoShow_Count <- renderValueBox({
    valueBox(
      prettyNum(round(nrow(dataNoShow_1() %>% filter(Appt.Status %in% c("No Show"))) / length(unique((dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear)),0), big.mark = ","),
      subtitle = tags$p("Avg. No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # % No Shows per Day
  output$avgDailyNoShow_Perc <- renderValueBox({
    valueBox(
      paste0(round((nrow(dataNoShow_1() %>% filter(Appt.Status %in% c("No Show"))) / 
                      nrow(dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived", "No Show"))))*100,1), "%"),
      subtitle = tags$p("No Show Rate (%)", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Distribution of No Shows (%) by Lead Days 
  output$noShowLeadDays <- renderPlot({
    data <- dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- kpi.all.data[arrivedNoShow.data.rows,] %>% filter(Campus == "MSUS")
    
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    noShows <- 
      data %>%
      mutate(apptLeadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
      mutate(apptLeadDays = ifelse(is.na(apptLeadDays),0, apptLeadDays)) %>%
      mutate(apptLeadDays = ifelse(apptLeadDays > 14, "> 14 days",
                                   ifelse(apptLeadDays <= 14 & apptLeadDays>= 8, "8-14 days",
                                          ifelse(apptLeadDays <= 7 & apptLeadDays >= 1, "1-7 days",
                                                 ifelse(apptLeadDays < 0, "0 day","0 day")))))
    
    noShows <- reshape2::dcast(noShows, apptLeadDays + Appt.DateYear ~ Appt.Status)
    noShows$noShow_perc <- round(noShows$`No Show`/ (noShows$`No Show` + noShows$Arrived),2)
    noShows$noShow_perc[!is.finite(noShows$noShow_perc)] <- 0
    
    status <- c('0 day','1-7 days','8-14 days','> 14 days')
    
    if(input$distribution == TRUE){
      g1 <- ggplot(noShows, aes(x = factor(apptLeadDays, levels = status), y = noShow_perc)) +
        geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
        stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,max(noShows$noShow_perc)))+
        labs(x=NULL, y = "Percent",
             title = "Distribution of Average Daily No Show Rate by Wait Time to Appointment*",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        # caption = "*No Show includes no show and same-day bumped, 
        # canceled, and rescheduled appointments.")+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")+
        theme(plot.caption = element_text(size=12, face="italic"),
              axis.text.x = element_text(size = 16, angle=0, hjust=0.5))
      
      
      noShows.summary <- noShows %>%
        group_by(apptLeadDays) %>%
        dplyr::summarise(Avg = paste0(round(mean(noShow_perc),2)*100,"%"), 
                         Median = paste0(median(noShow_perc)*100,"%"),
                         Min = paste0(min(noShow_perc)*100,"%"), 
                         Max = paste0(max(noShow_perc)*100,"%"), N = n())
      
      cols <- c(1,6:10)
      
      level_order <- c("0 day", "1-7 days", "8-14 days", "> 14 days")
      
      data_melt <- reshape2::melt(noShows.summary, id = "apptLeadDays")
      
      n <- length(unique(data_melt$variable)) - 1
      if(n==0){
        hline_y <- 0
      } else{
        hline_y <- seq(1.5, 0.5+n, by= 1)
      }
      
      
      g2 <- ggplot(data_melt, aes(x = factor(apptLeadDays, level = level_order), y = variable, label = value))+
        scale_color_MountSinai('dark')+
        geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
        geom_hline(yintercept = hline_y, colour='black')+
        geom_vline(xintercept = 0, colour = 'black')+
        scale_x_discrete(position = "top") +
        labs(y = NULL, x = NULL, fill = "apptLeadDays")+
        theme_minimal() +
        table_theme()
      
      g1 + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(data_melt$variable))))
      
    } else{
      
      noShows_bar_tb <-
        noShows %>%
        group_by(apptLeadDays) %>%
        dplyr::summarise(Arrived = sum(Arrived),
                         `No Show` = sum(`No Show`)) %>%
        mutate(Average = round(`No Show`/(Arrived + `No Show`),2)) %>%
        select(apptLeadDays, Average)
      
      noShows_bar_tb <-
        reshape2::melt(noShows_bar_tb, id.vars = c("apptLeadDays"))
      
      ggplot(noShows_bar_tb, aes(x=factor(apptLeadDays, levels = status), y=value,fill=variable)) +
        geom_bar(stat="identity", position=position_dodge(), width = 0.8, fill="#f9878a", color="red") +
        labs(x=NULL, y = "Percent",
             # caption = "*No Show includes no show and same-day bumped,
             # canceled, and rescheduled appointments.",
             title = "Average No Show Rate by Wait Time to Appointment*",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1]),
                               " to ",isolate(input$dateRange[2])))+
        scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(noShows_bar_tb$value)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")+
        theme(plot.caption = element_text(size=12, face="italic"),
              axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
        geom_text(aes(label=paste0(value*100,"%")), vjust = -1, hjust = .5, color="black", fontface="bold",
                  position = position_dodge(1), size=5)
    }
    #grid.arrange(noShows_bar, noShows_box, ncol = 2)
    
  })
  
  
  
  # No Shows by Time of Day 
  output$avgNoShowCount <- renderPlot({
    data <- dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- arrivedNoShow.data
    
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    noShow_count <- data %>%
      filter(Appt.Status %in% "No Show") %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(Total = n()) 
    
    days <- unique(data[,c("Appt.Day","Appt.DateYear")])
    days <- days %>% group_by(Appt.Day) %>% dplyr::summarise(days = n())
    
    noShow_count$days <- days$days[match(noShow_count$Appt.Day, days$Appt.Day)]
    noShow_count$avgNoShows <- round(noShow_count$Total/noShow_count$days,0)
    
    noShow_count.df <- byDayTime.df %>% filter(Day %in% unique(noShow_count$Appt.Day))
    noShow_count.df <- merge(noShow_count.df, noShow_count, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
    
    noShow_count.df <- noShow_count.df %>% filter(Time %in% timeOptionsHr_filter)
    
    #noShow_count.df$avgNoShows[is.na(noShow_count$avgNoShows)] <- 0
    ggplot(noShow_count.df, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
      labs(x=NULL, y=NULL,
           title = "Average Daily No Shows*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",
                             isolate(input$dateRange[2])))+
      # caption = "*No Show includes no show and same-day bumped,
      # canceled, and rescheduled appointments.")+
      geom_tile(aes(fill=avgNoShows), colour = "black", size=0.5)+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Shows ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_count.df$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.7,"cm"),
            legend.text = element_text(size="14"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avgNoShows),"",avgNoShows)), color="black", size=5, fontface="bold")
    
  })
  
  output$avgNoShowPercent <- renderPlot({
    data <- dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- arrivedNoShow.data
    
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    noShow_perc <- data %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr, Appt.Status) %>%
      dplyr::summarise(Total = n())
    
    noShow_perc <- reshape2::dcast(noShow_perc, Appt.Day +  Appt.TM.Hr ~ Appt.Status, sum) 
    noShow_perc <- mutate(noShow_perc, percentage = round((`No Show` / (Arrived + `No Show`))*100,0))
    
    noShow_perc.df <- byDayTime.df %>% filter(Day %in% unique(noShow_perc$Appt.Day))
    noShow_perc.df <- merge(noShow_perc.df, noShow_perc, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
    
    noShow_perc.df <- noShow_perc.df %>% filter(Time %in% timeOptionsHr_filter)
    
    ggplot(noShow_perc.df, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
      labs(x=NULL, y=NULL,
           title = "Average No Show %*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",
                             isolate(input$dateRange[2])))+
      # caption = "*No Show includes no show and same-day bumped, 
      # canceled, and rescheduled appointments.")+
      geom_tile(aes(fill=percentage), colour = "black", size=0.5)+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Show % ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_perc.df$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            plot.caption = element_text(size = 12, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.7,"cm"),
            legend.text = element_text(size="12"),
            axis.title.x = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black", vjust=0.5, hjust = 0.5, margin = margin(b=15, t=100)),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30))+
      geom_text(aes(label= ifelse(is.na(percentage),"",paste0(percentage,"%"))), color="black", size=5, fontface="bold")
    
  })
  
  #-----------------------------------------------------------------------------------------------------------
  
  # Total Canceled/Bumped/Rescheduled Appointments 
  output$totalBumpedCanceledRescheduledBox <- renderValueBox({
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(nrow(data),big.mark=","), 
      subtitle = tags$p("Total Bumped/Canceled/Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyBumpedBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Bumped"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg. Daily Bumped Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyCanceledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Canceled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg. Daily Canceled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyRescheduledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Rescheduled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg. Daily Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  
  ## Average Bumps/Canc/Resc Rate 
  output$avgBumpsCancRescRate <- renderPlot({
    data <- dataAll()
    # data <- all.data
    
    apptsCanceled <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = n()) %>%
      arrange(desc(value)) %>%
      mutate(percent = round((value/sum(value)), 2)) %>%
      select(Appt.Status, percent) %>%
      filter(Appt.Status %in% c("Bumped", "Canceled", "Rescheduled"))
    
    ggplot(apptsCanceled, aes(reorder(Appt.Status, -percent), percent, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Rate* of Scheduling Activities - \nBump/Cancel/Reschedule",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*% of Total Appointments Scheduled.")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=paste0(round(percent*100),"%")), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  ## Lead Days to Bumps/Canc/Resc 
  output$leadDaysBumpsCancResc <- renderPlot({
    data <- dataAll() %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    # data <- kpi.all.data[canceled.bumped.rescheduled.data.rows ,] %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    
    lead.days.df <- data %>%
      filter(Lead.Days >= 0) %>%
      mutate(leadDays = ifelse(Lead.Days> 14, "> 14 days", 
                               ifelse(Lead.Days <= 14 & Lead.Days >= 8, "8-14 days",
                                      ifelse(Lead.Days < 8 & Lead.Days >= 1, "1-7 days",
                                             ifelse(Lead.Days < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Appt.Status) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Status) %>%
      mutate(perc = total/sum(total))
    
    ggplot(lead.days.df, aes(x=Appt.Status, y=perc, fill=factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("grey","#00aeef","#d80b8c","midnightblue"))+
      labs(x=NULL, y=NULL,
           title = "% of Bumped/Canceled/Rescheduled \nAppointments by Lead days \nto Appt Cancellation*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*Time from appointment scheduled to status changed.")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=paste0(round(perc*100),"%")), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
                                legend.title = element_blank())
    
  })
  
  
  ## Average Daily Same-day Bumps/Canc/Resc Rate 
  output$sameDayBumpedCanceledRescheduled <- renderPlot({
    data <- dataNoShow() %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    # data <- noShow.data %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(total = n()) %>%
      mutate(avg = round(total/length(unique((kpi.all.data[all.data.rows,])$Appt.DateYear))))
    
    ggplot(sameDay, aes(reorder(Appt.Status, -avg), avg, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(sameDay$avg))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Average Daily Same-day \nBumped/Canceled/Rescheduled \nAppointments",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*Appointment status changed on the day of appoinment.")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+ theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      geom_text(aes(label=avg), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  
  ## Bumped Reasons by Lead Days
  output$reasonsBumps <- renderPlot({
    
    data <- dataBumped()
    # data <- kpi.all.data[bumped.data.rows,] %>% filter(Campus == "MSUS")
    
    total <- nrow(data)
    
    bumps <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    bumps$percent <- round(bumps$total/total,2)*100
    
    top10 <- bumps %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    if(input$percent == FALSE){
      
      graph <- 
        ggplot(bumps %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days to Bump",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Bumped Appointments\n by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(bumps %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days to Bump",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total % of Bumped \nAppointments by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(percent),"",paste0(percent,"%"))), color="black", size=5, fontface="bold")
      
    }
    
    graph +
      scale_x_discrete(labels = wrap_format(25))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, colour = "black"),
            axis.text.y = element_text(size = 14, colour = "black"))
    
  })
  
  
  ## Canceled Reasons by Lead Days
  output$reasonsCanc <- renderPlot({
    
    data <- dataCanceled()
    # data <- canceled.data
    
    total <- nrow(data)
    
    cancellations <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    cancellations$percent <- round(cancellations$total/total,2)*100
    cancellations$Cancel.Reason[which(is.na(cancellations$Cancel.Reason))] <- "No Reasons Recorded"
    
    top10 <- cancellations %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    if(input$percent == FALSE){
      
      graph <- 
        ggplot(cancellations %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days to Cancellation",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Canceled Appointments\n by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(cancellations %>% filter(Cancel.Reason %in% top10), 
               aes(Cancel.Reason, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","> 14 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days to Cancellation",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="% of Total Canceled \nAppointments by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(percent),"",paste0(percent,"%"))), color="black", size=5)
    }
    
    graph +
      scale_x_discrete(labels = wrap_format(25))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
            legend.position = "top",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, colour = "black"),
            axis.text.y = element_text(size = 14, colour = "black"))
    
  })
  
  
  ### Utilization Tab -----------------------------------------------------------------------------------------------------------------
  # Average Rooms Required --------------------------------------------------------------------------------------------
  output$roomStat1 <- renderValueBox({
    valueBox(NULL,
             # paste0(input$setRooms," rooms available\n throughout",input$setHours," hours"),
             subtitle = tags$p(paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours"), style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "yellow"
    )
  })
  
  output$maxRoomsRequired <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Max # of rooms required during the day: ",
                                      max((dataUtilization() %>%
                                             filter(comparison == 0) %>%
                                             select(Appt.DateYear, timeOptionsHr_filter) %>%
                                             gather(Time, sum, 2:15) %>%
                                             group_by(Time) %>%
                                             summarise(avg = ceiling((sum(sum)/length(unique(Appt.DateYear)))/60)))$avg)), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "aqua"
    )
  })
  
  output$avgUtilization <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Avg utilization per day: ",
                                      paste0(round((sum((dataUtilization() %>% filter(comparison == 0))$sum))/
                                                     (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%")), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  output$maxUtilization <- renderValueBox({
    
    valueBox(NULL,
             subtitle = tags$p(paste0("Peak utilization during the day: ",
                                      max((dataUtilization() %>%
                                             filter(comparison == 0) %>%
                                             select(Appt.DateYear, timeOptionsHr_filter) %>%
                                             gather(Time, sum, 2:15) %>%
                                             group_by(Time) %>%
                                             summarise(avg = round((sum(sum)/ 
                                                                      (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setRooms)))*100)))$avg),"%"), 
                               style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
    )
  })
  
  # # Scheduled and Avg Utilization --------------------------------------------------------------------------------------------------------
  # output$avgScheduledUtilization <- renderValueBox({
  #   
  #   data <- dataUtilization() 
  #   # data <- utilization.data[scheduled.utilization.data.rows,]
  #   
  #   paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%") %>%
  #     valueBox(
  #       subtitle = tags$p("Average Daily Booked Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  # })
  # 
  # output$avgUtilization <- renderValueBox({
  #   
  #   data <- dataUtilization() %>% filter(Appt.Status == "Arrived")
  #   # data <- utilization.data %>% filter(util.type == "actual") %>% filter(Appt.Status == "Arrived")
  #   
  #   paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%") %>%
  #     valueBox(
  #       subtitle = tags$p("Average Daily Filled Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  # })
  
  # Average Number of Rooms Required -----------------------------------------------
  output$spaceUsed <- renderPlot({
    data <- dataUtilization() %>% filter(comparison == 0)
    
    # data <- as.data.frame(utilization.data[arrived.utilization.data.rows,])
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$Appt.Day),FUN = sum)
    space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$average <- round(space.hour.day$value/(space.hour.day$days*60), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Req")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=Average_Req, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Average Space Required by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))+
      guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=Average_Req), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Req)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Required ")+
      scale_x_discrete(position = "bottom")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(Average_Req),"", round(Average_Req))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Average Utilization by Time of Day
  output$spaceUtil <- renderPlot({
    data <- dataUtilization() %>% filter(comparison == 0)
    # data <- utilization.data[arrived.utilization.data.rows,]
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$Appt.Day),FUN = sum)
    space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*input$setRooms), 1)
    #space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*8), 1)
    
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Util")
    #space.hour.day$Average_Util <- space.hour.day$Average_Util*100
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    #space.hour.day$target <- 80
    space.hour.day$target <- 0.8
    
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=Average_Util, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Utilization (%)", 
           title = "Average Space Utilization (%) by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      #geom_hline(yintercept = .8, color = "red", linetype="dashed")+
      geom_hline(aes(yintercept = .8), color = "red", linetype="dashed")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour.day$Average_Util)*1.2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))+
      guides(colour = guide_legend(nrow = 1))
    
    space.hour.day$Average_Util <- space.hour.day$Average_Util*100
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=Average_Util), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Util)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization")+
      #scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Util_tble)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization %", labels = scales::percent)+
      scale_x_discrete(position = "bottom")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(Average_Util),"",paste0(round(Average_Util,2),"%"))), color="black", size=5, fontface="bold")
    #geom_text(aes(label= ifelse(is.na(Average_Util),"",paste0(round(Average_Util*100,2)*100,"%"))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Rooms Required by Percentile 
  output$spaceUsedPerc <- renderPlot({
    data <- dataUtilization() %>% filter(comparison == 0)
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = round(quantile(value, probs=0.5)/60,1),
        `70th Percentile`= round(quantile(value, probs=0.75)/60,1),
        `90th Percentile`= round(quantile(value, probs=0.90)/60,1))
    
    colnames(space.hour)[1] <- "Time"
    
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(limits=c(0, max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Space Required by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
    guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour, aes(x=variable, y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=value), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour$value)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Required ")+
      #scale_y_discrete(limits = unique(sort(space.hour$Time)), position = "bottom")+
      scale_x_discrete(position = "bottom")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(value),"", round(value,1))), color="black", size=5, fontface="bold")
    
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,2))
    
  })
  
  # Utilization by Percentile
  output$spaceUtilPerc <- renderPlot({
    data <- dataUtilization() %>% filter(comparison == 0)
    #data <- utilization.data %>% filter(comparison == 0)
    
    c.start <- which(colnames(data)=="07:00")
    c.end <- which(colnames(data)=="20:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = quantile(value, probs=0.5)/(60*input$setRooms),
        `70th Percentile`= quantile(value, probs=0.75)/(60*input$setRooms),
        `90th Percentile`= quantile(value, probs=0.90)/(60*input$setRooms))
    
    # space.hour <- space.hour %>%
    #   group_by(variable) %>%
    #   dplyr::summarise( 
    #     Median = quantile(value, probs=0.5)/(60*8),
    #     `70th Percentile`= quantile(value, probs=0.75)/(60*8),
    #     `90th Percentile`= quantile(value, probs=0.90)/(60*8))
    
    
    colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n", 
           title = "Space Utilization (%) by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2])))+
      scale_color_MountSinai("main")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
    guides(colour = guide_legend(nrow = 1))
    
    space.hour$value <- space.hour$value*100
    
    table <- ggplot(space.hour, aes(x=variable, y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=value), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour$value)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization ")+
      #scale_y_discrete(limits = unique(sort(space.hour$Time)), position = "bottom")+
      scale_x_discrete(position = "bottom")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(value),"",paste0(round(value,2),"%"))), color="black", size=5, fontface="bold")
    
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,2))
    
  })
  
  ### [3. ] Population Tab Output -----------------------------------------------------------------------------------------------------
  
  ## Demographics Breakdown
  
  output$ins_breakdown_tb <- function(){
    
    data <- dataArrived() %>%
      group_by(Coverage) %>%
      dplyr::summarise(`Total Arrived Patients` = n()) %>%
      mutate(Percent = paste0(round((`Total Arrived Patients`/sum(`Total Arrived Patients`))*100, 1),"%")) %>%
      arrange(desc(`Total Arrived Patients`))
    
    data$Coverage[is.na(data$Coverage)] <- "Unknown"
    data$`Total Arrived Patients` <- prettyNum(data$`Total Arrived Patients`, big.mark = ',')
    
    data %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      row_spec(0, bold=T, background = "#dddedd", color = "black") %>%
      column_spec(1, bold=T) %>%
      add_header_above(c("Coverage Breakdown" = length(data)),
                       background = "#d80b8c", color = "white", font_size = 18, align = "center")
    
  }
  
  output$sex_breakdown <- renderPlot({
    data <- dataArrived() %>% drop_na(Sex) %>%
      group_by(Sex) %>% dplyr::summarise(total = n()) %>%
      filter(Sex %in% c("Female","Male")) %>%
      arrange(desc(total)) %>%
      mutate(percent = round(total/sum(total)*100),1) %>%
      mutate(ypos = cumsum(percent) - 0.5*percent)
    
    ggplot(data, aes(x="", y=percent, fill=Sex)) +
      geom_bar(stat="identity", width=1, color="white") +
      ggtitle(paste0("Total Number of Completed Appointments: ",sum(data$total))) +
      coord_polar("y", start=0, direction = ifelse(data$Sex[1] == "Female",-1,1)) +
      theme_void() + 
      theme(
        legend.position="none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
      geom_text(aes(y = ypos, label = paste(Sex,paste0(percent,"%"),sep="\n")), color = "white", size=6) +
      scale_fill_manual(values = c("Male" = "midnightblue", "Female" = "#d80b8c"))
    
  })
  
  
  output$pop_breakdown <- renderPlot({
    arrived.data <- dataArrived()
    
    arrived.data$age <- round(age_calc(as.Date(arrived.data$Birth.Date, format="%Y-%m-%d"), units='years'),0)
    arrived.data$age_group <- cut(arrived.data$age, breaks = c(10,20,30,40,50,60,70,80), na.rm=TRUE)
    
    age_data <- arrived.data %>% drop_na(Sex, age_group) %>%
      group_by(Sex, age_group) %>% dplyr::summarise(total = n()) %>%
      filter(Sex %in% c("Male","Female"))
    
    age_data <- reshape2::dcast(age_data, age_group ~ Sex) %>%
      mutate(female_perc = Female / sum(Female)) %>%
      mutate(male_perc = Male / sum(Male)) 
    
    g.mid<-ggplot(arrived.data %>% drop_na(age_group),aes(x=1,y=age_group))+geom_text(aes(label=age_group),size=5)+
      geom_segment(aes(x=0.94,xend=0.96,yend=age_group))+
      geom_segment(aes(x=1.04,xend=1.065,yend=age_group))+
      ggtitle("")+
      ylab(NULL)+
      scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
      theme(axis.title=element_blank(),
            panel.grid=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background=element_blank(),
            axis.text.x=element_text(color=NA),
            axis.ticks.x=element_line(color=NA),
            plot.margin = unit(c(1,-1,1,-1), "mm"))
    
    g1 <- ggplot(age_data, aes(x = age_group, y = female_perc)) +
      geom_bar(stat = "identity", fill="#d80b8c") + ggtitle("% of Female Visits by Age Group") +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            plot.margin = unit(c(1,-1,1,0), "mm")) +
      scale_y_reverse(labels = scales::percent_format(accuracy = 1)) +
      expand_limits(y = max(age_data$female_perc)*1.2)+
      coord_flip()+
      geom_text(aes(label=paste0(round(female_perc*100,0),"%")), vjust = .5, hjust =1.2, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
    g2 <- ggplot(age_data, aes(x = age_group, y = male_perc)) +xlab(NULL)+
      geom_bar(stat = "identity", fill="midnightblue") + ggtitle("% of Male Visits by Age Group") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,0,1,-1), "mm")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      expand_limits(y = max(age_data$male_perc)*1.2)+
      coord_flip()+
      geom_text(aes(label=paste0(round(male_perc*100,0),"%")), vjust = .5, hjust =-.2, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
    gg1 <- ggplot_gtable(ggplot_build(g1))
    gg2 <- ggplot_gtable(ggplot_build(g2))
    gg.mid <- ggplot_gtable(ggplot_build(g.mid))
    
    graph <- grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))
    
    print(graph)
    
  })
  
  ## Breakdown of Arrived Visits by Zip Code
  output$zipCode_tb <- function(){
    
    data <- dataArrivedPop()
    #data <- population.data_filtered %>% filter(Campus.Specialty == "Allergy")
    newdata <- uniquePts_df_system(data)
    
    
    a_table <- newdata %>% 
      filter(`Zip Code Layer: A` != "EXCLUDE") %>%
      group_by(`Zip Code Layer: A`) %>% summarise(total = n()) %>%
      arrange(-total) %>%
      mutate(perc = round(total/sum(total),2)*100) %>%
      adorn_totals("row") %>%
      mutate(perc = paste0(perc,"%")) %>%
      `colnames<-` (c("Zip Code Layer", "total", "perc")) %>%
      mutate(Layer = `Zip Code Layer`)
    
    b_table <- newdata %>%
      filter(`Zip Code Layer: A` != "EXCLUDE") %>%
      group_by(`Zip Code Layer: A`, `Zip Code Layer: B`) %>% summarise(total = n()) %>%
      arrange(-total)
    
    b_table <- b_table %>%
      mutate(perc = round(total/sum(b_table$total),2)*100) %>%
      mutate(perc = paste0(perc,"%")) %>%
      `colnames<-` (c("Layer", "Zip Code Layer", "total", "perc")) %>%
      filter(`Layer` %in% c("Manhattan", "Out of NYS", "Long Island", "Northern New York"))
    
    zip_table <- bind_rows(a_table, b_table)
    zip_table <- zip_table[order(factor(zip_table$Layer, levels = unique(a_table$Layer))),]
    zip_table$Layer <- NULL
    
    
    
    
    # Table subtitle based on date range filter
    manhattan_ref <- which(zip_table$`Zip Code Layer` == "Manhattan")
    out_state_ref <-  which(zip_table$`Zip Code Layer` == "Out of NYS")
    long_is_ref <-  which(zip_table$`Zip Code Layer` == "Long Island")
    northern_ny_ref <-  which(zip_table$`Zip Code Layer` == "Northern New York")
    header_above <- c("Subtitle" = 3)
    names(header_above) <- paste0(c("Based on data from "),c(isolate(input$dateRangepop[1])),c(" to "),c(isolate(input$dateRangepop[2])))
    total <-  which(zip_table$`Zip Code Layer` == "Total") - 1 
    northern_ny_ref_1 <- northern_ny_ref + 1
    northern <- c(northern_ny_ref_1:total)
    #names(header_above) <- paste0(c("Based on data from "),c("test"),c(" to "),c("test"))
    
    zip_table %>%
      kable(escape = F,
            col.names = c("Zip Code Layer", "Total Unique Patients", "Percent of Total")) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Total Unique Patients by Zipcode" = length(zip_table)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      add_indent(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3,
                   out_state_ref+1, out_state_ref+2, out_state_ref+3, out_state_ref+4, out_state_ref+5,
                   long_is_ref+1, long_is_ref+2,
                   northern#northern_ny_ref+1#, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5
      ),
      level_of_indent = 2) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      row_spec(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3, out_state_ref+1, out_state_ref+2,
                 out_state_ref+3, out_state_ref+4, out_state_ref+5, long_is_ref+1, long_is_ref+2,
                 northern#northern_ny_ref+1#\, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5
      ), font_size = 14) %>%
      row_spec(nrow(zip_table), background = "#d80b8c", color = "white", bold = T)
    
  }
  
  
  output$population1 <- renderLeaflet({
    # #data(zipcode)
    # zipcode <- read_feather(here::here("Data/zipcode.feather"))
    # 
    # population.data <- 
    #   dataArrived()[,c("Campus","Campus.Specialty","Department","MRN","Zip.Code","Sex","Coverage","uniqueId")]
    # 
    # population.data$zip <- normalize_zip(population.data$Zip.Code)
    # 
    # population.data <- merge(population.data, zipcode, by.x='zip', by.y='zip')
    
    population.data <- dataArrivedPop()
    
    newdata <- population.data %>% group_by(latitude, longitude) %>% dplyr::summarise(total = round(n(),0))
    
    # Create a color palette with handmade bins.
    mybins <- round(seq(min(newdata$total), max(newdata$total), length.out=5),0)
    mypalette <- colorBin(palette=MountSinai_palettes$pinkBlue, domain=quakes$mag, na.color="transparent", bins=mybins)
    
    # Prepare the text for the tooltip:
    mytext <- paste(
      "Total Visits: ", newdata$total, "<br/>", 
      "Latitude: ", newdata$latitude, "<br/>", 
      "Longitude: ", newdata$longitude, sep="") %>%
      lapply(htmltools::HTML)
    
    # Set icons for each MSHS hospital
    icons <- awesomeIcons(
      icon = 'hospital-o',
      lib = 'fa',
      iconColor = "white",
      markerColor = "lightgray")
    
    # Visit volume map 
    leaflet(newdata) %>% 
      addTiles()  %>% 
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(~longitude, ~latitude, 
                       fillColor = ~mypalette(total), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                       label = mytext,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend( pal=mypalette, values=~total, opacity=0.9, title = "Appointment Demand", position = "bottomright") %>%
      addAwesomeMarkers(
        lng=-73.943324, lat=40.79171,
        label='Mount Sinai Hospital',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.92606, lat=40.77084,
        label='Mount Sinai Queens',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.98840, lat=40.73139,
        label='Mount Sinai Union Square',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.99181, lat=40.76719,
        label='Mount Sinai West',
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold'))) %>%
      addAwesomeMarkers(
        lng=-73.96316, lat=40.79834,
        label="Mount Sinai Morningside",
        icon = icons,
        labelOptions = labelOptions(noHide = T, textsize='15px', textOnly = TRUE,
                                    style=list('font-weight'= 'bold')))
    
  })
  
  
  ### [3. ] Volume Tab Output ---------------------------------------------------------------------------------------------------------
  # Daily Patient Volume over Time ....................................................................................................
  output$trend_visitstable <- function(){
    
    #options(knitr.kable.NA = "-")
    data <- dataArrived()
    #data <- kpi.all.data[arrived.data.rows,]
    #created an if statement to include another table for all of the visit types
    #to show the total volume and the variance per month per year.
    
    #get the total patients per year
    if(input$annualVolSummary == "Total"){
      visits_tb_yearly <- data %>%
        group_by(Appt.Year) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      visits_tb_yearly$Appt.Month <- "Total Annual Comparison"
      visits_tb_yearly <- visits_tb_yearly %>% relocate(Appt.Month)
      
      #get the total patients per year per month
      visits_tb <- data %>%
        group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      
      
    } else {
      #get the total patients per year
      visits_tb_yearly <- data %>% 
        filter(Visit.Method %in% input$annualVolSummary) %>%
        group_by(Appt.Year) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
      visits_tb_yearly$Appt.Month <- paste0("Total ",input$annualVolSummary,"\nAnnual Comparison")
      visits_tb_yearly <- visits_tb_yearly %>% relocate(Appt.Month)
      
      #get the total patients per year per month
      visits_tb <- data %>% 
        filter(AssociationListA %in% input$annualVolSummary) %>%
        group_by(Appt.Year, Appt.Month) %>% summarise(total = n()) %>%
        spread(Appt.Year, total)
    }
    
    
    #include all the months needed
    visits_tb <- visits_tb[match(monthOptions, visits_tb$Appt.Month),]
    visits_tb$Appt.Month <- monthOptions
    
    visits_ytd <- visits_tb
    visits_ytd[is.na(visits_ytd)] = 0  
    visits_ytd$Appt.Month <- NULL
    #visits_ytd <- visits_ytd[1:month(input$dateRange[2]),]
    visits_ytd <- visits_ytd[1:2,]
    visits_ytd <- as.data.frame(colSums(visits_ytd))
    visits_ytd <- as.data.frame(t(as.matrix(visits_ytd)))
    visits_ytd <- cbind(Appt.Month = "YTD Comparison",visits_ytd)
    
    #bind the total visits per month per year with the total yeraly visits 
    visits_tb_total <- rbind(visits_tb, visits_ytd,visits_tb_yearly)
    
    #created an if statement to change the table based on the different years
    #if the number of years provided is one then there will be no need to calculate any variance
    #and only the volume will be showing for that specific year
    #if the number of years are more than 1 and less than or equal 3 then we calculate variance
    #if the number of years are more than 3 the code will raise a user error
    
    if(length(visits_tb_total)-1 == 1){
      visits_tb_total <- visits_tb_total
      year1 <- colnames(visits_tb_total)[2]
      column_names <- c("Month", paste0(year1))
      header_above <- c("Total Visit Volume" = 2)
      names(header_above) <- paste(c(input$annualVolSummary), c("Visit Volume"))
      rownames(visits_tb_total) <- NULL
      
      column_border <- c(1, 2)
      
    } else if(length(visits_tb_total)-1 == 2){
      
      visits_tb_total$variance <- visits_tb_total %>% select(length(visits_tb_total)) - visits_tb_total %>% select(length(visits_tb_total)-1)
      
      visits_tb_total$variance_percentage <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-2)
      
      #######
      
      visits_tb_total$variance_percentage <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      
      visits_variance_only <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),4])
      visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),5])
      #visits_variance_only <- as.data.frame(visits_tb_total[1:3,4])
      #visits_variance_percentage <- as.data.frame(visits_tb_total[1:3,5])
      
      visits_variance_only[is.na(visits_variance_only),] <- 0
      visits_variance_percentage[is.na(visits_variance_percentage),] <- 0
      visits_variance_only <- as.data.frame(colSums(visits_variance_only))
      visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage))
      visits_tb_total <- as.data.frame(visits_tb_total)
      visits_tb_total[13,4] <- visits_variance_only[1,1]
      visits_tb_total[13,5] <- visits_variance_percentage[1,1]
      
      
      #######
      
      column_names <- c("Month", paste0(year1), paste0(year2), 
                        paste0("Variance"," ", "(", paste0(year1), "-", paste0(year2), ")"), 
                        paste0("% Variance", " ", "(", paste0(year1), "-", paste0(year2), ")"))
      
      header_above <- c("Total Visit Volume" = 3, "Volume Variance" = 2)
      names(header_above) <- paste(c(input$annualVolSummary,input$annualVolSummary), c("Visit Volume","Volume Variance"))
      
      
      column_border <- c(1, 3, 5)
      
    } else if (length(visits_tb_total)-1 == 3){
      
      visits_tb_total$variance_1 <- visits_tb_total %>% select(length(visits_tb_total)-1) - visits_tb_total %>% select(length(visits_tb_total)-2)
      
      visits_tb_total$variance_percentage_1 <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-3)
      
      #######
      
      visits_tb_total$variance_2 <- visits_tb_total %>% select(length(visits_tb_total)-3) - visits_tb_total %>% select(length(visits_tb_total)-4)
      
      visits_tb_total$variance_percentage_2 <- visits_tb_total %>% select(length(visits_tb_total)) / visits_tb_total %>% select(length(visits_tb_total)-5)
      
      #######
      
      visits_tb_total$variance_percentage_1 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_1)))
      visits_tb_total$variance_percentage_2 <- formattable::percent(as.numeric(unlist(visits_tb_total$variance_percentage_2)))
      
      year1 <- colnames(visits_tb_total)[2]
      year2 <- colnames(visits_tb_total)[3]
      year3 <- colnames(visits_tb_total)[4]
      
      
      visits_variance_only <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),4:5])
      visits_variance_percentage <- as.data.frame(visits_tb_total[1:month(isolate(input$dateRange[2])),6])
      #visits_variance_only <- as.data.frame(visits_tb_total[1:3,4:5])
      #visits_variance_percentage <- as.data.frame(visits_tb_total[1:3,6])
      
      
      visits_variance_only[is.na(visits_variance_only)] <- 0
      visits_variance_percentage[is.na(visits_variance_percentage)] <- 0
      visits_variance_only <- t(as.data.frame(colSums(visits_variance_only)))
      visits_variance_percentage <- as.data.frame(colSums(visits_variance_percentage))
      visits_tb_total <- as.data.frame(visits_tb_total)
      visits_tb_total[13,4:5] <- visits_variance_only[1,1:2]
      visits_tb_total[13,6] <- visits_variance_percentage[1,1]
      
      #######
      
      column_names <- c("Month", paste0(year1), paste0(year2), paste0(year3), 
                        paste0("Variance"," ", "(", paste0(year1), "-", paste0(year2), ")"),
                        paste0("% Variance", " ", "(", paste0(year1), "-", paste0(year2), ")"),
                        paste0("Variance"," ", "(", paste0(year2), "-", paste0(year3), ")"), 
                        paste0("% Variance", " ", "(", paste0(year2), "-", paste0(year3), ")"))
      
      header_above <- c("Total Visit Volume" = 4, "Volume Variance" = 4) 
      names(header_above) <- paste(c(input$annualVolSummary,input$annualVolSummary), c("Visit Volume","Volume Variance"))
      
      
      column_border <- c(1, 4, 8)
      
    } else {print("Please select <= 3 years")}
    
    visits_tb_total %>%
      kable(escape = F, align = "c",
            col.names = column_names) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above,  background = "#d80b8c", color = "white", font_size = 20, align = "center") %>%
      column_spec(column = column_border, border_right = "thin solid lightgray", width_min = "125px") %>%
      column_spec(column = 1, bold = T) %>%
      row_spec(row = 0, font_size = 18, bold=TRUE, background = "#d80b8c", color = "white") %>%
      row_spec(row = 14, bold = TRUE, background = "#d80b8c", color = "white") %>%
      row_spec(row = 13, bold = TRUE, background = "#dddedd")
    
  }
  
  output$volume1 <- renderPlotly({
    
    data <- dataArrived()
    
    # data <- arrived.data %>% filter(!holiday %in% c("Christmas"))
    # data <- setDT(data)
    # 
    # pts.count <- data[,list(Volume = .N), by = list(Appt.DateYear)]  
    # 
    pts.count <- aggregate(data$uniqueId,
                           by=list(data$Appt.DateYear), FUN=NROW)
    
    # pts.count <- aggregate(arrived.data$uniqueId,
    #                        by=list(arrived.data$Appt.DateYear), FUN=NROW)
    
    names(pts.count) <- c("Date","Volume")
    pts.count$Date <- as.Date(pts.count$Date, format="%Y-%m-%d")
    
    volume_fig <- plot_ly(pts.count, x = ~Date, y = ~Volume, name = 'Patients', type = "scatter", mode = 'lines+markers', 
                          hoverinfo = "text", hovertext = ifelse(pts.count$Volume == 1,paste0(pts.count$Date, ": ",pts.count$Volume, " ", "Patient"),paste0(pts.count$Date, ": ",pts.count$Volume, " ", "Patients")))#,
    #textfont = list(color = '#000000', size = 16), height = 700)
    # volume_fig <- volume_fig %>% add_trace(y= ~Volume, name = 'Total Patients', mode = 'lines+markers',
    #                                        marker = list(color = "#212070"), line = list(color = "#212070"))
    
    volume_fig %>% layout(
      title = "Daily Patients Over Time", font = list(size = 20),
      autosize = TRUE,margin=list( l = 50, r = 50, b = 100, t = 130,  pad = 4),
      xaxis = list(
        title = "Date",
        font = list(size = 14),
        tickfont = list(size = 14),
        rangeslider = list(type = "date"),
        mirror = TRUE,
        ticks = 'outside',
        showline = TRUE),
      yaxis = list(title = "Patients",
                   font = list(size = 14),
                   tickfont = list(size = 14),
                   mirror = TRUE,
                   ticks = 'outside',
                   showline = TRUE),
      hoverlabel = list(font = list(size = 15)),
      hovermode = "x unified"
    )
    
  })
  
  # output$volume1 <- renderHighchart({
  #   data <- dataArrived()
  # 
  #   # data <- arrived.data %>% filter(!holiday %in% c("Christmas"))
  #   pts.count <- aggregate(data$uniqueId,
  #                          by=list(data$Appt.DateYear), FUN=NROW)
  # 
  #   # pts.count <- aggregate(arrived.data$uniqueId,
  #   #                        by=list(arrived.data$Appt.DateYear), FUN=NROW)
  # 
  #   names(pts.count) <- c("Date","Volume")
  #   pts.count$Date <- as.Date(pts.count$Date, format="%Y-%m-%d")
  # 
  #   model <- lm(Volume ~ Date, data = pts.count)
  #   fit <- augment(model) %>% arrange(Date)
  # 
  #   # Visualization
  #   pts.count %>%
  #     hchart('line', hcaes(x = Date, y = Volume), name = "Patients") %>%
  #     hc_add_series(
  #       fit, type = "line", hcaes(x = Date, y = .fitted),
  #       name = "Linear Regression", id = "fit") %>%
  #     hc_colors(c("#212070","red")) %>%
  #     hc_title(text="Daily Arrived Patients over Time\n", align="center", style = list(color = "black", fontSize = "16px", fontWeight = "bold", useHTML = TRUE)) %>%
  #     hc_add_theme(hc_theme_elementary()) %>%
  #     hc_subtitle(text = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))) %>%
  #     hc_xAxis(title = list(text = ''), labels = list(format = '{value:%Y-%m}', rotation = "310")) %>%
  #     hc_yAxis(title = list(text = 'Patients'))
  # })
  
  # Total Monthly Patient Volume
  
  output$volume2 <- renderPlot({
    
    
    data <- dataArrived()
    #data <- kpi.all.data[arrived.data.rows,]
    
    data <- setDT(data)
    #data <- unique(data, by = "uniqueId")
    
    pts.by.month <- data[,list(Volume = .N), by = list(Appt.MonthYear,Visit.Method)]  
    
    # pts.by.month <- aggregate(data$uniqueId,
    #                           by=list(data$Appt.MonthYear, data$Visit.Method), FUN=NROW)
    
    # pts.by.month <- aggregate(kpi.all.data[arrived.data.rows,]$uniqueId,
    #                           by=list(kpi.all.data[arrived.data.rows,]$Appt.MonthYear,kpi.all.data[arrived.data.rows,]$Visit.Method), FUN=NROW)
    
    names(pts.by.month) <- c("Month", "Visit.Method", "Volume")
    pts.by.month$Volume <- as.numeric(pts.by.month$Volume)
    #pts.by.month$Month <- as.yearmon(pts.by.month$Month, format="%Y-%m")
    #pts.by.month$Month <- as.Date(pts.by.month$Month, format="%Y-%m")
    
    pts.by.month <- pts.by.month[order(pts.by.month$Month, pts.by.month$Volume),]
    factor_levels <- unique(pts.by.month$Visit.Method)
    
    #factor_levels = c("TELEPHONE VISITS", "VIDEO_TELEHEALTH VISITS","IN PERSON")
    
    #factor_levels = c("TELEPHONE VISITS", "VIDEO_TELEHEALTH VISITS","IN PERSON", "PORTING VV TH TP")
    pts.by.month$Visit.Method <- factor(pts.by.month$Visit.Method, levels = factor_levels)
    
    percent <- pts.by.month %>% 
      group_by(Month) %>%
      mutate(percent = paste0("(",round(Volume/sum(Volume)*100,0), "%)"))
    
    pts.by.month <- full_join(pts.by.month,percent)
    
    g3 <- ggplot(pts.by.month, aes(x=Month, y=Volume, group = Visit.Method, fill = Visit.Method))+
      geom_bar(position = "stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      labs(x = NULL, y = "Patients",
           title = "Monthly Patient Volume",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
      )+
      scale_y_continuous(limits=c(0,(max(pts.by.month$Volume, na.rm = TRUE))*2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      geom_text(data=subset(pts.by.month, Volume > 0.15 * max(Volume)),aes(label=paste0(Volume, " ", percent)), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Month), geom="text", color="black", 
                   size=5, fontface="bold.italic")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    n <- length(unique(pts.by.month$Visit.Method))
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    
    
    # percent <- pts.by.month %>% 
    #            group_by(Month) %>%
    #            mutate(percent = paste0("(",round(Volume/sum(Volume)*100,0), "%)"))
    
    Total <- pts.by.month[,list(Volume = sum(Volume)), by = list(Month)]
    Total$Visit.Method <- "Total"
    pts.by.month <- full_join(pts.by.month, Total)
    #pts.by.month <- full_join(pts.by.month,percent)
    pts.by.month$percent[is.na(pts.by.month$percent)] <- ""
    pts.by.month$Visit.Method <- factor(pts.by.month$Visit.Method, levels = c("Total",rev(factor_levels)))
    
    
    
    pd <- position_dodge(width = 0.5)
    
    g4 <- ggplot(pts.by.month, aes(x=Month, y= Visit.Method, label=paste0(Volume," ",percent)))+
      scale_color_MountSinai('dark')+
      #scale_color_manual(values = types_pallete[1:length(unique(pts.by.month$Visit.Method))])+
      geom_text(size = 5, vjust = 0.5, hjust = 0.5, fontface = 'bold')+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top")+ 
      labs(y = NULL, x = NULL, fill = "Visit.Method")+
      theme_minimal()+
      table_theme()#+
    #(data = percent, aes(x=Month, y= Visit.Method, label=percent), size = 3, vjust = 0.5, hjust = 0.5, fontface = 'bold', position = pd)
    
    library(patchwork)
    g3 + g4 + plot_layout(ncol = 1, heights = c(10, 1 * length(unique(pts.by.month$Visit.Method))))
    
  })
  # Average Daily Patient Volume by Day of Week
  output$volume3 <- renderPlot({
    
    data <- dataArrived()
    #data <- kpi.all.data[arrived.data.rows,]
    
    data <- setDT(data)
    
    pts.by.day <- data[,list(Volume = .N), by = list(Appt.Day,Visit.Method)]  
    # pts.by.day <- aggregate(data$uniqueId, 
    #                         by=list(data$Appt.Day, data$Visit.Method), FUN=NROW)
    
    
    # pts.by.day <- aggregate(kpi.all.data[arrived.data.rows,]$uniqueId,
    #                           by=list(kpi.all.data[arrived.data.rows,]$Appt.Day,kpi.all.data[arrived.data.rows,]$Visit.Method), FUN=NROW)
    
    #names(pts.by.day) <- c("Day", "Volume")
    names(pts.by.day) <- c("Day","Visit.Method", "Volume")
    # totalDates <- seq(as.Date(min((kpi.all.data[arrived.data.rows,])$Appt.DTTM)),
    #                                 as.Date(max((kpi.all.data[arrived.data.rows,])$Appt.DTTM)),by="days")
    first_date <- arrived_first_date
    second_date <- arrived_last_date
    totalDates <- seq(first_date,second_date, by = "days")
    totalDates <- as.data.frame(totalDates)
    names(totalDates) <- c("Dates")
    totalDates$day <- format(as.Date(totalDates$Dates, format="%Y-%m-%d"), "%a")
    totalDates <- aggregate(totalDates$Dates,
                            by=list(totalDates$day), FUN=NROW)
    names(totalDates) <- c("Day","Count")
    
    pts.by.day$Day.Count <- totalDates$Count[match(pts.by.day$Day, totalDates$Day)]
    pts.by.day$Avg.Volume <- as.numeric(round(pts.by.day$Volume/pts.by.day$Day.Count,1))
    
    pts.by.day <- pts.by.day[order(pts.by.day$Day, pts.by.day$Avg.Volume),]
    factor_levels <- unique(pts.by.day$Visit.Method)
    
    pts.by.day$Visit.Method <- factor(pts.by.day$Visit.Method, levels = factor_levels)
    
    ggplot(pts.by.day, aes(x=factor(Day, level = daysOfWeek.options), y=Avg.Volume, group = Visit.Method, fill = Visit.Method))+
      geom_bar(position = "stack",stat="identity")+
      scale_fill_MountSinai('dark')+
      labs(x = NULL, y = "Patients",
           title = "Average Daily Patient Volume",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      scale_y_continuous(limits=c(0,(max(pts.by.day$Avg.Volume, na.rm = TRUE))*2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
      geom_text(data=subset(pts.by.day, Avg.Volume > 0.15 * max(Avg.Volume)),aes(label=Avg.Volume), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Day), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
  })
  
  # Daily Volume Distribution by Month
  output$volume4 <- renderPlot({
    
    
    data <- dataArrived()
    data <- setDT(data)
    
    pts.dist <- data[,list(Volume = .N), by = list(Appt.MonthYear,Appt.Date)]  
    
    # pts.dist <- aggregate(data$uniqueId, 
    #                       by=list(data$Appt.MonthYear, data$Appt.Date), FUN=NROW)
    
    # pts.dist <- aggregate(kpi.all.data[arrived.data.rows,]$uniqueId,
    #                       by=list(kpi.all.data[arrived.data.rows,]$Appt.MonthYear, kpi.all.data[arrived.data.rows,]$Appt.Date), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Volume")
    pts.dist$Month <- as.yearmon(pts.dist$Month, format="%Y-%m")
    pts.dist$Month <- as.Date(pts.dist$Month, format="%Y-%m")
    pts.dist <- pts.dist[order(pts.dist$Month),]
    
    g1 <- ggplot(pts.dist, aes(x=Month, y=Volume, group=Month))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Month - All Visits",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    
    pts.dist <- data[,list(Volume = .N), by = list(Appt.MonthYear,Appt.Date)]  
    
    # pts.dist <- aggregate(dataArrived()$uniqueId, 
    #                       by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date), FUN=NROW)
    
    # pts.dist <- aggregate(arrived.data$uniqueId,
    #                       by=list(arrived.data$Appt.MonthYear, arrived.data$Appt.Date), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Month) %>%
      dplyr::summarise(Avg = round(mean(Volume)), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    
    pts.dist.summary <- 
      pts.dist.summary[order(as.yearmon(pts.dist.summary$Month,format="%Y-%m")),]
    
    pts.dist.summary.t <-setNames(data.frame(t(pts.dist.summary[,-1])), pts.dist.summary[,1])
    colnames(pts.dist.summary.t) <- pts.dist.summary$Month
    
    data_melt <- reshape2::melt(pts.dist.summary, id = "Month")
    
    n <- length(unique(data_melt$variable)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    g2 <- ggplot(data_melt, aes(x = Month, y = variable, label = value))+
      scale_color_MountSinai('dark' )+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL, fill = "AssociationListA")+
      theme_minimal() +
      table_theme()
    
    
    g1 + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(data_melt$variable))))
    
  })
  
  
  # Daily Volume Distribution by Day of Week
  output$volume5 <- renderPlot({
    
    data <- dataArrived()
    data <- setDT(data)
    
    pts.dist <- data[,list(Volume = .N), by = list(Appt.MonthYear,Appt.Date,Appt.Day)]  
    
    # pts.dist <- aggregate(data$uniqueId, 
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    g1 <- ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+ 
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Day of Week - All Visits",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(axis.text.x = element_text(size = 16, angle=0, hjust = 0.5))
    
    
    # data <- filter(arrived.data, Campus == "MSUS")
    # pts.dist <- aggregate(data$uniqueId,
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    
    
    pts.dist <- data[,list(Volume = .N), by = list(Appt.MonthYear,Appt.Date,Appt.Day)] 
    
    # pts.dist <- aggregate(data$uniqueId,
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Day) %>%
      dplyr::summarise(Avg = round(mean(Volume)), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    
    pts.dist.summary <- pts.dist.summary[match(daysOfWeek.options,pts.dist.summary$Day),]
    pts.dist.summary <- pts.dist.summary[complete.cases(pts.dist.summary),]
    
    pts.dist.summary.t <-setNames(data.frame(t(pts.dist.summary[,-1])), pts.dist.summary[,1])
    colnames(pts.dist.summary.t) <- pts.dist.summary$Day
    
    data_melt <- reshape2::melt(pts.dist.summary, id = "Day")
    
    n <- length(unique(data_melt$variable)) - 1
    if(n==0){
      hline_y <- 0
    } else{
      hline_y <- seq(1.5, 0.5+n, by= 1)
    }
    
    g2 <- ggplot(data_melt, aes(x = Day, y = variable, label = value))+
      scale_color_MountSinai('dark' )+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black')+
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL, fill = "AssociationListA")+
      theme_minimal() +
      table_theme()
    
    g1 + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(data_melt$variable))))
    
    
  })
  
  #Daily Volume Distribution by Day Table
  output$volume5.1 <- function(){
    
    pts.dist <- aggregate(dataArrived()$uniqueId,
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Day) %>%
      dplyr::summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    
    pts.dist.summary <- pts.dist.summary[match(daysOfWeek.options,pts.dist.summary$Day),]
    pts.dist.summary <- pts.dist.summary[complete.cases(pts.dist.summary),]
    
    pts.dist.summary.t <-setNames(data.frame(t(pts.dist.summary[,-1])), pts.dist.summary[,1])
    colnames(pts.dist.summary.t) <- pts.dist.summary$Day
    
    pts.dist.summary.t %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      row_spec(0, bold=T) %>%
      column_spec(1, bold=T)
    
  }
  
  ### [3. ] Access Tab ------------------------------------------------------------------------------------------------------------------
  
  # New Patient Ratio by Department
  output$newPtRatioByDept <- renderPlot({
    data <- dataArrived()
    # data <- kpi.all.data[arrived.data.rows,]
    
    newpatients.ratio <- data %>%
      group_by(Appt.MonthYear,New.PT3) %>%
      dplyr::summarise(Total = n()) %>%
      spread(New.PT3, Total)
    
    newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
    #newpatients.ratio$Appt.MonthYear <- as.Date(newpatients.ratio$Appt.MonthYear, format="%Y-%m") ## Create date-year column
    #newpatients.ratio[is.na(newpatients.ratio)] <- 0
    ggplot(newpatients.ratio, aes(x=Appt.MonthYear, y=ratio, group=1)) +
      geom_bar(stat = "identity", width = 0.8, fill = "#221f72") +
      labs(x=NULL, y=NULL,
           #title = "New Patient Ratio Trending over Time",
           title = "Monthly New Patient Ratio",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(newpatients.ratio$ratio)*1.5)) +
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = Appt.MonthYear), geom="text", color="black", 
                   size=5, fontface="bold.italic")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    # scale_x_date(breaks = "day", date_labels = "%Y-%m", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))
    
  })
  
  
  # New Patient Ratio by Provideer
  output$newPtRatioByProv <- renderPlot({
    data <- dataArrived()
    # data <- arrived.data %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
    
    newpatients.ratio <- data %>%
      group_by(Provider, Appt.DateYear,New.PT3) %>%
      dplyr::summarise(Total = n()) %>%
      spread(New.PT3, Total)
    
    newpatients.ratio[is.na(newpatients.ratio)] <- 0
    
    newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
    newpatients.ratio$Appt.DateYear <- as.Date(newpatients.ratio$Appt.DateYear, format="%Y-%m-%d") ## Create date-year column
    
    ggplot(newpatients.ratio, aes(x=Appt.DateYear, y=ratio)) +
      geom_line(aes(color=Provider), size=1) +
      scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
      labs(x=NULL, y=NULL, 
           title = "New Patient Ratio over Time by Provider",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("bottom")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
    
  })
  
  # New Patient Wait Time
  output$newPtWaitTimeByDept <- renderPlot({
    data <- dataAll()
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0) %>%
      group_by(Appt.MonthYear, New.PT3) %>%
      dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
      filter(New.PT3 %in% c("TRUE","FALSE"))
    
    waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
    #waitTime$Appt.MonthYear <- as.Date(waitTime$Appt.MonthYear, format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(New.PT3, medWaitTime) 
    #waitTime$`New Patient Target <= 14` <- 14
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 2:3)
    target <- 14
    
    
    ggplot(waitTime, aes(x=Appt.MonthYear, y=value, fill = variable))+
      #geom_line(aes(linetype=variable, color=variable, size=variable)) +
      geom_bar(stat = "identity", position = 'dodge')+
      geom_abline(slope=0, intercept=14,  col = "red",lty=2, size = 1) +
      #geom_line(aes(linetype = variable))+
      #scale_linetype_manual(values=c("solid", "solid", "dashed"))+
      scale_fill_manual(values=c('#212070','#d80b8c'))+
      #scale_size_manual(values=c(1, 1, 1.3))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      labs(x=NULL, y=NULL,
           #title = "Median Wait Time to New and Established Appointment Over Time",
           title = "Monthly Median Wait Time to New and Established Appointment",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
      )+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
      geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
      scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
    # stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..)), group = value), geom="text", color="black", 
    #              size=5, fontface="bold.italic")
    
  })
  
  
  # New Patient Wait Time
  output$newPtWaitTimeByProv <- renderPlot({
    data <- dataAll()
    # data <- all.data %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0) %>%
      group_by(Provider, Appt.DateYear, New.PT3) %>%
      dplyr::summarise(medWaitTime = round(median(wait.time)))
    
    waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
    waitTime$Appt.DateYear <- as.Date(waitTime$Appt.DateYear, format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(New.PT3, medWaitTime)
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 3:4)
    ggplot(waitTime %>% filter(variable == "Established"), aes(x=Appt.DateYear, y=value, group=Provider)) +
      geom_line(aes(color=Provider), size=1) +
      geom_hline(yintercept=14, linetype="dashed", color = "red", size=1)+
      scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
      labs(x=NULL, y=NULL, 
           title = "Median Wait Time to New and Established Appointment Over Time by Provider",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("bottom")+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
  })
  
  
  # New Patient Wait Time
  output$newPtApptSourceByDept <- renderPlot({
    data <- dataArrived()
    # data <- kpi.all.data[arrivedNoShow.data.rows,]
    
    newpatients.ratio <- data %>%
      group_by(Appt.Source.New, New.PT3) %>%
      filter(New.PT3 == TRUE) %>%
      dplyr::summarise(Total = n()) 
    
    newpatients.ratio$Appt.Source.New[which(newpatients.ratio$Appt.Source.New == "Other")] <- "Practice"
    
    newpatients.ratio$ratio <- round(newpatients.ratio$Total / sum(newpatients.ratio$Total), 2)
    
    newRatio <-
      ggplot(newpatients.ratio, aes(x=factor(Appt.Source.New, levels = c("Practice","Access Center","MyChart","StayWell","Zocdoc")), 
                                    y=ratio, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      coord_flip() +
      scale_fill_MountSinai('purple')+
      labs(x=NULL, y=NULL,
           title = "New Patient Ratio*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),
                             "\nTotal New Patients = ",prettyNum(sum(newpatients.ratio$Total), big.mark = ',')),
           caption = "*Based on arrived patients\n**New patients defined by CPT codes (level of service).")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(newpatients.ratio$ratio)*1.3))+
      geom_text(aes(label=paste0(ratio*100,"%")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0) %>%
      group_by(Appt.Source.New, New.PT3) %>%
      dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
      filter(New.PT3 == TRUE)
    waitTime$target <- 14
    
    waitTime$Appt.Source.New[which(waitTime$Appt.Source.New == "Other")] <- "Practice"
    
    newWaitTime <-
      ggplot(waitTime, aes(x=factor(Appt.Source.New, levels = c("Practice","Access Center","MyChart","StayWell","Zocdoc")), 
                           y=medWaitTime, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      geom_hline(aes(yintercept=target), linetype="dashed", color = "red", size=1)+
      scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.3))+
      coord_flip() +
      scale_fill_MountSinai('pink')+
      labs(x=NULL, y=NULL, 
           title = "Wait Time* to New Appointment",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),
                             "\nWait Time = (Scheduled Appt Date - Appt Made Date)"),
           caption = "*Based on all of scheduled patients\n**New patients defined by CPT codes (level of service).")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
      geom_text(aes(label=paste0(medWaitTime," days")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    # No Show Rate
    
    data.noShow <- dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data.noShow <- arrivedNoShow.data
    
    noShows <- data.noShow %>%
      filter(New.PT3 == TRUE) %>%
      group_by(Appt.Source.New, Appt.Status) %>%
      dplyr::summarise(Total = n()) %>%
      spread(Appt.Status, Total)
    
    noShows[is.na(noShows)] <- 0
    
    noShows$`No Show Perc` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)
    noShows$Appt.Source.New[which(noShows$Appt.Source.New == "Other")] <- "Practice"
    
    
    noShows$Appt.Source.New <- ifelse(noShows$Appt.Source.New == "Other", "Practice", noShows$Appt.Source.New)
    
    
    
    newNoShow <-
      
      ggplot(noShows, aes(x=factor(Appt.Source.New, levels =  c("Practice","Access Center","MyChart","StayWell","Zocdoc")), 
                          y=`No Show Perc`, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,max(noShows$`No Show Perc`))*1.3)+
      coord_flip() +
      scale_fill_MountSinai('blue')+
      labs(x=NULL, y=NULL,
           title = "New Patient No Show Rate*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]),
                             "\nNo Show Rate = Total No Shows / (Arrived + No Shows)"),
           caption = "*Based on all of scheduled patients\n**New patients defined by CPT codes (level of service).")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      geom_text(aes(label=paste0(`No Show Perc`*100,"%")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    grid.arrange(newRatio, newWaitTime, newNoShow, ncol=3)
    
  })
  
  # Booked and Filled Rate
  
  output$slotManageGraph <- renderPlotly({
    
    #data <- slot.data.subset %>% filter(Campus == "MSUS")
    
    data <- dataAllSlot()
    
    booked_filled <- data %>%
      group_by(Appt.DateYear) %>%
      summarise(`Available Hours` = sum(`Available Hours`),
                `Booked Hours` = sum(`Booked Hours`),
                `Filled Hours` = sum(`Arrived Hours`)) %>%
      mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`, 2)*100,
             `Filled Rate` = round(`Filled Hours`/`Available Hours`, 2)*100) 
    
    slot_fig <- plot_ly(booked_filled, x = ~Appt.DateYear,
                        textfont = list(color = '#000000', size = 16))
    
    if(input$byRate == TRUE){
      y_axis <- "Booked Rate" 
    } else{
      y_axis <- "Available Hours"
    }
    
    today <- max(dataAll()$Appt.DateYear) + 2
    annon <- list()
    i <- 1
    # annon <- list(list(text = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
    #                    x = 0.78,
    #                    y = 1.14,
    #                    yref = "paper",
    #                    xref = "paper",
    #                    showarrow = FALSE
    # ))
    # i <- 2
    
    
    
    
    
    # if(min(booked_filled$Appt.DateYear) <= today  && today  <= max(booked_filled$Appt.DateYear)){
    #   slot_fig <- slot_fig %>% add_segments(x = today,
    #                                         xend = today,
    #                                         y = 0, yend = max(booked_filled[[y_axis]]  * 1.2),
    #                                         line = list(color = "#000000", dash = "dash"),
    #                                         showlegend = FALSE,
    #                                         hoverinfo = 'skip')
    #   
    #   annon[[i]] <- list(
    #     x = today,
    #     y = max(booked_filled[[y_axis]]  * 1.25, na.rm = TRUE),
    #     text = "Today",
    #     xref = "x",
    #     yref= "y",
    #     showarrow = FALSE,
    #     ax = 20,
    #     ay=  -40,
    #     font = list(color = '#000000',size = 18)
    #   )
    # 
    # 
    #   i <- i + 1
    # }
    
    
    if(input$byRate == TRUE){ # by Booked and Filled Rate
      
      slot_fig <- slot_fig %>% add_segments(x = min(booked_filled$Appt.DateYear),
                                            xend = max(booked_filled$Appt.DateYear),
                                            y = 100,
                                            yend = 100,
                                            line = list(color = "#FF0000", dash = "dash"),
                                            showlegend = FALSE)
      
      annon[[i]] <- list(
        x = max(booked_filled$Appt.DateYear)+5,
        y = 100 ,
        text = "100 %",
        xref = "x",
        yref= "y",
        showarrow = FALSE,
        ax = 20,
        ay=  -40,
        font = list(color = '#FF0000',size = 18)
      )
      
      i <- i + 1
      
      slot_fig <- slot_fig %>% add_trace(y = ~`Booked Rate`, name = "Booked Rate (%)", mode = 'lines+markers',
                                         marker = list(color = "#d80b8c"), line = list(color = "#d80b8c"))
      slot_fig <- slot_fig %>% add_trace(y = ~`Filled Rate`, name = "Filled Rate (%)", mode = 'lines+markers',
                                         marker = list(color = "#00aeef"), line = list(color = "#00aeef"))
      
      slot_fig %>% layout(
        #annotations = annon,
        #shapes=list(type='line', x0= max(dataAll()$Appt.DateYear + 2), x1= max(dataAll()$Appt.DateYear + 2), y0=50000, y1=50000, line=list(dash='dot', width=1)),
        title = "Past and Upcoming Slot Usage (%)", font=list(size=20),
        autosize = T, margin=list( l = 50, r = 50, b = 100, t = 130,  pad = 4),
        xaxis = list(
          title = "Date", 
          font = list(size = 14),
          tickfont = list(size = 14),
          
          rangeslider = list(type = "date"),
          mirror = TRUE,
          ticks = 'outside',
          showline = TRUE),
        yaxis = list(title = "Percent",
                     font = list(size = 14),
                     tickfont = list(size = 14),
                     ticksuffix = "%",
                     mirror = TRUE,
                     ticks = 'outside',
                     showline = TRUE),
        hovermode = "x unified") 
      
    } else{
      
      
      slot_fig <- slot_fig %>% add_trace(y = ~`Available Hours`, name = "Available Hours", mode = 'lines+markers',
                                         marker = list(color = "#212070"), line = list(color = "#212070"))
      slot_fig <- slot_fig %>% add_bars(y = ~`Booked Hours`, name = "Booked Hours",
                                        marker = list(color = "#d80b8c"))
      slot_fig <- slot_fig %>% add_bars(y = ~`Filled Hours`, name = "Filled Hours",
                                        marker = list(color = "#00aeef"))
      
      
      
      slot_fig %>% layout(
        #annotations = annon,
        #shapes=list(type='line', x0= max(dataAll()$Appt.DateYear + 2), x1= max(dataAll()$Appt.DateYear + 2), y0=50000, y1=50000, line=list(dash='dot', width=1)),
        title = "Past and Upcoming Slot Usage", font=list(size=20),
        autosize = T, margin=list( l = 50, r = 50, b = 100, t = 130,  pad = 4),
        xaxis = list(
          font = list(size = 16),
          title = "Date", 
          tickfont = list(size = 14),
          rangeslider = list(type = "date"),
          mirror = TRUE,
          ticks = 'outside',
          showline = TRUE),
        yaxis = list(
          font = list(size = 14),
          title = "Hours",
          tickfont = list(size = 14),
          mirror = TRUE,
          ticks = 'outside',
          showline = TRUE),
        hovermode = "x unified") 
    }
    
    
    
  })
  
  
  # Slot Usage Table by Practice and Provider 
  slotUsageTb_data <- reactive({
    
    data <- dataAllSlot()
    #data <- slot.data.subset[all.slot.rows,] %>% filter(Campus == "MSUS") %>% filter(Campus.Specialty == "Cardiology")
    
    if(input$byProvider2 == TRUE){
      
      summary.prov <- data %>%
        group_by(Campus, Campus.Specialty, Provider, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`, na.rm=TRUE),1),
                         `Booked Hours` = round(sum(`Booked Hours`),1),
                         `Filled Hours` = round(sum(`Arrived Hours`),1)) 
      
      summary.prov[is.na(summary.prov)] <- 0
      
      summary.prov <- summary.prov %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100), "%")) %>%
        gather(variable, value, 5:9) %>%
        spread(Appt.MonthYear, value)
      
      summary.prov[summary.prov == "Inf%"] <- "-"
      summary.prov[summary.prov == "NaN%"] <- "-"
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov <- summary.prov[order(match(summary.prov$variable, level.order)),]
      summary.prov <- summary.prov %>% arrange(Campus, Campus.Specialty, Provider)
      
      names(summary.prov)[names(summary.prov) == "Campus.Specialty"] <- "Specialty"
      names(summary.prov)[names(summary.prov) == "variable"] <- "Status"
      
      
      
      
      #### Get daily avergae
      
      # summary.prov.day <- data %>%
      #   group_by(Campus, Campus.Specialty, Provider, Appt.DateYear) %>%
      #   dplyr::summarise(`Available Hours` = round(sum(`Available Hours`),1),
      #                    `Booked Hours` = round(sum(`Booked Hours`),1),
      #                    `Filled Hours` = round(sum(`Arrived Hours`),1)) 
      # 
      # summary.prov.day[is.na(summary.prov.day)] <- 0
      # 
      # summary.prov.day <- summary.prov.day %>%
      #   mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100),
      #          `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100)) %>%
      #   gather(variable, value, 5:9) %>%
      #   spread(Appt.DateYear, value)
      # 
      # 
      # level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      # summary.prov.day <- summary.prov.day[order(match(summary.prov.day$variable, level.order)),]
      # summary.prov.day <- summary.prov.day %>% arrange(Campus, Campus.Specialty, Provider)
      # 
      # names(summary.prov.day)[names(summary.prov.day) == "Campus.Specialty"] <- "Specialty"
      # names(summary.prov.day)[names(summary.prov.day) == "variable"] <- "Status"
      # 
      # summary.prov.day.final <- data.frame(summary.prov.day[,1:4], `Daily Average` = round(rowMeans(summary.prov.day[,-1:-4], na.rm = TRUE),1), check.names = FALSE)
      # summary.prov.day.final[summary.prov.day.final == as.character("Inf")] <- "-"
      # summary.prov.day.final[summary.prov.day.final == as.character("NaN")] <- "-"
      # summary.prov.day.final$`Daily Average` <- ifelse(summary.prov.day.final$Status == c("Booked Rate (%)", "Filled Rate (%)"), paste0(summary.prov.day.final$`Daily Average`, "%"),summary.prov.day.final$`Daily Average`)
      # summary.prov.day.final[summary.prov.day.final == as.character("-%")] <- "-"
      
      
      
      ###Monthly Averages
      
      
      summary.prov.month <- data %>%
        group_by(Campus, Campus.Specialty, Provider, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`, na.rm=TRUE),1),
                         `Booked Hours` = round(sum(`Booked Hours`),1),
                         `Filled Hours` = round(sum(`Arrived Hours`),1)) 
      
      summary.prov.month[is.na(summary.prov.month)] <- 0
      
      summary.prov.month <- summary.prov.month %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100)) %>%
        gather(variable, value, 5:9) %>%
        spread(Appt.MonthYear, value)
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov.month <- summary.prov.month[order(match(summary.prov.month$variable, level.order)),]
      summary.prov.month <- summary.prov.month %>% arrange(Campus, Campus.Specialty, Provider)
      
      
      summary.prov.month <- data.frame(summary.prov.month[,1:4], `Monthly Average` = round(rowMeans(summary.prov.month[,-1:-4], na.rm = TRUE),1), check.names = FALSE)
      names(summary.prov.month)[names(summary.prov.month) == "variable"] <- "Status"
      names(summary.prov.month)[names(summary.prov.month) == "Campus.Specialty"] <- "Specialty"
      summary.prov.month[summary.prov.month == as.character("Inf")] <- "-"
      summary.prov.month[summary.prov.month == as.character("NaN")] <- "-"
      
      summary.prov.month$`Monthly Average` <- ifelse(summary.prov.month$Status %in% c("Booked Rate (%)", "Filled Rate (%)"), paste0(summary.prov.month$`Monthly Average`, "%"),summary.prov.month$`Monthly Average`)
      summary.prov.month[summary.prov.month == as.character("-%")] <- "-"
      
      
      summary.prov <- merge(summary.prov.month,summary.prov, by = c("Campus", "Specialty", "Provider", "Status"))
      
      # summary.prov %>%
      #   knitr::kable("html", align = "l") %>%
      #   kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      #   row_spec(0, bold=T, background = "#7f7f7f", color = "white") %>%
      #   column_spec(1:4, bold=T) %>%
      #   column_spec(4, width = "2in") %>%
      #   collapse_rows(columns = 1:3, valign = "middle") %>%
      #   add_header_above(c("Summary of Booked and Filled Rate by Site and Specialty" = length(summary.prov)),
      #                    background = "#221f72", color = "white", font_size = 18, align = "center") %>%
      #   scroll_box(height = "800px")
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov <- summary.prov[order(match(summary.prov$Status, level.order)),]
      summary.prov <- summary.prov %>% arrange(Campus, Specialty, Provider)
      
      
      
      summary.prov
      
    } else {
      
      
      
      summary.dept <- data %>%
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`),1),
                         `Booked Hours` = round(sum(`Booked Hours`),1),
                         `Filled Hours` = round(sum(`Arrived Hours`),1))
      
      summary.dept[is.na(summary.dept)] <- 0
      
      summary.dept <- summary.dept %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100), "%")) %>%
        gather(variable, value, 4:8) %>%
        spread(Appt.MonthYear, value)
      
      summary.dept[summary.dept == "Inf%"] <- "-"
      summary.dept[summary.dept == "NaN%"] <- "-"
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept <- summary.dept[order(match(summary.dept$variable, level.order)),]
      summary.dept <- summary.dept %>% arrange(Campus, Campus.Specialty)
      
      names(summary.dept)[names(summary.dept) == "Campus.Specialty"] <- "Specialty"
      names(summary.dept)[names(summary.dept) == "variable"] <- "Status"
      
      
      
      
      #### Get daily avergae
      # summary.prov.day <- data %>%
      #   group_by(Campus, Campus.Specialty, Appt.MonthYear) %>%
      #   dplyr::summarise(`Available Hours` = round(sum(`Available Hours`),1),
      #                    `Booked Hours` = round(sum(`Booked Hours`),1),
      #                    `Filled Hours` = round(sum(`Arrived Hours`),1),
      #                    `Booked Rate (%)` = round(sum(`Booked Hours`/`Available Hours`)*100),
      #                    `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100))
      # 
      # summary.prov.day[is.na(summary.prov.day)] <- 0
      # 
      # for (i in 1:nrow(test.dept)){
      #   if(summary.prov.day[i,3] == test.dept[i,1]){
      #     summary.prov.day[i,4:6] <- summary.prov.day[i,4:6] / test.dept[[i,2]]
      #     #summary.prov.day[i,7:8] <- summary.prov.day[i,7:8] * test.dept[[i,2]]
      # 
      #   }
      # }
      # 
      # summary.prov.day <- summary.prov.day %>%
      #   gather(variable, value, 4:8) %>%
      #   spread(Appt.MonthYear, value)
      # 
      # 
      # level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      # summary.prov.day <- summary.prov.day[order(match(summary.prov.day$variable, level.order)),]
      # summary.prov.day <- summary.prov.day %>% arrange(Campus, Campus.Specialty)
      # 
      # names(summary.prov.day)[names(summary.prov.day) == "Campus.Specialty"] <- "Specialty"
      # names(summary.prov.day)[names(summary.prov.day) == "variable"] <- "Status"
      # 
      # summary.prov.day.final <- data.frame(summary.prov.day[,1:3], `Monthly Average` = round(rowMeans(summary.prov.day[,-1:-3], na.rm = TRUE),1), check.names = FALSE)
      # summary.prov.day.final[summary.prov.day.final == as.character("Inf")] <- "-"
      # summary.prov.day.final[summary.prov.day.final == as.character("NaN")] <- "-"
      # summary.prov.day.final$`Monthly Average` <- ifelse(summary.prov.day.final$Status %in% c("Booked Rate (%)", "Filled Rate (%)"), paste0(summary.prov.day.final$`Monthly Average`, "%"),summary.prov.day.final$`Monthly Average`)
      # summary.prov.day.final[summary.prov.day.final == as.character("-%")] <- "-"
      # 
      # 
      
      
      
      
      #Get Monthly Averages
      
      summary.dept.month <- data %>%
        group_by(Campus, Campus.Specialty, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`),1),
                         `Booked Hours` = round(sum(`Booked Hours`),1),
                         `Filled Hours` = round(sum(`Arrived Hours`),1))
      
      summary.dept.month[is.na(summary.dept.month)] <- 0
      
      summary.dept.month <- summary.dept.month %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100)) %>%
        gather(variable, value, 4:8) %>%
        spread(Appt.MonthYear, value)
      
      summary.dept.month[summary.dept.month == "Inf%"] <- "-"
      summary.dept.month[summary.dept.month == "NaN%"] <- "-"
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept.month <- summary.dept.month[order(match(summary.dept.month$variable, level.order)),]
      summary.dept.month <- summary.dept.month %>% arrange(Campus, Campus.Specialty)
      
      names(summary.dept.month)[names(summary.dept.month) == "Campus.Specialty"] <- "Specialty"
      names(summary.dept.month)[names(summary.dept.month) == "variable"] <- "Status"
      
      # summary.dept.month[summary.dept.month == as.character("Inf")] <- "-"
      # summary.dept.month[summary.dept.month == as.character("NaN")] <- "-"
      
      summary.dept.month <- data.frame(summary.dept.month[,1:3], `Monthly Average` = round(rowMeans(summary.dept.month[,-1:-3], na.rm = TRUE),1), check.names = FALSE)
      
      
      summary.dept.month$`Monthly Average` <- ifelse(summary.dept.month$Status %in% c("Booked Rate (%)", "Filled Rate (%)"), paste0(summary.dept.month$`Monthly Average`, "%"),summary.dept.month$`Monthly Average`)
      summary.dept.month[summary.dept.month == as.character("-%")] <- "-"
      
      
      summary.dept <- merge(summary.dept.month,summary.dept, by = c("Campus", "Specialty", "Status"),sort=FALSE)
      
      
      
      # summary.dept %>%
      #   knitr::kable("html", align = "l") %>%
      #   kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      #   row_spec(0, bold=T, background = "#7f7f7f", color = "white") %>%
      #   column_spec(1:3, bold=T) %>%
      #   column_spec(3, width = "4in") %>%
      #   collapse_rows(columns = 1:2, valign = "middle") %>%
      #   add_header_above(c("Summary of Booked and Filled Rate by Site and Specialty" = length(summary.dept)),
      #                    background = "#221f72", color = "white", font_size = 18, align = "center") %>%
      #   scroll_box(height = "800px")
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept <- summary.dept[order(match(summary.dept$Status, level.order)),]
      summary.dept <- summary.dept %>% arrange(Campus, Specialty)
      summary.dept
    }
  })
  
  output$slotUsageTb <- DT::renderDT(slotUsageTb_data(), server = FALSE,
                                     class = 'cell-border stripe',
                                     rownames = FALSE,
                                     extensions = c('Buttons','Scroller'),
                                     options = list(
                                       scrollX = TRUE,
                                       list(pageLength = 20, scrollY = "400px"),
                                       dom = 'Bfrtip',
                                       buttons = c('csv','excel'),
                                       sDom  = '<"top">lrt<"bottom">ip',
                                       initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                                         "}"),
                                       fixedColumns = list(leftColumns =
                                                             ifelse(colnames(slotUsageTb_data())[3] == "Provider", 4, 3))))
  
  
  # datatable(summary.prov,
  #           class = 'cell-border stripe',
  #           rownames = FALSE,
  #           extensions = c('Buttons','Scroller'),
  #           title = "Subset Selection",
  #           options = list(
  #             scrollX = TRUE,
  #             list(pageLength = 20, scrollY = "400px"),
  #                          dom = 'Bfrtip',
  #                          buttons = c('csv','excel'),
  #                          sDom  = '<"top">lrt<"bottom">ip',
  #                          initComplete = JS(
  #                            "function(settings, json) {",
  #                            "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
  #                            "}"),
  #                          fixedColumns = list(leftColumns =
  #                                                ifelse(colnames(summary.prov)[3] == "Provider", 4, 3)))) 
  
  
  
  ### [3. ] Day of Visit Tab -----------------------------------------------------------------------------------------------------------
  
  # Reactive Filters for Scheduling Tab: Appointment Type & Insurance 
  output$apptTypeControl2 <- renderUI({
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType2", label = h4("Select Appointment Type for Comparison:"), 
                  choices = sort(unique((dataAll() %>% filter(New.PT3 == FALSE))$Appt.Type)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Types", 
                    dropupAuto = FALSE),
                  selected = sort(unique((dataAll() %>% filter(New.PT3 == FALSE))$Appt.Type))))
  })
  
  
  output$apptTypeControl3 <- renderUI({
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType3", label = h4("Select Appointment Type for Comparison:"), 
                  choices = sort(unique((dataAll() %>% filter(New.PT3 == FALSE))$Appt.Type)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Types", 
                    dropupAuto = FALSE),
                  selected = sort(unique((dataAll() %>% filter(New.PT3 == FALSE))$Appt.Type))))
    
  })
  
  
  # Arrived Data with Additional Filters (Appointment Type)
  dataNewComparison <- reactive({
    groupByFilters_3(dataArrived(),
                     input$selectedApptType2)
  })
  
  # Arrived Data with Additional Filters (Appointment Type)
  dataNewComparison2 <- reactive({
    groupByFilters_3(dataArrived(),
                     input$selectedApptType3)
  })
  
  
  # (1) Cycle Times --------------------------------------------------------------------------
  
  output$cycleTimeCompNew <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(cycleTime > 0, New.PT3 == TRUE))$cycleTime))," min"),
      title = toupper("Average New Patients Check-in to Visit-end Time"),
      subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(cycleTime > 0))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$cycleTimeCompOther <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE))$cycleTime))," min"),
      title = toupper(ifelse(length(unique(dataNewComparison()$Appt.Type)) == 1,
                             paste0("Average ", input$selectedApptType2," Appointments Check-in to Visit-end Time"),
                             "Average Non-New Patients Check-in to Visit-end Time")),
      subtitle = paste0("*Based on ",round(nrow(dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$cycleTimeTrend <- renderPlot({
    data_new <- dataArrived() %>% filter(cycleTime > 0, New.PT3 == TRUE)
    # data_new <- arrived.data %>% filter(cycleTime > 0 & New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    
    data_other <- dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE)
    # data_other <- arrived.data %>% filter(cycleTime > 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    if(nrow(data_other) == 0){
      
      data <- data_new
    } else{
      
      data <- rbind(data_new, data_other)
    }
    
    data <- data %>%
      mutate(New.PT3 = ifelse(New.PT3 == TRUE, "NEW", Appt.Type)) %>%
      filter(!is.na(New.PT3))
    
    
    ggplot(data, aes(x=cycleTime, fill=New.PT3, color=New.PT3)) +
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     # position = "identity",
                     alpha = 0.8)+
      scale_color_MountSinai()+
      scale_fill_MountSinai()+
      labs(title = "Check-in to Visit-end Time Comparison by Appointment Type",
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  output$newCycleTimeBoxPlot <- renderPlot({
    data <- dataArrived() %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    
    ggplot(data, aes(x=cycleTime)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 22,
                     color="#d80b8c", fill="#fcc9e9") +
      labs(title = "Distribution of NEW Appointment\nCheck-in to Visit-end Time*", 
           y = "% of Patients",
           x = "Minutes",
           caption = "-",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
    
  })
  
  output$establishedCycleTimeBoxPlot <- renderPlot({
    data_other <- dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE)
    # data_other <- arrived.data %>% filter(cycleTime > 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type %in% c("NEW PATIENT", "FOLLOW UP"))
    
    data <- data_other
    
    if(length(unique(data$Appt.Type)) == 1){
      appt.type <- unique(data$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    ggplot(data, aes(x=cycleTime)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 22,
                     color="#d80b8c", fill="#fcc9e9") +
      labs(title = paste0("Distribution of ",appt.type," Appointments\nCheck-in to Visit-end Time*"), 
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
  })
  
  
  output$cycleTimeByHour <- renderPlot({
    data <- dataArrived() %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    
    data_other <- dataNewComparison() %>% filter(New.PT3 == FALSE, cycleTime > 0)
    # data_other <- arrived.data %>% filter(New.PT3 == FALSE, cycleTime > 0)
    
    names <- paste(unique(data_other$Appt.Type),sep="", collapse=", ")
    
    if(input$median2 == TRUE){
      
      data <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = median(cycleTime)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      data_other <- data_other %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = median(cycleTime)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      input <- "Median"
    } else{
      
      data <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = mean(cycleTime)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      data_other <- data_other %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = mean(cycleTime)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      input <- "Average"
    }
    
    if(length(unique(data_other$Appt.Type)) == 1){
      appt.type <- unique(data_other$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    new <- ggplot(data, aes(Appt.TM.Hr, Appt.Day, fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#d80b8c")+
      labs(title = paste0(input," NEW Appointments Check-in to Visit-end Time by Hour"),
           y = NULL,
           #subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]),
           fill = "Minutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg))), color="black", size=5, fontface="bold")
    
    other <- ggplot(data_other, aes(Appt.TM.Hr, Appt.Day, fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#00aeef")+
      labs(title = paste0(input," ",appt.type," Appointments Check-in to Visit-end Time by Hour\n"), 
           y = NULL,
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"),
           fill = "Minutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg))), color="black", size=5, fontface="bold")
    
    grid.arrange(new, other, ncol = 1)
    
  })
  
  
  output$newCycleTimeByProv <- renderPlot({
    data <- dataArrived() %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    
    cycle.df <- data %>%
      select(Provider, New.PT3, cycleTime) %>%
      group_by(Provider, New.PT3)
    
    avg.cycleTime <- data.frame(New.PT3 = c("Avg"),
                                target =  round(mean(cycle.df$cycleTime)))
    
    ggplot(cycle.df, aes(x = Provider, y = cycleTime)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(limits = c(0,quantile(cycle.df$cycleTime,0.75)*1.5))+
      geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = "Distribution of NEW Appointment Check-in to Visit-end Time by Provider", 
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "-")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
    
  })
  
  output$establishedCycleTimeByProv <- renderPlot({
    data_other <- dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE)
    # data_other <- arrived.data %>% filter(cycleTime > 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    data <- data_other
    
    if(length(unique(data$Appt.Type)) == 1){
      appt.type <- unique(data$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    cycle.df <- data %>%
      select(Provider, New.PT3, cycleTime) %>%
      group_by(Provider, New.PT3)
    
    avg.cycleTime <- data.frame(New.PT3 = c("Avg"),
                                target =  round(mean(cycle.df$cycleTime)))
    
    ggplot(cycle.df, aes(x = Provider, y = cycleTime)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(limits = c(0,quantile(cycle.df$cycleTime,0.75)*1.5))+
      geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      annotate("text",x=length(unique(cycle.df$Provider))/2,y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Visit-end Time by Provider"), 
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
  })
  
  # (2) Room-in Times ----------------------------------------------------------------------
  
  output$roomInTimeCompNew <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE))$checkinToRoomin))," min"),
      title = toupper("Avg. New Appointments Check-in to Room-in Time"),
      subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeCompOther <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataNewComparison2() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))$checkinToRoomin))," min"),
      title = toupper(ifelse(length(unique(dataNewComparison2()$Appt.Type)) == 1,
                             paste0("Avg. ", input$selectedApptType2," Appointments Check-in to Room-in Time"),
                             "Avg. Non-New* Appointments Check-in to Room-in Time")),
      subtitle = paste0("*Based on ",round(nrow(dataNewComparison2() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeTrend <- renderPlot({
    data_new <- dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE)
    # data_new <- arrived.data %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE)
    
    data_other <- dataNewComparison2() %>% filter(checkinToRoomin >= 0,  New.PT3 == FALSE)
    # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    if(nrow(data_other) == 0){
      data <- data_new
    } else{
      data <- rbind(data_new, data_other)
    }
    
    data <- data %>%
      mutate(New.PT3 = ifelse(New.PT3 == TRUE, "NEW", Appt.Type)) %>%
      filter(!is.na(New.PT3))
    
    
    ggplot(data, aes(x=checkinToRoomin, fill=New.PT3, color=New.PT3)) +
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     # position = "identity",
                     alpha = 0.8)+
      scale_color_MountSinai()+
      scale_fill_MountSinai()+
      labs(title = "Check-in to Room-in Time Comparison by Appointment Type",
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  output$newRoomInTimeBoxPlot <- renderPlot({
    data <- dataArrived() %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE)
    
    ggplot(data, aes(x=checkinToRoomin)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 22,
                     color="#d80b8c", fill="#fcc9e9") +
      labs(title = "Distribution of NEW Appointment\nCheck-in to Room-in Time", 
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "-")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  output$establishedRoomInTimeBoxPlot <- renderPlot({
    data_other <- dataNewComparison2() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE)
    # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    data <- data_other
    
    if(length(unique(data$Appt.Type)) == 1){
      appt.type <- unique(data$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    ggplot(data, aes(x=checkinToRoomin)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 22,
                     color="#d80b8c", fill="#fcc9e9") +
      labs(title = paste0("Distribution of ",appt.type,"Appointments\nCheck-in to Room-in Time"), 
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  output$roomInTimeByHour <- renderPlot({
    data <- dataArrived() %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    
    data_other <- dataNewComparison2() %>% filter(New.PT3 == FALSE, checkinToRoomin > 0)
    # data_other <- arrived.data %>% filter(New.PT3 == FALSE, checkinToRoomin > 0)
    
    names <- paste(unique(data_other$Appt.Type),sep="", collapse=", ")
    
    if(input$median3 == TRUE){
      
      data <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = median(checkinToRoomin)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = median(checkinToRoomin)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      input <- "Median"
    } else{
      
      data <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = mean(checkinToRoomin)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        summarise(avg = mean(checkinToRoomin)) %>%
        filter(Appt.TM.Hr %in% timeOptionsHr_filter)
      
      input <- "Average"
    }
    
    if(length(unique(data_other$Appt.Type)) == 1){
      appt.type <- unique(data_other$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    new <- ggplot(data, aes(Appt.TM.Hr, Appt.Day, fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#d80b8c")+
      labs(title = paste0(input," New Patients Check-in to Visit-end Time by Hour"),
           y = NULL,
           fill = "Minutes")+
      #subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg))), color="black", size=5, fontface="bold")
    
    
    other <- ggplot(data_other, aes(Appt.TM.Hr, Appt.Day, fill = avg))+ 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#00aeef")+
      labs(title = paste0(input," ",appt.type," Patients Check-in to Visit-end Time by Hour\n"), 
           y = NULL,
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"),
           fill = "Minutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size = unit(.8,"cm"),
            legend.text = element_text(size="10"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size="14", margin = unit(c(8, 8, 8, 8), "mm")),
            axis.text.x = element_text(color="black"),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,30,30,30))+
      geom_text(aes(label= ifelse(is.na(avg),"", round(avg))), color="black", size=5, fontface="bold")
    
    grid.arrange(new, other, ncol = 1)
    
  })
  
  
  output$newRoomInTimeByProv <- renderPlot({
    data <- dataArrived() %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    
    roomin.df <- data %>%
      select(Provider, New.PT3, checkinToRoomin) %>%
      group_by(Provider, New.PT3)
    
    avg.roomInTime <- data.frame(New.PT3 = c("Avg"),
                                 target =  round(mean(roomin.df$checkinToRoomin)))
    
    ggplot(roomin.df, aes(x = Provider, y = checkinToRoomin)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(limits = c(0,quantile(roomin.df$checkinToRoomin,0.75)*1.5))+
      geom_hline(yintercept= round(mean(roomin.df$checkinToRoomin)), linetype="dashed", color = "red")+
      annotate("text",x=length(unique(roomin.df$Provider))/2,y=round(mean(roomin.df$checkinToRoomin))+3,size=5,color="red",label=c('Average'))+
      labs(title = "Distribution of NEW Appointment Check-in to Room-in Time by Provider", 
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
    
  })
  
  output$establishedRoomInTimeByProv <- renderPlot({
    data_other <- dataNewComparison2() %>% filter(checkinToRoomin >= 0)
    # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    data <- data_other
    
    if(length(unique(data$Appt.Type)) == 1){
      appt.type <- unique(data$Appt.Type)
    } else{
      appt.type <- "Other*"
    }
    
    roomIn.df <- data %>%
      select(Provider, New.PT3, checkinToRoomin) %>%
      group_by(Provider, New.PT3)
    
    avg.roomInTime <- data.frame(New.PT3 = c("Avg"),
                                 target =  round(mean(roomIn.df$checkinToRoomin)))
    
    ggplot(roomIn.df, aes(x = Provider, y = checkinToRoomin)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(limits = c(0,quantile(roomIn.df$checkinToRoomin,0.75)*1.5))+
      geom_hline(yintercept= round(mean(roomIn.df$checkinToRoomin)), linetype="dashed", color = "red")+
      annotate("text",x=length(unique(roomIn.df$Provider))/2,y=round(mean(roomIn.df$checkinToRoomin))+3,size=5,color="red",label=c('Average'))+
      labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Room-in Time by Provider"), 
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Includes ", length(unique(data$Appt.Type)), "non-new appointments"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
  })
  
  # # Example Event Log Dataset 
  # 
  # ex_pts_throughput <- as.data.frame(ex_patients %>% throughput_time("case"))
  # ex_pts_processing <- as.data.frame(ex_patients %>% processing_time("case"))
  # ex_pts_idle <- as.data.frame(ex_patients %>% idle_time("case"))
  # ex_pts_df <- merge(ex_pts_throughput, ex_pts_processing)
  # ex_pts_df <- merge(ex_pts_df, ex_pts_idle)
  # 
  # ex_pts_df_melt <- ex_pts_df
  # colnames(ex_pts_df_melt) <- c("patient","Cycle Time (Hrs.)","Value Added Time (Hrs.)","Non-Value Added Time (Hrs.)")
  # ex_pts_df_melt <- reshape2::melt(ex_pts_df_melt, id=c("patient"))
  # 
  # # Average Cycle Time
  # output$avgCycleTime2 <- renderValueBox({
  # 
  #   valueBox(
  #     paste0(round(mean(ex_pts_df$throughput_time, na.rm = TRUE),1)," hour(s)"),
  #     subtitle = tags$p("Avg Cycle Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
  #   )
  # 
  # })
  
  # # Average Value Added Time
  # output$avgValueAdded <- renderValueBox({
  #   
  #   valueBox(
  #     paste0(round(mean(ex_pts_df$processing_time, na.rm = TRUE),1)," hour(s) (",
  #            round(mean(ex_pts_df$processing_time, na.rm = TRUE)/mean(ex_pts_df$throughput_time, na.rm = TRUE)*100,0),"%)"),
  #     subtitle = tags$p("Avg Value Added Time", style = "font-size: 130%;"), icon = NULL, color = "aqua"
  #   )
  #   
  # })
  # 
  # # Average Non-Value Added Time
  # output$avgNonValueAdded <- renderValueBox({
  #   
  #   valueBox(
  #     paste0(round(mean(ex_pts_df$idle_time, na.rm = TRUE),1)," hour(s) (",
  #            round(mean(ex_pts_df$idle_time, na.rm = TRUE)/mean(ex_pts_df$throughput_time, na.rm = TRUE)*100,0),"%)"),
  #     subtitle = tags$p("Avg Non-Value Added Time", style = "font-size: 130%;"), icon = NULL, color = "fuchsia"
  #   )
  #   
  # })
  # 
  # # throughput_tb <- ex_patients %>% throughput_time("log")
  # # processing_tb <- ex_patients %>% processing_time("log")
  # # idle_tb <- ex_patients %>% idle_time("log")
  # 
  # # Distribution of cycle, value added, non-value added times 
  # output$cycleTimeDis <- renderPlot ({
  #   
  #   ggplot(ex_pts_df_melt, aes(x=variable, y=value, fill=variable))+ 
  #     scale_fill_manual(values = c("#dddedd", "#5cd3ff", "#f75dbe"))+
  #     stat_summary(fun=mean, geom="point", shape=18, size=5, color="red", fill="red")+
  #     theme_new_line()+
  #     theme_bw()+
  #     theme(
  #       plot.title = element_blank(),
  #       legend.title = element_blank(),
  #       legend.position = "none",
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #       axis
  # })
  # 
  # # Patient flow by frequency count
  # output$vsm_freqCount <- renderGrViz({
  #   process_map(ex_patients, type = frequency("absolute"))
  # })
  # 
  # # Patient flow by relative frequency (%) (Total count of activity / Total count of prior activity)
  # output$vsm_freqPerc <- renderGrViz({
  #   process_map(ex_patients, type = frequency("relative_case"))
  # })
  # 
  # # Patient flow by duration (average)
  # output$vsm_durAvg <- renderGrViz({
  #   process_map(ex_patients, performance(FUN=mean, "hours"))
  # })
  # 
  # # Patient flow by duration (median)
  # output$vsm_durMed<- renderGrViz({
  #   process_map(ex_patients, performance(FUN=median, "hours"))
  # })
  # 
  # 
  # 
  # 
  # 
  # 
  
  
  
  
  
  # detachAllPackages <- function() {
  #   
  #   basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  #   
  #   package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  #   
  #   package.list <- setdiff(package.list,basic.packages)
  #   
  #   if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  #   
  # }
  # 
  # detachAllPackages()
  
  ### [3. ] Data Tab Output -----------------------------------------------------------------------------------------------------------
  # dataDisplay <- reactive({
  #   groupByFilters(kpi.all.data[,c("Campus","Campus.Specialty","Department","Resource","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status","holiday")],
  #                  input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                  input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  # })
  # 
  # output$dTableAll <- DT::renderDataTable({
  #   DT::datatable(dataDisplay()[,c("Campus","Campus.Specialty","Department","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status","holiday")])
  # })
  
  output$dTableAll <- DT::renderDT(
    dataAll()[,c("Campus","Campus.Specialty","Department","Resource","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status")],
    filter = "top",
    server = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv','excel')
    )
  )
  
  ### [3. ] Volume Comparison -----------------------------------------------------------------------------------------------------------
  output$vol_month_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    
    paste0("Total Monthly Volume by ", name_1 , " and ", name_2)
  })
  
  output$vol_day_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    paste0("Average Daily Volume by ", name_1 , " and ", name_2)
  })
  
  
  output$npr_month_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    paste0("Monthly New Patient Ratio (%) by ", name_1 , " and ", name_2)
  })
  
  
  
  output$new_wait_month_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    paste0("Monthly Median New Patient Wait Time (Days) by ", name_1 , " and ", name_2)
  })
  
  
  
  output$new_patient_fill_rate_day_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    
    paste0("Average Daily Booked and Filled Rate by ", name_1 , " and ", name_2)
  })
  
  
  output$new_patient_fill_rate_month_title <- renderText({
    if(input$breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "Department"){
      name_1 <- input$compare_filters
    }
    if(input$compare_filters == "Provider"){
      name_1 <- input$compare_filters
    }
    
    paste0("Total Monthly Booked and Filled Rate by ", name_1 , " and ", name_2)
  })
  
  rows_group <- reactive({
    if(input$compare_filters == "Campus.Specialty"){
      list <- list(0)
    }
    if(input$compare_filters == "Department"){
      list <- list(0,1)
    }
    if(input$compare_filters == "Provider"){
      list <- list(0,1,2)
    }
    list
    
  })
  
  rows_group_slot <- reactive({
    if(input$compare_filters == "Campus.Specialty"){
      list <- list(0,1)
    }
    if(input$compare_filters == "Department"){
      list <- list(0,1,2)
    }
    if(input$compare_filters == "Provider"){
      list <- list(0,1,2,3)
    }
    list
    
  })
  
  
  vol_comp_day <- reactive({
    data <- dataArrived() #%>% filter(Resource == "Provider")
    # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Campus.Specialty"
    # breakdown_filters <- "Visit.Method"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    if(input$breakdown_filters == "New.PT3"){
      
      ### Group data by inputs and Month and Date.  Spread data to make columns FALSE and TRUE that determine number of new or established patients
      volume <- data %>% group_by(across(cols),Appt.DateYear, Appt.MonthYear) %>%
        summarise(total = n()) %>%
        spread(!!breakdown_filters, total)
      
      
      volume[is.na(volume)] <- 0 ## NAs to 0
      
      
      #### Create "New" column to establish number of new patients. then group by Month and create "avg" column that is the sum of all new patients within 
      #### the month divided by the total number of days we have data in the month
      volume_new <- volume %>% group_by(across(!!tot_cols)) %>%
        mutate(New = round(`TRUE`)) %>% 
        group_by(across(!!tot_cols), Appt.MonthYear) %>%
        summarise(avg = round(sum(New)/n(),2)) 
      
      drop <- c("FALSE","TRUE", "<NA>")
      volume_new = volume_new[,!(names(volume_new) %in% drop)]
      
      
      #### Take volume_new and get the Appt.Month columns and make the values into columns themselves by widening the data
      #### also added a column named so it can be used as the "New" patients group
      volume_new <- volume_new %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = avg,
                    values_fill = 0) %>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      
      
      #### Create "Established" column to establish number of new patients. then group by Month and create "avg" column that is the sum of all new patients within 
      #### the month divided by the total number of days we have data in the month
      volume_est<- volume %>% group_by(across(!!tot_cols),Appt.DateYear) %>%
        mutate(New = round(`FALSE`))%>% 
        group_by(across(!!tot_cols), Appt.MonthYear) %>%
        summarise(avg = round(sum(New)/n(),2)) 
      
      drop <- c("FALSE","TRUE", "<NA>")
      volume_est = volume_est[,!(names(volume_est) %in% drop)]
      
      #### Take volume_new and get the Appt.Month columns and make the values into columns themselves by widening the data
      #### also added a column named so it can be used as the "Establihsed" patients group
      volume_est <- volume_est %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = avg,
                    values_fill = 0)%>%
        add_column(!!breakdown_filters := "Established") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      ### Join numbers for new and est patients
      volume <- full_join(volume_new,volume_est)
      # volume <- volume %>% relocate(breakdown_filters, .after = !!compare_filters)
      
      
      ### Get average total by adding all the numbers in the months columns grouped by the volume filters
      #### Also added a column named by the breakdown filter to store the number Avergae Total NUmber
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Average Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      volume <- full_join(volume,tot)
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
      
      
      
    }else {
      
      #### group Data by inputs and Month and data get the total for each day then group by the Month in order to sum all the vistis within the month and 
      #### divide it by the number of days within the month in the data, then we make a wider data sets with the months values made into a column
      volume <- data %>% group_by(across(all_of(cols)),Appt.DateYear, Appt.MonthYear) %>%
        summarise(total = n()) %>%
        group_by(across(cols), Appt.MonthYear) %>%
        summarise(avg = round(sum(total)/n(),2)) %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = avg,
                    values_fill = 0) 
      
      
      volume[is.na(volume)] <- 0
      
      ### Get average total by adding all the numbers in the months columns grouped by the volume filters
      #### Also added a column named by the breakdown filter to store the number Avergae Total NUmber
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Average Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      volume <- full_join(volume,tot)
      
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
    }
    
    volume <- setnames(volume, old = cols, new = cols_name)
    
    month_names <- colnames(volume[setdiff(names(volume), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    volume <- setnames(volume, old = month_names, new = month_names_new)
    
    
    volume$Total_YN <- ifelse(volume[[name_2]] == "Average Total", 1,0)
    # volume[[month_names_new]] <- paste0(volume[[month_names_new]], "patients")
    volume
    
    
  })
  
  
  output[["volume_comparison_tb_day"]] <- renderDT({
    
    col_dissappear <- which(names(vol_comp_day()) %in% c("Total_YN"))
    num_of_cols <- length(vol_comp_day())
    
    dtable <-   datatable(vol_comp_day(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('Average Daily Volume = Total arrived visits by month and breakdown / Total number of days within the month.')
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Daily Volume Comaprsion'),
                              list(extend = 'excel', filename = 'Daily Volume Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(vol_comp_day())[3] == "Provider", 4, 3)
                            ),
                            #fixedColumns = list(leftColumns = 2),
                            rowsGroup = rows_group(),
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            )
                          )
    )%>%
      formatStyle(
        'Total_YN',
        target = "row",
        fontWeight = styleEqual(1, "bold")
        #backgroundColor = styleEqual(c(1),c('grey'))
      ) %>%
      formatStyle(columns = c("Total"), fontWeight = 'bold') %>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  
  vol_comp_month <- reactive({
    data <- dataArrived() #%>% filter(Resource == "Provider")
    # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Campus.Specialty"
    # breakdown_filters <- "Visit.Method"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    
    if(breakdown_filters == "New.PT3"){
      
      #### Group data by inputs and Month.  Spread data to make TRUE and FALSe columns for new patietns
      volume <- data %>% group_by(across(cols),Appt.MonthYear) %>%
        summarise(total = n()) %>%
        spread(!!breakdown_filters, total)
      
      volume[is.na(volume)] <- 0
      
      ### Getting total new patients for month
      volume_new <- volume %>% group_by(across(!!tot_cols),Appt.MonthYear) %>%
        mutate(total = round(`TRUE`))
      
      drop <- c("FALSE","TRUE", "<NA>")
      volume_new = volume_new[,!(names(volume_new) %in% drop)]
      
      #### Pivoting new patients for each month
      volume_new <- volume_new %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = total,
                    values_fill = 0) %>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      
      #### Getting total number of establihsed patietns
      volume_est<- volume %>% group_by(across(!!tot_cols),Appt.MonthYear) %>%
        mutate(total = round(`FALSE`))
      
      drop <- c("FALSE","TRUE", "<NA>")
      volume_est = volume_est[,!(names(volume_est) %in% drop)]
      
      #### Pivot data established patietns for each month 
      volume_est <- volume_est %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = total,
                    values_fill = 0) %>%
        add_column(!!breakdown_filters := "Established") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      
      volume <- full_join(volume_new,volume_est)
      
      #### Get total for each moth by adding all columns with months 
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      volume <- full_join(volume,tot)
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
      
      
    }else {
      
      #### Group data by inputs and Month get total for the month and pivot wider to moths are now columns
      volume <- data %>% group_by(across(cols),Appt.MonthYear) %>%
        summarise(total = n()) %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = total,
                    values_fill = 0) 
      
      
      volume[is.na(volume)] <- 0
      
      #### Sum all cloumns with months to get the total for the month
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      volume <- full_join(volume,tot)
      
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
    }
    volume <- setnames(volume, old = cols, new = cols_name)
    
    volume$Total_YN <- ifelse(volume[[name_2]] == "Total", 1,0)
    
    month_names <- colnames(volume[setdiff(names(volume), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    volume <- setnames(volume, old = month_names, new = month_names_new)
    
    
    volume
    
    
  })
  
  
  output[["volume_comparison_tb_month"]] <- renderDT({
    
    col_dissappear <- which(names(vol_comp_month()) %in% c("Total_YN"))
    num_of_cols <- length(vol_comp_month())
    
    dtable <-   datatable(vol_comp_month(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('Total Monthly Volume = sum of all patients within the month by breakdown.')
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly Volume Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly Volume Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(vol_comp_month())[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group(),
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            )
                          )
    )%>%
      formatStyle(
        'Total_YN',
        target = "row",
        fontWeight = styleEqual(1, "bold")
      ) %>%
      formatStyle(columns = c("Total"), fontWeight = 'bold')%>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  
  patient_ratio_month <- reactive({
    data <- dataArrived() #%>% filter(Resource == "Provider")
    # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Campus.Specialty"
    # breakdown_filters <- "Appt.Type"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    
    
    if(breakdown_filters == "New.PT3"){
      
      ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
      newpatients.ratio <- data %>% group_by(across(cols),Appt.MonthYear) %>%
        summarise(total = n()) %>%
        drop_na() %>%
        spread(!!breakdown_filters, total)
      
      newpatients.ratio[is.na(newpatients.ratio)] <- 0
      
      #### Calulate new patient ratio for the whole month sum of all new patients within the month divide by sum of new and established patients
      newpatients.ratio.new <- newpatients.ratio %>% group_by(across(!!tot_cols),Appt.MonthYear) %>%
        mutate(ratio = round(`TRUE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)*100)
      
      
      
      drop <- c("FALSE","TRUE", "<NA>")
      newpatients.ratio.new = newpatients.ratio.new[,!(names(newpatients.ratio.new) %in% drop)]
      
      #### Pivot the data so that month are now in a columns
      newpatients.ratio.new <- newpatients.ratio.new %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = ratio,
                    values_fill = 0)%>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      #### Calulate est patient ratio for the whole month sum of all est patients within the month divide by sum of new and established patients
      newpatients.ratio.est <- newpatients.ratio %>% group_by(across(!!tot_cols),Appt.MonthYear) %>%
        mutate(ratio = round(`FALSE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)*100)
      
      
      
      drop <- c("FALSE","TRUE", "<NA>")
      newpatients.ratio.est = newpatients.ratio.est[,!(names(newpatients.ratio.est) %in% drop)]
      
      #### Pivot data wider to months are their own columns
      newpatients.ratio.est <- newpatients.ratio.est %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = ratio,
                    values_fill = 0)%>%
        add_column(!!breakdown_filters := "Established") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      
      newpatients.ratio <- full_join(newpatients.ratio.new,newpatients.ratio.est)
      
      
      #### Get total by summarising all columns with months in them 
      tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      newpatients.ratio <- full_join(newpatients.ratio,tot)
      
    }else{
      
      ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
      newpatients.ratio <- data %>% group_by(across(cols),Appt.MonthYear, New.PT3) %>%
        summarise(total = n()) %>%
        drop_na() %>%
        spread(New.PT3, total) %>%
        drop_na()
      
      newpatients.ratio[is.na(newpatients.ratio)] <- 0
      
      ### Calculate new patient ratio by breakdown
      newpatients.ratio <- newpatients.ratio %>% group_by(across(!!tot_cols),Appt.MonthYear) %>%
        mutate(ratio = round(`TRUE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)*100)
      
      
      drop <- c("FALSE","TRUE", "<NA>")
      newpatients.ratio = newpatients.ratio[,!(names(newpatients.ratio) %in% drop)]
      newpatients.ratio <- newpatients.ratio %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = ratio,
                    values_fill = 0)
      
      
      ##### sum all the columns with months in them to get the Totoal for the month
      tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      newpatients.ratio <- full_join(newpatients.ratio,tot)
      
      
    }
    
    newpatients.ratio <- setnames(newpatients.ratio, old = cols, new = cols_name)
    
    newpatients.ratio$Total_YN <- ifelse(newpatients.ratio[[name_2]] == "Total", 1,0)
    
    month_names <- colnames(newpatients.ratio[setdiff(names(newpatients.ratio), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    newpatients.ratio <- setnames(newpatients.ratio, old = month_names, new = month_names_new)
    
    newpatients.ratio
    
    
    
    
  })
  
  
  output[["new_patient_ratio_month"]] <- renderDT({
    col_dissappear <- which(names(patient_ratio_month()) %in% c("Total_YN"))
    num_of_cols <- length(patient_ratio_month())
    dtable <-   datatable(patient_ratio_month(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('Monthly New Patient Ratio = Total new arrived patients within the month / Total new and established patients within the month')
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly Median New Patient Ratio Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly Median New Patient Ratio Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(patient_ratio_month())[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group(),
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            )
                          )
    )%>%
      formatStyle(
        'Total_YN',
        target = "row",
        fontWeight = styleEqual(1, "bold")
      )%>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  # patient_ratio_day <- reactive({
  #   data <- dataArrived() %>% filter(Resource == "Provider")
  #   # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
  #   # compare_filters <- "Campus.Specialty"
  #   # breakdown_filters <- "Visit.Method"
  #   
  #   data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
  #   
  #   compare_filters <- input$compare_filters
  #   breakdown_filters <- input$breakdown_filters
  #   
  #   
  #   
  #   if(breakdown_filters == "Visit.Method"){
  #     name_2 <- "Visit Method"
  #   }
  #   if(breakdown_filters == "Appt.Type"){
  #     name_2 <- "Vist Type"
  #   }
  #   if(breakdown_filters == "New.PT3"){
  #     name_2 <- "Patient Status"
  #   }
  #   
  #   
  #   if(compare_filters == "Campus.Specialty"){
  #     name_1 <- "Specialty"
  #     cols <- c(compare_filters,breakdown_filters)
  #     cols_name <- c(name_1,name_2)
  #     tot_cols <- c(compare_filters)
  #   }
  #   if(compare_filters == "Department"){
  #     name_1 <- compare_filters
  #     cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
  #     cols_name <- c("Specialty",name_1,name_2)
  #     tot_cols <- c("Campus.Specialty",compare_filters)
  #   }
  #   if(compare_filters == "Provider"){
  #     name_1 <- compare_filters
  #     cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
  #     cols_name <- c("Specialty","Department",name_1,name_2)
  #     tot_cols <- c("Campus.Specialty", "Department",compare_filters)
  #   }
  #   
  #  
  #   
  #   if(breakdown_filters == "New.PT3"){
  #     
  #     #### Get total number of new patients daily and then spread the data to create TRUE and FALSE columns to count the number of enw patietns
  #     newpatients.ratio <- data %>% group_by(across(cols),Appt.DateYear, Appt.MonthYear) %>%
  #       summarise(total = n()) %>%
  #       drop_na() %>%
  #       spread(!!breakdown_filters, total)
  #     
  #     newpatients.ratio[is.na(newpatients.ratio)] <- 0
  #     
  #     
  #     #### Calculate the daily new patients ration and then group by month to get the daily avearge (sum of new patietns ratio within the minth divide by number of 
  #     #### days in the data)
  #     newpatients.ratio.new <- newpatients.ratio %>% group_by(across(!!tot_cols),Appt.DateYear) %>%
  #       mutate(ratio = round(`TRUE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)) %>%
  #       group_by(across(!!tot_cols), Appt.MonthYear) %>%
  #       summarise(avg = round(sum(ratio)/n(),2))
  #     
  #     
  #     
  #     drop <- c("FALSE","TRUE", "<NA>")
  #     newpatients.ratio.new = newpatients.ratio.new[,!(names(newpatients.ratio.new) %in% drop)]
  #     
  #     #### Pivot the data so the months are now columns and showing the average daily new patient ratio
  #     newpatients.ratio.new <- newpatients.ratio.new %>%
  #       pivot_wider(names_from = Appt.MonthYear,
  #                   values_from = avg,
  #                   values_fill = 0) %>%
  #       add_column(!!breakdown_filters := "New") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #     
  #     #### Calculate the daily established patients ration and then group by month to get the daily avearge (sum of new established ratio within the minth divide by number of 
  #     #### days in the data)
  #     newpatients.ratio.est <- newpatients.ratio %>% group_by(across(!!tot_cols),Appt.DateYear) %>%
  #       mutate(ratio = round(`FALSE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)) %>%
  #       group_by(across(!!tot_cols), Appt.MonthYear) %>%
  #       summarise(avg = round(sum(ratio)/n(),2))
  #     
  # 
  #     drop <- c("FALSE","TRUE", "<NA>")
  #     newpatients.ratio.est = newpatients.ratio.est[,!(names(newpatients.ratio.est) %in% drop)]
  #     newpatients.ratio.est <- newpatients.ratio.est %>%
  #       pivot_wider(names_from = Appt.MonthYear,
  #                   values_from = avg,
  #                   values_fill = 0)%>%
  #       add_column(!!breakdown_filters := "Established") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #  
  #     
  #     newpatients.ratio <- full_join(newpatients.ratio.new,newpatients.ratio.est)
  # 
  #       #### Sum of all with months as columns to get total (should all equal to one in this case)
  #     tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols))) %>%
  #       summarise_at(vars(-!!breakdown_filters), sum) %>%
  #       add_column(!!breakdown_filters := "Average Total") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #     newpatients.ratio <- full_join(newpatients.ratio,tot)
  #     
  #   }else{
  #     
  #     data <- data %>% filter(New.PT3  == "TRUE")
  #     
  #     newpatients.ratio <- data %>% group_by(across(all_of(cols)),Appt.DateYear, Appt.MonthYear) %>%
  #       summarise(total = n()) %>%
  #       group_by(across(cols), Appt.MonthYear) %>%
  #       summarise(avg = round(sum(total)/n(),2)) 
  #     
  #     
  #     tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols)), Appt.MonthYear) %>%
  #       summarise_at(vars(-!!breakdown_filters), sum) %>%
  #       rename(tot = avg)
  #     
  #     
  #     newpatients.ratio <- full_join(newpatients.ratio,tot)
  #     
  #     newpatients.ratio <- newpatients.ratio %>%
  #                           group_by(across(all_of(cols)), Appt.MonthYear) %>%
  #                           summarise(ratio = round(avg/tot,2)) %>%
  #                           pivot_wider(names_from = Appt.MonthYear,
  #                                       values_from = ratio,
  #                                       values_fill = 0) 
  #     
  #     
  #     # #### get total of new and established patients for each day, and spread them into TRUE and FALSE columns 
  #     # newpatients.ratio <- data %>% group_by(across(cols),Appt.DateYear, Appt.MonthYear, New.PT3) %>%
  #     #   summarise(total = n()) %>%
  #     #   drop_na() %>%
  #     #   spread(New.PT3, total) %>%
  #     #   drop_na()
  #     # 
  #     # newpatients.ratio[is.na(newpatients.ratio)] <- 0
  #     # 
  #     # #### create daily new patient ratio, then group by month and sum all the new patients ratios for the month and divide by number of days in the data
  #     # newpatients.ratio <- newpatients.ratio %>% group_by(across(!!cols),Appt.DateYear) %>%
  #     #   mutate(ratio = round(`TRUE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)) %>%
  #     #   group_by(across(!!cols), Appt.MonthYear) %>%
  #     #   summarise(avg = round(sum(ratio)/n(),2))
  #     # 
  #     # 
  #     # 
  #     # drop <- c("FALSE","TRUE", "<NA>")
  #     # newpatients.ratio = newpatients.ratio[,!(names(newpatients.ratio) %in% drop)]
  #     # 
  #     # ### Widen data so the months are in their own columns
  #     # newpatients.ratio <- newpatients.ratio %>%
  #     #   pivot_wider(names_from = Appt.MonthYear,
  #     #               values_from = avg,
  #     #               values_fill = 0)
  #     
  #     
  #     ### Get average total new patietn ratio for the month 
  #     tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols))) %>%
  #       summarise_at(vars(-!!breakdown_filters), sum) %>%
  #       add_column(!!breakdown_filters := "Average Total") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #     
  #     
  #     newpatients.ratio <- full_join(newpatients.ratio,tot)
  #     
  #     
  #   }
  #   
  #   newpatients.ratio <- setnames(newpatients.ratio, old = cols, new = cols_name)
  #   
  #   newpatients.ratio$Total_YN <- ifelse(newpatients.ratio[[name_2]] == "Average Total", 1,0)
  #   
  #   month_names <- colnames(newpatients.ratio[setdiff(names(newpatients.ratio), c(cols_name, "Total", "Total_YN"))])
  #   month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
  #   
  #   newpatients.ratio <- setnames(newpatients.ratio, old = month_names, new = month_names_new)
  #   newpatients.ratio
  #   
  #   
  # })
  # 
  # 
  # output[["new_patient_ratio_day"]] <- renderDT({
  #   num_of_cols <- length(patient_ratio_day())
  #   col_dissappear <- which(names(patient_ratio_day()) %in% c("Total_YN"))
  #   
  #   dtable <-   datatable(patient_ratio_day(), 
  #                         class = 'cell-border stripe',
  #                         rownames = FALSE,
  #                         extensions = c('Buttons','Scroller'),
  #                         caption = htmltools::tags$caption(
  #                           style = 'caption-side: bottom; text-align: left;',
  #                           htmltools::em('Average Daily New Patient Ratio = Average daily new patients within the month / Average daily number of total patients within the month')
  #                         ),
  #                         options = list(
  #                           scrollX = TRUE,
  #                           columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
  #                           list(pageLength = 20, scrollY = "400px"),
  #                           dom = 'Bfrtip',
  #                           #buttons = c('csv','excel'),
  #                           buttons = list(
  #                             list(extend = 'csv', filename = 'Daily Median New Patient Ratio Comaprsion'),
  #                             list(extend = 'excel', filename = 'Volume Comaprsion')
  #                           ),
  #                           sDom  = '<"top">lrt<"bottom">ip',
  #                           initComplete = JS(
  #                             "function(settings, json) {",
  #                             "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
  #                             "}"),
  #                           fixedColumns = list(leftColumns =
  #                                                 ifelse(colnames(patient_ratio_day())[3] == "Provider", 4, 3)
  #                           ),
  #                           rowsGroup = rows_group(),
  #                           headerCallback = DT::JS(
  #                             "function(thead) {",
  #                             "  $(thead).css('font-size', '115%');",
  #                             "}"
  #                           )
  #                           
  #                         )
  #   )%>%
  #     formatStyle(
  #       'Total_YN',
  #       target = "row",
  #       fontWeight = styleEqual(1, "bold")
  #     )%>%
  #     formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
  #   path <- here::here("www")
  #   
  #   dep <- htmltools::htmlDependency(
  #     "RowsGroup", "2.0.0", 
  #     path, script = "dataTables.rowsGroup.js")
  #   dtable$dependencies <- c(dtable$dependencies, list(dep))
  #   dtable
  # },server = FALSE)
  
  
  
  # patient_lead_day <- reactive({
  #   data <- dataAll()
  #   # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
  #   # compare_filters <- "Department"
  #   # breakdown_filters <- "New.PT3"
  #   
  #   data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
  # 
  #   compare_filters <- input$compare_filters
  #   breakdown_filters <- input$breakdown_filters
  #   
  #   
  #   if(breakdown_filters == "Visit.Method"){
  #     name_2 <- "Visit Method"
  #   }
  #   if(breakdown_filters == "Appt.Type"){
  #     name_2 <- "Vist Type"
  #   }
  #   if(breakdown_filters == "New.PT3"){
  #     name_2 <- "Patient Status"
  #   }
  #   
  #   
  #   if(compare_filters == "Campus.Specialty"){
  #     name_1 <- "Specialty"
  #     cols <- c(compare_filters,breakdown_filters)
  #     cols_name <- c(name_1,name_2)
  #     tot_cols <- c(compare_filters)
  #   }
  #   if(compare_filters == "Department"){
  #     name_1 <- compare_filters
  #     cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
  #     cols_name <- c("Specialty",name_1,name_2)
  #     tot_cols <- c("Campus.Specialty",compare_filters)
  #   }
  #   if(compare_filters == "Provider"){
  #     name_1 <- compare_filters
  #     cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
  #     cols_name <- c("Specialty","Department",name_1,name_2)
  #     tot_cols <- c("Campus.Specialty", "Department",compare_filters)
  #   }
  #   
  #   
  #   
  #   if(breakdown_filters == "New.PT3"){
  #     
  #     #### Calculate wait time which is the difference between the the patients made the appt to the data of the appt
  #     data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  #     
  #     #### Filter out for wait times grater than 0 and calculate the daily median wait time 
  #     waitTime <- data %>%
  #       filter(wait.time >= 0) %>%
  #       group_by(across(cols),Appt.DateYear, Appt.MonthYear,New.PT3) %>%
  #       dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
  #       filter(New.PT3 %in% c("TRUE","FALSE"))
  #     
  #     
  #     #### convert to new and established and filter out established and drop the NEW.PT3 columns
  #     waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
  #     
  #     
  #     #### Get the average daily for new patietns and arrived patients 
  #     waitTime <- waitTime %>% 
  #                   group_by(across(cols), Appt.MonthYear) %>%
  #                   summarise(avg = round(sum(medWaitTime)/n(),2)) %>%
  #                   pivot_wider(names_from = Appt.MonthYear,
  #                               values_from = avg,
  #                               values_fill = 0) 
  #     
  #     #### Get total by summing all columns that are months
  #     tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
  #       summarise_at(vars(-!!breakdown_filters), sum) %>%
  #       add_column(!!breakdown_filters := "Average Total") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #     waitTime <- full_join(waitTime,tot) 
  #     
  # 
  #     
  #     
  #   }else{
  #     #### Calculate wait time which is the difference between the the patients made the appt to the data of the appt
  #     data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
  #     
  #     #### Filter out for wait times grater than 0 and calculate the daily median wait time 
  #     waitTime <- data %>%
  #       filter(wait.time >= 0) %>%
  #       group_by(across(cols),Appt.DateYear, Appt.MonthYear,New.PT3) %>%
  #       dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
  #       filter(New.PT3 %in% c("TRUE","FALSE"))
  #     
  #     
  #     #### convert to new and established and filter out established and drop the NEW.PT3 columns
  #     waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
  #     waitTime <- waitTime %>% filter(New.PT3 == "New")
  #     drop <- c("New.PT3")
  #     waitTime = waitTime[,!(names(waitTime) %in% drop)]
  #     
  #     
  #     
  #     #### Get average median wait Time for new patients the month by adding daily wait time and dividing by number of days in the month
  #     #### and pivot so months are now columns
  #     waitTime <- waitTime %>% 
  #                   group_by(across(cols), Appt.MonthYear) %>%
  #                   summarise(avg = round(sum(medWaitTime)/n(),2)) %>%
  #                   pivot_wider(names_from = Appt.MonthYear,
  #                               values_from = avg,
  #                               values_fill = 0) 
  #     
  #     
  # 
  #     #### Get toal but summing all columns that are months
  #     tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
  #       summarise_at(vars(-!!breakdown_filters), sum) %>%
  #       add_column(!!breakdown_filters := "Average Total") %>%
  #       relocate(all_of(breakdown_filters), .after = !!compare_filters)
  #     
  #     waitTime <- full_join(waitTime,tot) 
  # 
  #     
  # 
  #   }
  #   waitTime <- setnames(waitTime, old = cols, new = cols_name)
  #   
  #   waitTime
  # })
  # 
  # output[["new_patient_lead_time_day"]] <- renderDT({
  #   dtable <-   datatable(patient_lead_day(), 
  #                         class = 'cell-border stripe',
  #                         rownames = FALSE,
  #                         extensions = c('Buttons','Scroller'),
  #                         options = list(
  #                           scrollX = TRUE,
  #                           #columnDefs = list(list(visible = F, targets = 0)),
  #                           list(pageLength = 20, scrollY = "400px"),
  #                           dom = 'Bfrtip',
  #                           #buttons = c('csv','excel'),
  #                           buttons = list(
  #                             list(extend = 'csv', filename = 'Daily New Patient Wait Time Comaprsion'),
  #                             list(extend = 'excel', filename = 'Daily New Patient Wait Time Comaprsion')
  #                           ),
  #                           sDom  = '<"top">lrt<"bottom">ip',
  #                           initComplete = JS(
  #                             "function(settings, json) {",
  #                             "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
  #                             "}"),
  #                           fixedColumns = list(leftColumns =
  #                                                 ifelse(colnames(patient_lead_day())[3] == "Provider", 4, 3)
  #                           ),
  #                           #rowsGroup = list(0)
  #                           rowsGroup = rows_group()
  #                         )
  #   )
  #   dtable <- dtable %>%
  #     formatStyle(
  #       0,
  #       target = "row",
  #       fontWeight = styleEqual(2, "bold")
  #     )
  #   path <- here::here("www")
  #   
  #   dep <- htmltools::htmlDependency(
  #     "RowsGroup", "2.0.0", 
  #     path, script = "dataTables.rowsGroup.js")
  #   dtable$dependencies <- c(dtable$dependencies, list(dep))
  #   dtable
  # },server = FALSE)
  
  
  
  
  
  patient_lead <- reactive({
    data <- dataAll()
    # data <- kpi.all.data[arrived.data.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Department"
    # breakdown_filters <- "New.PT3"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    start.time <- Sys.time()
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time,2)
    time.taken
    
    
    
    
    
    if(breakdown_filters == "New.PT3"){
      
      
      #### Calculate wait time which is the difference between the the patients made the appt to the data of the appt
      data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
      
      #### Filter out for wait times grater than 0 and calculate the monthly median wait time 
      waitTime <- data %>%
        filter(wait.time >= 0) %>%
        group_by(across(cols), Appt.MonthYear,New.PT3) %>%
        dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
        filter(New.PT3 %in% c("TRUE","FALSE"))
      
      
      #### convert to new and established and filter out established and drop the NEW.PT3 columns
      waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
      
      
      #### Get the average daily for new patients and arrived patients 
      waitTime <- waitTime %>% 
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = medWaitTime,
                    values_fill = 0) 
      
      #### Get total by summing all columns that are months
      tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Average Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      waitTime <- full_join(waitTime,tot)
      
      
    }else{
      ### Calculate wait time which is the difference between when the patient made the appt and the scheduled date
      data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
      
      
      #### Filter out wait time that equals 0 and calculate the median wait time for NEw and est patients by month
      waitTime <- data %>%
        filter(wait.time >= 0) %>%
        group_by(across(cols),Appt.MonthYear, New.PT3) %>%
        dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
        filter(New.PT3 %in% c("TRUE","FALSE"))
      
      
      #### Change the TRUE and FALSE to New and Established and filter our new patients and drop the New.PT3 column
      waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
      waitTime <- waitTime %>% filter(New.PT3 == "New")
      drop <- c("New.PT3")
      waitTime = waitTime[,!(names(waitTime) %in% drop)]
      
      #### Pivot the data so the months are in the columns and shows only new patient median time   
      waitTime <- waitTime %>%
        pivot_wider(names_from = Appt.MonthYear,
                    values_from = medWaitTime,
                    values_fill = 0)
      
      
      tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      waitTime <- full_join(waitTime,tot) 
      
      
      
    }
    waitTime <- setnames(waitTime, old = cols, new = cols_name)
    
    waitTime$Total_YN <- ifelse(waitTime[[name_2]] == "Total", 1,0)
    
    month_names <- colnames(waitTime[setdiff(names(waitTime), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    waitTime <- setnames(waitTime, old = month_names, new = month_names_new)
    
    waitTime
  })
  
  
  output[["new_patient_lead_time_month"]] <- renderDT({
    num_of_cols <- length(patient_lead())
    col_dissappear <- which(names(patient_lead()) %in% c("Total_YN"))
    
    dtable <-   datatable(patient_lead(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('Median New Patient Wait Time = median wait time of all arrived new patients within the month')
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly New Patient Wait Time Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly New Patient Wait Time Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(patient_lead())[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group(),
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            )
                            
                          )
    )
    dtable <- dtable %>%
      formatStyle(
        'Total_YN',
        target = "row",
        fontWeight = styleEqual(1, "bold")
      )%>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  booked_and_filled_month <- reactive({
    data <- dataAllSlot_comp()
    # data <- slot.data.subset[all.slot.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Specialty"
    # breakdown_filters <- "Visit.Method"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    
    
    slot_metrics <- c("Available Hours", "Booked Hours", "Filled Hours", "Booked Rate (%)", "Filled Rate (%)")
    
    if(breakdown_filters != "Visit.Method"){
      
      
      
      validate(
        need(input$breakdown_filters != "Appt.Type", ("Slot data can only be viewed by Visit Method")),
        need(input$breakdown_filters != "New.PT3", ("Slot data can only be viewed by Visit Method"))
      )
      
      
      
      
    }else{
      
      #### Group data by inputs and month, sum all available, booked, and filled hours for the whole month
      slot <- data %>% 
        group_by(across(cols), Appt.MonthYear)%>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`, na.rm=TRUE),1),
                         `Booked Hours` = round(sum(`Booked Hours`),1),
                         `Filled Hours` = round(sum(`Arrived Hours`),1)) 
      
      slot[is.na(slot)] <- 0
      
      #### Add booked and filled rate columns( respective metric/Available Hours) multiply by 100 pivot the data so that the months are now columns
      #### and the metric oclumns become rows
      slot <- slot %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100), "%")) %>%
        gather(variable, value, !!slot_metrics) %>%
        spread(Appt.MonthYear, value) %>%
        rename(Status = variable)
      
      slot[slot == "Inf%"] <- "-"
      slot[slot == "NaN%"] <- "-"
      
      #### Order that the status appears
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      slot <- slot[order(match(slot$Status, level.order)),]
      
      
      slot <- setnames(slot, old = cols, new = cols_name)
      
      slot$Total_YN <- ifelse(slot[[name_2]] == "Total", 1,0)
      
      month_names <- colnames(slot[setdiff(names(slot), c(cols_name, "Total", "Total_YN"))])
      month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
      
      slot <- setnames(slot, old = month_names, new = month_names_new)
      
      slot
      
    }
    
  })
  
  
  output[["new_patient_fill_rate_month"]] <- renderDT({
    num_of_cols <- length(booked_and_filled_month())
    col_dissappear <- which(names(booked_and_filled_month()) %in% c("Total_YN"))
    dtable <-   datatable(booked_and_filled_month(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          options = list(
                            scrollX = TRUE,
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            ),
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly New Patient Wait Time Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly New Patient Wait Time Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(booked_and_filled_month())[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group_slot()
                          )
    )
    dtable <- dtable %>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual(2, "bold")
      )%>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  
  booked_and_filled_day <- reactive({
    data <- dataAllSlot_comp()
    # data <- slot.data.subset[all.slot.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Specialty"
    # breakdown_filters <- "Visit.Method"
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    
    if(breakdown_filters == "Visit.Method"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "Appt.Type"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "New.PT3"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("Campus.Specialty",compare_filters)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols <- c("Campus.Specialty","Department",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("Campus.Specialty", "Department",compare_filters)
    }
    
    
    
    slot_metrics <- c("Available Hours", "Booked Hours", "Filled Hours", "Booked Rate (%)", "Filled Rate (%)")
    
    if(breakdown_filters != "Visit.Method"){
      
      validate(
        need(input$breakdown_filters != "Appt.Type", ("Slot data can only be viewed by Visit Method")),
        need(input$breakdown_filters != "New.PT3", ("Slot data can only be viewed by Visit Method"))
      )
      
      
      
    }else{
      
      #### Group data by inputs, month, and date sum all available, booked, and filled hours for the whole month
      slot <- data %>% 
        group_by(across(cols), Appt.MonthYear, Appt.DateYear)%>%
        dplyr::summarise(`Available Hours` = round(sum(`Available Hours`, na.rm=TRUE),1),
                         `Booked Hours` = sum(`Booked Hours`),
                         `Filled Hours` = sum(`Arrived Hours`)
        )
      
      slot[is.na(slot)] <- 0
      
      
      ### Group by the month and get the monthl avaerage for each month by summing the column and dividing by number of rows within the month
      ### then gather all created columns make them categories for the STatus column
      ### Spread data to make monhts in Appt.Month into columns
      slot <- slot %>%  group_by(across(!!cols), Appt.MonthYear) %>%
        summarise(
          `Booked Rate (%)` = paste0(round(sum(`Booked Hours`)/sum(`Available Hours`)*100),"%"),
          `Filled Rate (%)` = paste0(round(sum(`Filled Hours`)/sum(`Available Hours`)*100),"%"),
          `Available Hours` = round(sum(`Available Hours`)/n(),1),
          `Booked Hours` = round(sum(`Booked Hours`)/n(),1),
          `Filled Hours` = round(sum(`Filled Hours`)/n(),1)
        ) %>%
        gather(variable, value, !!slot_metrics) %>%
        spread(Appt.MonthYear, value) %>%
        rename(Status = variable)
      
      
      
      #### Add booked and filled rate columns( respective metric/Available Hours) multiply by 100 pivot the data so that the months are now columns
      #### and the metric oclumns become rows
      
      
      #### Order that the status appears
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      slot <- slot[order(match(slot$Status, level.order)),]
      
      
    }
    slot <- setnames(slot, old = cols, new = cols_name)
    
    slot$Total_YN <- ifelse(slot[[name_2]] == "Total", 1,0)
    
    month_names <- colnames(slot[setdiff(names(slot), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    slot <- setnames(slot, old = month_names, new = month_names_new)
    
    slot
  })
  
  
  output[["new_patient_fill_rate_day"]] <- renderDT({
    num_of_cols <- length(booked_and_filled_day())
    col_dissappear <- which(names(booked_and_filled_day()) %in% c("Total_YN"))
    dtable <-   datatable(booked_and_filled_day(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          options = list(
                            scrollX = TRUE,
                            headerCallback = DT::JS(
                              "function(thead) {",
                              "  $(thead).css('font-size', '115%');",
                              "}"
                            ),
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly New Patient Wait Time Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly New Patient Wait Time Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(booked_and_filled_day())[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group_slot()
                          )
    )
    dtable <- dtable %>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual(2, "bold")
      )%>%
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  observeEvent(input$download1, {
    screenshot(filename = "Ambulatory Care Dashboard")
  })
  
  setBookmarkExclude(c("bookmark" ,"dateRange"))
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  #callModule(profvis_server, "profiler")
  
} # Close server 

#shinyApp(ui, server)


#shinyApp(ui, server, options = list(launch.browser = T,browser = "C:/Program Files/Google/Chrome/Application/chrome.exe"))
