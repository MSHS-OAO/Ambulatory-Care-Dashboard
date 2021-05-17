server <- function(input, output, session) {
  
  ### (1) Create reactive filters ===================================================================================================
  ## Scheduling Data
  
  # observeEvent(input$resetheight, {
  #   updateSliderInput(session,"plotHeight",value = 650)
  # 
  # })
  observeEvent(input$selectedCampus,{
    specialty_choices <- sort(unique(historical.data[historical.data$Campus %in% input$selectedCampus, "Campus.Specialty"]))
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = specialty_choices,
                      selected = specialty_choices
    )
    
    department_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))

    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = department_choices,
                      selected = department_choices
    )
    
    provider_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment, "Provider"]))

    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )


    visit_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider, "Visit.Method"]))

    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visit_choices,
                      selected = visit_choices
    )

    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))

    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
    
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE)
  
  observeEvent(input$selectedSpecialty,{
    department_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))
    
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = department_choices,
                      selected = department_choices
    )
    
    provider_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment, "Provider"]))
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )
    
    
    visit_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider, "Visit.Method"]))
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visit_choices,
                      selected = visit_choices
    )
    
    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedDepartment,{
    provider_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource, "Provider"]))

    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )
    
    visit_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider, "Visit.Method"]))
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visit_choices,
                      selected = visit_choices
    )
    
    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedResource,{
    provider_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource, "Provider"]))
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = provider_choices,
                      selected = provider_choices
    )
    
    visit_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider, "Visit.Method"]))
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visit_choices,
                      selected = visit_choices
    )
    
    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedProvider, {
    visit_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider, "Visit.Method"]))
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = visit_choices,
                      selected = visit_choices
    )
    
    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedVisitMethod, {
    prc_choices <- sort(unique(historical.data[
      historical.data$Campus %in% input$selectedCampus &
        historical.data$Campus.Specialty %in% input$selectedSpecialty &
        historical.data$Department %in% input$selectedDepartment &
        historical.data$Resource %in% input$selectedResource &
        historical.data$Provider %in% input$selectedProvider &
        historical.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = prc_choices,
                      selected = prc_choices
    )
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  output$specialtyControl <- renderUI({
    
    box(
      title = "Select Specialty:",
      width = 12,
      solidHeader = FALSE,
      pickerInput("selectedSpecialty",label=NULL,
                  choices=sort(unique(historical.data[historical.data$Campus %in% input$selectedCampus, "Campus.Specialty"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1",
                    countSelectedText = "{0}/{1} Specialties",
                    dropupAuto = FALSE),
                  selected = sort(unique((historical.data %>% filter(Campus == "MSUS"))$Campus.Specialty))))
  })
  output$departmentControl <- renderUI({
    
    box(
      title = "Select Department:",
      width = 12, 
      solidHeader = FALSE, 
      pickerInput("selectedDepartment",label=NULL,
                  choices=sort(unique(historical.data[
                    historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty, "Department"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Departments",
                    dropupAuto = FALSE),
                  selected = sort(unique((historical.data %>% filter(Campus == "MSUS"))$Department))))
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
  
  # checkboxGroupButtons(
  #   inputId = "Id002",
  #   label = "Choices", 
  #   choices = c("Choice 1", "Choice 2", "Choice 3"),
  #   status = "danger"
  # )
  
  # output$providerControl <- renderUI({
  #   
  #   box(
  #     title = "Select Provider:",
  #     width = 12, 
  #     solidHeader = FALSE, 
  #     pickerInput("selectedProvider",label=NULL,
  #                 choices=sort(unique(historical.data[
  #                   historical.data$Campus %in% input$selectedCampus &
  #                     historical.data$Campus.Specialty %in% input$selectedSpecialty &
  #                     historical.data$Department %in% input$selectedDepartment & 
  #                     historical.data$Resource %in% input$selectedResource, "Provider"])),
  #                 multiple=TRUE,
  #                 options = pickerOptions(
  #                   liveSearch = TRUE,
  #                   actionsBox = TRUE,
  #                   selectedTextFormat = "count > 1", 
  #                   countSelectedText = "{0}/{1} Providers", 
  #                   dropupAuto = FALSE),
  #                 selected = sort(unique((historical.data %>% filter(Campus == "MSUS"))$Provider))))
  # })
  
  output$dateRangeControl <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRange", label = NULL,
                     start = min(arrived.data$Appt.DateYear), end = max(arrived.data$Appt.DateYear),
                     min = min(arrived.data$Appt.DateYear), max = max(arrived.data$Appt.DateYear)))
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
                     min = min(arrived.data$Appt.DateYear), max = max(arrived.data$Appt.DateYear)))
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
                     min = min(arrived.data$Appt.DateYear), max = max(arrived.data$Appt.DateYear)))
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
  dataAll <- reactive({
    groupByFilters(all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShow <- reactive({
    groupByFilters(arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrived <- reactive({
    groupByFilters(arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataNoShow <- reactive({
    groupByFilters(noShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedRescheduled<- reactive({
    groupByFilters(canceled.bumped.rescheduled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataRescheduled<- reactive({
    groupByFilters(rescheduled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] All pre-processed data for kpi tabs --------------------------------------------------------------------------------------
  dataAllKpi <- reactive({
    groupByFilters(kpi.all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShowKpi <- reactive({
    groupByFilters(kpi.arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedKpi <- reactive({
    groupByFilters(kpi.arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedKpi <- reactive({
    groupByFilters(kpi.canceled.bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledKpi <- reactive({
    groupByFilters(kpi.canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumpedKpi <- reactive({
    groupByFilters(kpi.bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  # [2.2] All pre-processed data for utilization tabs --------------------------------------------------------------------------------------
  
  dataScheduledUtilization <- reactive({
    groupByFilters_2(scheduled.utilization.data,
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, input$selectedPRCName, 
                     input$dateRange[1], nput$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$utilType)
  }) 
  
  dataArrivedUtilization <- reactive({
    groupByFilters_2(arrived.utilization.data,
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, input$selectedPRCName, 
                     input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays, input$utilType)
  }) 
  
  dataHourScheduled <- reactive({
    groupByFilters(data.hour.scheduled,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  dataHourArrived <- reactive({
    groupByFilters(data.hour.arrived,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  # [2.3] All pre-processed data for access tabs --------------------------------------------------------------------------------------
  dataFutureSlot <- reactive({
    groupByFilters_4(future.slot.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  dataPastSlot <- reactive({
    groupByFilters_4(past.slot.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod,
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  
  
  dataPastSlotTest <- reactive({
    groupByFilters_4_Test(past.slot.data,
                     input$selectedCampus, 
                     input$selectedSpecialty, 
                     input$selectedDepartment,
                     input$selectedResource,
                     #input$selectedProvider,
                      input$selectedVisitMethod,
                     input$dateRange[1], input$dateRange[2],
                     input$daysOfWeek,
                     input$excludeHolidays)
  }) 
  
  ### (3) Dashboard Layout ============================================================================================================
  ### [3.1] Title of  Dashboard -------------------------------------------------------------------------------------------------------
  # Site Overview Tab -------------------------------------------------------------------------------------------------
  output$siteTotalPts <- renderValueBox({
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    valueBox(
      prettyNum(round(length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark = ','),
      subtitle = tags$p("Avg Patients per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$siteNewPtRatio <- renderValueBox({
    
    valueBox(
      paste0(round((nrow(dataArrived() %>% filter(New.PT3 == TRUE)) / nrow(dataArrived()))*100),"%"),
      subtitle = tags$p("Avg New Patient Ratio", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$siteTotalProvs <- renderValueBox({

    valueBox(
      prettyNum(round(mean((dataArrived() %>% group_by(Appt.DateYear, Provider) %>% dplyr::summarise(n()) 
                            %>% group_by(Appt.DateYear) %>% dplyr::summarise(total = n()))$total)), big.mark = ','),
      subtitle = tags$p("Avg Providers Available per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$sitePtsPerProv <- renderValueBox({
    
    valueBox(
      prettyNum(round((length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear)))/
                        (mean((dataArrived() %>% group_by(Appt.DateYear, Provider) %>% dplyr::summarise(n()) 
                               %>% group_by(Appt.DateYear) %>% dplyr::summarise(total = n()))$total))), big.mark = ','),
      subtitle = tags$p("Avg Patients per Provider per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  output$siteSpecialties <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2])) +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
            legend.position = "none",
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  
  output$siteWaitTime <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataAll() %>% filter(New.PT3 == TRUE)
    # data <- all.data %>% filter(Campus == "MSUS") %>% filter(New.PT3 == TRUE)
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0, !is.na(New.PT3)) %>%
      group_by(Campus.Specialty) %>%
      dplyr::summarise(avgWaitTime = round(mean(wait.time)),
                       medWaitTime = round(median(wait.time)))
    
    if(input$median1 == TRUE){ # Median Wait Time
      
      ggplot(waitTime, 
             aes(reorder(Campus.Specialty, -medWaitTime), medWaitTime, fill = medWaitTime <= 14))+
        geom_bar(stat="identity", width = 0.8) +
        scale_fill_manual("New Access Target: <= 14 Days", values = c("#f8696b", "#63be7b"))+
        scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.2), expand = c(0,0))+
        labs(x=NULL, y="Days",
             title = "Median New Appointment Lead Days by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_new_line()+
        theme_bw()+
        graph_theme("top")
      
    }else{ # Average Wait Time 
      
      ggplot(waitTime, 
             aes(reorder(Campus.Specialty, -avgWaitTime), avgWaitTime, fill = avgWaitTime <= 14))+
        geom_bar(stat="identity", width = 0.8) +
        scale_fill_manual("New Access Target:\n <= 14 Days", values = c("#f8696b", "#63be7b"))+
        scale_y_continuous(limits=c(0,max(waitTime$avgWaitTime)*1.2), expand = c(0,0))+
        labs(x=NULL, y="Days",
             title = "Average New Appointment Lead Days by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_new_line()+
        theme_bw()+
        graph_theme("top")
      
    }
  })
  
  output$siteWorkingFTE <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataPastSlot()
    # data <- past.slot.data %>% filter(Campus == "MSUS")
    
    summary <- data %>%
      group_by(Campus.Specialty, Appt.DateYear) %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60,0)) %>%
      gather(variable, value, 3) %>%
      group_by(Campus.Specialty, variable) %>%
      dplyr::summarise(avgHrs = mean(value),
                       avgFTE = avgHrs/7.5*7.5)
    
    
    ggplot(summary, aes(x=Campus.Specialty)) +
      geom_bar(aes(y=avgHrs),stat="identity", width = 0.8, fill="#d80b8c")+
      scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"), limits=c(0,max(summary$avgHrs)*1.3))+
      labs(x=NULL, y="Hours\n",
           title = "Daily Avg Provider Hours and FTE Available by Specialty",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "1 FTE = 7.5 patient care hours per day")+
      theme_bw() + graph_theme(legend_pos="none") + theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
                                                          strip.background = element_rect(fill="#dddedd"))
    
  })
  
  
  output$sitePtsPerFTE <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataPastSlot()
    # data <- past.slot.data %>% filter(Campus == "MSUS")
    
    summary <- data %>%
      group_by(Campus.Specialty) %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60,0),
                       `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
      mutate(avgFTE = `Available Hours`/7.5,
             avgPtsPerFTE = `Arrived Slots`/avgFTE)
    
    summary$avgPtsPerFTE[which(!is.finite(summary$avgPtsPerFTE))] <- 0
    
    ggplot(summary, aes(x=Campus.Specialty, y=avgPtsPerFTE)) +
      geom_bar(stat="identity", width = 0.8, fill="#00aeef")+
      scale_y_continuous(limits=c(0,max(summary$avgPtsPerFTE)*1.3))+
      labs(x=NULL, y="Patientss\n",
           title = "Daily Avg Patients Arrived per Provider FTE",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "1 FTE = 7.5 patient care hours per day")+
      theme_bw()+ graph_theme(legend_pos="none") + theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"), strip.background = element_rect(fill="#dddedd"))
  })
  
  # Site Comparison Tab -------------------------------------------------------------------------------------------------
  
  output$siteComparisonPts <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    # Scheduling Arrived Data
    arrivedPts <- dataArrived()
    # arrivedPts <- arrived.data %>% filter(Campus.Specialty == "Cardiology")
    arrivedPts$siteSpecialty <- paste0(arrivedPts$Campus," - ",arrivedPts$Campus.Specialty)
    
    if(input$bySpecialty1 == TRUE) {
      
      arrivedPts.tb <- arrivedPts %>% group_by(siteSpecialty, Appt.Week) %>% dplyr::summarise(`Total Patients Arrived` = n())
      
      ggplot(arrivedPts.tb, aes(Appt.Week, `Total Patients Arrived`, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Patients",
             title = "Total Arrived Patients by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(arrivedPts.tb$`Total Patients Arrived`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") +theme(legend.title = element_blank())
    }
    
  })
  
  output$siteComparisonNewPtRatio <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_y_continuous(labels = percent_format(), limits=c(0,1)) +
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
    
  })
  
  
  output$siteComparisonNewPtWaitTime <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
             title = "Median New Appointment Lead Days by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             title = "Median New Appointment Lead Days by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(newWaitTime.tb$`Median New Wait Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
      
    }
  })
  
  
  output$siteComparisonNoShow <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
        scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Avg No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
        #scale_y_continuous(labels = percent_format(), limits = c(0,1))+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Avg No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
    
  })
  
  testdataset <- reactive({
    slotData <- dataPastSlot()
    slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
    bookedFilledRate <- slotData %>%
      group_by(siteSpecialty, Appt.Week) %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                       `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                       `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                       `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
      mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
             `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
      gather(variable, value, 3:9)
    
    bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
  })
  
  output$Testtable <- renderDataTable(testdataset(), filter = "top")
  
  output$siteComparisonBookedRate <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    # Slot Data
    slotData <- dataPastSlot()
    #slotData <- dataPastSlotTest()
     #slotData <- past.slot.data %>% filter(Campus == "MSUS")
    #slotData_maternal <- slotData %>% filter(Campus.Specialty == "Maternal Fetal Medicine")

    slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)

    if(input$bySpecialty4 == TRUE) {
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(siteSpecialty, Appt.Week) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
        gather(variable, value, 3:9)
      
      bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
      #bookedFilledRate_maternal <- bookedFilledRate %>% filter(siteSpecialty == "MSUS - Maternal Fetal Medicine")
      
      ggplot(bookedFilledRate, aes(Appt.Week, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_wrap(variable~., dir ="v")+
        #scale_y_continuous(labels= percent_format(), limits = c(0,1))+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y=NULL,
             title = "Avg Daily Booked vs. Filled Rate (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+ theme(legend.title = element_blank())+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
    } else{
      
      # Booked Rate and Filled Rate
      bookedFilledRate <- slotData %>%
        group_by(siteSpecialty, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
        gather(variable, value, 3:9)
      
      bookedFilledRate <- bookedFilledRate %>% filter(variable %in% c("Booked Rate","Filled Rate"))
      
      #bookedFilledRate_maternal <- bookedFilledRate %>% filter(siteSpecialty == "MSUS - Maternal Fetal Medicine")
      
      ggplot(bookedFilledRate, aes(Appt.MonthYear, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_wrap(variable~., dir = "v")+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
        #scale_y_continuous(labels = percent_format(), limits = c(0,1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y=NULL,
             title = "Avg Daily Booked vs. Filled Rate (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        #scale_y_continuous(expand = c(0,0), limits = c(0,max(bookedFilledRate$value)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
      
    }
    
  })
  
  
  output$siteComparisonMedianCheckInCycleTime <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(cycleTimes$`Median Cycle Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
  })
  
  
  output$siteComparisonMedianCycleTime <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             title = "Median Check-in to Room-in Time (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        scale_y_continuous(expand = c(0,0), limits = c(0,max(roomedTime$`Median Check-in to Room-in Time`)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom") + theme(legend.title = element_blank())
    }
  })
  
  output$siteComparisonWorkingFTE <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    # Slot Data
    slotData <- dataPastSlot()
    # slotData <- past.slot.data %>% filter(Campus == "MSUS")
    slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
    
    
    if(input$bySpecialty7 == TRUE) {
      # Total Provider Hours Available
      provAvail <- slotData %>%
        group_by(siteSpecialty, Appt.Week) %>%
        dplyr::summarise(avgHrs = round(sum(AVAIL_MINUTES)/60,0)) %>%
        mutate(avgFTE = avgHrs/7.5*7.5)
      
      
      ggplot(provAvail, aes(x=Appt.Week, y=avgHrs, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Hours\n",
             title = "Daily Avg Provider Hours and FTE Available by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "1 FTE = 7.5 patient care hours per day")+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
    } else{
      
      # Total Provider Hours Available
      provAvail <- slotData %>%
        group_by(siteSpecialty, Appt.MonthYear) %>%
        dplyr::summarise(avgHrs = round(sum(AVAIL_MINUTES)/60,0)) %>%
        mutate(avgFTE = avgHrs/7.5*7.5)
      
      
      ggplot(provAvail, aes(x=Appt.MonthYear, y=avgHrs, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(sec.axis = sec_axis(~./7.5, name = "Provider FTE\n"))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Hours\n",
             title = "Daily Avg Provider Hours and FTE Available by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "1 FTE = 7.5 patient care hours per day")+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")
      
    }
  })
  
  output$siteComparisonPtsPerFTE <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    # Slot Data
    slotData <- dataPastSlot()
    # slotData <- past.slot.data %>% filter(Campus == "MSUS")
    slotData$siteSpecialty <- paste0(slotData$Campus," - ",slotData$Campus.Specialty)
    
    if(input$bySpecialty8 == TRUE) {
      summary <- slotData %>%
        group_by(siteSpecialty, Appt.Week) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60),
                         `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
        mutate(avgFTE = `Available Hours`/7.5,
               avgPtsPerFTE = `Arrived Slots`/avgFTE)
      
      ggplot(summary, aes(x=Appt.Week, y=avgPtsPerFTE, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Patients per Provider FTE\n",
             title = "Daily Avg Arrived Patients per Provider FTE by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "1 FTE = 7.5 patient care hours per day")+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
    } else{
      
      summary <- slotData %>%
        group_by(siteSpecialty, Appt.MonthYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60),
                         `Arrived Slots` = sum(ARRIVED_SLOTS)) %>%
        mutate(avgFTE = `Available Hours`/7.5,
               avgPtsPerFTE = `Arrived Slots`/avgFTE)
      
      ggplot(summary, aes(x=Appt.MonthYear, y=avgPtsPerFTE, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Patients per Provider FTE\n",
             title = "Daily Avg Arrived Patients per Provider FTE by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
             caption = "1 FTE = 7.5 patient care hours per day")+
        theme_new_line()+
        theme_bw()+
        graph_theme("bottom")
      
    }
    
  })
  
  
  # Date Range Header --------------------------------------------------------------------------
  output$practiceName_KPIs <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_siteOverview <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_siteComp <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_practice <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_provider <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_volume <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_population <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2])})
  
  output$practiceName_utilization <- renderText({
    paste0("Based on data from ", input$dateRangeUtil[1]," to ", input$dateRangeUtil[2])})
 
  
  ### (4) Analysis Output ==============================================================================================================
  ### Practice Overview Tab ------------------------------------------------------------------------------------------------------------
  ### Volume Section
  output$uniquePts <- renderValueBox({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
      subtitle = tags$p("Avg Visits per Patient", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgVisitsDay <- renderValueBox({
    valueBox(
      prettyNum(round(length((unique(dataArrived()$uniqueId))) / length((unique(dataArrived()$Appt.DateYear))),1), big.mark = ','),
      subtitle = tags$p("Avg Visits per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ### Average Daily Patients by Scheduled vs. Check-in Times (1-hour interval)
  
  output$avgPtArrival <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "Scheduled = Arrived + No Show Patients")+
      theme_new_line()+
      theme_bw()+
      graph_theme("top") + theme(legend.title = element_blank())
    
  })
  
  ### Access Section
  
  output$newPtRatio <- renderInfoBox({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    infoBox(
      title = tags$p("Arrived New Patient Ratio", style = "font-size: 130%;"), subtitle = NULL, 
      value = paste0(round((nrow(dataArrived() %>% filter(New.PT3 == TRUE)) / nrow(dataArrived()))*100),"%"), icon = icon("user")
    )
  })
  
  output$newNoShow <- renderInfoBox({
    infoBox(
      title = tags$p("Avg New Patient No Show %", style = "font-size: 130%;"), subtitle = NULL,
      value = paste0(round(nrow(dataArrivedNoShow() %>% filter(New.PT3 == TRUE) %>% filter(Appt.Status != "Arrived"))/ nrow(dataArrivedNoShow() %>% filter(New.PT3 == TRUE)),2)*100,"%"), icon = icon("clock")
    )
  })
  
  output$newApptWaitTime <- renderInfoBox({
    infoBox(
      title = tags$p("Avg Lead Days to New Appointment", style = "font-size: 130%;"), subtitle = NULL, 
      value =  paste0(round(mean((dataAll() %>% filter(New.PT3 == TRUE) %>%
                                    mutate(wait.time = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
                                    filter(!is.na(wait.time)) %>% filter(wait.time >= 0))$wait.time))," days"), icon = icon("clock")
    )
  })
  
  ### Scheduling Section
  output$fillRate_tb <- function(){

    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )

    # Booked and Filled Rate
    data <- dataPastSlot()
    # data <- past.slot.data

      daily.booked <- data %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                       `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                       `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                       `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
      mutate(`Booked Rate` = paste0(round((`Booked Hours`/`Available Hours`),2)*100, "%"),
             `Filled Rate` = paste0(round((`Arrived Hours`/`Available Hours`),2)*100, "%")) %>%
      gather(variable, value, 1:7) %>%
      filter(variable %in% c("Booked Rate","Filled Rate")) %>%
      select(variable, value)

    
    # Scheduling Activity 
      
      data2 <- dataAll()
      # data2 <- all.data
      
      apptsCanceled <- data2 %>% 
        group_by(Appt.Status) %>%
        summarise(value = n()) %>%
        arrange(desc(value)) %>%
        mutate(percent = paste0(round((value/sum(value))*100),"%")) %>%
        select(Appt.Status, percent) %>%
        `colnames<-` (c("variable", "value"))
      
      # apptsCanceled$percent[apptsCanceled$Status == "No Show"] <- round((apptsCanceled$Total[apptsCanceled$Status == "No Show"] / (apptsCanceled$Total[apptsCanceled$Status == "Arrived"] + apptsCanceled$Total[apptsCanceled$Status == "No Show"])),2)
      
      scheduling_tb <- bind_rows(daily.booked, apptsCanceled)
      
      
      kable(scheduling_tb, col.names = NULL) %>%
        pack_rows("Slot Usage", 1, 2, label_row_css = "background-color: #fcc9e9;") %>%
        pack_rows("Scheduling Activity", 3, nrow(scheduling_tb), label_row_css = "background-color: #fcc9e9;") %>%
        kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE,
                      position = "center", row_label_position = "l", font_size = 18) %>%
        column_spec(1,  background = "#d80b8c", color = "white")
  }
  
  
  ### Day of Visit Section
  
  output$avgCycleTime <- renderValueBox({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(cycleTime > 0))$cycleTime, na.rm = TRUE))," min"),
      title = toupper("Avg Check-in to Visit-End"),
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
      labs(title = "Distribution of Check-in to Visit-end Time\n", 
           y = "\n% of Patients",
           x = "\nMinutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.line = element_line(size = 0.3, colour = "black"))+
      scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
      scale_y_continuous(labels = scales::percent)
    
  })
  
  
  output$avgCheckinToRoomin <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0))$checkinToRoomin, na.rm = TRUE))," min"),
      title = toupper("Avg Check-in to Room-in"),
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataArrived() %>% filter(checkinToRoomin >= 0)
    # data <- arrived.data %>% filter(checkinToRoomin >= 0)
    
    ggplot(data, aes(x=checkinToRoomin)) + 
      geom_histogram(aes(y = (..count..)/sum(..count..)),
                     bins = 14,
                     color="#212070", fill="#c7c6ef") +
      labs(title = "Distribution of Check-in to Room-in Time\n", 
           y = "\n% of Patients",
           x = "\nMinutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.line = element_line(size = 0.3, colour = "black"))+
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
  
  ### Provider Overview Tab ------------------------------------------------------------------------------------------------------------
  
  # Value Boxes for Key Metrics
  output$vbox1 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(length(unique(dataArrived()$uniqueId)), big.mark=',')," Patients"),
      subtitle = tags$p("TOTAL PATIENTS SEEN", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox2 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(nrow(dataArrived() %>% filter(New.PT == "Y") %>% distinct(uniqueId)), big.mark=',')," Patients"),
      subtitle = tags$p("TOTAL NEW PATIENTS SEEN", style = "font-size: 130%;"), icon = NULL, color = "yellow")
  })

  output$vbox3 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round(median((dataAll() %>% filter(New.PT3 == TRUE, Wait.Time >= 0))$Wait.Time)), big.mark=',')," Days"),
      subtitle = tags$p("MEDIAN NEW APPT LEAD TIME", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })

  output$vbox4 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round(length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark=',')," Patients/Day"),
      subtitle = tags$p("AVG PATIENTS SEEN", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox5 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round(nrow(dataArrived() %>% filter(New.PT3 == TRUE) %>% 
                                    distinct(uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark=',')," Patients/Day"),
      subtitle = tags$p("AVG NEW PATIENTS SEEN", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox6 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round(mean((dataAll() %>% filter(New.PT3 == TRUE, Wait.Time >= 0))$Wait.Time)), big.mark=',')," Days"),
      subtitle = tags$p("AVG NEW APPT LEAD TIME", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox7 <- renderValueBox({
    
      valueBox(
        paste0(prettyNum(round((sum(dataPastSlot()$AVAIL_MINUTES)/60)), big.mark=','), " Hrs."),
        subtitle = tags$p("TOTAL HRS AVAILABLE", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox8 <- renderValueBox({
    
    valueBox(
      paste0(prettyNum(round((sum(dataPastSlot()$AVAIL_MINUTES) / length(unique(dataPastSlot()$Appt.DateYear)))/60), big.mark=','), " Hrs."),
      subtitle = tags$p("AVG DAILY HRS AVAILABLE", style = "font-size: 140%;"), icon = NULL, color = "yellow")
  })
  
  output$vbox9 <- renderValueBox({
    
    valueBox(
      paste0(round((sum(dataPastSlot()$AVAIL_MINUTES) / length(unique(dataPastSlot()$Appt.DateYear)))/
                     (length(unique(dataArrived()$uniqueId)) / length(unique(dataArrived()$Appt.DateYear))), 1), " Min/Patient"),
      subtitle = tags$p("AVG WORKED MINS PER PATIENT", style = "font-size: 140%;"), icon = NULL, color = "yellow")
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
      
      data <- dataArrivedNoShow() 
      
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
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
      # data <- past.slot.data %>% filter(Campus == "MSUS")
      # nrow(data)
      
      # TEMPORARY SLOT DATE
      daily.booked <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`)*100))
      
      daily.booked.df <- byDayTime.df %>% filter(Day %in% unique(daily.booked$Appt.Day))
      daily.booked.df <- merge(daily.booked.df, daily.booked, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.booked.df <- daily.booked.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.booked.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=`Booked Rate`), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Booked Rate (%)")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily Booked Rate",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
      # data <- past.slot.data
      
      # TEMPORARY SLOT DATE
      daily.filled <- data %>%
        group_by(Appt.Day, Appt.TM.Hr) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`)*100))
      
      daily.filled.df <- byDayTime.df %>% filter(Day %in% unique(daily.filled$Appt.Day))
      daily.filled.df <- merge(daily.filled.df, daily.filled, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE)
      
      daily.filled.df <- daily.filled.df %>% filter(Time %in% timeOptionsHr_filter)
      
      ggplot(daily.filled.df, aes(x=factor(Day, levels = daysOfWeek.options), y=factor(Time, levels = rev(timeOptionsHr))))+
        geom_tile(aes(fill=`Filled Rate`), colour = "black", size=0.5)+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="Filled Rate (%)")+
        scale_x_discrete(position = "top")+
        labs(x=NULL, y=NULL,
             title = "Average Daily Filled Rate",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
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
  
  
  output$provApptStatusPie <- renderPlot({

    data <- dataAll()
    # data <- all.data

    apptsCanceled <- data %>%
      group_by(Appt.Status) %>%
      summarise(value = n()) %>%
      arrange(desc(value)) %>%
      mutate(percent = round((value/sum(value)), 2)) %>%
      select(Appt.Status, percent) 

    ggplot(apptsCanceled, aes(reorder(Appt.Status, -percent), percent, fill=Appt.Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL, 
           title = "Scheduling Activity Breakdown")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      geom_text(aes(label=paste0(round(percent*100),"%")), hjust = 0.5, vjust = -1, color="black", fontface="bold",
                position = position_dodge(1), size=5)

  })

  output$provCoverage <- function(){
    
    data <- dataArrivedNoShow()
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
  
  output$provSlotUsagesAvg <- renderPlot({
    
    data <- dataPastSlot()
    # data <- past.slot.data %>% filter(Campus == "MSUS")
    
    # slot.usage <- data %>%
    #   dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
    #                    `Booked Hours` = round(sum(BOOKED_MINUTES),0),
    #                    `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
    #                    `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
    #                    `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
    #   mutate(`Slot Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
    #          `Slot Filled Rate` = round((`Arrived Hours`/`Available Hours`),2),
    #          `Slot No Show Rate` = round((`No Show Hours`/`Booked Hours`), 2),
    #          `Slot Canceled Rate` = round((`No Show Hours`/`Booked Hours`), 2)) %>%
    #   gather(variable, value, 1:7)
    # 
    
    slot.usage <- data %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                       `Arrived Hours` = round(sum(ARRIVED_MINUTES),0)) %>%
      mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`),2),
             `Filled Rate (%)` = round((`Arrived Hours`/`Available Hours`),2),
             `Unused Rate (%)` = 1 - `Filled Rate (%)`) %>%
      gather(variable, value, 1:6) %>%
      group_by(variable) %>%
      filter(variable %in% c("Unused Rate (%)","Filled Rate (%)","Booked Rate (%)"))
    
    ggplot(slot.usage, aes(x=factor(x=variable, level= c("Unused Rate (%)","Filled Rate (%)","Booked Rate (%)")), y=value, fill=variable)) +
      geom_bar(stat="identity", width=0.8, color="white") +
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(slot.usage$value)*1.2)) +
      labs(x = NULL, y = NULL,
           title = "Average Booked vs. Filled Rate",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      scale_fill_manual(values = c("#212070","#d80b8c","#dddedd")) +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x =  element_text(color= "black"),
            axis.text.x = element_text(size = "14"),
            axis.text.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30)) +
      geom_text(aes(label= paste0(value*100,"%")), hjust=0.5, vjust=-1, color="black", size=6) 
    # annotate(geom="label",x=.7,y=dept.booked.avg,label="Avg. Dept\nBooked Rate", fill = "white", color = "#212070", size=5) +
    # annotate(geom="label",x=1.3,y=dept.filled.avg,label="Avg. Dept\nFilled Rate", fill = "white", color = "#d80b8c", size=5)
    
  })
  
  
  
  ### KPIs Tab ------------------------------------------------------------------------------------------------------------------------
  # Volume KPI ====================================================================
  output$kpiVolumeGraph <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    kpiVolumeData <- aggregate(dataArrivedKpi()$uniqueId, by=list(dataArrivedKpi()$Appt.Year,dataArrivedKpi()$Appt.Quarter,
                                                                  dataArrivedKpi()$Appt.Month, dataArrivedKpi()$Appt.Date, dataArrivedKpi()$Appt.MonthYear, dataArrivedKpi()$Appt.DateYear), FUN=NROW)
    
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataQuarter$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")
        
      } else if(input$kpiFreq == 3){ # Month
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    statusData <- dataAll() %>% 
      group_by(Appt.Year, Appt.Quarter, Appt.Month, Appt.Date, Appt.Status, Appt.MonthYear, Appt.DateYear) %>%
      summarise(total = n()) %>%
      `colnames<-` (c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count"))
    
    # statusData <- all.data %>% 
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
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(statusDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_color_MountSinai("main")
        
      } else { # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%Y-%m-%d"), y=value, col=variable, group=variable)) +
          geom_line() +
          labs(x = NULL, y = NULL,  
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%m-%d"), y=value, col=Year,group=Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
               )+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0,max(statusDataYear$value)*1.2))+
          facet_wrap(variable~., scales = "free", dir = "v")+
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
               title = "Average New Appointment Lead Days by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               title = "Average New Appointment Lead Days by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               title = "Average New Appointment Lead Days by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
    
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])
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
  

  ### Utilization Tab -----------------------------------------------------------------------------------------------------------------
  # Average Rooms Required --------------------------------------------------------------------------------------------
  output$roomStat1 <- renderValueBox({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    valueBox(NULL,
             # paste0(input$setRooms," rooms available\n throughout",input$setHours," hours"),
             subtitle = tags$p(paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours"), style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgRoomsRequired <- renderValueBox({
    
    max((dataArrivedUtilization() %>%
           select(Appt.DateYear, timeOptionsHr_filter) %>%
           gather(Time, sum, 2:15) %>%
           group_by(Time) %>%
           summarise(avg = ceiling((sum(sum)/length(unique(Appt.DateYear)))/60)))$avg) %>%
      valueBox(
        subtitle = tags$p("Max Rooms Required During the Day", style = "font-size: 160%;"), icon = NULL, color = "fuchsia")
  })
  
  # Scheduled and Avg Utilization --------------------------------------------------------------------------------------------------------
  output$avgScheduledUtilization <- renderValueBox({
    
    data <- dataHourScheduled()
    # data <- data.hour.scheduled
    
    paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%") %>%
      valueBox(
        subtitle = tags$p("Average Daily Booked Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  })
  
  output$avgUtilization <- renderValueBox({
    
    data <- dataArrivedUtilization()
    
    paste0(round((sum(data$sum))/(length(unique(data$Appt.DateYear))*(60*input$setHours*input$setRooms))*100)," %") %>%
      valueBox(
        subtitle = tags$p("Average Daily Filled Utilization", style = "font-size: 160%;"), icon = NULL, color = "aqua")
  })
  
  # Average Number of Rooms Required -----------------------------------------------
  output$spaceUsed <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataArrivedUtilization()
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
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
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      theme_bw()+
      # theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      #       plot.subtitle = element_text(hjust=0.5, size = 14),
      #       legend.position = "top",
      #       legend.text = element_text(size="12"),
      #       legend.direction = "horizontal",
      #       legend.key.size = unit(1.0,"cm"),
      #       legend.title = element_blank(),
      #       axis.title = element_text(size="14"),
      #       axis.text = element_text(size="14"),
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_text(margin = margin(r=5)),
      #       axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
      #       axis.text.y = element_text(margin = margin(l=5, r=5)),
      #       panel.grid.minor = element_blank(),
      #       panel.border = element_blank(),
      #       panel.background = element_blank(),
      #       axis.line = element_line(size = 0.3, colour = "black"),
      #       plot.margin = margin(30,30,10,30))+
    graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
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
      geom_text(aes(label= ifelse(is.na(Average_Req),"", round(Average_Req,1))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Average Utilization by Time of Day
  output$spaceUtil <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataArrivedUtilization()
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(Appt.Day, Appt.DateYear) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day) %>%
      dplyr::summarise(count = n())
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
    space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$Appt.Day),FUN = sum)
    space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*input$setRooms), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Util")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=Average_Util, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Utilization (%)\n", 
           title = "Average Space Utilization (%) by Time of Day and Day of Week",
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      geom_hline(yintercept=.8, linetype="dashed", color = "red")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour.day$Average_Util)*1.2))+
      theme_bw()+
      # theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      #       plot.subtitle = element_text(hjust=0.5, size = 14),
      #       legend.position = "top",
      #       legend.text = element_text(size="12"),
      #       legend.direction = "horizontal",
      #       legend.key.size = unit(1.0,"cm"),
      #       legend.title = element_blank(),
      #       axis.title = element_text(size="14"),
      #       axis.text = element_text(size="14"),
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_text(size=14),
      #       axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
      #       axis.text.y = element_text(margin = margin(l=5, r=5)),
      #       panel.grid.minor = element_blank(),
      #       panel.border = element_blank(),
      #       panel.background = element_blank(),
      #       axis.line = element_line(size = 0.3, colour = "black"),
      #       plot.margin = margin(30,30,30,30))+
    graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
      guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=Average_Util), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Util)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization ")+
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
      geom_text(aes(label= ifelse(is.na(Average_Util),"",paste0(round(Average_Util,2)*100,"%"))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Rooms Required by Percentile 
  output$spaceUsedPerc <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataArrivedUtilization()
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
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
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      theme_bw()+
      # theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      #       plot.subtitle = element_text(hjust=0.5, size = 14),
      #       legend.position = "top",
      #       legend.text = element_text(size="12"),
      #       legend.direction = "horizontal",
      #       legend.key.size = unit(1.0,"cm"),
      #       legend.title = element_blank(),
      #       axis.title = element_text(size="14"),
      #       axis.text = element_text(size="14"),
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_text(margin = margin(r=5)),
      #       axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
      #       axis.text.y = element_text(margin = margin(l=5, r=5)),
      #       panel.grid.minor = element_blank(),
      #       panel.border = element_blank(),
      #       panel.background = element_blank(),
      #       axis.line = element_line(size = 0.3, colour = "black"),
      #       plot.margin = margin(30,30,30,30))+
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    data <- dataArrivedUtilization()
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = quantile(value, probs=0.5)/(60*input$setRooms),
        `70th Percentile`= quantile(value, probs=0.75)/(60*input$setRooms),
        `90th Percentile`= quantile(value, probs=0.90)/(60*input$setRooms))
    
    colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n", 
           title = "Space Utilization (%) by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      theme_bw()+
      # theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
      #       plot.subtitle = element_text(hjust=0.5, size = 14),
      #       legend.position = "top",
      #       legend.text = element_text(size="12"),
      #       legend.direction = "horizontal",
      #       legend.key.size = unit(1.0,"cm"),
      #       legend.title = element_blank(),
      #       axis.title = element_text(size="14"),
      #       axis.text = element_text(size="14"),
      #       axis.title.x = element_blank(),
      #       axis.title.y = element_text(size = 14),
      #       axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
      #       axis.text.y = element_text(margin = margin(l=5, r=5)),
      #       panel.grid.minor = element_blank(),
      #       panel.border = element_blank(),
      #       panel.background = element_blank(),
      #       axis.line = element_line(size = 0.3, colour = "black"),
      #       plot.margin = margin(30,30,30,30))+
    graph_theme("top") + theme(legend.title = element_blank(), legend.direction = "horizontal", legend.key.size = unit(1.0,"cm"))
      guides(colour = guide_legend(nrow = 1))
    
    
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
      geom_text(aes(label= ifelse(is.na(value),"",paste0(round(value,2)*100,"%"))), color="black", size=5, fontface="bold")
    
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,2))
    
  })
  
  ### [3. ] Population Tab Output -----------------------------------------------------------------------------------------------------
  
  ## Demographics Breakdown
  
  output$ins_breakdown <- renderGirafe({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    data <- dataArrived() %>%
      group_by(Coverage) %>%
      dplyr::summarise(value = n()) %>%
      mutate(percent = round((value/sum(value))*100)) %>%
      arrange(desc(value))
    
    data$Coverage[is.na(data$Coverage)] <- "Unknown"
    
    packing <- circleProgressiveLayout(data$value, sizetype='area')
    packing$radius <- 0.95*packing$radius
    
    data <- cbind(data, packing)
    
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    
    p <- ggplot() + 
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
      # Add text in the center of each bubble + control its size
      geom_text(data = data, aes(x, y, size=value, label = paste0(str_wrap(Coverage, 15),"\n",prettyNum(value, big.mark = ','),"\n(",percent,"%)"), fontface="bold")) +
      ggtitle(label="Coverage Breakdown",
              subtitle = "Count and Percent of Total Arrived Patients") +
      scale_size_continuous(range = c(1,4)) +
      scale_fill_MountSinai() +
      # General theme:
      theme_void() + 
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            plot.subtitle = element_text(hjust=0.5, size = 12, face = "italic"),
            legend.position="none") +
      coord_equal()
    
    # ggiraph(ggobj = p, width_svg = 7, height_svg = 7)
    
    girafe(ggobj = p)
    
    
    
    
    # data <- dataArrived() %>%
    #   group_by(Coverage) %>%
    #   dplyr::summarise(total = n()) %>%
    #   arrange(desc(total))
    # 
    # ggplot(data, aes(x=Coverage, y=total)) +
    #   stat_pareto(point.color = "#d80b8c",
    #               point.size = 3,
    #               line.color = "#d80b8c",
    #               #size.line = 1,
    #               bars.fill = c("midnightblue"))+
    #   ggtitle("Number and Cumulative Percent of Completed Appointments by Insurance Type")+
    #   theme_new_line()+
    #   theme(
    #     legend.position = "none",
    #     axis.title.y = element_blank(),
    #     axis.title.x = element_blank(),
    #     axis.text.x = element_text(size = "16", hjust=1, vjust=1, angle = 45),
    #     axis.text.y = element_text(size = "16"))
    # 
  })
  
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
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 14) %>%
      row_spec(0, bold=T, background = "#dddedd", color = "black") %>%
      column_spec(1, bold=T) %>%
      scroll_box(height = "600px")
    
  }
  
  output$sex_breakdown <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
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
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
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
  
  
  output$population1 <- renderLeaflet({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    #data(zipcode)
    zipcode <- read_feather(here::here("Data/zipcode.feather"))
    
    population.data <- 
      dataArrived()[,c("Campus","Campus.Specialty","Department","MRN","Zip.Code","Sex","Coverage","uniqueId")]
    
    population.data$zip <- normalize_zip(population.data$Zip.Code)
    
    population.data <- merge(population.data, zipcode, by.x='zip', by.y='zip')
    
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
  output$volume1 <- renderHighchart({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    data <- dataArrived()
    
    # data <- arrived.data %>% filter(!holiday %in% c("Christmas"))
    pts.count <- aggregate(data$uniqueId,
                           by=list(data$Appt.DateYear), FUN=NROW)
    
    # pts.count <- aggregate(arrived.data$uniqueId,
    #                        by=list(arrived.data$Appt.DateYear), FUN=NROW)
    
    names(pts.count) <- c("Date","Volume")
    pts.count$Date <- as.Date(pts.count$Date, format="%Y-%m-%d")
    
    model <- lm(Volume ~ Date, data = pts.count)
    fit <- augment(model) %>% arrange(Date)
    
    # Visualization
    pts.count %>% 
      hchart('line', hcaes(x = Date, y = Volume)) %>%
      hc_add_series(
        fit, type = "line", hcaes(x = Date, y = .fitted),
        name = "Linear Regression", id = "fit") %>%
      hc_colors(c("#212070","red")) %>%
      hc_title(text="Daily Arrived Patients over Time\n", align="center", style = list(color = "black", fontSize = "16px", fontWeight = "bold", useHTML = TRUE)) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_subtitle(text = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2])) %>%
      hc_xAxis(title = list(text = ''), labels = list(format = '{value:%Y-%m}', rotation = "310")) %>%
      hc_yAxis(title = list(text = 'Patients'))
    # ggplot(pts.count,  aes(x=Date, y=Volume))+
    #   geom_line(color="midnightblue")+
    #   #geom_point(color="midnightblue")+
    #   geom_smooth(method='lm', col = "red", se=FALSE, size=0.5)+
    #   scale_x_date(breaks = seq(min(pts.count$Date), 
    #                             max(pts.count$Date), by = "1 month"), date_labels = "%b\n%Y")+
    #   ggtitle("Daily Patient Volume over Time")+
    #   theme_new_line()+
    #   theme(
    #     legend.position = "none",
    #     axis.title.y = element_blank(),
    #     axis.title.x = element_blank(),
    #     axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
    #     axis.text.y = element_text(size = "16"))
    # 
  })
  
  # Total Monthly Patient Volume
  output$volume2 <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    pts.by.month <- aggregate(dataArrived()$uniqueId, 
                              by=list(dataArrived()$Appt.MonthYear), FUN=NROW)
    
    # pts.by.month <- aggregate(arrived.data$uniqueId, 
    #                           by=list(arrived.data$Appt.MonthYear), FUN=NROW)
    
    names(pts.by.month) <- c("Month","Volume")
    pts.by.month$Volume <- as.numeric(pts.by.month$Volume)
    pts.by.month$Month <- as.yearmon(pts.by.month$Month, format="%Y-%m")
    pts.by.month$Month <- as.Date(pts.by.month$Month, format="%Y-%m")
    
    ggplot(pts.by.month, aes(x=Month, y=Volume))+
      geom_bar(stat="identity",fill="midnightblue")+
      labs(x = NULL, y = "Patients",
           title = "Monthly Patient Volume",
           subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
      scale_x_date(breaks = seq(min(pts.by.month$Month), 
                                max(pts.by.month$Month), by = "1 month"), date_labels = "%Y-%m")+
      scale_y_continuous(limits=c(0,(max(pts.by.month$Volume))*1.2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
      # theme(
      #   legend.position = "none",
      #   axis.title.y = element_blank(),
      #   axis.title.x = element_blank(),
      #   axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
      #   axis.text.y = element_text(size = "16"))+
      # geom_text(aes(label=Volume), vjust = -.5, color="black", fontface="bold",
      #           position = position_dodge(1), size=5)
    
  })
  
  # Average Daily Patient Volume by Day of Week
  output$volume3 <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    pts.by.day <- aggregate(dataArrived()$uniqueId, 
                            by=list(dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.by.day) <- c("Day","Volume")
    totalDates <- as.data.frame(seq(as.Date(min(arrived.data$Appt.DTTM)), as.Date(max(arrived.data$Appt.DTTM)),by="days"))
    names(totalDates) <- c("Dates")
    totalDates$day <- format(as.Date(totalDates$Dates, format="%Y-%m-%d"), "%a")
    totalDates <- aggregate(totalDates$Dates,
                            by=list(totalDates$day), FUN=NROW)
    names(totalDates) <- c("Day","Count")
    
    pts.by.day$Day.Count <- totalDates$Count[match(pts.by.day$Day, totalDates$Day)]
    pts.by.day$Avg.Volume <- as.numeric(round(pts.by.day$Volume/pts.by.day$Day.Count,1))
    
    ggplot(pts.by.day, aes(x=factor(Day, level = daysOfWeek.options), y=Avg.Volume))+
      geom_bar(stat="identity",fill="midnightblue")+
      labs(x = NULL, y = "Patients",
           title = "Average Daily Patient Volume",
           subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
      scale_y_continuous(limits=c(0,(max(pts.by.day$Avg.Volume))*1.2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
      # theme(
      #   legend.position = "none",
      #   axis.title.y = element_blank(),
      #   axis.title.x = element_blank(),
      #   axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
      #   axis.text.y = element_text(size = "16"))+
      # geom_text(aes(label=Avg.Volume), vjust = -.5, color="black", fontface="bold",
      #           position = position_dodge(1), size=5)
    
  })
  
  # Daily Volume Distribution by Month
  output$volume4 <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    pts.dist <- aggregate(dataArrived()$uniqueId, 
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date), FUN=NROW)
    
    # pts.dist <- aggregate(arrived.data$uniqueId, 
    #                       by=list(arrived.data$Appt.MonthYear, arrived.data$Appt.Date), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Volume")
    pts.dist$Month <- as.yearmon(pts.dist$Month, format="%Y-%m")
    pts.dist$Month <- as.Date(pts.dist$Month, format="%Y-%m")
    pts.dist <- pts.dist[order(pts.dist$Month),]
    
    ggplot(pts.dist, aes(x=Month, y=Volume, group=Month))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Month",
           subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
      scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")

    
  })
  
  # Daily Volume Distribution by Month Table
  output$volume4.1 <- function(){
    
    pts.dist <- aggregate(dataArrived()$uniqueId, 
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date), FUN=NROW)
    
    # pts.dist <- aggregate(arrived.data$uniqueId, 
    #                       by=list(arrived.data$Appt.MonthYear, arrived.data$Appt.Date), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Month) %>%
      dplyr::summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    
    pts.dist.summary <- 
      pts.dist.summary[order(as.yearmon(pts.dist.summary$Month,format="%Y-%m")),]
    
    pts.dist.summary.t <-setNames(data.frame(t(pts.dist.summary[,-1])), pts.dist.summary[,1])
    colnames(pts.dist.summary.t) <- pts.dist.summary$Month
    
    pts.dist.summary.t %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      row_spec(0, bold=T) %>%
      column_spec(1, bold=T)
    
  }
  
  # Daily Volume Distribution by Day of Week
  output$volume5 <- renderPlot({
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    
    
    pts.dist <- aggregate(dataArrived()$uniqueId, 
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+ 
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Day of Week",
           subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
    
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

} # Close server 

#shinyApp(ui, server)


#shinyApp(ui, server, options = list(launch.browser = T,browser = "C:/Program Files/Google/Chrome/Application/chrome.exe"))





