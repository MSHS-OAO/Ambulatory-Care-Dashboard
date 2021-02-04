server <- function(input, output, session) {
  
  ### (1) Create reactive filters ===================================================================================================
  ## SCheduling Data
  observeEvent(input$selectedCampus,{
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = sort(unique(historical.data[historical.data$Campus %in% input$selectedCampus, "Campus.Specialty"]))
    )})
  
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty),{
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = sort(unique(historical.data[
                        historical.data$Campus %in% input$selectedCampus &
                          historical.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))
    )})
  
  observeEvent(c(input$selectedCampus,input$selectedSpecialty,input$selectedDepartment,input$selectedResource),{
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices=sort(unique(historical.data[
                        historical.data$Campus %in% input$selectedCampus &
                          historical.data$Campus.Specialty %in% input$selectedSpecialty &
                          historical.data$Department %in% input$selectedDepartment & 
                          historical.data$Resource %in% input$selectedResource, "Provider"])),
    )})  
  
  
  
  
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
  
  ## KPI Data - Date Range Filters
  output$dateRangeControlKpi <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRangeKpi", label = NULL,
                     start = min(kpi.arrived.data$Appt.DateYear), end = max(kpi.arrived.data$Appt.DateYear),
                     min = min(arrived.data$Appt.DateYear), max = max(arrived.data$Appt.DateYear)))
  })
  
  output$daysOfWeekControlKpi <- renderUI({
    
    box(
      title = "Select Days of Week:",
      width = 12, 
      solidHeader = FALSE, 
      selectInput("daysOfWeekKpi",label = NULL,
                  choices=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = daysOfWeek.options,
                  multiple=TRUE, selectize=TRUE))
  })
  
  # Utilization Data - Date Range Filters
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
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShow <- reactive({
    groupByFilters(arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrived <- reactive({
    groupByFilters(arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataNoShow <- reactive({
    groupByFilters(noShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedRescheduled<- reactive({
    groupByFilters(canceled.bumped.rescheduled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataRescheduled<- reactive({
    groupByFilters(rescheduled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # [2.2] All pre-processed data for kpi tabs --------------------------------------------------------------------------------------
  dataAllKpi <- reactive({
    groupByFilters(kpi.all.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  })
  
  dataArrivedNoShowKpi <- reactive({
    groupByFilters(kpi.arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  })
  
  dataArrivedKpi <- reactive({
    groupByFilters(kpi.arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  })
  
  dataCanceledBumpedKpi <- reactive({
    groupByFilters(kpi.canceled.bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  })
  
  dataCanceledKpi <- reactive({
    groupByFilters(kpi.canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  })
  
  dataBumpedKpi <- reactive({
    groupByFilters(kpi.bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeKpi [1], input$dateRangeKpi[2], input$daysOfWeekKpi, input$excludeHolidays)
  }) 
  
  # [2.2] All pre-processed data for utilization tabs --------------------------------------------------------------------------------------
  # Hour Interval
  # dataHourScheduled <- reactive({
  #   groupByFilters_2(utilization.data,
  #                  input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
  #                  input$selectedVisitMethod, input$dateRangeUtil[1], input$dateRangeUtil[2], input$daysOfWeekUtil, input$excludeHolidays, input$utilType)
  # }) 
  # 
  
  
  dataScheduledUtilization <- reactive({
    groupByFilters_2(scheduled.utilization.data,
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, input$dateRangeUtil [1], input$dateRangeUtil[2], input$daysOfWeekUtil, input$excludeHolidays, input$utilType)
  }) 
  
  dataArrivedUtilization <- reactive({
    groupByFilters_2(arrived.utilization.data,
                     input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                     input$selectedVisitMethod, input$dateRangeUtil [1], input$dateRangeUtil[2], input$daysOfWeekUtil, input$excludeHolidays, input$utilType)
  }) 
  
  dataHourScheduled <- reactive({
    groupByFilters(data.hour.scheduled,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeUtil [1], input$dateRangeUtil[2], input$daysOfWeekUtil, input$excludeHolidays)
  }) 
  
  dataHourArrived <- reactive({
    groupByFilters(data.hour.arrived,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeUtil [1], input$dateRangeUtil[2], input$daysOfWeekUtil, input$excludeHolidays)
  }) 
  
  # [2.3] All pre-processed data for access tabs --------------------------------------------------------------------------------------
  dataFutureSlot <- reactive({
    groupByFilters(future.slot.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRangeFutureSlot [1], input$dateRangeFutureSlot[2], input$daysOfWeekFutureSlot, input$excludeHolidays)
  }) 
  
  dataPastSlot <- reactive({
    groupByFilters(past.slot.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  }) 
  
  groupByFilters <- function(dt, campus, specialty, department, resource, provider, visitMethod, mindateRange, maxdateRange, daysofweek, holidays){
    result <- dt %>% filter(Campus %in% campus, Campus.Specialty %in% specialty, Department %in% department, Provider %in% provider, Resource %in% resource,
                            Visit.Method %in% visitMethod, mindateRange <= Appt.DateYear, maxdateRange >= Appt.DateYear, Appt.Day %in% daysofweek, !holiday %in% holidays)
    return(result)
  }
  
  ### (3) Dashboard Layout ============================================================================================================
  ### [3.1] Title of  Dashboard -------------------------------------------------------------------------------------------------------
  # Site Overview Tab -------------------------------------------------------------------------------------------------
  output$siteTotalPts <- renderValueBox({
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
    
    data <- dataAll()
    # data <- all.data %>% filter(Campus == "MSUS")
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0, !is.na(New.PT3)) %>%
      group_by(Campus.Specialty, New.PT3) %>%
      dplyr::summarise(avgWaitTime = round(mean(wait.time)),
                       medWaitTime = round(median(wait.time)))
    
    waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
    waitTime$New.PT3 <- factor(waitTime$New.PT3, levels=c('New','Established'))
    
    target <- data.frame(New.PT3 = c("New"),
                         target = c(14))
    
    
    if(input$median1 == TRUE){ # Average Wait Time
      
      
      ggplot(waitTime, aes(reorder(Campus.Specialty, -medWaitTime),avgWaitTime, fill=New.PT3)) +
        geom_bar(stat="identity", width = 0.8) +
        geom_hline(data=target, aes(yintercept=target), linetype="dashed", color = "red", size=1)+
        scale_fill_manual(values = c("#d80b8c","#00aeef")) +
        scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.2), expand = c(0, 0))+
        facet_grid(New.PT3~., scales = "free")+
        labs(x=NULL, y="Days",
             title = "Median New Appointment Lead Days by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+ graph_theme(legend_pos="none") + theme(plot.margin = margin(0,80,0,80),strip.background = element_rect(fill="#dddedd")) + 
        geom_label(data = target, aes(x = length(unique(waitTime$Campus.Specialty))/2, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)
      
      
    }else{ # Median Wait Time 
      ggplot(waitTime, aes(reorder(Campus.Specialty, -avgWaitTime),avgWaitTime, fill=New.PT3)) +
        geom_bar(stat="identity", width = 0.8) +
        geom_hline(data=target, aes(yintercept=target), linetype="dashed", color = "red", size=1)+
        scale_fill_manual(values = c("#d80b8c","#00aeef")) +
        scale_y_continuous(limits=c(0,max(waitTime$avgWaitTime)*1.2), expand = c(0, 0))+
        facet_grid(New.PT3~., scales = "free")+
        labs(x=NULL, y="Days",
             title = "Avg New Appointment Lead Days by Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw() + graph_theme(legend_pos="none") + theme(plot.margin = margin(0,80,0,80),    strip.background = element_rect(fill="#dddedd")) +
        geom_label(data = target, aes(x = length(unique(waitTime$Campus.Specialty))/2, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)
      
    }
    
  })
  
  output$siteWorkingFTE <- renderPlot({
    
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
        theme_bw()+ graph_theme(legend_pos = "bottom") +
        theme(
          legend.title = element_blank(),
          strip.background = element_rect(fill="#dddedd"),
          plot.margin = margin(0,80,0,80))+
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
    }
    
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
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))+
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
      
    }
    
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
             title = "Median New Appointment Lead Days by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))+
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
      
    }
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
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Avg No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))+
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
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="No Show Rate (%)",
             title = "Avg No Show (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
    }
    
  })
  
  
  output$siteComparisonBookedRate <- renderPlot({
    
    # Slot Data
    slotData <- dataPastSlot()
    # slotData <- past.slot.data %>% filter(Campus == "MSUS")
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
      
      ggplot(bookedFilledRate, aes(Appt.Week, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_grid(variable~.)+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        # labs(x="Site - Specialty", y=NULL,
        #      title = "Avg Daily Booked vs. Filled Rate (%) by Site and Specialty",
        #      subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))+
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
      
      ggplot(bookedFilledRate, aes(Appt.MonthYear, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_grid(variable~.)+
        scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y=NULL,
             title = "Avg Daily Booked vs. Filled Rate (%) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
      
    }
    
  })
  
  
  output$siteComparisonCycleTime <- renderPlot({
    
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
      
      cycleTimes <- merge(cycleTimes, roomedTime, all.x = TRUE)
      cycleTimes <- cycleTimes %>% gather(variable, value, 3:4)
      
      ggplot(cycleTimes, aes(Appt.Week, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_grid(variable~., scales = "free")+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Cycle Times (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))+
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week", expand = c(0,0.2))
      
    } else{
      
      # Check in to Visit End 
      cycleTimes <- cycleTimes.tb %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Median Cycle Time` = round(median(cycleTime)))
      
      # Check in to Room in  
      roomedTime <- roomedTime.tb %>% 
        group_by(siteSpecialty, Appt.MonthYear) %>% dplyr::summarise(`Median Check-in to Room-in Time` = round(median(checkinToRoomin)))
      
      cycleTimes <- merge(cycleTimes, roomedTime, all.x = TRUE)
      cycleTimes <- cycleTimes %>% gather(variable, value, 3:4)
      
      ggplot(cycleTimes, aes(Appt.MonthYear, value, group=siteSpecialty, col=siteSpecialty)) +
        geom_line()+
        geom_point(size=2)+
        facet_grid(variable~., scales = "free")+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x="Site - Specialty", y="Min.",
             title = "Median Cycle Times (Min.) by Site and Specialty",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title = element_text(size = 12),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank(),
          plot.margin = margin(0,80,0,80))
    }
  })
  
  
  output$siteComparisonWorkingFTE <- renderPlot({
    
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())+
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    }
    
    
  })
  
  output$siteComparisonPtsPerFTE <- renderPlot({
    
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())+
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
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    }
    
  })
  
  # Practice Overview Tab ----------------------------------------------------------------------------------------------
  output$practiceName_profile <- renderText({
    paste0(input$selectedDepartment," from ",input$dateRange[1]," to ", input$dateRange[2])
    
  })
  
  # KPIs Tab
  output$practiceName_KPIs <- renderText({
    paste0(input$selectedDepartment," from ",input$dateRange[1]," to ", input$dateRange[2])
    
  })
  
  # Utilization Tab
  output$practiceName_utilization <- renderText({
    paste0(input$selectedDepartment," from ",input$dateRange[1]," to ", input$dateRange[2])
    
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
    
    avgPtsByHour <- reshape::melt(avgPtsByHour, id="Time", measure = c("Scheduled","Arrived"))
    
    avgPtsByHour <- avgPtsByHour %>% filter(Time %in% timeOptionsHr_filter)
    
    # Scheduled vs. Actual Arrival in Hour Interval 
    
    ggplot(avgPtsByHour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(aes(linetype=variable), size=1.2)+
      scale_linetype_manual(values=c("dashed","solid"))+
      scale_color_manual(values=c("maroon1","midnightblue"))+
      labs(x=NULL, y=NULL,
           title = "Average Scheduled vs. Arrived Patients",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "Scheduled = Arrived + No Show Patients")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 35,hjust = 1),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
  })
  
  ### Access Section
  
  output$newPtRatio <- renderInfoBox({
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
  
  output$fillRate <- renderPlot({
    
    data <- dataPastSlot()
    # data <- past.slot.data
    
    daily.booked <- data %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                       `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                       `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                       `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
      mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
             `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
      gather(variable, value, 1:7) %>%
      filter(variable %in% c("Booked Rate","Filled Rate"))
    
    
    ggplot(daily.booked %>% filter(variable %in% c("Booked Rate","Filled Rate")), aes(factor(variable,levels=c("Filled Rate","Booked Rate")),value, fill=variable)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,max(daily.booked$value)*1.5))+
      labs(subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      coord_flip() +
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      theme_new_line()+
      theme(
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_text(size =14),
        axis.text.x = element_blank(),
        axis.line.x = element_blank()) +
      geom_text(aes(label=paste0(round(value*100),"%")), vjust=-.1, hjust=-.3, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  output$apptStatus <- renderPlot({
    
    apptsCanceled <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status), FUN=NROW)
    names(apptsCanceled) <- c("Status","Total")
    
    apptsCanceled <- apptsCanceled %>% filter(Status %in% c("Rescheduled","Canceled","No Show","Bumped","Scheduled","Arrived")) %>%
      mutate(Percent = as.numeric(round((Total / sum(Total)),2)))
    
    apptsCanceled$percent[apptsCanceled$Status == "No Show"] <- round((apptsCanceled$Total[apptsCanceled$Status == "No Show"] / (apptsCanceled$Total[apptsCanceled$Status == "Arrived"] + apptsCanceled$Total[apptsCanceled$Status == "No Show"])),2)
    
    
    ggplot(apptsCanceled, aes(reorder(Status, Percent),Percent, fill=Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$Percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(caption = paste0("% No Show = No Show / No Show and Arrived\n",
                            "Based on breakdown of all appintments by status"))+
      coord_flip()+
      theme_new_line()+
      theme(
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size =14),
        axis.line.x = element_blank())+
      geom_text(aes(label=paste0(round(Percent*100),"%")), hjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
    
  })
  
  ### Day of Visit Section
  
  output$avgCycleTime <- renderValueBox({
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
  ## Total Patients Seen
  output$vbox <- renderValueBox(
    
    valueBoxSpark(
      value =  paste0(prettyNum(length(unique(dataArrived()$uniqueId)), big.mark=',')," Patients"),
      title = toupper("Total Patients Seen"),
      sparkobj = 
        hchart(dataArrived() %>%
                 filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(total = n()), "area", hcaes(Appt.DateYear, total), name = "Total Patients: ")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Total number of patients seen over time.",
      icon = icon("users"),
      subtitle = NULL,
      width = 4,
      color = "yellow",
      href = NULL
      
    )
  )
  
  # Total New Patients Seen
  output$vbox2 <- renderValueBox(
    
    valueBoxSpark(
      value = paste0(prettyNum(nrow(dataArrived() %>% filter(New.PT == "Y") %>% distinct(uniqueId)), big.mark=',')," Patients"),
      title = toupper("Total New Patients Seen"),
      sparkobj = 
        hchart(dataArrived() %>%
                 filter(New.PT3 == TRUE, Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(total = n()), "area", hcaes(Appt.DateYear, total), name = "Total New Patients: ")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Total and average daily number of new patients seen.",
      icon = icon("users"),
      subtitle = NULL,
      width = 4,
      color = "yellow",
      href = NULL
    )
  )
  
  # Median New Appt Lead Days
  output$vbox3 <- renderValueBox(
    valueBoxSpark(
      value = paste0(prettyNum(round(median((dataAll() %>% filter(New.PT3 == TRUE, Wait.Time >= 0))$Wait.Time)), big.mark=',')," Days"),
      title = toupper("Median New Appt Lead Days"),
      sparkobj = 
        hchart(dataAll() %>%
                 filter(Appt.DateYear >= max(Appt.DateYear) - 7, Wait.Time >= 0) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(avg = round(median(Wait.Time),1)), "line", hcaes(Appt.DateYear, avg), name = "Median Lead Days:")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Median lead time from appointment made to scheduled appointment date.",
      icon = icon("clock"),
      subtitle = NULL,
      width = 4,
      color = "yellow",
      href = NULL
    )
  )
  
  ## Average Patients Seen
  output$vbox4 <- renderValueBox(
    
    valueBoxSpark(
      value =  paste0(prettyNum(round(length(unique(dataArrived()$uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark=',')," Patients/Day"),
      title = toupper("Average Patients Seen"),
      sparkobj = 
        hchart(dataArrived() %>%
                 filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(total = n()), "area", hcaes(Appt.DateYear, total), name = "Avg Patients: ")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Average daily number of patients seen over time.",
      icon = icon("users"),
      subtitle = NULL,
      width = 4,
      color = "fuchsia",
      href = NULL
      
    )
  )
  
  # Average New Patients Seen
  output$vbox5 <- renderValueBox(
    
    # data <- arrived.data %>% filter(Campus == "MSUS")
    # paste0(prettyNum(round(nrow(data %>% filter(New.PT3 == TRUE) %>% distinct(uniqueId))/length(unique(data$Appt.DateYear))), big.mark=',')," Patients/Day")
    # 
    valueBoxSpark(
      value = paste0(prettyNum(round(nrow(dataArrived() %>% filter(New.PT3 == TRUE) %>% distinct(uniqueId))/length(unique(dataArrived()$Appt.DateYear))), big.mark=',')," Patients/Day"),
      title = toupper("Avg New Patients Seen"),
      sparkobj = 
        hchart(dataArrived() %>%
                 filter(New.PT3 == TRUE, Appt.DateYear >= max(Appt.DateYear)) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(total = n()), "area", hcaes(Appt.DateYear, total), name = "Avg New Patients: ")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Average daily number of new patients seen.",
      icon = icon("users"),
      subtitle = NULL,
      width = 4,
      color = "fuchsia",
      href = NULL
    )
  )
  
  # Average New Appt Lead Days
  output$vbox6 <- renderValueBox(
    valueBoxSpark(
      value = paste0(prettyNum(round(mean((dataAll() %>% filter(New.PT3 == TRUE, Wait.Time >= 0))$Wait.Time)), big.mark=',')," Days"),
      title = toupper("Avg New Appt Lead Days"),
      sparkobj = 
        hchart(dataAll() %>%
                 filter(New.PT3 == TRUE, Appt.DateYear >= max(Appt.DateYear) - 7, Wait.Time >= 0) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(avg = round(mean(Wait.Time),1)), "line", hcaes(Appt.DateYear, avg), name = "Avg. Lead Days:")  %>% 
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Average lead time from appointment made to scheduled appointment date.",
      icon = icon("clock"),
      subtitle = NULL,
      width = 4,
      color = "fuchsia",
      href = NULL
    )
  )
  
  output$vbox7 <- renderValueBox(
    valueBoxSpark(
      value = paste0(prettyNum(round((sum(dataPastSlot()$AVAIL_MINUTES)/60)), big.mark=','), " Hrs."),
      title = toupper("Total Hrs Available"),
      sparkobj = 
        hchart(dataPastSlot() %>%
                 # filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(total = round(sum(AVAIL_MINUTES)/60,1)), "column", hcaes(Appt.DateYear, total), name = "Total Hours:") %>%
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Total scheduling hours available per provider.",
      icon = icon("clock"),
      subtitle = NULL,
      width = 4,
      color = "aqua",
      href = NULL
    )
  )
  
  output$vbox8 <- renderValueBox(
    valueBoxSpark(
      value = paste0(prettyNum(round((sum(dataPastSlot()$AVAIL_MINUTES) / length(unique(dataPastSlot()$Appt.DateYear)))/60), big.mark=','), " Hrs."),
      title = toupper("Avg Daily Hrs Available"),
      sparkobj = 
        hchart(dataPastSlot() %>%
                 # filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                 group_by(Appt.DateYear) %>%
                 dplyr::summarise(avg = round(mean(AVAIL_MINUTES)/60,1)), "column", hcaes(Appt.DateYear, avg), name = "Avg. Hours:") %>%
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Average daily scheduling hours available per provider.",
      icon = icon("clock"),
      subtitle = NULL,
      width = 4,
      color = "aqua",
      href = NULL
    )
  )
  
  output$vbox9 <- renderValueBox(
    valueBoxSpark(
      value = paste0(round((sum(dataPastSlot()$AVAIL_MINUTES) / length(unique(dataPastSlot()$Appt.DateYear)))/
                             (length(unique(dataArrived()$uniqueId)) / length(unique(dataArrived()$Appt.DateYear))), 1), " Min/Patient"),
      title = toupper("Avg Worked Mins per Patient"),
      sparkobj = 
        hchart(merge(dataPastSlot() %>%
                       # filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                       group_by(Appt.DateYear) %>%
                       dplyr::summarise(total = round(sum(AVAIL_MINUTES)/60,1)), 
                     dataArrived() %>%
                       # filter(Appt.DateYear >= max(Appt.DateYear) - 7) %>%
                       group_by(Appt.DateYear) %>%
                       dplyr::summarise(pts = n())) %>% 
                 mutate(avg = round(total/pts,1)), "column", hcaes(Appt.DateYear, avg), name = "Min per Patient:") %>%
        hc_size(height = 80) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()),
      info = "Calculated worked minutes or minutes spent per patient (total scheduling hours available / total patients seen).",
      icon = icon("clock"),
      subtitle = NULL,
      width = 4,
      color = "aqua",
      href = NULL
    )
  )
  
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
    
    apptsCanceled <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status), FUN=NROW)
    names(apptsCanceled) <- c("Status","Total")
    
    apptsCanceled <- apptsCanceled %>% filter(Status %in% c("Rescheduled","Canceled","No Show","Bumped","Scheduled","Arrived")) %>%
      mutate(Percent = as.numeric(round((Total / sum(Total)),2)))
    
    apptsCanceled$percent[apptsCanceled$Status == "No Show"] <- round((apptsCanceled$Total[apptsCanceled$Status == "No Show"] / (apptsCanceled$Total[apptsCanceled$Status == "Arrived"] + apptsCanceled$Total[apptsCanceled$Status == "No Show"])),2)
    
    
    ggplot(apptsCanceled, aes(reorder(Status, Percent),Percent, fill=Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$Percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL, title = NULL,
           plot.title = "Appointment Status Breakdown",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "% No Show = No Show / No Show and Arrived\n
           Based breakdown of all appointments by appointment status")+
      coord_flip()+
      theme_new_line()+
      theme(
        plot.subtitle = element_text(hjust=0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = "16"),
        axis.line.x = element_blank())+
      geom_text(aes(label=paste0(round(Percent*100),"%")), hjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)

  })
  
  output$provCoveragePie <- renderPlot({
    
    major <- dataArrivedNoShow() %>% 
      group_by(Coverage) %>%
      dplyr::summarise(Total = n()) %>% 
      arrange(desc(Total)) %>% 
      mutate(percent = round((Total / sum(Total))*100)) %>%
      filter(percent >= 5) # Filter out coverages
    
    noShow <- dataArrivedNoShow() %>% 
      group_by(Coverage, Appt.Status) %>%
      dplyr::summarise(Total = n()) %>%
      spread(Appt.Status, Total) %>%
      mutate(noShow = round((`No Show`/ (Arrived + `No Show`))*100))
    
    major$noShow <- noShow$noShow[match(major$Coverage, noShow$Coverage)]
    major$Coverage[is.na(major$Coverage)] <- "Unknown"
    
    ggplot(major, aes(area = percent, fill = noShow, label = paste0(Coverage,"\n(",percent,"%, ",noShow,"%)"))) +
      geom_treemap() +
      geom_treemap_text(grow = F, reflow = T, colour = "black", fontface = "bold",
                        place = "centre", family = "Calibri", size = 16) +
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "grey50", name = "% No Show") +
      coord_flip()+
      labs(x = NULL, y = NULL, 
           title = "% Coverage and % No Show",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "% No Show = No Show / No Show and Arrived") +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(1,"cm"),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
  })
  
  output$provSlotUsagesAvg <- renderPlot({
    
    data <- dataPastSlot()
    # data <- past.slot.data %>% filter(Campus == "MSUS")
    
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
      # geom_hline(yintercept=dept.booked.avg, linetype="dashed", color = "#212070", size=1)+
      # geom_hline(yintercept=dept.filled.avg, linetype="dashed", color = "#d80b8c", size=1)+
      coord_flip() +
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(slot.usage$value)*1.2)) +
      labs(x = NULL, y = NULL,
           title = "Average Booked vs. Filled Rate",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      scale_fill_manual(values = c("#212070","#d80b8c","#dddedd")) +
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_text(color= "black", margin = margin(r=15)),
            axis.text = element_text(size="14"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.margin = margin(30,30,30,30)) +
      geom_text(aes(label= paste0(value*100,"%")), hjust=-0.5, color="black", size=6) 
    # annotate(geom="label",x=.7,y=dept.booked.avg,label="Avg. Dept\nBooked Rate", fill = "white", color = "#212070", size=5) +
    # annotate(geom="label",x=1.3,y=dept.filled.avg,label="Avg. Dept\nFilled Rate", fill = "white", color = "#d80b8c", size=5)
    
  })
  
  
  
  ### KPIs Tab ------------------------------------------------------------------------------------------------------------------------
  # Volume KPI ====================================================================
  output$kpiVolumeGraph <- renderPlot({
    kpiVolumeData <- aggregate(dataArrivedKpi()$uniqueId, by=list(dataArrivedKpi()$Appt.Year,dataArrivedKpi()$Appt.Quarter,
                                                                  dataArrivedKpi()$Appt.Month, dataArrivedKpi()$Appt.Date, dataArrivedKpi()$Appt.MonthYear, dataArrivedKpi()$Appt.DateYear), FUN=NROW)
    
    # kpiVolumeData <- aggregate(arrived.data$uniqueId, by=list(arrived.data$Appt.Year,arrived.data$Appt.Quarter,
    #                                                          arrived.data$Appt.Month, arrived.data$Appt.Date, arrived.data$Appt.MonthYear, arrived.data$Appt.DateYear), FUN=NROW)
    # 
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
          labs(x = NULL, y = NULL,  
               title = "Historical Trend of Patient Volume by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
        
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(kpiVolumeDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL,  
               title = "Historical Trend of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(kpiVolumeDataMonth, aes(x=YearMonth, y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          geom_smooth(method='lm', col = "red", se=FALSE, size=0.5) +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Patient Volume by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else { # Day
        ggplot(kpiVolumeData, aes(x=DateYear, y=Volume,group=1)) +
          geom_line(color="midnightblue") +
          geom_smooth(method='lm', col = "red", se=FALSE, size=0.5)+
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Patient Volume by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
      
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(kpiVolumeDataYear %>% mutate(Label = "Year"), aes(x=Label, y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL,  
               title = "Comparison of Patient Volume by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(kpiVolumeDataQuarter, aes(x=Quarter, y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Comparison of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(kpiVolumeDataMonth, aes(x = factor(x=Month, level= monthOptions), y=Total, col=Year,group=Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Comparison of Patient Volume by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(kpiVolumeData, aes(x=as.Date(Date,"%m-%d"), y=Volume, col=Year,group=Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Comparison of Patient Volume by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")+
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  # Appt Status KPI ========================================================================
  output$kpiApptStatusGraph <- renderPlot({
    
    statusData <- aggregate(dataAllKpi()$uniqueId, by=list(dataAllKpi()$Appt.Year,dataAllKpi()$Appt.Quarter,
                                                           dataAllKpi()$Appt.Month, dataAllKpi()$Appt.Date,
                                                           dataAllKpi()$Appt.Status, dataAllKpi()$Appt.MonthYear, dataAllKpi()$Appt.DateYear), FUN=NROW)
    
    colnames(statusData) <- c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count")
    
    # statusData <- aggregate(all.data$uniqueId, by=list(all.data$Appt.Year,all.data$Appt.Quarter,
    #                                                           all.data$Appt.Month, all.data$Appt.Date,
    #                                                           all.data$Appt.Status,  all.data$Appt.MonthYear,  all.data$Appt.DateYear), FUN=NROW)
    # 
    # colnames(statusData) <- c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count")
    
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
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "none",
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(statusDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "none",
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=YearMonth, y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "none",
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_color_MountSinai("main")
        
      } else { # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%Y-%m-%d"), y=value, col=variable, group=variable)) +
          geom_line() +
          labs(x = NULL, y = NULL,  
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "none",
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
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
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "top",
            legend.title = element_blank(),
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter
        ggplot(statusDataQuarter, aes(x=Quarter, y=value, col=Year,group=Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "top",
            legend.title = element_blank(),
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=factor(Month, level = monthOptions), y=value, col=Year,group=Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "top",
            legend.title = element_blank(),
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%Y-%m-%d"), y=value, col=Year,group=Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Historical Trend of Scheduling Status by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
          facet_grid(variable~., scales = "free")+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(fill="#b2b3b2"),
            legend.position = "top",
            legend.title = element_blank(),
            panel.spacing = unit(1, "lines"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))
        
      }
    }
    
  })
  
  
  # Access KPI ========================================================================
  ## Avg New Wait Time
  output$kpiNewWaitTimeGraph <- renderPlot({
    
    data <- dataAllKpi() 
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    data <- data %>% filter(New.PT3 == TRUE) %>% filter(wait.time >= 0)
    
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))), aes(x=Appt.Year, y=mean, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Month,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else { # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))), aes(x=interaction(Appt.Year,as.Date(Appt.Date, format="%Y-%m-%d"),lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = NULL,  
               title = "Average New Appointment Lead Days by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
                 mutate(Label = "Year"), aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
                 mutate(Label = "Quarter"), aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
                 mutate(Label = "Month"), aes(x=Appt.Month, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL,  
               title = "Average New Appointment Lead Days by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(wait.time, na.rm=TRUE))) %>% 
                 mutate(Label = "Date"), aes(x=as.Date(Appt.Date, format = "%m/%d/%y"), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Average New Appointment Lead Days by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  
  # Day of Visit KPI ========================================================================
  ## Avg Check-in to Visit-end
  output$kpiCycleTimeGraph <- renderPlot({
    
    data <- dataArrivedKpi() %>% filter(cycleTime > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))), aes(x=Appt.Year, y=mean, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Visit-End Time (Min.) by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Visit-End Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Month,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Visit-End Time (Min.) by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else { # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))), aes(x=interaction(Appt.Year,as.Date(Appt.Date, format="%Y-%m-%d"),lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = NULL,  
               title = "Average Check-in to Visit-End Time (Min.) by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
                 mutate(Label = "Year"), aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL,  
               title = "Average Check-in to Visit-End Time (Min.) by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
                 mutate(Label = "Quarter"), aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Visit-End Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
                 mutate(Label = "Month"), aes(x=Appt.Month, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL,
               title = "Average Check-in to Visit-End Time (Min.) by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(cycleTime, na.rm=TRUE))) %>% 
                 mutate(Label = "Date"), aes(x=as.Date(Appt.Date, format = "%m/%d/%y"), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Visit-End Time (Min.) by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  
  ## Check-in to Room-in Wait Time
  output$kpiWaitTimeGraph <- renderPlot({
    
    data <- dataArrivedKpi() %>% filter(checkinToRoomin > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))), aes(x=Appt.Year, y=mean, group=1)) +
          #stat_summary(fun.y="mean", geom="line")+
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
        
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))), aes(x=interaction(Appt.Year,Appt.Month,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        
      } else { # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))), aes(x=interaction(Appt.Year,as.Date(Appt.Date, format="%Y-%m-%d"),lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(data %>% group_by(Appt.Year) %>% dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
                 mutate(Label = "Year"), aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Year",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(data %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
                 mutate(Label = "Quarter"), aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Quarter",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(data %>% group_by(Appt.Year, Appt.Month) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
                 mutate(Label = "Month"), aes(x=Appt.Month, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          geom_point() +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Month",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_color_MountSinai("main")
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(data %>% group_by(Appt.Year, Appt.Date) %>% 
                 dplyr::summarise(mean = round(mean(checkinToRoomin, na.rm=TRUE))) %>% 
                 mutate(Label = "Date"), aes(x=as.Date(Appt.Date, format = "%m/%d/%y"), y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line() +
          labs(x = NULL, y = NULL, 
               title = "Average Check-in to Room-in Time (Min.) by Day",
               subtitle = paste0("Based on data from ",input$dateRangeKpi[1]," to ",input$dateRangeKpi[2]))+
          theme_new_line()+
          theme_bw()+
          theme(
            plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", hjust=1, angle = 35),
            axis.text.y = element_text(size = "14"))+
          scale_color_MountSinai("main")
        # scale_x_date(breaks = "day", date_labels = "%m/%d/%y", date_breaks = "1 week", 
        #              date_minor_breaks = "1 day", expand = c(0, 0.6))
      }
    }
    
  })
  
  
  ### Scheduling Tab -------------------------------------------------------------------------------------------------------------------
  
  # Scheduled Patients
  output$scheduledPts <- renderPlot({
    
    data <- dataArrivedNoShow()
    # data <- arrivedNoShow.data
    
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
    data <- melt(data, id=c("Time"))
    
    data$variable <- as.character(data$variable)
    
    data$variable[which(data$variable == "Bumped")] <- "Same-day Bumped"
    data$variable[which(data$variable == "Canceled")] <- "Same-day Canceled"
    data$variable[which(data$variable == "Rescheduled")] <- "Same-day Rescheduled"
    
    data <- data %>% filter(Time %in% timeOptionsHr_filter)
    
    #data <- data %>%
    # arrange(desc(variable))
    
    ggplot(data, aes(x=Time, y=value, fill=factor(variable, levels=c("Same-day Bumped", "Same-day Canceled", "Same-day Rescheduled", "No Show","Arrived"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_MountSinai(reverse = TRUE)+
      ggtitle(label="Average Patients Arrived",
              subtitle = paste0("Based on scheduled appointment time\n",
                                "Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      scale_y_continuous(limits=c(0,(max(data$value))*2))+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 35, hjust =1),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=ifelse(value < max(value)*0.1," ",value)), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun.y = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Time), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  # Arrived Patients 
  output$arrivedPts <- renderPlot({
    
    arrived <- dataArrived() %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(total = n()) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      dplyr::summarise(avg = round(mean(total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(arrived$Day)),]
    
    arrived <- as.data.frame(merge(byDayTime.df,arrived, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))
    arrived[is.na(arrived)] <- 0
    
    arrived <- arrived %>% filter(Time %in% timeOptionsHr_filter)
    
    ggplot(arrived, aes(x=Time, y=avg, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      #scale_color_manual(values=c("deepskyblue","maroon1","midnightblue"))+
      ggtitle(label="Average Patients Arrived",
              subtitle = "Based on scheduled appointment time")+
      scale_color_MountSinai("main")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 35,hjust = 1),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))
    
  })
  
  # Reactive Filters for Scheduling Tab: Appointment Type & Insurance 
  output$apptTypeControl <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType", label=h4("Select Appointment Type:"),
                  choices = sort(unique(historical.data[
                    historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Appt.Type"])),
                  # choices=sort(unique(dataAll()$Appt.Type)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    dropupAuto = FALSE),
                  selected = unique(dataAll()$Appt.Type)))
  })
  
  output$insuranceControl <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedInsurance", label=h4("Select Insurance Type:"),
                  choices = sort(unique(historical.data[
                    historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Coverage"])),
                  # choices=sort(unique(dataAll()$Coverage)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    dropupAuto = FALSE),
                  selected = unique(dataAll()$Coverage)))
  })
  
  # Arrived No Show Data with Additional Filteres (Appointment Type and Insurance)
  dataArrivedNoShow_1 <- reactive({
    groupByFilters_1(dataArrivedNoShow(),
                     input$selectedApptType, input$selectedInsurance)
  })
  
  dataNoShow_1 <- reactive({
    groupByFilters_1(dataNoShow(),
                     input$selectedApptType, input$selectedInsurance)
  })
  
  # Total No Shows per Day
  output$avgDailyNoShow_Count <- renderValueBox({
    
    valueBox(
      round(nrow(dataNoShow_1()) / length(unique(dataArrivedNoShow_1()$Appt.DateYear)),0),
      subtitle = tags$p("No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # % No Shows per Day
  output$avgDailyNoShow_Perc <- renderValueBox({
    
    valueBox(
      paste0(round((nrow(dataNoShow_1()) / nrow(dataArrivedNoShow_1()))*100,1), " %"),
      subtitle = tags$p("% No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Distribution of No Shows (%) by Lead Days 
  output$noShowLeadDays <- renderPlot({
    
    data <- dataArrivedNoShow_1()
    # data <- arrivedNoShow.data
    
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
    
    noShows_box <- 
      ggplot(noShows, aes(x = factor(apptLeadDays, levels = status), y = noShow_perc)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,max(noShows$noShow_perc)*1.2))+
      labs(x=NULL, y=NULL,
           caption = "No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")+
      ggtitle("Distribution of No Show Rate by Lead Days to Appointment")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size=12, face="italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
    
    
    noShows_bar_tb <-
      noShows %>%
      group_by(apptLeadDays) %>%
      dplyr::summarise(Arrived = sum(Arrived),
                       `No Show` = sum(`No Show`)) %>%
      mutate(Average = round(`No Show`/(Arrived + `No Show`),2)) %>%
      select(apptLeadDays, Average)
    
    noShows_bar_tb <-
      melt(noShows_bar_tb, id.vars = c("apptLeadDays"))
    
    noShows_bar <-
      ggplot(noShows_bar_tb, aes(x=factor(apptLeadDays, levels = status), y=value,fill=variable)) +
      geom_bar(stat="identity", position=position_dodge(), width = 0.8, fill="midnightblue") +
      labs(x=NULL, y=NULL,
           caption = "No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")+
      ggtitle("Average No Show Rate by Lead Days to Appointment")+
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(noShows_bar_tb$value)*1.2))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size=12, face="italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    geom_text(aes(label=paste0(value*100,"%")), vjust = -1, hjust = .5, color="black", fontface="bold",
              position = position_dodge(1), size=5)
    
    grid.arrange(noShows_bar, noShows_box, ncol = 2)
    
  })
  
  
  
  # No Shows by Time of Day 
  output$avgNoShowCount <- renderPlot({
    
    data <- dataArrivedNoShow_1()
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
           caption = "No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")+
      geom_tile(aes(fill=avgNoShows), colour = "black", size=0.5)+
      ggtitle("Average Daily No Shows")+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Shows ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_count.df$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
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
    
    data <- dataArrivedNoShow_1()
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
           caption = "No Show includes no show and same-day bumped, canceled, and rescheduled appointments.")+
      geom_tile(aes(fill=percentage), colour = "black", size=0.5)+
      ggtitle("Average No Show %")+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "#dddedd", guide = "colourbar", name="No Show % ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_perc.df$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
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
  
  # Average Bumped Appointments 
  output$totalBumpedBox<- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(nrow(data %>% filter(Appt.Status == "Bumped")),big.mark=","), 
      subtitle = tags$p("Total Bumped Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Average Canceled Appointments 
  output$totalCanceledBox<- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(nrow(data %>% filter(Appt.Status == "Canceled")),big.mark=","), 
      subtitle = tags$p("Total Canceled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Average Daily Rescheduled Appointments 
  output$totalRescheduledBox<- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(nrow(data %>% filter(Appt.Status == "Rescheduled")),big.mark=","), 
      subtitle = tags$p("Total Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyBumpedCanceledRescheduledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data)/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Bumped/Canceled/Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "fuchsia"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyBumpedBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Bumped"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Bumped Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "fuchsia"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyCanceledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Canceled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Canceled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "fuchsia"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyRescheduledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
    valueBox(
      prettyNum(round(nrow(data %>% filter(Appt.Status == "Rescheduled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      subtitle = tags$p("Avg Daily Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "fuchsia"
    )
  })
  
  output$sameDayBumpedCanceledRescheduled <- renderPlot({
    
    data <- dataNoShow() %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    # data <- noShow.data %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    
    sameDay <- data %>%
      group_by(Appt.Status) %>%
      summarise(total = n())
    
    sameDay$avg <- round(sameDay$total / length(unique(dataAll()$Appt.DateYear)),1)
    
    ggplot(sameDay, aes(x = "", y = avg, fill = Appt.Status))+
      geom_bar(stat = "identity", color = "white")+
      scale_fill_MountSinai('blue')+
      ggtitle(label="Avg Daily Appointments by Status\n")+
      geom_text(aes(x = 1.6, label = paste0(Appt.Status,"\n",avg), size=10, fontface="bold"), position = position_stack(vjust = .5))+
      coord_polar("y")+
      theme_void()+
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            plot.subtitle = element_text(size = 15, face = "italic",hjust=0.5),
            legend.position = "none",
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_blank())
    
  })
  
  
  # Appt Status by Lead Days
  output$bumpedCanceledRescheduledLeadDays <- renderPlot({
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data
    
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
    
    ggplot(lead.days.df, aes(x=Appt.Status, y=perc, fill=factor(leadDays, levels=c("> 14 days","8-14 days","1-7 days","0 day"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("grey","#00aeef","#d80b8c","midnightblue"))+
      ggtitle(label="Status by Lead Days to Appointment\n")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=paste0(round(perc*100),"%")), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))
    
  })
  
  
  # Canceled Reasons by Lead Days
  output$canceledReasonsLeadDays <- renderPlot({
    
    #### TEMPORARY DATASET #### 
    # canceled.data <- historical.data %>% filter(Appt.Status %in% c("Canceled")) 
    
    canceled.data <- dataCanceled()
    total <- nrow(canceled.data)
    
    cancellations <- 
      canceled.data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    cancellations$percent <- round(cancellations$total/total,2)
    
    top10 <- cancellations %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    # Create the heatmap
    ggplot(cancellations %>% filter(Cancel.Reason %in% top10), 
           aes(Cancel.Reason, factor(leadDays, levels=c("> 14 days","8-14 days","1-7 days","0 day")), fill = percent))+
      geom_tile(color = "grey")+
      ggtitle(label="Canceled Reasons by Lead Days\n")+
      scale_fill_gradient2(low = "#d80b8c", high = "#212070", mid = "white", 
                           midpoint = median(cancellations$percent), limit = c(min(cancellations$percent),max(cancellations$percent)), space = "Lab",
                           name="Percent of\nTotal Canceled\n") + # Change gradient color
      #scale_x_discrete(labels = wrap_format(20))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 40, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 14))+
      coord_fixed()
    
  })
  
  
  
  # Bumped Reasons by Lead Days
  output$bumpedReasonsLeadDays <- renderPlot({
    
    #### TEMPORARY DATASET #### 
    bumped.data <- historical.data %>% filter(Appt.Status %in% c("Bumped")) 
    
    total <- nrow(bumped.data)
    
    bumps <- 
      bumped.data %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays,Cancel.Reason) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    bumps$percent <- round(bumps$total/total,2)
    
    top10 <- bumps %>% 
      group_by(Cancel.Reason) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$Cancel.Reason)
    
    # Create the heatmap
    ggplot(bumps %>% filter(Cancel.Reason %in% top10), 
           aes(Cancel.Reason, factor(leadDays, levels=c("> 14 days","8-14 days","1-7 days","0 day")), fill = percent))+
      geom_tile(color = "grey")+
      ggtitle(label="Bumped Reasons by Lead Days\n")+
      scale_fill_gradient2(low = "#212070", high = "#d80b8c", mid = "white", 
                           midpoint = median(bumps$percent), limit = c(min(bumps$percent),max(bumps$percent)), space = "Lab",
                           name="Percent of\nTotal Bumped\n") + # Change gradient color
      #scale_x_discrete(labels = wrap_format(20))+
      theme_minimal()+ # minimal theme
      theme(plot.title = element_text(face = "bold", size = 20, hjust=0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 40, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 14))+
      coord_fixed()
    
  })
  
  ### Utilization Tab -----------------------------------------------------------------------------------------------------------------
  # Average Rooms Required --------------------------------------------------------------------------------------------
  output$roomStat1 <- renderValueBox({
    
    valueBox(NULL,
             # paste0(input$setRooms," rooms available\n throughout",input$setHours," hours"),
             subtitle = tags$p(paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours"), style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "yellow"
    )
  })
  
  # output$roomStat2 <- renderValueBox({
  #   
  #   valueBox(
  #     paste0("Daily Available Rooms: ",input$setRooms),
  #     subtitle = NULL, icon = NULL, color = "yellow"
  #   )
  # })
  
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
    space.hour.day <- melt(space.hour.day, id=c("Group.1"))
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
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(margin = margin(r=5)),
            axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,10,30))+
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
    space.hour.day <- melt(space.hour.day, id=c("Group.1"))
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
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))
    
    table <- ggplot(space.hour.day, aes(x=factor(Day, levels = rev(daysOfWeek.options)), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=Average_Util), colour = "black", size=0.5)+
      coord_flip()+
      scale_fill_gradient2(midpoint = median(unique(space.hour.day$Average_Util)), low = "#5a8ac6", mid = "white", high = "#f8696b", space = "Lab", na.value = "black", guide = "colourbar", name="Space Utilization ")+
      #scale_y_discrete(limits = unique(sort(space.hour.day$Time)), position = "bottom")+
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
    
    data <- dataArrivedUtilization()
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = round(quantile(value, probs=0.5)/60,1),
        `70th Percentile`= round(quantile(value, probs=0.75)/60,1),
        `90th Percentile`= round(quantile(value, probs=0.90)/60,1))
    
    colnames(space.hour)[1] <- "Time"
    
    space.hour <- as.data.frame(melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(limits=c(0, max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Space Required by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(margin = margin(r=5)),
            axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
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
    
    data <- dataArrivedUtilization()
    
    c.start <- which(colnames(data)=="00:00")
    c.end <- which(colnames(data)=="23:00")
    
    space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    space.hour <- melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = quantile(value, probs=0.5)/(60*input$setRooms),
        `70th Percentile`= quantile(value, probs=0.75)/(60*input$setRooms),
        `90th Percentile`= quantile(value, probs=0.90)/(60*input$setRooms))
    
    colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n", 
           title = "Space Utilization (%) by Percentile by Time of Day",
           subtitle = paste0("Based on scheduled appointment time and duration from ",input$dateRangeUtil[1]," to ",input$dateRangeUtil[2]))+
      scale_color_MountSinai("main")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 14),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
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
  
  
  output$population1 <- renderLeaflet({
    
    data(zipcode)
    
    population.data <- 
      dataArrived()[,c("Campus","Campus.Specialty","Department","MRN","Zip.Code","Sex","Coverage","uniqueId")]
    
    population.data$zip <- clean.zipcodes(population.data$Zip.Code)
    
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
      hc_xAxis(title = list(text = '')) %>%
      hc_yAxis(title = list(text = ''))
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
      scale_x_date(breaks = seq(min(pts.by.month$Month), 
                                max(pts.by.month$Month), by = "1 month"), date_labels = "%b\n%Y")+
      scale_y_continuous(limits=c(0,(max(pts.by.month$Volume))*1.3))+
      ggtitle("Monthly Patient Volume")+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"))+
      geom_text(aes(label=Volume), vjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # Average Daily Patient Volume by Day of Week
  output$volume3 <- renderPlot({
    
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
      ggtitle("Average Daily Patient Volume")+
      scale_y_continuous(limits=c(0,(max(pts.by.day$Avg.Volume))*1.3))+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"))+
      geom_text(aes(label=Avg.Volume), vjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # Daily Volume Distribution by Month
  output$volume4 <- renderPlot({
    
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
      ggtitle("Daily Patient Volume Distribution by Month")+
      scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y")+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"))
    
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
    
    pts.dist <- aggregate(dataArrived()$uniqueId, 
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+ 
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      ggtitle("Daily Patient Volume Distribution by Day of Week")+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"))
    
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
    # data <- arrived.data
    
    newpatients.ratio <- data %>%
      group_by(Appt.DateYear,New.PT3) %>%
      dplyr::summarise(Total = n()) %>%
      spread(New.PT3, Total)
    
    newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
    newpatients.ratio$Appt.DateYear <- as.Date(newpatients.ratio$Appt.DateYear, format="%Y-%m-%d") ## Create date-year column
    
    ggplot(newpatients.ratio, aes(x=Appt.DateYear, y=ratio, group=1)) +
      geom_line(color="midnightblue", size=1) +
      geom_smooth(method='lm', col = "red", se=FALSE, size=0.5)+
      labs(x=NULL, y=NULL,
           title = "New Patient Ratio and Trending over Time",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "% No Show = No Show / No Show and Arrived")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", hjust=1, angle = 35),
        axis.text.y = element_text(size = "14"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
    
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
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        legend.position = "bottom",
        legend.text = element_text(size="12"),
        legend.direction = "horizontal",
        legend.key.size = unit(1.0,"cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", hjust=1, angle = 35),
        axis.text.y = element_text(size = "14"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
    
  })
  
  # New Patient Wait Time
  output$newPtWaitTimeByDept <- renderPlot({
    
    data <- dataAll()
    # data <- all.data %>% filter(Campus == "MSUS")
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0) %>%
      group_by(Appt.DateYear, New.PT3) %>%
      dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
      filter(New.PT3 %in% c("TRUE","FALSE"))
    
    waitTime$New.PT3 <- ifelse(waitTime$New.PT3 == TRUE, "New","Established")
    waitTime$Appt.DateYear <- as.Date(waitTime$Appt.DateYear, format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(New.PT3, medWaitTime) 
    waitTime$`New Patient Target` <- 14
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 2:4)
    
    
    ggplot(waitTime, aes(x=Appt.DateYear, y=value, group=variable)) +
      geom_line(aes(linetype=variable, color=variable, size=variable)) +
      scale_linetype_manual(values=c("solid", "solid", "dashed"))+
      scale_color_manual(values=c('#212070','#7f7f7f','red'))+
      scale_size_manual(values=c(1, 1, 1.3))+
      labs(x=NULL, y=NULL,
           title = "New and Established Appointment Lead Days (Median) Over Time",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size="12"),
        legend.direction = "horizontal",
        legend.key.size = unit(1.0,"cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", hjust=1, angle = 35),
        axis.text.y = element_text(size = "14"))+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
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
           title = "New and Established Appointment Lead Days (Median) Over Time by Provider",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size="12"),
        legend.direction = "horizontal",
        legend.key.size = unit(1.0,"cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", hjust=1, angle = 35),
        axis.text.y = element_text(size = "14"))+
      scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
                   date_minor_breaks = "1 day", expand = c(0, 0.6))
  })
  
  
  # New Patient Wait Time
  output$newPtApptSourceByDept <- renderPlot({
    
    data <- dataArrived()
    # data <- arrived.data
    
    newpatients.ratio <- data %>%
      group_by(Appt.Source.New, New.PT3) %>%
      dplyr::summarise(Total = n()) %>%
      spread(New.PT3, Total)
    
    newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
    
    newRatio <-
      ggplot(newpatients.ratio, aes(x=factor(Appt.Source.New, levels = c("Zocdoc","StayWell","Other","MyChart","Access Center")), y=ratio, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      coord_flip() +
      scale_fill_MountSinai('purple')+
      labs(x=NULL, y=NULL, 
           title = "New Patient Ratio",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "Based on arrived patients")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      geom_text(aes(label=paste(ratio*100,"%")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(wait.time >= 0) %>%
      group_by(Appt.Source.New, New.PT3) %>%
      dplyr::summarise(medWaitTime = round(median(wait.time))) %>%
      filter(New.PT3 == TRUE)
    waitTime$target <- 14
    
    newWaitTime <-
      ggplot(waitTime, aes(x=factor(Appt.Source.New, levels = c("Zocdoc","StayWell","Other","MyChart","Access Center")), y=medWaitTime, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      geom_hline(aes(yintercept=target), linetype="dashed", color = "red", size=1)+
      scale_y_continuous(limits=c(0,max(max(waitTime$medWaitTime)*1.2,14*1.2)))+
      coord_flip() +
      scale_fill_MountSinai('pink')+
      labs(x=NULL, y=NULL, 
           title = "New Appointment Lead Days",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "Based on all of scheduled patients")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
      geom_text(aes(label=paste(medWaitTime," days")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    # No Show Rate
    
    data.noShow <- dataArrivedNoShow()
    # data.noShow <- arrivedNoShow.data
    
    noShows <- data.noShow %>%
      filter(New.PT3 == TRUE) %>%
      group_by(Appt.Source.New, Appt.Status) %>%
      dplyr::summarise(Total = n()) %>%
      spread(Appt.Status, Total)
    
    noShows[is.na(noShows)] <- 0
    
    noShows$`No Show Perc` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)
    
    
    newNoShow <-
      ggplot(noShows, aes(x=factor(Appt.Source.New, levels = c("Zocdoc","StayWell","Other","MyChart","Access Center")), y=`No Show Perc`, group=Appt.Source.New, fill=Appt.Source.New)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,max(noShows$`No Show Perc`))*1.2)+
      coord_flip() +
      scale_fill_MountSinai('blue')+
      labs(x=NULL, y=NULL,
           title = "New Patient No Show Rate",
           subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]),
           caption = "Based on all of scheduled patients")+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      geom_text(aes(label=paste(`No Show Perc`*100,"%")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5)
    
    
    grid.arrange(newRatio, newWaitTime, newNoShow, ncol=3)
    
  })
  
  
  
  
  
  
  
  
  
  
  # Upcoming Demand - 2 Weeks - by Practice and Provider ---------------------------------------------------------
  output$demandWeeksGraph <-  renderPlot({
    
    ### ***TEMPORARY*** ###
    future.slot.data <- past.slot.data # DELETE WITH NEW DATA
    
    summary.hrs <- future.slot.data %>%
      # filter(Appt.DTTM >= todays_date - days(14)) # ONLY INCLUDE FUTURE 2 WEEKS
      group_by(Campus, Department, Provider, Appt.DateYear) %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                       `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                       `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                       `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
      mutate(`Remaining Hours` = ifelse(`Booked Hours`-`Available Hours` < 0,0, round((`Booked Hours`-`Available Hours`),1)),
             `Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100,1),
             `Filled Rate (%)` = round((`Arrived Hours`/`Available Hours`)*100,1)) %>%
      gather(variable, value, 5:12)
    
    # ***Booked or Filled Rate = Inf: appts completed outside of slotted hours*** -> convert to 0 for now
    # ***Booked or Filled Rate = NaN: no available and booked hours*** -> convert to 0 for now
    summary.hrs$value[which(!is.finite(summary.hrs$value))] <- 0
    
    summary.hrs$Date <- format(as.Date(summary.hrs$Date, format="%Y-%m-%d"), "%m/%d/%y")
    summary.hrs <- summary.hrs %>% mutate(unique = paste0(Campus, Department, Provider, Date, variable))
    
    summary.df <- sapply(as.data.frame(unique(summary.hrs[,c("Campus","Department","Provider","variable")])), rep.int, times = length(unique(summary.hrs$Date)))
    summary.df <- summary.df %>% as.data.frame() %>% arrange(Campus, Department, Provider, variable)
    
    full_dates <- data.frame(Date = rep(seq.dates(min(summary.hrs$Date), max(summary.hrs$Date), by = "days"), nrow(unique(summary.df))))
    
    summary.hrs.df <- cbind(summary.df,full_dates)
    summary.hrs.df <- summary.hrs.df %>% mutate(unique = paste0(Campus, Department, Provider, Date, variable))
    summary.hrs.df$value <- summary.hrs$value[match(summary.hrs.df$unique,summary.hrs$unique)]
    
    level.order <- c("Available Hours", "Booked Hours","Arrived Hours","Canceled Hours","No Show Hours","Remaining Hours","Booked Rate (%)","Filled Rate (%)")
    summary.hrs.df <- summary.hrs.df[order(match(summary.hrs.df$variable, level.order)),]
    summary.hrs.df$value[is.na(summary.hrs.df$value)] <- 0
    summary.hrs.df$Date <- as.Date(summary.hrs.df$Date, format="%Y-%m-%d")
    
    
    if(input$byProvider2 == TRUE){ # Provider - Future Filled and Booked Rates
      
      summary.hrs.df <- summary.hrs.df %>% filter(Provider %in% c("AMIN, SHYAM M","GOWDA, RAMESH M","PUSKAS, JOHN D","SHAH, ANKIT R","GOWDA, RAMESH M"))
      
      # if(length(unique(summary.hrs.df$Provider))>6){
      #   
      #   cowplot::ggdraw() +
      #     cowplot::draw_label('Select fewer providers (<5)', fontface = "bold", color = "red", size = 50)
      #   
      # } else {
      
      # Provider - Booked vs.Remaining Hours - 2 Weeks #Provider %in% c("AMIN, SHYAM M","GOWDA, RAMESH M","PUSKAS, JOHN D","SHAH, ANKIT R","GOWDA, RAMESH M")
      #booked.prov <-
      
      booked.prov <- 
        ggplot(summary.hrs.df %>% filter(variable %in% c("Booked Rate (%)")) %>% mutate(value = round(ifelse(value == 0, 0, value*.001),1)), 
               aes(x=Date, y=value, fill= value > 1.0, color=value)) +
        geom_bar(stat="identity", color='black', size=0.5) +
        geom_hline(yintercept=1.0, linetype="dashed", color = "red", size=1)+
        facet_grid(Provider~., scales = "free_y")+
        #facet_wrap(~ Department + Provider, scales="free_y")+
        labs(title = paste0("\nDaily Slot Booked Rate (%) - Upcoming 2 Weeks"), x = NULL, y = NULL)+
        scale_fill_manual("Status", labels = c("<= 100%", "> 100%"), values = c("#c7c6ef","#212070"))+
        scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0,0.2))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              legend.position = "top",
              legend.text = element_text(size="12"),
              legend.direction = "horizontal",
              legend.key.size = unit(1.0,"cm"),
              legend.title = element_blank(),
              axis.title = element_text(size="14"),
              axis.text = element_text(size="14"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
              axis.text.y = element_text(margin = margin(l=5, r=5)),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))
      
      remaining.prov <-
        ggplot(summary.hrs.df %>% filter(variable %in% c("Remaining Hours","Booked Hours")), 
               aes(x=Date, y=value, fill=factor(variable, levels=c("Remaining Hours","Booked Hours")),
                   color = variable =='Remaining Hours')) +
        geom_bar(stat="identity", size=1) +
        facet_grid(Provider~., scales = "free_y")+
        #facet_wrap(~ Department + Provider, scales="free_y")+
        labs(title = paste0("\n Total Booked and Remaining Hours  - Upcoming 2 Weeks"), x = NULL, y = NULL)+
        scale_fill_manual("Status",values = c("#f75dbe","#a5a7a5"))+
        scale_color_manual(values = c(NA, '#d80b8c'), guide=F)+
        scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0,0.2))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              legend.position = "top",
              legend.text = element_text(size="12"),
              legend.direction = "horizontal",
              legend.key.size = unit(1.0,"cm"),
              legend.title = element_blank(),
              axis.title = element_text(size="14"),
              axis.text = element_text(size="14"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
              axis.text.y = element_text(margin = margin(l=5, r=5)),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"))+
        geom_text(aes(label=ifelse(value==0,"",paste0(value," hrs."))), color="white", 
                  size=5, position = position_stack(vjust = 0.5))
      
      grid.arrange(booked.prov, remaining.prov, ncol=2)
      
      # }
      
    } else {
      
      # Practice - Summary of future booked and filled rates 
      remaining.dept.df <- summary.hrs.df %>%
        group_by(Campus, Department, Date, variable) %>%
        dplyr::summarise(value = sum(value))
      
      # Practice - Future Booked vs.Remaining Hours - 2 Weeks 
      booked.dept <-
        ggplot(remaining.dept.df %>% filter(variable %in% c("Booked Rate (%)")) %>% mutate(value = round(ifelse(value == 0, 0, value*.001),1)), 
               aes(x=Date, y=value, fill= value > 1.0, color=value)) +
        geom_bar(stat="identity", color='black', size=1) +
        geom_hline(yintercept=1.0, linetype="dashed", color = "red", size=1.5)+
        labs(title = paste0("Daily Slot Booked Rate (%) - Upcoming 2 Weeks\n"), x = NULL, y = NULL)+
        scale_fill_manual("Status", labels = c("<= 100%", "> 100%"), values = c("#c7c6ef","#212070"))+
        scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0,0.2))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              legend.position = "top",
              axis.title = element_text(size="14"),
              axis.text = element_text(size="14"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
              axis.text.y = element_text(margin = margin(l=5, r=5)),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"),
              plot.margin=unit(c(1,1,0.5,1), "cm"))+
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits=c(0,max(remaining.dept.df$value)*0.0012))+
        geom_text(aes(label=paste(value*100,"%")), color="white", 
                  size=5, position = position_stack(vjust = 0.5))
      
      # Practice - Future Remaining Hours - 2 Weeks 
      remaining.dept <-
        ggplot(remaining.dept.df %>% filter(variable %in% c("Remaining Hours","Booked Hours")), 
               aes(x=Date, y=value, fill=factor(variable, levels=c("Remaining Hours","Booked Hours")),
                   color = variable =='Remaining Hours')) +
        geom_bar(stat="identity", size=1) +
        labs(title = paste0("\n Total Booked and Remaining Hours  - Upcoming 2 Weeks"), x = NULL, y = NULL)+
        scale_fill_manual("Status",values = c("#f75dbe","#a5a7a5"))+
        scale_color_manual(values = c(NA, '#d80b8c'), guide=F)+
        scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0,0.2))+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              legend.position = "top",
              legend.text = element_text(size="12"),
              legend.direction = "horizontal",
              legend.key.size = unit(1.0,"cm"),
              legend.title = element_blank(),
              axis.title = element_text(size="14"),
              axis.text = element_text(size="14"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(hjust=1, angle = 35, margin = margin(t=10)),
              axis.text.y = element_text(margin = margin(l=5, r=5)),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"),
              plot.margin=unit(c(-0.5,1,1,1), "cm"))+
        geom_text(aes(label=paste0(value," hrs.")), color="white", 
                  size=5, position = position_stack(vjust = 0.5))
      
      grid.arrange(booked.dept, remaining.dept, ncol=1)
      
    } # End Booked and Filled Rate by Practice
    
  })
  
  
  
  # Upcoming Demand -  Months - by Practice and Provider ------------------------------------------------
  # Slot Usage Graph by Practice and Provider 
  output$demandMonthsGraph <-  renderPlot({
    
    if(input$byProvider3 == TRUE){ # By Provider
      
      # Future Booked Rate by Month and Day Table
      summary.tb <- dataPastSlot() %>%
        group_by(Campus, Department, Provider, Date) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
        gather(variable, value, 5:11)
      
      summary.tb$Date <- format(as.Date(summary.tb$Date, format="%Y-%m-%d"), "%m/%d/%Y")
      summary.tb$Month <- format(as.Date(summary.tb$Date, format="%m/%d/%Y"), "%m")
      summary.tb$DateD <- format(as.Date(summary.tb$Date, format="%m/%d/%Y"), "%d")
      
      dept <- summary.tb %>% filter(variable == "Booked Rate")
      dept$unique <- paste0(dept$Department, dept$Provider, dept$Month, dept$DateD)
      
      # Dataframe for dates, month, date as number
      DateD <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                 "25", "26", "27", "28", "29", "30", "31")
      DateD <- data.frame(DateD = rep(DateD, length(unique(summary.tb$Month))*nrow(unique(summary.tb[,c("Department","Provider")]))))
      prov <- as.data.frame(unique(summary.tb[,c("Department", "Provider")]))
      final_df <- prov[rep(seq_len(nrow(prov)), length(unique(summary.tb$Month))*31), ]
      final_df <- final_df %>% arrange(Department, Provider)
      Month <- data.frame(Month = rep(unique(summary.tb$Month), nrow(final_df)))
      Month <- Month %>% arrange(Month)
      
      final_df <- cbind(final_df, Month, DateD)
      final_df$unique <-  paste0(final_df$Department, final_df$Provider, final_df$Month, final_df$DateD)
      
      final_df$value <- dept$value[match(final_df$unique, dept$unique)]
      
      is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
      final_df[is.nan(final_df)] <- NA
      
      
      ggplot(final_df, aes(y=Month, x=DateD))+
        geom_tile(aes(fill=value), colour = "black", size=0.5)+
        scale_fill_gradientn("Fill Rate", colors = c("#5a8ac6","white","#f8696b","#ff0000"), breaks = c(0.4,0.6,0.8,1.0))+
        facet_grid(Provider~., switch = "y")+
        coord_fixed(ratio=1/3)+
        labs(title = paste0("\n","Booked Rate (%) by Provider - Upcoming 3 Months\n"), x = "\nDay", y = "Provider\nby Month\n")+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              strip.text.y.left = element_text(angle = 0),
              panel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"),
              strip.placement = "outside")
      
    } else{ # By Practice 
      
      # Future Booked Rate by Month and Day Table
      summary.dept.tb <- dataPastSlot() %>%
        group_by(Campus, Department, Date) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),0),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),0),
                         `Arrived Hours` = round(sum(ARRIVED_MINUTES),0),
                         `Canceled Hours` = round(sum(CANCELED_MINUTES),0),
                         `No Show Hours` = round(sum(NOSHOW_MINUTES , LEFTWOBEINGSEEN_MINUTES),0)) %>%
        mutate(`Booked Rate` = round((`Booked Hours`/`Available Hours`),2),
               `Filled Rate` = round((`Arrived Hours`/`Available Hours`),2)) %>%
        gather(variable, value, 4:10)
      
      summary.dept.tb$Date <- format(as.Date(summary.dept.tb$Date, format="%Y-%m-%d"), "%m/%d/%Y")
      summary.dept.tb$Month <- format(as.Date(summary.dept.tb$Date, format="%m/%d/%Y"), "%m")
      summary.dept.tb$DateD <- format(as.Date(summary.dept.tb$Date, format="%m/%d/%Y"), "%d")
      
      dept <- summary.dept.tb %>% filter(variable == "Booked Rate")
      dept$unique <- paste0(dept$Department, dept$Month, dept$DateD)
      
      # Dataframe for dates, month, date as number
      DateD <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                 "25", "26", "27", "28", "29", "30", "31")
      DateD <- data.frame(DateD = rep(DateD, length(unique(summary.dept.tb$Month))*length(unique(summary.dept.tb$Department))))
      prov <- as.vector(unique(summary.dept.tb$Department))
      final_df <- data.frame(Department = rep(prov, length(unique(summary.dept.tb$Month))*31))
      final_df <- final_df %>% arrange(Department)
      Month <- data.frame(Month = rep(unique(summary.dept.tb$Month), nrow(final_df)))
      Month <- Month %>% arrange(Month)
      
      final_df <- cbind(final_df, Month, DateD)
      final_df$unique <-  paste0(final_df$Department, final_df$Month, final_df$DateD)
      
      final_df$value <- dept$value[match(final_df$unique, dept$unique)]
      
      is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
      final_df[is.nan(final_df)] <- NA
      
      
      ggplot(final_df, aes(y=Month, x=DateD))+
        geom_tile(aes(fill=value), colour = "black", size=0.5)+
        scale_fill_gradientn("Fill Rate", colors = c("#5a8ac6","white","#f8696b","#ff0000"), breaks = c(0.4,0.6,0.8,1.0))+
        facet_grid(Department~., switch = "y")+
        coord_fixed(ratio=1/3)+
        labs(title = paste0("\n","Booked Rate (%) by Practice - Upcoming 3 Months\n"), x = "\nDay", y = "Department\nby Month\n")+
        theme_bw()+
        theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
              plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              strip.text.y.left = element_text(angle = 0),
              #anel.background = element_rect(colour = "black", size=0.5),
              axis.line = element_line(size = 0.3, colour = "black"),
              strip.placement = "outside")
    }
    
  })
  
  
  # Slot Usage Graph by Practice and Provider -----------------------------------------------------------------
  output$slotUsageGraph <-  renderPlot({
    
    data <- dataPastSlot()
    # data <- past.slot.data
    # data <- past.slot.data %>% filter(Campus == "MSUS") %>% filter(Campus.Specialty == "Cardiology")
    # data$siteSpecialtyProvider <- paste0(data$Campus," - ",data$Campus.Specialty," - ",data$Provider)
    
    summary.md <- data %>%
      group_by(Provider, Appt.Week) %>%
      dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES)/60,0),
                       `Booked Hours` = round(sum(BOOKED_MINUTES)/60,0),
                       `Filled Hours` = round(sum(ARRIVED_MINUTES)/60,0),
                       `Canceled Hours` = round(sum(CANCELED_MINUTES)/60,0)) %>%
      mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`),2),
             `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`),2)) 
    
    if(input$slotUsageChoice == 1) { # Available Hours
      
      ggplot(summary.md, aes(x=Appt.Week, y=`Available Hours`, group=Provider, col=Provider)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        # labs(x=NULL, y="Hours\n",
        #      title = "Available Hours by Provider per Week",
        #      subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
    } else if(input$slotUsageChoice == 2) { # Booked Hours
      
      ggplot(summary.md, aes(x=Appt.Week, y=`Booked Hours`, group=Provider, col=Provider)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Hours\n",
             title = "Total Booked Hours by Provider per Week",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    } else if(input$slotUsageChoice == 3) { # Filled Hours
      
      ggplot(summary.md, aes(x=Appt.Week, y=`Filled Hours`, group=Provider, col=Provider)) +
        geom_line()+
        geom_point(size=2)+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Hours\n",
             title = "Total Filled Hours by Provider per Week",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    } else if(input$slotUsageChoice == 4) { # Booked Rate
      
      ggplot(summary.md, aes(x=Appt.Week, y=`Booked Rate (%)`, group=Provider, col=Provider)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Percent\n",
             title = "Average Booked Rate by Provider per Week",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    } else { # Filled Rate
      
      ggplot(summary.md, aes(x=Appt.Week, y=`Filled Rate (%)`, group=Provider, col=Provider)) +
        geom_line()+
        geom_point(size=2)+
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
        scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
        labs(x=NULL, y="Percent\n",
             title = "Average Filled Rate (%) by Provider per Week",
             subtitle = paste0("Based on data from ",input$dateRange[1]," to ",input$dateRange[2]))+
        theme_bw()+
        theme(
          plot.title = element_text(hjust=0.5, face = "bold", size = 20),
          plot.subtitle = element_text(hjust=0.5, size = 14),
          plot.caption = element_text(hjust = 0, size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill="#dddedd"),
          strip.text = element_text(size=14),
          axis.title = element_text(size=14),
          axis.text.x = element_text(size = 14, angle=40, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.line.x = element_blank())
      
    } 
    
  })
  
  
  # Slot Usage Table by Practice and Provider 
  output$slotUsageTb <- function(){
    
    data <- dataPastSlot()
    # data <- past.slot.data %>% filter(Campus == "MSUS") %>% filter(Campus.Specialty == "Cardiology")
    
    
    if(input$byProvider2 == TRUE){
      
      summary.prov <- data %>%
        group_by(Campus, Campus.Specialty, Provider, Appt.DateYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),1),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),1),
                         `Filled Hours` = round(sum(ARRIVED_MINUTES),1)) %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100)) %>%
        dplyr::mutate(`Available Hours` = round(`Available Hours`/60,1),
                      `Booked Hours` = round(`Booked Hours`/60,1),
                      `Filled Hours` = round(`Filled Hours`/60,1)) %>%
        gather(variable, value, 5:9) %>%
        spread(Appt.DateYear, value)
      
      summary.prov[is.na(summary.prov)] <- 0
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov <- summary.prov[order(match(summary.prov$variable, level.order)),]
      summary.prov <- summary.prov %>% arrange(Campus, Campus.Specialty, Provider)
      
      names(summary.prov)[names(summary.prov) == "Campus.Specialty"] <- "Specialty"
      names(summary.prov)[names(summary.prov) == "variable"] <- "Status"
      
      summary.prov %>%
        knitr::kable("html", align = "l") %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
        row_spec(0, bold=T, background = "#7f7f7f", color = "white") %>%
        column_spec(1:4, bold=T) %>%
        column_spec(4, width = "2in") %>%
        collapse_rows(columns = 1:3, valign = "middle") %>%
        add_header_above(c("Summary of Booked and Filled Rate by Site and Specialty" = length(summary.prov)),
                         background = "#221f72", color = "white", font_size = 18, align = "center") %>%
        scroll_box(height = "800px")
      
    } else {
      
      summary.dept <- data %>%
        group_by(Campus, Campus.Specialty, Appt.DateYear) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAIL_MINUTES),1),
                         `Booked Hours` = round(sum(BOOKED_MINUTES),1),
                         `Filled Hours` = round(sum(ARRIVED_MINUTES),1)) %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100)) %>%
        dplyr::mutate(`Available Hours` = round(`Available Hours`/60,1),
                      `Booked Hours` = round(`Booked Hours`/60,1),
                      `Filled Hours` = round(`Filled Hours`/60,1)) %>%
        gather(variable, value, 4:8) %>%
        spread(Appt.DateYear, value)
      
      summary.dept[is.na(summary.dept)] <- 0
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept <- summary.dept[order(match(summary.dept$variable, level.order)),]
      
      names(summary.dept)[names(summary.dept) == "Campus.Specialty"] <- "Specialty"
      names(summary.dept)[names(summary.dept) == "variable"] <- "Status"
      
      summary.dept %>%
        knitr::kable("html", align = "l") %>%
        kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
        row_spec(0, bold=T, background = "#7f7f7f", color = "white") %>%
        column_spec(1:3, bold=T) %>%
        column_spec(3, width = "4in") %>%
        collapse_rows(columns = 1:2, valign = "middle") %>%
        add_header_above(c("Summary of Booked and Filled Rate by Site and Specialty" = length(summary.dept)),
                         background = "#221f72", color = "white", font_size = 18, align = "center") %>%
        scroll_box(height = "800px")
    }
  }
  
  
  
  
  
  
  ### [3. ] Day of Visit Tab -----------------------------------------------------------------------------------------------------------
  
  # Reactive Filters for Scheduling Tab: Appointment Type & Insurance 
  output$apptTypeControl2 <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType2", label = h4("Select Appointment Type for Comparison:"), 
                  choices = sort(unique(historical.data[
                    historical.data$New.PT3 == FALSE &
                      historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Appt.Type"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Types", 
                    dropupAuto = FALSE),
                  selected = sort(unique(historical.data[
                    historical.data$New.PT3 == FALSE &
                      historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Appt.Type"]))[[1]]))
    
  })
  
  
  output$apptTypeControl3 <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType3", label = h4("Select Appointment Type for Comparison:"), 
                  choices = sort(unique(historical.data[
                    historical.data$New.PT3 == FALSE &
                      historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Appt.Type"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Types", 
                    dropupAuto = FALSE),
                  selected = sort(unique(historical.data[
                    historical.data$New.PT3 == FALSE &
                      historical.data$Campus %in% input$selectedCampus &
                      historical.data$Campus.Specialty %in% input$selectedSpecialty &
                      historical.data$Department %in% input$selectedDepartment & 
                      historical.data$Resource %in% input$selectedResource &
                      historical.data$Provider %in% input$selectedProvider, "Appt.Type"]))[[1]]))
    
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
      title = toupper("Avg NEW Appointments Check-in to Visit-end Time"),
      subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(cycleTime > 0))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$cycleTimeCompOther <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE))$cycleTime))," min"),
      title = toupper(ifelse(length(unique(dataNewComparison()$Appt.Type)) == 1,
                             paste0("Avg ", input$selectedApptType2," Appointments Check-in to Visit-end Time"),
                             "Avg Other* Appointments Check-in to Visit-end Time")),
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
      labs(title = "Check-in to Visit-end Time Comparison by Appointment Type\n",
           y = "\n% of Patients",
           x = "\nMinutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "right",
            legend.text = element_text(size="12"),
            legend.direction = "vertical",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.line = element_line(size = 0.3, colour = "black"))+
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
      labs(title = "Distribution of NEW Appointment\nCheck-in to Visit-end Time\n", 
           y = "\n% of Patients",
           x = "\nMinutes",
           caption = "-")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = "12", color = "white"),
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
      labs(title = paste0("Distribution of ",appt.type," Appointments\nCheck-in to Visit-end Time\n"), 
           y = "\n% of Patients",
           x = "\nMinutes",
           caption = paste0("*Includes following appointment types: ", paste(unique(data$Appt.Type),sep="", collapse=", ")))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = "12", face = "italic"),
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
           caption = paste0("*Includes following appointment types: ", names),
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
      labs(title = "Distribution of NEW Appointment Check-in to Visit-end Time by Provider\n", 
           y = "Minutes",
           caption = "-")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = "12", color = "white"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
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
      labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Visit-end Time by Provider\n"), 
           y = "Minutes",
           caption = paste0("*Includes following appointment types: ",paste(unique(data$Appt.Type),sep="", collapse=", ")))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = "12", face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
  })
  
  # (2) Room-in Times ----------------------------------------------------------------------
  
  output$roomInTimeCompNew <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE))$checkinToRoomin))," min"),
      title = toupper("Avg NEW Appointments Check-in to Room-in Time"),
      subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0))/nrow(dataArrived()),2)*100,"% of total arrived patients"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeCompOther <- renderValueBox({
    
    valueBoxSpark(
      value =  paste0(round(mean((dataNewComparison2() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))$checkinToRoomin))," min"),
      title = toupper(ifelse(length(unique(dataNewComparison2()$Appt.Type)) == 1,
                             paste0("Avg ", input$selectedApptType2," Appointments Check-in to Room-in Time"),
                             "Avg Other* Appointments Check-in to Room-in Time")),
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
      labs(title = "Check-in to Room-in Time Comparison by Appointment Type\n",
           y = "\n% of Patients",
           x = "\nMinutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "right",
            legend.text = element_text(size="12"),
            legend.direction = "vertical",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.line = element_line(size = 0.3, colour = "black"))+
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
      labs(title = "Distribution of NEW Appointment\nCheck-in to Room-in Time\n", 
           y = "\n% of Patients",
           x = "\nMinutes",
           caption = "-")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = 12, colour = "white"),
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
      labs(title = paste0("Distribution of ",appt.type,"Appointments\nCheck-in to Room-in Time\n"), 
           y = "\n% of Patients",
           x = "\nMinutes",
           caption = paste0("*Includes following appointment types: ", paste(unique(data$Appt.Type),sep="", collapse=", ")))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = 12, face = "italic"),
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
  
  output$roomInTimeByHour <- renderPlot({
    
    data <- dataArrived() %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    # data <- arrived.data %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    
    data_other <- dataNewComparison() %>% filter(New.PT3 == FALSE, checkinToRoomin > 0)
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
      labs(title = paste0(input," NEW Appointments Check-in to Visit-end Time by Hour"),
           y = NULL,
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
           caption = paste0("*Includes following appointment types: ", names),
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
      labs(title = "Distribution of NEW Appointment Check-in to Room-in Time by Provider\n", 
           y = "Minutes")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
  })
  
  output$establishedRoomInTimeByProv <- renderPlot({
    
    data_other <- dataNewComparison() %>% filter(checkinToRoomin >= 0)
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
      labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Room-in Time by Provider\n"), 
           y = "Minutes",
           caption = paste0("*Includes following appointment types: ",paste(unique(data$Appt.Type),sep="", collapse=", ")))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            plot.subtitle = element_text(hjust=0.5, size = 15, face = "italic"),
            plot.caption = element_text(size = "12", face = "italic"),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_text(),
            axis.title.y = element_text(),
            axis.text.x = element_text(angle = 35, hjust = 1, size = 12),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))
    
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
  #     geom_boxplot()+
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
  
  
  
  ### [3. ] Data Tab Output -----------------------------------------------------------------------------------------------------------
  dataDisplay <- reactive({
    groupByFilters(historical.data[,c("Campus","Campus.Specialty","Department","Resource","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status","holiday")],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  output$dTableAll <- DT::renderDataTable({
    DT::datatable(dataDisplay()[,c("Campus","Campus.Specialty","Department","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status","holiday")])
  })
  
  observeEvent(input$download, {
    screenshot()
  })
  
} # Close server 

shinyApp(ui, server)


shinyApp(ui, server, options = list(launch.browser = T,browser = "C:/Program Files/Google/Chrome/Application/chrome.exe"))





