server <- function(input, output, session) {
  
    ### (1) Create reactive filters ===================================================================================================
  output$specialtyControl <- renderUI({
    
      box(
        title = "Select Specialty:",
        width = 12, 
        solidHeader = FALSE,
        pickerInput("selectedSpecialty",label=NULL,
                    choices=sort(unique(data.subset.new[data.subset.new$Campus %in% input$selectedCampus, "Campus.Specialty"])),
                    multiple=TRUE,
                    options = pickerOptions(
                      liveSearch = TRUE,
                      actionsBox = TRUE,
                      dropupAuto = FALSE),
                    selected = NULL))
  })
  
  output$departmentControl <- renderUI({
    
    box(
      title = "Select Department:",
      width = 12, 
      solidHeader = FALSE, 
      pickerInput("selectedDepartment",label=NULL,
                  choices=sort(unique(data.subset.new[data.subset.new$Campus.Specialty %in% input$selectedSpecialty, "Department"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    dropupAuto = FALSE),
                  selected = NULL))
  })
  
  output$providerControl <- renderUI({
    
    box(
      title = "Select Provider:",
      width = 12, 
      solidHeader = FALSE, 
      pickerInput("selectedProvider",label=NULL,
                  choices=sort(unique(data.subset.new[data.subset.new$Department %in% input$selectedDepartment, "Provider"])),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    dropupAuto = FALSE),
                  selected = NULL))
  })
  
  output$dateRangeControl <- renderUI({
    
    box(
      title = "Select Date Range:",
      width = 12, 
      solidHeader = FALSE, 
      dateRangeInput("dateRange", label = NULL,
                     start = min(data.subset.new$Appt.DTTM), end = max(data.subset.new$Appt.DTTM)))
  })
  
  output$daysOfWeekControl <- renderUI({
    
    box(
      title = "Select Days of Week:",
      width = 12, 
      solidHeader = FALSE, 
      selectInput("daysOfWeek",label = NULL,
                  choices=c("Mon","Tue","Wed","Thu","Fri","Sat"), selected = daysOfWeek.options,
                  multiple=TRUE, selectize=TRUE))
  })

  
  ### (2) Prepare datasets for analysis ===============================================================================================
  # [2.1] All pre-processed data ------------------------------------------------------------------------------------------------------
  dataAll <- reactive({
    groupByFilters(data.subset.new,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  # [2.2] Arrived + No Show data --------------------------------------------------------------------------------------------------------
  dataArrivedNoShow <- reactive({
    groupByFilters(arrivedNoShow.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  # [2.3] Arrived data ------------------------------------------------------------------------------------------------------------------
  dataArrived <- reactive({
    groupByFilters(arrived.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  # Canceled data -----------------------------------------------------------------------------------------------------------------------
  dataCanceled<- reactive({
    groupByFilters(canceled.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  # Bumped data -----------------------------------------------------------------------------------------------------------------------
  dataBumped<- reactive({
    groupByFilters(bumped.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  

  ### (3) Dashboard Layout ============================================================================================================
  ### [3.1] Title of  Dashboard -------------------------------------------------------------------------------------------------------
  # Practice Overview Tab
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
      length((unique(dataArrived()$MRN))),
      subtitle = tags$p("Total Unique Patients", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$totalVisits <- renderValueBox({
    valueBox(
      length((unique(dataArrived()$uniqueId))), 
      subtitle = tags$p("Total Visits", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgVisitsPt <- renderValueBox({
    valueBox(
      round(length((unique(dataArrived()$uniqueId))) / length((unique(dataArrived()$MRN))),1),
      subtitle = tags$p("Avg Visits per Patient", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  output$avgVisitsDay <- renderValueBox({
    valueBox(
      round(length((unique(dataArrived()$uniqueId))) / length((unique(dataArrived()$Appt.DateYear))),1),
      subtitle = tags$p("Avg Visits per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  ### Average Daily Patients by Scheduled vs. Check-in Times (1-hour interval)
  
  output$avgPtArrival <- renderPlot({
    
    ptsScheduled <- aggregate(dataArrivedNoShow()$uniqueId, by=list(dataArrivedNoShow()$Appt.TM.Hr), FUN=NROW)
    names(ptsScheduled) <- c("Time","Total")
    ptsScheduled$Scheduled <- round(ptsScheduled$Total / length(unique(dataArrivedNoShow()$Appt.DateYear)),1)
    
    ptsArrived <- aggregate(dataArrived()$uniqueId, by=list(dataArrived()$Appt.TM.Hr), FUN=NROW)
    names(ptsArrived) <- c("Time","Total")
    ptsArrived$Arrived <- round(ptsArrived$Total / length(unique(dataArrived()$Appt.DateYear)),1)
    
    ptsByHour <- as.data.frame(timeOptionsHr) 
    ptsByHour <- merge(ptsByHour,ptsScheduled, by.x = "timeOptionsHr", by.y = "Time", all.x = TRUE)  
    ptsByHour <- merge(ptsByHour, ptsArrived, by.x = "timeOptionsHr", by.y = "Time", all.x = TRUE)
    ptsByHour[is.na(ptsByHour)] <- 0
    
    names(ptsByHour) <- c("Time","Total Scheduled","Scheduled","Total Arrived","Arrived")
    avgPtsByHour <- ptsByHour[,c("Time","Scheduled","Arrived")]
    
    avgPtsByHour <- reshape::melt(avgPtsByHour, id="Time", measure = c("Scheduled","Arrived"))
 
    # Scheduled vs. Actual Arrival in Hour Interval 
    
    ggplot(avgPtsByHour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(aes(linetype=variable), size=1.2)+
      scale_linetype_manual(values=c("dashed","solid"))+
      scale_color_manual(values=c("maroon1","midnightblue"))+
      ggtitle("Average Scheduled vs. Arrived Patients")+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 16),
            legend.position = "top",
            legend.text = element_text(size="12"),
            legend.direction = "horizontal",
            legend.key.size = unit(1.0,"cm"),
            legend.title = element_blank(),
            axis.title = element_text(size="14"),
            axis.text = element_text(size="14"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
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
      title = tags$p("New Patient Ratio", style = "font-size: 130%;"), subtitle = NULL, value = paste0("value","%"), icon = icon("user")
    )
  })
  
  output$thirdDays <- renderInfoBox({
    infoBox(
      title = tags$p("Days to 3rd Next Available", style = "font-size: 130%;"), subtitle = NULL, value = paste0("value"," %"), icon = icon("calendar-check")
    )
  })
  
  output$apptWaitTime <- renderInfoBox({
    infoBox(
      title = tags$p("Appointment Wait Time", style = "font-size: 130%;"), subtitle = NULL, value = paste0("value"," %"), icon = icon("clock")
    )
  })
  
  ### Scheduling Section
  
  output$fillRate <- renderPlot({
    
    a <- c("Booked","Filled")
    b <- c("98","82")
    fillBooked <- as.data.frame(cbind(a,b))
    names(fillBooked) <- c("variable","value")
    
    ggplot(fillBooked, aes(factor(variable,levels=c("Filled","Booked")),value, fill=variable)) +
      geom_bar(stat="identity", width = 1) +
      coord_flip() +
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank()) +
      geom_text(aes(label=paste0(value,"%")), vjust=-.1, hjust=-.3, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
    })
  
  output$apptStatus <- renderPlot({
    
    apptsCanceled <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status), FUN=NROW)
    names(apptsCanceled) <- c("Status","Total")
    apptsCanceled$Percent <- as.numeric(round((apptsCanceled$Total / sum(apptsCanceled$Total)*100),1))
    apptsCanceled <- apptsCanceled[which(apptsCanceled$Status != "Arrived"),]
    
    ggplot(apptsCanceled, aes(reorder(Status, -Percent),Percent, fill=Status)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,(max(apptsCanceled$Percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5),
        axis.text.y = element_text(size = "16"))+
      geom_text(aes(label=paste0(Percent,"%")), vjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  ### Day of Visit Section
  
  output$avgCycleTime <- renderValueBox({
    
    valueBox(
      paste0(round(mean(dataArrived()$cycleTime, na.rm = TRUE),1)," min"),
      subtitle = tags$p("Avg Cycle Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$avgCheckinToRoomin <- renderValueBox({
    
    valueBox(
      paste0(round(mean(dataArrived()$checkinToRoomin, na.rm = TRUE),1)," min"),
      subtitle = tags$p("Avg Check-in to Room-in", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$avgProviderTime <- renderValueBox({
    
    valueBox(
      paste0(round(mean(dataArrived()$providerinToOut, na.rm = TRUE),1)," min"),
      subtitle = tags$p("Avg Provider Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$avgCheckoutTime <- renderValueBox({
    
    valueBox(
      paste0(round(mean(dataArrived()$visitEndToCheckout, na.rm = TRUE),1)," min"),
      subtitle = tags$p("Avg Check-out Time", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  ### KPIs Tab ------------------------------------------------------------------------------------------------------------------------
  
  # Volume KPI ====================================================================
  output$kpiVolumeGraph <- renderPlot({
    kpiVolumeData <- aggregate(dataArrived()$uniqueId, by=list(dataArrived()$Appt.Year,dataArrived()$Appt.Quarter,
                                                              dataArrived()$Appt.Month, dataArrived()$Appt.Date), FUN=NROW)

    #kpiVolumeData <- aggregate(arrived.data$uniqueId, by=list(arrived.data$Appt.Year,arrived.data$Appt.Quarter,
     #                                                         arrived.data$Appt.Month, arrived.data$Appt.Date), FUN=NROW)

    colnames(kpiVolumeData) <- c("Year","Quarter","Month","Date","Volume")

    #a <- list("Year" = 1, "Quarter" = 2, "Month" = 3, "Day" = 4)
    #a <- as.numeric(unlist(a))
    
    kpiVolumeDataYear <- kpiVolumeData %>% group_by(Year) %>% summarize(Total = sum(Volume))
    kpiVolumeDataQuarter <- kpiVolumeData %>% group_by(Year, Quarter) %>% summarize(Total = sum(Volume))
    kpiVolumeDataMonth <- kpiVolumeData %>% group_by(Year, Month) %>% summarize(Total = sum(Volume))
    
    if(input$kpiTrend ==1){
      if(input$kpiFreq == 1){ #Year
        ggplot(kpiVolumeDataYear, aes(x=Year, y=Total,group=1)) +
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Year")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(kpiVolumeDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=Total,group=1)) +
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Quarter")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else if(input$kpiFreq == 3){ # Month
        ggplot(kpiVolumeDataMonth, aes(x=interaction(Year,Month,lex.order = TRUE), y=Total,group=1)) +
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Month")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else { # Day
        ggplot(kpiVolumeData, aes(x=interaction(Year,Date,lex.order = TRUE), y=Volume,group=1)) +
          geom_line(size=1, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Day")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", vjust=0.5, angle = 90),
            axis.text.y = element_text(size = "16"))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(kpiVolumeDataYear %>% mutate(Label = "Year"), aes(x=Label, y=Total, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(kpiVolumeDataQuarter, aes(x=Quarter, y=Total, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 3){
        ggplot(kpiVolumeDataMonth, aes(x=Month, y=Total, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 4){
        ggplot(kpiVolumeData, aes(x=Date, y=Volume, col=Year,group=Year)) +
          geom_line(size=1) +
          scale_color_MountSinai("main")
        }
    }
    
  })
  
  # Appt Status KPI ========================================================================
  output$kpiApptStatusGraph <- renderPlot({

    statusData <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Year,dataAll()$Appt.Quarter,
                                                               dataAll()$Appt.Month, dataAll()$Appt.Date,
                                                               dataAll()$Appt.Status), FUN=NROW)

    colnames(statusData) <- c("Year","Quarter","Month","Date","Status","Count")
    
    # statusData <- aggregate(all.data$uniqueId, by=list(all.data$Appt.Year,all.data$Appt.Quarter,
    #                                                           all.data$Appt.Month, all.data$Appt.Date,
    #                                                           all.data$Appt.Status), FUN=NROW)
    # 
    # colnames(statusData) <- c("Year","Quarter","Month","Date","Status","Count")

    statusDataYear <- statusData %>% group_by(Year,Status) %>% summarize(Total = sum(Count))
    statusDataYear <- reshape2::dcast(statusDataYear, Year ~ Status)
    statusDataYear[is.na(statusDataYear)] <- 0
    statusDataYear <- statusDataYear %>%
      mutate(canceled_perc = Canceled / rowSums(statusDataYear[,2:6])) %>%
      mutate(bumped_perc = Bumped / rowSums(statusDataYear[,2:6])) %>%
      mutate(noShow_perc = `No Show` / rowSums(statusDataYear[,c("Arrived","No Show")])) %>%
      select(c("Year","canceled_perc","bumped_perc","noShow_perc"))
    statusDataYear[is.na(statusDataYear)] <- 0
    statusDataYear <- reshape2::melt(statusDataYear, id.vars = c("Year"))
            
    statusDataQuarter <- statusData %>% group_by(Year, Quarter, Status) %>% summarize(Total = sum(Count))
    statusDataQuarter <- reshape2::dcast(statusDataQuarter, Year + Quarter ~ Status)
    statusDataQuarter[is.na(statusDataQuarter)] <- 0
    statusDataQuarter <- statusDataQuarter %>%
      mutate(canceled_perc = Canceled / rowSums(statusDataQuarter[,3:7])) %>%
      mutate(bumped_perc = Bumped / rowSums(statusDataQuarter[,3:7])) %>%
      mutate(noShow_perc = `No Show` / rowSums(statusDataQuarter[,c("Arrived","No Show")])) %>%
      select(c("Year","Quarter","canceled_perc","bumped_perc","noShow_perc"))
    statusDataQuarter[is.na(statusDataQuarter)] <- 0
    statusDataQuarter <- reshape2::melt(statusDataQuarter, id.vars = c("Year","Quarter"))
    
    statusDataMonth <- statusData %>% group_by(Year, Month, Status) %>% summarize(Total = sum(Count))
    statusDataMonth <- reshape2::dcast(statusDataMonth, Year + Month ~ Status)
    statusDataMonth[is.na(statusDataMonth)] <- 0
    statusDataMonth <- statusDataMonth %>%
      mutate(canceled_perc = Canceled / rowSums(statusDataMonth[,3:7])) %>%
      mutate(bumped_perc = Bumped / rowSums(statusDataMonth[,3:7])) %>%
      mutate(noShow_perc = `No Show` / rowSums(statusDataMonth[,c("Arrived","No Show")])) %>%
      select(c("Year","Month","canceled_perc","bumped_perc","noShow_perc"))
    statusDataMonth[is.na(statusDataMonth)] <- 0
    statusDataMonth <- reshape2::melt(statusDataMonth, id.vars = c("Year","Month"))
    
    statusDataDay <- statusData %>% group_by(Year, Date, Status) %>% summarize(Total = sum(Count))
    statusDataDay <- reshape2::dcast(statusDataDay, Year + Date ~ Status)
    statusDataDay[is.na(statusDataDay)] <- 0
    statusDataDay <- statusDataDay %>%
      mutate(canceled_perc = Canceled / rowSums(statusDataDay[,3:7])) %>%
      mutate(bumped_perc = Bumped / rowSums(statusDataDay[,3:7])) %>%
      mutate(noShow_perc = `No Show` / rowSums(statusDataDay[,c("Arrived","No Show")])) %>%
      select(c("Year","Date","canceled_perc","bumped_perc","noShow_perc"))
    statusDataDay[is.na(statusDataDay)] <- 0
    statusDataDay <- reshape2::melt(statusDataDay, id.vars = c("Year","Date"))
    
    if(input$kpiTrend ==1){
      if(input$kpiFreq == 1){ #Year
        ggplot(statusDataYear, aes(x=Year, y=value,col=variable, group=variable)) +
          ggtitle("Historical Trend of Scheduling Status by Year")+
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")

      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(statusDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=value, col=variable, group=variable)) +
          ggtitle("Historical Trend of Scheduling Status by Quarter")+
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=interaction(Year,Month,lex.order = TRUE), y=value, col=variable, group=variable)) +
          ggtitle("Historical Trend of Scheduling Status by Quarter")+
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else { # Day
        ggplot(statusDataDay, aes(x=interaction(Year,Date,lex.order = TRUE), y=value, col=variable, group=variable)) +
          ggtitle("Historical Trend of Scheduling Status by Day")+
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      }
    } else {
      if(input$kpiFreq == 1){ # Year
        ggplot(statusDataYear %>% mutate(Label = "Year"), aes(x=Label, y=value, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 2){ # Quarter
        ggplot(statusDataQuarter, aes(x=Quarter, y=value, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 3){
        ggplot(statusDataMonth, aes(x=Month, y=value, col=Year,group=Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 4){
        ggplot(statusDataDay, aes(x=Date, y=value, col=Year,group=Year)) +
          geom_line(size=1) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
          facet_wrap(variable ~ ., dir="v", strip.position = "right")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))+
          scale_color_MountSinai("main")
      }
    }

  })


  # ggplot(kpiVolumeData, aes_string(x=names(kpiVolumeData)[1], y="Total",group=1)) +
  #   geom_line(size=1.2) +
  #   geom_point(size=2) +
  #   scale_color_MountSinai("main")
  # } else {
  #   ggplot(kpiVolumeData %>% mutate(Label = colnames(kpiVolumeData)[1]), aes(x=Label, y=Total, col=Year,group=1)) +
  #     geom_line(size=1.2) +
  #     geom_point(size=2) +
  #     scale_color_MountSinai("main")
  # }
  
  
  
  #  plottest1 <- reactive({
  #   if(input$kpiFreq == 1 & input$selectedKPIs == "Volume"){
  #     a <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status,dataAll()$Appt.MonthYear), FUN=NROW)
  #     names(a) <- c("Status","Month","Total")
  #     a <- a %>% group_by(Month) %>%
  #       mutate(percentage = Total / sum(Total)*100)
  #     names(a) <- c("Status","Month","Total","Percent")
  #     
  #     a <- a[a$Status %in% c("Bumped","Canceled"), ]
  #     
  #     a <- ggplot(a, aes(x=Month, y=Percent, col=Status, group=Status))+
  #       geom_line(size=1.2)+
  #       scale_color_MountSinai("main")
  #   } 
  #   return(a)
  # })
  # 
  # plottest2 <- reactive({
  #   if(input$kpiFreq == 1 & input$selectedKPIs == "Appointment Status"){
  #     a <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status,dataAll()$Appt.MonthYear), FUN=NROW)
  #     names(a) <- c("Status","Month","Total")
  #     a <- a %>% group_by(Month) %>%
  #       mutate(percentage = Total / sum(Total)*100)
  #     names(a) <- c("Status","Month","Total","Percent")
  #     
  #     a <- a[a$Status %in% c("Arrived"), ]
  #     
  #     a <- ggplot(a, aes(x=Month, y=Percent, col=Status, group=Status))+
  #       geom_line(size=1.2)+
  #       scale_color_MountSinai("main")
  #   } 
  #   return(a)
  # })
  # 
  # output$kpiGraphs1 <- renderPlot({
  #   grid.arrange(plottest1(), plottest2())
  # 
  # })
  # 
  # output$kpiGraphs2 <- renderPlot({
  #   plottest2()
  # })
  
  
  # 
  # output$kpiGraphs <- renderPlot({
  #   if(input$kpiFreq == 1 & input$selectedKPIs == "Volume"){
  #     a <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status,dataAll()$Appt.MonthYear), FUN=NROW)
  #     names(a) <- c("Status","Month","Total")
  #     a <- a %>% group_by(Month) %>%
  #       mutate(percentage = Total / sum(Total)*100)
  #     names(a) <- c("Status","Month","Total","Percent")
  # 
  #     a <- a[a$Status %in% c("Bumped","Canceled"), ]
  # 
  #     a1 <- ggplot(a, aes(x=Month, y=Percent, col=Status, group=Status))+
  #       geom_line(size=1.2)+
  #       scale_color_MountSinai("main")
  #     print(a1)
  #   }
  #   if(input$kpiFreq == 1 & input$selectedKPIs == "Appointment Status"){
  #     a <- aggregate(dataAll()$uniqueId, by=list(dataAll()$Appt.Status,dataAll()$Appt.MonthYear), FUN=NROW)
  #     names(a) <- c("Status","Month","Total")
  #     a <- a %>% group_by(Month) %>%
  #       mutate(percentage = Total / sum(Total)*100)
  #     names(a) <- c("Status","Month","Total","Percent")
  # 
  #     a <- a[a$Status %in% c("Arrived"), ]
  # 
  #     a2 <- ggplot(a, aes(x=Month, y=Percent, col=Status, group=Status))+
  #       geom_line(size=1.2)+
  #       scale_color_MountSinai("main")
  #     print(a2)
  #   }
  #   
  #   
  #   
  # })
  
  # Day of Visit KPI ========================================================================
  
  ## Check-in to Room-in Wait Time
  output$kpiWaitTimeGraph <- renderPlot({
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        ggplot(dataArrived() %>% group_by(Appt.Year) %>% summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=Appt.Year, y=mean, group=1)) +
          #stat_summary(fun.y="mean", geom="line")+
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Average Check-in to Room-in Time by Year")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else if(input$kpiFreq == 2) { # Quarter
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Quarter")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else if(input$kpiFreq == 3){ # Month
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Month) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Month,lex.order = TRUE), y=mean,group=1)) +
          geom_line(size=1.2, color="midnightblue") +
          geom_point(size=2, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Month")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
            axis.text.y = element_text(size = "16"))
      } else { # Day
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Date) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Date,lex.order = TRUE), y=mean,group=1)) +
          geom_line(size=1, color="midnightblue") +
          ggtitle("Historical Trend of Patient Volume by Day")+
          theme_new_line()+
          theme(
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = "12", vjust=0.5, angle = 90),
            axis.text.y = element_text(size = "16"))
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(dataArrived() %>% group_by(Appt.Year) %>% summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>% 
                 mutate(Label = "Year"), aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Quarter) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>% 
                 mutate(Label = "Quarter"), aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 3){
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Month) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>% 
                 mutate(Label = "Month"), aes(x=Appt.Month, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line(size=1.2) +
          geom_point(size=2) +
          scale_color_MountSinai("main")
      } else if(input$kpiFreq == 4){
        ggplot(dataArrived() %>% group_by(Appt.Year, Appt.Date) %>% 
                 summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>% 
                 mutate(Label = "Date"), aes(x=Appt.Date, y=mean, col=Appt.Year,group=Appt.Year)) +
          geom_line(size=1) +
          scale_color_MountSinai("main")
      }
    }

  })
  
  ## % Provider Time
  # output$kpiProviderTimeGraph <- renderPlot({
  # 
  #   if(input$kpiTrend ==1){ # Historical Trend
  #     if(input$kpiFreq == 1){ #Year
  #       ggplot(arrived.data %>% 
  #                mutate(providerTime_perc = round(providerinToOut / cycleTime),2) %>% group_by(Appt.Year) %>% summarise(mean = mean(providerTime_perc, na.rm=TRUE)),
  #              aes(x=Appt.Year, y=mean, group=1)) +
  #         #stat_summary(fun.y="mean", geom="line")+
  #         geom_line(size=1.2, color="midnightblue") +
  #         geom_point(size=2, color="midnightblue") +
  #         ggtitle("Average Check-in to Room-in Time by Year")+
  #         theme_new_line()+
  #         theme(
  #           legend.position = "none",
  #           axis.title.y = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #           axis.text.y = element_text(size = "16"))
  #     } else if(input$kpiFreq == 2) { # Quarter
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Quarter) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Quarter,lex.order = TRUE), y=mean,group=1)) +
  #         geom_line(size=1.2, color="midnightblue") +
  #         geom_point(size=2, color="midnightblue") +
  #         ggtitle("Historical Trend of Patient Volume by Quarter")+
  #         theme_new_line()+
  #         theme(
  #           legend.position = "none",
  #           axis.title.y = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #           axis.text.y = element_text(size = "16"))
  #     } else if(input$kpiFreq == 3){ # Month
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Month) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Month,lex.order = TRUE), y=mean,group=1)) +
  #         geom_line(size=1.2, color="midnightblue") +
  #         geom_point(size=2, color="midnightblue") +
  #         ggtitle("Historical Trend of Patient Volume by Month")+
  #         theme_new_line()+
  #         theme(
  #           legend.position = "none",
  #           axis.title.y = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #           axis.text.y = element_text(size = "16"))
  #     } else { # Day
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Date) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)), aes(x=interaction(Appt.Year,Appt.Date,lex.order = TRUE), y=mean,group=1)) +
  #         geom_line(size=1, color="midnightblue") +
  #         ggtitle("Historical Trend of Patient Volume by Day")+
  #         theme_new_line()+
  #         theme(
  #           legend.position = "none",
  #           axis.title.y = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.text.x = element_text(size = "12", vjust=0.5, angle = 90),
  #           axis.text.y = element_text(size = "16"))
  #     }
  #   } else {
  #     if(input$kpiFreq == 1){ # Year
  #       ggplot(arrived.data %>% group_by(Appt.Year) %>% summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>%
  #                mutate(Label = "Year"), aes(x=Label, y=mean, col=Appt.Year,group=Appt.Year)) +
  #         geom_line(size=1.2) +
  #         geom_point(size=2) +
  #         scale_color_MountSinai("main")
  #     } else if(input$kpiFreq == 2){ # Quarter
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Quarter) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>%
  #                mutate(Label = "Quarter"), aes(x=Appt.Quarter, y=mean, col=Appt.Year,group=Appt.Year)) +
  #         geom_line(size=1.2) +
  #         geom_point(size=2) +
  #         scale_color_MountSinai("main")
  #     } else if(input$kpiFreq == 3){
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Month) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>%
  #                mutate(Label = "Month"), aes(x=Appt.Month, y=mean, col=Appt.Year,group=Appt.Year)) +
  #         geom_line(size=1.2) +
  #         geom_point(size=2) +
  #         scale_color_MountSinai("main")
  #     } else if(input$kpiFreq == 4){
  #       ggplot(arrived.data %>% group_by(Appt.Year, Appt.Date) %>%
  #                summarise(mean = mean(checkinToRoomin, na.rm=TRUE)) %>%
  #                mutate(Label = "Date"), aes(x=Appt.Date, y=mean, col=Appt.Year,group=Appt.Year)) +
  #         geom_line(size=1) +
  #         scale_color_MountSinai("main")
  #     }
  #   }
  # 
  # })
  
  
  
  ### Scheduling Tab -------------------------------------------------------------------------------------------------------------------
  
  # Scheduled Patients
  output$scheduledPts <- renderPlot({
    
    data <- dataAll() %>%
      group_by(Appt.DateYear, Appt.TM.Hr, Appt.Status) %>%
      filter(Appt.Status %in% c("Arrived","Canceled","No Show")) %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(Remove)
      filter(Appt.Status %in% "Canceled" & leadDays <=0 | is.na(leadDays)) %>%
      summarise(total = n()) %>%
      group_by(Appt.TM.Hr, Appt.Status) %>%
      summarise(avg = round(mean(total),1))
    
    data <- dcast(data, Appt.TM.Hr ~ Appt.Status, sum)
    
    byTime.df <- as.data.frame(byTime.df[which(byTime.df$Time %in% unique(data$Appt.TM.Hr)),])
    colnames(byTime.df) <- "Time"
    
    data <- as.data.frame(merge(byTime.df,data, by.x = c("Time"), by.y = c("Appt.TM.Hr"), all.x = TRUE, all.y = TRUE))
    
    #data[is.na(data)] <- 0
    data <- melt(data, id=c("Time"))
    
    #data <- data %>%
     # arrange(desc(variable))
 
    ggplot(data, aes(x=Time, y=value, fill=factor(variable, levels=c("Canceled","No Show","Arrived"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("grey","#d80b8c","midnightblue"))+
      ggtitle(label="Average Patients Arrived",
              subtitle = "Based on scheduled appointment time")+
      scale_y_continuous(limits=c(0,(max(data$value))*2))+
      #scale_fill_MountSinai("main")+
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
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
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
      summarise(total = n()) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      summarise(avg = round(mean(total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(arrived$Day)),]
    
    arrived <- as.data.frame(merge(byDayTime.df,arrived, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))
    arrived[is.na(arrived)] <- 0
    
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
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
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
                  choices=sort(unique(dataAll()$Appt.Type)),
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
                  choices=sort(unique(dataAll()$Coverage)),
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
  
  # Total No Shows per Day
  output$avgDailyNoShow_Count <- renderValueBox({
    
    valueBox(
      round(nrow(dataArrivedNoShow_1()[which(dataArrivedNoShow_1()$Appt.Status == "No Show"),]) / length(unique(dataArrivedNoShow_1()$Appt.DateYear)),0),
      subtitle = tags$p("No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Total No Shows per Day
  output$avgDailyNoShow_Perc <- renderValueBox({
    
    valueBox(
      paste0(round((nrow(dataArrivedNoShow_1()[which(dataArrivedNoShow_1()$Appt.Status == "No Show"),]) / nrow(dataArrivedNoShow_1()))*100,1), " %"),
      subtitle = tags$p("% No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # No Shows (%) by Lead Days to Appointment
  output$noShowLeadDays <- renderPlot({
    
    noShows <- 
      dataArrivedNoShow() %>%
      mutate(apptLeadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2))) %>%
      mutate(apptLeadDays = ifelse(is.na(apptLeadDays),0, apptLeadDays)) %>%
      mutate(apptLeadDays = ifelse(apptLeadDays > 14, "> 14 days",
                                   ifelse(apptLeadDays <= 14 & apptLeadDays>= 8, "8-14 days",
                                          ifelse(apptLeadDays <= 7 & apptLeadDays >= 1, "1-7 days",
                                                 ifelse(apptLeadDays < 0, "0 day","0 day")))))
    
    noShows <- reshape2::dcast(noShows, apptLeadDays + Appt.DateYear ~ Appt.Status)
    noShows$noShow_perc <- round(noShows$`No Show`/noShows$Arrived,2)
    noShows$noShow_perc[!is.finite(noShows$noShow_perc)] <- 0
    
    status <- c('0 day','1-7 days','8-14 days','> 14 days')
    
    noShows_box <- 
      ggplot(noShows, aes(x = factor(apptLeadDays, levels = status), y = noShow_perc)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,.9))+
      theme_new_line()+
      theme(
        plot.title =element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"),
        plot.margin = margin(0,30,30,30))
    
    noShows_bar_tb <-
      noShows %>%
      group_by(apptLeadDays) %>%
      summarise(Average = round(mean(noShow_perc),2), Median = round(median(noShow_perc),2))

    noShows_bar_tb <-
      melt(noShows_bar_tb, id.vars = c("apptLeadDays"))

    noShows_bar <-
      ggplot(noShows_bar_tb, aes(x=factor(apptLeadDays, levels = status), y=value,fill=variable)) +
      geom_bar(stat="identity", position=position_dodge(), width = 0.8) +
      ggtitle("Average No Show Rate by Lead Days to Appointment")+
      scale_y_continuous(labels=scales::percent_format(accuracy=1),limits=c(0,max(noShows_bar_tb$value)*1.3))+
      theme_new_line()+
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = "16"),
        plot.margin = margin(30,0,30,30))+
      geom_text(aes(label=paste0(value*100,"%")), vjust = .5, hjust = .5, color="black", fontface="bold",
                position = position_dodge(1), size=5)

    grid.arrange(noShows_bar, noShows_box, ncol = 1, heights = c(2, 2))

    
  })
  
  
  # # Daily % No Shows Summary Table
  # output$volume4.1 <- function(){
  #   
  #   pts.dist <- aggregate(dataArrived()$uniqueId, 
  #                         by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
  #   
  #   names(pts.dist) <- c("Month","Date","Day","Volume")
  #   
  #   pts.dist.summary <-
  #     pts.dist %>%
  #     group_by(Month) %>%
  #     summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
  #   
  #   pts.dist.summary <- 
  #     pts.dist.summary[order(as.yearmon(pts.dist.summary$Month,format="%b-%Y")),]
  #   
  #   pts.dist.summary %>%
  #     knitr::kable("html", align = "l") %>%
  #     kable_styling(bootstrap_options = c("striped", "hover"), full_width=F, position="center", font_size = 15) %>%
  #     row_spec(0, bold=T) %>%
  #     column_spec(1, bold=T, width = "3cm")
  #   
  # }
  
  
  # No Shows by Time of Day 
    output$avgNoShowCount <- renderPlot({
    
    noShow_count <- dataArrivedNoShow_1() %>%
      filter(Appt.Status %in% "No Show") %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr) %>%
      summarise(Total = n()) %>%
      group_by(Appt.Day, Appt.TM.Hr) %>%
      summarise(avgNoShow = round(sum(Total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(noShow_count$Day)),]
    
    noShow_count <- as.data.frame(merge(byDayTime.df,noShow_count, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))
    
    ggplot(noShow_count, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=avgNoShow), colour = "black", size=0.5)+
      ggtitle("Average Daily No Shows")+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "white", guide = "colourbar", name="No Show % ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_count$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
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
      geom_text(aes(label= ifelse(is.na(avgNoShow),"",avgNoShow)), color="black", size=5, fontface="bold")
    
  })
  
  output$avgNoShowPercent <- renderPlot({
    
    noShow_perc <- dataArrivedNoShow_1() %>%
      group_by(Appt.DateYear, Appt.Day, Appt.TM.Hr, Appt.Status) %>%
      summarise(Total = n())
    
    noShow_perc <- reshape2::dcast(noShow_perc, Appt.Day +  Appt.TM.Hr ~ Appt.Status, sum) 
    noShow_perc <- mutate(noShow_perc, percentage = round((`No Show` / (Arrived + `No Show`))*100,0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(noShow_perc$Day)),]
    
    noShow_perc <- as.data.frame(merge(byDayTime.df,noShow_perc, by.x = c("Day","Time"), by.y = c("Appt.Day","Appt.TM.Hr"), all = TRUE))

    ggplot(noShow_perc, aes(x=factor(Day, levels = daysOfWeek.options), y=Time))+
      labs(x=NULL, y=NULL)+
      geom_tile(aes(fill=percentage), colour = "black", size=0.5)+
      ggtitle("Average No Show %")+
      scale_fill_gradient(low = "white", high = "red", space = "Lab", na.value = "white", guide = "colourbar", name="No Show % ")+
      scale_y_discrete(limits = rev(unique(sort(noShow_perc$Time))))+
      scale_x_discrete(position = "top")+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
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
      geom_text(aes(label= ifelse(is.na(percentage),"",paste(percentage,"%", sep = " "))), color="black", size=5, fontface="bold")
    
  })
  
  # Cancellation Lead Days
  output$canceledLeadDays <- renderPlot({
    
    cancellations <- 
      dataCanceled() %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays) %>%
      summarise(total = n()) %>%
      mutate(percent = round(total / sum(total),2))
    
    status <- c('0 day','1-7 days','8-14 days','> 14 days')
    
    ggplot(cancellations, aes(x=factor(leadDays, levels = status), y=percent))+
      geom_bar(stat="identity",  fill="grey", width=0.8)+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(cancellations$percent))*1.3))+
      ggtitle("Lead Days to Appointment Cancellation")+
      #scale_color_manual(values=MountSinai_pal("main")(9))+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_blank())+
      geom_text(aes(label=paste0(round(percent*100,0)," %")), vjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  # Bump Lead Days
  output$bumpedLeadDays <- renderPlot({
    
    bumps <- 
      dataBumped() %>%
      mutate(leadDays = as.numeric(round(difftime(Appt.DTTM, Appt.Cancel.DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 14, "> 14 days", 
                               ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                      ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                             ifelse(leadDays < 0, "0 day","0 day"))))) %>%
      group_by(leadDays) %>%
      summarise(total = n()) %>%
      mutate(percent = round(total / sum(total),2))
    
    status <- c('0 day','1-7 days','8-14 days','> 14 days')
    
    ggplot(bumps, aes(x=factor(leadDays, levels = status), y=percent))+
      geom_bar(stat="identity",  fill="grey", width=0.8)+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(bumps$percent))*1.3))+
      ggtitle("Lead Days to Appointment Bumps")+
      #scale_color_manual(values=MountSinai_pal("main")(9))+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_blank())+
      geom_text(aes(label=paste0(round(percent*100,0)," %")), vjust = -.5, color="black", fontface="bold",
                position = position_dodge(1), size=5)
    
  })
  
  ### Utilization Tab ------------------------------------------------------------------------------------------------------------------
  
  dataHourScheduled <- reactive({
    groupByFilters(data.hour.scheduled,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  dataHourArrived <- reactive({
    groupByFilters(data.hour.arrived,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  daysOfWeek.Table <- 
    data.hour.arrived %>%
    group_by(Appt.Day,Appt.DateYear) %>%
    summarise(count = n()) %>%
    summarise(count = n())
  
  # Average Rooms Required
  output$avgRoomsRequired <- renderValueBox({
    
    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    round(sum(dataHourArrived()[c(c.start:c.end)])/(length(unique(dataHourArrived()$Appt.DateYear))*(60*input$setHours)),1) %>%
    valueBox(
      subtitle = tags$p("Avg Rooms Required", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
  })
  
  # Average Utilization --------------------------------------------------------------------------------------------------------
  output$avgUtilization <- renderValueBox({
    
    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    paste0(round((sum(dataHourArrived()[c(c.start:c.end)])/(length(unique(dataHourArrived()$Appt.DateYear))*(60*input$setHours*input$setRooms)))*100,1)," %") %>%
      valueBox(
        subtitle = tags$p("Avg Utilization", style = "font-size: 130%;"), icon = NULL, color = "yellow"
      )
  })
  
  # Average Number of Rooms Required
  output$spaceUsed <- renderPlot({
    
    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    space.hour.day <- aggregate(dataHourArrived()[c(c.start:c.end)], list(dataHourArrived()$Appt.Day),FUN = sum)
    space.hour.day <- melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$average <- round(space.hour.day$value/(space.hour.day$days*60), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Req")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0

    ggplot(space.hour.day, aes(x=Time, y=Average_Req, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      #scale_color_manual(values=c("deepskyblue","maroon1","midnightblue"))+
      ggtitle(label="Average Space Required by Time of Day and Day of Week",
              subtitle = "Based on scheduled appointment time and duration")+
      ylab("Room Count")+
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
            axis.title.y = element_text(margin = margin(r=5)),
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))
    
  })
  
  # Average Utilization by Time of Day
  output$spaceUtil <- renderPlot({
    
    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    space.hour.day <- aggregate(dataHourArrived()[c(c.start:c.end)], list(dataHourArrived()$Appt.Day),FUN = sum)
    space.hour.day <- melt(space.hour.day, id=c("Group.1"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$Appt.Day,space.hour.day$Group.1)]
    
    space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*input$setRooms), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Util")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    ggplot(space.hour.day, aes(x=Time, y=Average_Util, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      #scale_color_manual(values=c("deepskyblue","maroon1","midnightblue"))+
      ggtitle(label="Average Space Utilization (%) by Time of Day and Day of Week",
              subtitle = "Based on scheduled appointment time and duration")+
      scale_color_MountSinai("main")+
      geom_hline(yintercept=.8, linetype="dashed", color = "red")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1))+
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
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))
    
  })
  
  # Rooms Required by Percentile 
  
  output$spaceUsedPerc <- renderPlot({
    
    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    space.hour <- aggregate(dataHourArrived()[c(c.start:c.end)], list(dataHourArrived()$Appt.DateYear),FUN = sum)
    space.hour <- melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      summarise( 
        Median = round(quantile(value, probs=0.5)/60,1),
        `70th Percentile`= round(quantile(value, probs=0.75)/60,1),
        `90th Percentile`= round(quantile(value, probs=0.90)/60,1))
    
    colnames(space.hour)[1] <- "Time"
   
    space.hour <- as.data.frame(melt(space.hour, id=c("Time")))
    
    ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      ggtitle(label="Space Required by Percentile by Time of Day",
              subtitle = "Based on scheduled appointment time and duration")+
      ylab("Room Count")+
      scale_color_MountSinai("main")+
      theme_bw()+
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
            axis.title.y = element_text(margin = margin(r=5)),
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))
  
  })
  
  # Utilization by Percentile
  
  output$spaceUtilPerc <- renderPlot({

    c.start <- which(colnames(dataHourArrived())=="00:00")
    c.end <- which(colnames(dataHourArrived())=="23:00")
    
    space.hour <- aggregate(dataHourArrived()[c(c.start:c.end)], list(dataHourArrived()$Appt.DateYear),FUN = sum)
    space.hour <- melt(space.hour, id=c("Group.1"))
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      summarise( 
        Median = round(quantile(value, probs=0.5)/(60*input$setRooms),1),
        `70th Percentile`= round(quantile(value, probs=0.75)/(60*input$setRooms),1),
        `90th Percentile`= round(quantile(value, probs=0.90)/(60*input$setRooms),1))
    
    colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(melt(space.hour, id=c("Time")))
    
    ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
      ggtitle(label="Space Utilization (%) by Percentile by Time of Day",
              subtitle = "Based on scheduled appointment time and duration")+
      scale_color_MountSinai("main")+
      theme_bw()+
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
            axis.text.x = element_text(angle = 90,hjust = 0.5, margin = margin(t=10)),
            axis.text.y = element_text(margin = margin(l=5, r=5)),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 0.3, colour = "black"),
            plot.margin = margin(30,30,30,30))+
      guides(colour = guide_legend(nrow = 1))

  })

  
      

  ### [3. ] Population Tab Output -----------------------------------------------------------------------------------------------------
  
  ## Demographics Breakdown
  install.packages("eeptools")
  library(eeptools)
  library(grid)
  library(gridExtra)
  
  arrived.data$age <- round(age_calc(as.Date(arrived.data$Birth.Date, format="%Y-%m-%d"), units='years'),0)
  arrived.data$age_group <- cut(arrived.data$age, breaks = c(10,20,30,40,50,60,70,80), na.rm=TRUE)
  
  age_data <- arrived.data %>% drop_na(Sex, age_group) %>%
    group_by(Sex, age_group) %>% summarise(total = n())
  
  age_data <- reshape2::dcast(age_data, age_group ~ Sex) %>%
    mutate(female_perc = Female / sum(Female)) %>%
    mutate(male_perc = Male / sum(Male))
  
  g.mid<-ggplot(arrived.data %>% drop_na(age_group),aes(x=1,y=age_group))+geom_text(aes(label=age_group))+
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
    geom_bar(stat = "identity") + ggtitle("% of Female Visits by Age Group") +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,-1,1,0), "mm")) +
    scale_y_reverse(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip()
  
  g2 <- ggplot(age_data, aes(x = age_group, y = male_perc)) +xlab(NULL)+
    geom_bar(stat = "identity") + ggtitle("% of Male Visits by Age Group") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm")) +
    coord_flip() 
  
  library(gridExtra)
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))
  
  
  output$population1 <- renderLeaflet({
    leaflet(population.data) %>%
      # set view to New York City
      setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      # Mount Sinai locations (lng,lat): MSH(-73.94332,40.79171); MSUS:(-73.98840,40.73139); MSSL: (-73.96316,40.79834); MSW: (-73.99181,40.76719); MSBI: (), NYEE: (); MSQ(-73.92606,40.77084); MSB: (); SN: ()
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
                                    style=list('font-weight'= 'bold'))) %>%
      addMarkers(
        lng=~longitude, # Longitude coordinates
        lat=~latitude, # Latitude coordinates
        clusterOptions = markerClusterOptions()
      )
  })
  
  ### [3. ] Volume Tab Output ---------------------------------------------------------------------------------------------------------
  # Daily Patient Volume over Time ....................................................................................................
  output$volume1 <- renderPlot({
    
    pts.count <- aggregate(dataArrived()$uniqueId, 
                           by=list(dataArrived()$Appt.DateYear), FUN=NROW)
    
    names(pts.count) <- c("Date","Volume")
    pts.count$Date <- as.Date(pts.count$Date, format="%Y-%m-%d")
   
    ggplot(pts.count,  aes(x=Date, y=Volume))+
      geom_line(color="midnightblue")+
      geom_point(color="midnightblue")+
      scale_x_date(breaks = "month", date_labels = "%b\n%Y")+
      ggtitle("Daily Patient Volume over Time")+
      theme_new_line()+
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "16"))
    
  })
  
  # Total Monthly Patient Volume
  output$volume2 <- renderPlot({
    
    pts.by.month <- aggregate(dataArrived()$uniqueId, 
                           by=list(dataArrived()$Appt.MonthYear), FUN=NROW)
    
    names(pts.by.month) <- c("Month","Volume")
    pts.by.month$Volume <- as.numeric(pts.by.month$Volume)
    pts.by.month$Month <- as.yearmon(pts.by.month$Month, format="%b-%Y")
    pts.by.month$Month <- as.Date(pts.by.month$Month, format="%b-%Y")
    pts.by.month <- pts.by.month[order(pts.by.month$Month),]
    
    ggplot(pts.by.month, aes(x=Month, y=Volume))+
      geom_bar(stat="identity",fill="midnightblue")+
      scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y")+
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
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    pts.dist$Month <- as.yearmon(pts.dist$Month, format="%b-%Y")
    pts.dist$Month <- as.Date(pts.dist$Month, format="%b-%Y")
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
                          by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Month) %>%
      summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    
    pts.dist.summary <- 
      pts.dist.summary[order(as.yearmon(pts.dist.summary$Month,format="%b-%Y")),]
    
    pts.dist.summary %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=F, position="center", font_size = 15) %>%
      row_spec(0, bold=T) %>%
      column_spec(1, bold=T, width = "3cm")
    
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
      summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())

    pts.dist.summary <- pts.dist.summary[match(daysOfWeek.options,pts.dist.summary$Day),]
    pts.dist.summary <- pts.dist.summary[complete.cases(pts.dist.summary),]

    pts.dist.summary %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=F, position="center", font_size = 15) %>%
      row_spec(0, bold=T) %>%
      column_spec(1, bold=T, width = "3cm")

  }
  
  
  # Display graphs for KPIs selected





  #render the plot when x=age, gender=gender_neutral, sample=world
  # output$kpiGraphs1<-renderPlot({
  #   validate(need(input$kpiFreq == 1 & input$kpiTrend == 1 & input$selectedKPIs %in% c("Volume"), message=FALSE))
  # 
  #   pts.dist <- aggregate(dataArrived()$uniqueId,
  #                         by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
  # 
  #   names(pts.dist) <- c("Month","Date","Day","Volume")
  # 
  #   ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
  #     geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
  #     stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
  #     ggtitle("Daily Patient Volume Distribution by Day of Week")+
  #     theme_new_line()+
  #     theme(
  #       legend.position = "none",
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #       axis.text.y = element_text(size = "16"))
  # 
  # })
  
  #render the plot when x=age, gender=gender_neutral, sample=world
  # output$kpiGraphs2<-renderPlot({
  #   validate(need(input$kpiFreq == 1 & input$kpiTrend == 1 & input$selectedKPIs %in% c("Appointment Status"), 'Select filters!'))
  #   
  #   pts.dist <- aggregate(dataArrived()$uniqueId, 
  #                         by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date, dataArrived()$Appt.Day), FUN=NROW)
  #   
  #   names(pts.dist) <- c("Month","Date","Day","Volume")
  #   
  #   ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
  #     geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+ 
  #     stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
  #     ggtitle("Daily Patient Volume Distribution by Day of Week")+
  #     theme_new_line()+
  #     theme(
  #       legend.position = "none",
  #       axis.title.y = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.text.x = element_text(size = "16", vjust=0.5, angle = 0),
  #       axis.text.y = element_text(size = "16"))
  #   
  # })
  
  # ### [3. ] Day of Visit Tab -----------------------------------------------------------------------------------------------------------
  # 
  # # Patient flow by frequency count
  # output$vsm_freqCount <- renderGrViz(
  #   process_map(ex_patients, type = frequency("absolute"))
  # )
  # 
  # # Patient flow by relative frequency (%) (Total count of activity / Total count of prior activity)
  # output$vsm_freqPerc <- renderGrViz(
  #   process_map(ex_patients, type = frequency("relative_case"))
  # )
  # 
  # # Patient flow by duration (average)
  # output$vsm_durAvg <- renderGrViz(
  #   process_map(ex_patients, performance(FUN=mean, "hours"))
  # )
  # 
  # # Patient flow by duration (median)
  # output$vsm_durMed<- renderGrViz(
  #   process_map(ex_patients, performance(FUN=median, "hours"))
  # )
  # 
  # 
  # 
  # 
  
  
  

  
  ### [3. ] Data Tab Output -----------------------------------------------------------------------------------------------------------
  dataDisplay <- reactive({
    groupByFilters(data.subset.new[,c("Campus","Campus.Specialty","Department","Provider","MRN","Appt.DTTM","Appt.Day","Appt.Type","Appt.Status")],
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedProvider,
                   input$dateRange [1], input$dateRange[2], input$daysOfWeek)
  })
  
  output$dTableAll <- DT::renderDataTable({
    DT::datatable(dataDisplay())
  })
  
  
} # Close server 

shinyApp(ui, server)






       
      