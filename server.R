
server <- function(input, output, session) {

#ObserveEvent for access tab
  
observeEvent(input$selectedCampus_access,{
    if(is.null(input$filter_list)){
      if(!is.null(input$selectedCampus_access)){
        
      campus <- input$selectedCampus_access
      
      specialty_choices <-  access_tbl %>% filter(CAMPUS %in% campus) %>% 
        select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
      specialty_choices <- sort(specialty_choices$CAMPUS_SPECIALTY, na.last= T)
      updatePickerInput(session,
                        inputId = "selectedSpecialty_access",
                        choices = specialty_choices,
                        selected = specialty_choices[1]
      )
    }
  }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  
  observeEvent(list(input$selectedSpecialty_access, 
                    input$selectedCampus_access), {
    if(is.null(input$filter_list)) {
      if(!is.null(input$selectedSpecialty_access)){
        if(!is.null(input$selectedCampus_access)){
        campus <- input$selectedCampus_access
        specialty <- input$selectedSpecialty_access
        
        department_choices <- access_tbl %>% filter(CAMPUS %in% campus  & 
                                         CAMPUS_SPECIALTY %in% specialty) %>%
                                          select(DEPARTMENT)  %>%
          mutate(DEPARTMENT= unique(DEPARTMENT)) %>% collect()
        
        department_choices <- sort(department_choices$DEPARTMENT, na.last= T)
        
        updatePickerInput(session,
                          inputId = "selectedDepartment_access",
                          choices = department_choices,
                          selected = department_choices
        )
      }
    }
    } 
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
 
  
  observeEvent(list(input$selectedDepartment_access),{
    if(is.null(input$filter_list)){
      if(!is.null(input$selectedDepartment_access)){

        campus <- input$selectedCampus_access
        specialty <- input$selectedSpecialty_access
        department <- input$selectedDepartment_access

        print("prov")
        provider_choices <- access_tbl %>%
          filter(CAMPUS %in% campus  &
                   CAMPUS_SPECIALTY %in% specialty  &
                   DEPARTMENT %in% department ) %>%
          select( PROVIDER)  %>%
          mutate(PROVIDER= unique(PROVIDER)) %>% collect()

        print("prov1")

        provider_choices <- sort(provider_choices$PROVIDER, na.last= T)
        updatePickerInput(session,
                          inputId = "selectedProvider_access",
                          choices = provider_choices,
                          selected = provider_choices
        )

      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)

  
  observeEvent(input$selectedResource_access,{
    if(is.null(input$filter_list)||
       !is.null(input$selectedResource_access)){

        campus <- input$selectedCampus_access
        specialty <- input$selectedSpecialty_access
        department <- input$selectedDepartment_access
        
        provider_choices <- access_tbl %>%
          filter(CAMPUS %in% campus  &
                   CAMPUS_SPECIALTY %in% specialty  &
                   DEPARTMENT %in% department ) %>%
          select( PROVIDER)  %>%
          mutate(PROVIDER= unique(PROVIDER)) %>% collect()

        provider_choices <- sort(provider_choices$PROVIDER, na.last= T)
        print("resource")

        updatePickerInput(session,
                          inputId = "selectedProvider_access",
                          choices = provider_choices,
                          selected = provider_choices
        )
      }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)


  
  
  
  observeEvent(input$selectedProvider_access, {
    if(is.null(input$filter_list)){
      if(!is.null(input$selectedProvider_access)){
        print("1-1")
        campus <- input$selectedCampus_access
        specialty <- input$selectedSpecialty_access
        department <- input$selectedDepartment_access
        resource <- input$selectedResource_access
        provider <- input$selectedProvider_access
        
        visit_choices <- access_tbl %>% filter(CAMPUS %in% campus & 
                                               CAMPUS_SPECIALTY %in% specialty & 
                                               DEPARTMENT %in% department  &
                                               PROVIDER %in% provider) %>% 
          select( VISIT_METHOD)  %>% 
          mutate(VISIT_METHOD= unique(VISIT_METHOD)) %>% collect()
        print("2-1")
        visit_choices <- sort(visit_choices$VISIT_METHOD, na.last= T)
        
        updatePickerInput(session,
                          inputId = "selectedVisitMethod_access",
                          choices = visit_choices,
                          selected = visit_choices
        )
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(list(input$selectedVisitMethod_access,
                    input$selectedProvider_access), {
    if(is.null(input$filter_list)){
      if(!is.null(input$selectedVisitMethod_access)){
        if(!is.null(input$selectedProvider_access)){
        
        campus <- input$selectedCampus_access
        specialty <- input$selectedSpecialty_access
        department <- input$selectedDepartment_access
        resource <- input$selectedResource_access
        provider <- input$selectedProvider_access
        visit_method <- input$selectedVisitMethod_access
        
        prc_choices <-  access_tbl %>% 
          filter(CAMPUS %in%  campus & 
                   CAMPUS_SPECIALTY %in% specialty & 
                   DEPARTMENT %in% department &
                   RESOURCES   %in% resource &
                   PROVIDER %in% provider &
                   VISIT_METHOD %in% visit_method) %>% 
          select(APPT_TYPE )  %>% 
          mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
      
        
        prc_choices <- sort(prc_choices$APPT_TYPE, na.last= T)
        updatePickerInput(session,
                          inputId = "selectedPRCName_access",
                          choices = prc_choices,
                          selected = prc_choices
        )
        }
      }
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  


# Reactive data for Access tab
dataArrived_access <- eventReactive(list(input$access_update_filters,input$update_filters1),{
  validate(
    need(input$selectedCampus_access != "", "Please select a Campus"),
    need(input$selectedSpecialty_access != "", "Please select a Specialty"),
    need(input$selectedDepartment_access != "", "Please select a Department"),
    need(input$selectedResource_access != "", "Please select a Resource"),
    need(input$selectedProvider_access != "", "Please select a Provider"),
    need(input$selectedVisitMethod_access != "", "Please select a Visit Method"),
    need(input$selectedPRCName_access != "", "Please select a Visit Type")
  )
  groupByFilters_Access(arrived_access_subset,
                         #bind_rows(kpi.all.data[arrived.data.rows,], future_access_data[future.data.rows,]),
                        input$selectedCampus_access, input$selectedSpecialty_access, input$selectedDepartment_access, input$selectedResource_access,
                        input$selectedProvider_access, input$selectedVisitMethod_access, input$selectedPRCName_access, 
                        input$dateRange_access[1], input$dateRange_access[2], input$daysOfWeek_access, input$excludeHolidays)
  
})


dataAll_access <- eventReactive(list(input$access_update_filters,input$update_filters1),{
  validate(
    need(input$selectedCampus_access != "", "Please select a Campus"),
    need(input$selectedSpecialty_access != "", "Please select a Specialty"),
    need(input$selectedDepartment_access != "", "Please select a Department"),
    need(input$selectedResource_access != "", "Please select a Resource"),
    need(input$selectedProvider_access != "", "Please select a Provider"),
    need(input$selectedVisitMethod_access != "", "Please select a Visit Method"),
    need(input$selectedPRCName_access != "", "Please select a Visit Type")
  )
  groupByFilters_Access(dataAll_access_subset,
                        input$selectedCampus_access, input$selectedSpecialty_access, input$selectedDepartment_access, input$selectedResource_access, 
                        input$selectedProvider_access,input$selectedVisitMethod_access, input$selectedPRCName_access, 
                        input$dateRange_access[1], input$dateRange_access[2], input$daysOfWeek_access, input$excludeHolidays)
}) 

dataArrivedNoShow_access <- eventReactive(list(input$access_update_filters,input$update_filters1),{
  validate(
    need(input$selectedCampus_access != "", "Please select a Campus"),
    need(input$selectedSpecialty_access != "", "Please select a Specialty"),
    need(input$selectedDepartment_access != "", "Please select a Department"),
    need(input$selectedResource_access != "", "Please select a Resource"),
    need(input$selectedProvider_access != "", "Please select a Provider"),
    need(input$selectedVisitMethod_access != "", "Please select a Visit Method"),
    need(input$selectedPRCName_access != "", "Please select a Visit Type")
  )
  groupByFilters_Access(data_arrived_noshow_subset,
                        #kpi.all.data[arrivedNoShow.data.rows,],
                        input$selectedCampus_access, input$selectedSpecialty_access, input$selectedDepartment_access, 
                        input$selectedResource_access,
                        input$selectedProvider_access,
                        input$selectedVisitMethod_access, input$selectedPRCName_access, 
                        input$dateRange_access[1], input$dateRange_access[2], input$daysOfWeek_access, input$excludeHolidays)
})




### Access Section

output$newpatients <- renderText({
  paste0("Based on data from ", input$dateRange_access[1]," to ", input$dateRange_access[2], 
         " for ", paste(sort(input$selectedCampus_access), collapse = ', '))
})



### [3. ] Access Tab ------------------------------------------------------------------------------------------------------------------

# New Patient Ratio by Department
output$newPtRatioByDept <- renderPlot({
  data <- dataArrived_access()
  #data <- arrived_access_subset %>% filter(CAMPUS== "MSQ", CAMPUS_SPECIALTY== "*No specialty") 
  
  
  data <- data %>% mutate(Appt.Made.MonthYear = TO_CHAR(APPT_MADE_DTTM, "YYYY-mm"))
  
  campus <- input$selectedCampus_access
  
 
  specialty_choices <- arrived_access_subset %>% filter(CAMPUS== campus) %>% 
    select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
  
  specialty_choices <- c(specialty_choices$CAMPUS_SPECIALTY)
  
  glob_data <<- data 
  
  
  newpatients.ratio <- data %>%
    group_by(Appt.Made.MonthYear, NEW_PT3) %>%
    dplyr::summarise(Total = n()) %>% collect() %>%
    spread(NEW_PT3, Total)
  

  
  validate(need(nrow(newpatients.ratio)> 0, paste0("Specialties available for ", 
                          campus, " are ", paste0(specialty_choices, collapse= ", ") , "." )))
                                                  
  
  col_names <- c("Appt.Made.MonthYear", "TRUE", "FALSE")
  col_index <- which(!(col_names %in% names(newpatients.ratio)))
  column_to_add <- col_names[col_index]
  
  if(length(column_to_add)>0){
    for (i in 1:length(column_to_add)){
    newpatients.ratio[[column_to_add[i]]] <- 0
  }}
  
  
  newpatients.ratio$`<NA>` <- NULL
  newpatients.ratio[is.na(newpatients.ratio)] <- 0
  
  
  
  
  newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
  #newpatients.ratio$Appt.MonthYear <- as.Date(newpatients.ratio$Appt.MonthYear, format="%Y-%m") ## Create date-year column
  
  new_test <<- newpatients.ratio
  
  
  ggplot(newpatients.ratio, aes(x=Appt.Made.MonthYear, y=ratio, group=1)) +
    geom_bar(stat = "identity", width = 0.8, fill = "#221f72") +
    labs(x=NULL, y=NULL,
         #title = "New Patient Ratio Trending over Time",
         title = "Monthly New Patient Ratio",
        # subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2]))
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("none")+
    theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(newpatients.ratio$ratio)*1.5)) +
    stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = Appt.Made.MonthYear), geom="text", color="black", 
                 size=5, fontface="bold.italic")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  # scale_x_date(breaks = "day", date_labels = "%Y-%m", date_breaks = "1 week",
  #              date_minor_breaks = "1 day", expand = c(0, 0.6))
  
})


# New Patient Ratio by Provideer
output$newPtRatioByProv <- renderPlot({
  data <- dataArrived_access() 
  #data <- arrived_access_subset %>% filter(CAMPUS== "MSQ", CAMPUS_SPECIALTY== "*No specialty")
  
  # data <- bind_rows(kpi.all.data[arrived.data.rows,], future_access_data[future.data.rows,]) %>%
  #            filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
  # data <- kpi.all.data[arrived.data.rows,] %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
  
  #data <- data %>% mutate(Appt.Made.MonthYear = format(as.Date(APPT_MADE_DTTM, format="%m/%d/%Y"), "%Y-%m"))
  
  data <- data %>% mutate(Appt.Made.MonthYear = TO_CHAR(APPT_MADE_DTTM, "YYYY-mm"))
  
  campus <- input$selectedCampus_access
  
  
  specialty_choices <- arrived_access_subset %>% filter(CAMPUS== campus) %>% 
    select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
  
  specialty_choices <- c(specialty_choices$CAMPUS_SPECIALTY)
  
  
  newpatients.ratio <- data %>%
    group_by(PROVIDER, Appt.Made.MonthYear, NEW_PT3) %>%
    dplyr::summarise(Total = n()) %>% collect() %>%
    spread(NEW_PT3, Total) 
  
  validate(need(nrow(newpatients.ratio)> 0, paste0("Specialties available for ", 
                                                   campus, " are ", paste0(specialty_choices, collapse= ", ") , "." )))
  
  col_names <- c("PROVIDER", "Appt.Made.MonthYear", "TRUE", "FALSE")
  col_index <- which(!(col_names %in% names(newpatients.ratio)))
  column_to_add <- col_names[col_index]
  
  if(length(column_to_add)>0){
    for (i in 1:length(column_to_add)){
    newpatients.ratio[[column_to_add[i]]] <- 0
  }}
  
  
  newpatients.ratio[is.na(newpatients.ratio)] <- 0
  
  newpatients.ratio$ratio <- round(newpatients.ratio$`TRUE` / (newpatients.ratio$`FALSE` + newpatients.ratio$`TRUE`),2)
  #newpatients.ratio$Appt.MonthYear <- as.Date(paste0(newpatients.ratio$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
  
  ggplot(newpatients.ratio, aes(x=Appt.Made.MonthYear, y=ratio, group = PROVIDER)) +
    geom_line(aes(color=PROVIDER), size=1) +
    # scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
    scale_color_MountSinai("main", reverse = TRUE, labels = wrap_format(25))+
    labs(x=NULL, y=NULL, 
         title = "New Patient Ratio Over Time by Provider",
         subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2]))
    )+
    theme_new_line()+
    theme_bw()+
    graph_theme("bottom")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,max(newpatients.ratio$ratio)*1.5))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    geom_point(aes(color=PROVIDER), size = 3.2)
  # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
  #              date_minor_breaks = "1 day", expand = c(0, 0.6))
  
})

# New Patient Wait Time
output$newPtWaitTimeByDept <- renderPlot({
  data <- dataAll_access() 
  # data <- dataAll_access_subset %>% filter(CAMPUS == "MSQ" & CAMPUS_SPECIALTY==  "*No specialty")
  #data <- bind_rows(kpi.all.data[all.data.rows,], future_access_data) %>% filter(Campus == "MSUS")
  
  #data <- data %>% mutate(wait.time = APPT_DTTM - APPT_MADE_DTTM)
 
  #data$wait.time <- as.numeric(round(difftime(data$APPT_DTTM, data$APPT_MADE_DTTM,  units = "days"),2))
  
  #data <- data %>% mutate(Appt.Made.MonthYear = format(as.Date(APPT_MADE_DTTM, format="%m/%d/%Y"), "%Y-%m"))
  data <- data %>% mutate(Appt.Made.MonthYear = TO_CHAR(APPT_MADE_DTTM, "YYYY-mm")) # %>% collect()
  
  test_wait_time <<- data
  
  campus <- input$selectedCampus_access
  
  
  specialty_choices <- arrived_access_subset %>% filter(CAMPUS== campus) %>% 
    select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
  
  specialty_choices <- c(specialty_choices$CAMPUS_SPECIALTY)
  
  
  waitTime <- data %>%
    filter(WAIT_TIME >= 0) %>%
    group_by(Appt.Made.MonthYear, NEW_PT3) %>%
    dplyr::summarise(medWaitTime = round(median(WAIT_TIME , na.rm = TRUE))) %>%
    filter(NEW_PT3 %in% c("TRUE","FALSE")) %>% collect()
  
  validate(need(nrow(waitTime)> 0, paste0("Specialties available for ", 
                                                   campus, " are ", paste0(specialty_choices, collapse= ", ") , "." )))
  
  
  waitTime$NEW_PT3 <- ifelse(waitTime$NEW_PT3 == TRUE, "New","Established")
  #waitTime$Appt.MonthYear <- as.Date(waitTime$Appt.MonthYear, format="%Y-%m-%d") ## Create date-year column
  
  waitTime <- waitTime %>% spread(NEW_PT3, medWaitTime)
  
  col_names <- c("Appt.Made.MonthYear", "Established", "New")
  col_index <- which(!(col_names %in% names(waitTime)))
  column_to_add <- col_names[col_index]
  
  if(length(column_to_add)> 0){
     for (i in 1:length(column_to_add)){
       waitTime[[column_to_add[i]]] <- 0
  }}
  
  
  #waitTime$`New Patient Target <= 14` <- 14
  waitTime[is.na(waitTime)] <- 0
  waitTime <- waitTime %>% gather(variable, value, 2:3)
  target <- 14
  
  wait_test <<- waitTime 
  
  ggplot(waitTime, aes(x=Appt.Made.MonthYear, y=value, fill = variable))+
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
         title = "Monthly Median Wait Time to New* and Established Appointment",
         subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2])),
         caption = "*New patients defined by CPT codes (level of service)."
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
  data <- dataAll_access() 
  #data <- dataAll_access_subset %>% filter(CAMPUS == "MSQ" & CAMPUS_SPECIALTY==  "*No specialty")
  # data <- all.data %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
  #data <- bind_rows(kpi.all.data[all.data.rows,], future_access_data) %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
  
  test3 <<- data 
  #data <- data %>% mutate(Appt.Made.MonthYear = format(as.Date(APPT_MADE_DTTM, format="%m/%d/%Y"), "%Y-%m"))
  data <- data %>% mutate(Appt.Made.MonthYear = TO_CHAR(APPT_MADE_DTTM, "YYYY-mm"))
  
  campus <- input$selectedCampus_access
  
  
  specialty_choices <- dataAll_access_subset %>% filter(CAMPUS== campus) %>% 
    select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
  
  specialty_choices <- c(specialty_choices$CAMPUS_SPECIALTY)
  
  
  #data$wait.time <- as.numeric(round(difftime(data$APPT_DTTM, data$APPT_MADE_DTTM,  units = "days"),2))
  data <- data %>% mutate(wait.time = APPT_DTTM - APPT_MADE_DTTM)
  
  waitTime <- data %>%
    filter(wait.time >= 0) %>%
    group_by(PROVIDER, Appt.Made.MonthYear, NEW_PT3) %>%
    dplyr::summarise(medWaitTime = round(median(wait.time, na.rm = TRUE))) %>% collect()
  
  
  # validate(need(nrow(waitTime)> 0, paste0("Specialties available for ", 
  #                                         campus, " are ", paste0(specialty_choices, collapse= ", ") , "." )))
  # 
  
  
  waitTime$NEW_PT3 <- ifelse(waitTime$NEW_PT3 == TRUE, "New","Established")
  #waitTime$Appt.MonthYear <- as.Date(paste0(waitTime$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
  
  waitTime <- waitTime %>% spread(NEW_PT3, medWaitTime)
  
  validate(need("Established" %in% names(waitTime), "There is No Previous Patient for this Capmus"))
  
  # col_names <- c("PROVIDER", "Appt.Made.MonthYear", "Established", "New")
  # col_index <- which(!(col_names %in% names(waitTime)))
  # column_to_add <- col_names[col_index]
  # 
  # if(length(column_to_add)>0){
  #   for(i in 1:length(column_to_add)){
  #     waitTime[[column_to_add[i]]] <- 0
  # }}
  
  
  waitTime[is.na(waitTime)] <- 0
  waitTime <- waitTime %>% gather(variable, value, 3:4)
  ggplot(waitTime %>% filter(variable == "Established"), aes(x=Appt.Made.MonthYear, y=value, group=PROVIDER)) +
    geom_line(aes(color=PROVIDER), size=1) +
    # geom_hline(yintercept=14, linetype="dashed", color = "red", size=1)+
    scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
    labs(x=NULL, y=NULL, 
         title = "Median Wait Time to New* and Established Appointment Over Time by Provider",
         subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2])),
         caption = "*New patients defined by CPT codes (level of service).")+
    theme_new_line()+
    theme_bw()+
    graph_theme("bottom")+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    scale_y_continuous(expand = c(0,0), limits = c(0,max(waitTime$value)*1.5))+
    # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
    geom_point(aes(color=PROVIDER), size = 3.2)
})


# New Patient Wait Time
output$newPtApptSourceByDept <- renderPlot({
  data <- dataArrived_access() 
  #data <- arrived_access_subset %>% filter(CAMPUS == "MSQ" & CAMPUS_SPECIALTY==  "Neurology") 
  #data <- bind_rows(kpi.all.data[arrived.data.rows,], future_access_data[future.data.rows,])
  # data <- kpi.all.data[arrivedNoShow.data.rows,]
  
  
  campus <- input$selectedCampus_access
  
  
  specialty_choices <- arrived_access_subset %>% filter(CAMPUS== campus) %>% 
    select( CAMPUS_SPECIALTY)  %>% mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
  
  specialty_choices <- c(specialty_choices$CAMPUS_SPECIALTY)
 
  
  newpatients.ratio <- data %>%
    group_by(APPT_SOURCE_NEW, NEW_PT3) %>%
    filter(NEW_PT3 == "TRUE") %>%
    dplyr::summarise(Total = n()) %>% collect()
  
  
  validate(need(nrow(newpatients.ratio)> 0, paste0("Specialties available for ",
                                          campus, " are ", paste0(specialty_choices, collapse= ", ") , "." )))

  

  #newpatients.ratio$APPT_SOURCE_NEW[which(newpatients.ratio$APPT_SOURCE_NEW == "Other")] <- "Practice"
  
  newpatients.ratio$ratio <- round(newpatients.ratio$Total / sum(newpatients.ratio$Total), 2)
  
  
  newRatio <-
    ggplot(newpatients.ratio, aes(x=factor(APPT_SOURCE_NEW, levels = c("Practice","Access Center","My MountSinai/ MyChart","StayWell","Zocdoc", "FindADoc")), 
                                  y=ratio, group=APPT_SOURCE_NEW, fill=APPT_SOURCE_NEW)) +
    geom_bar(stat="identity", width = 0.8) +
    coord_flip() +
    scale_fill_MountSinai('purple')+
    labs(x=NULL, y=NULL,
         title = "New Patient Source*",
         subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2]),
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
  

  #data$wait.time <- as.numeric(round(difftime(data$APPT_DTTM, data$APPT_MADE_DTTM,  units = "days"),2))
  data <- data %>% mutate(wait.time = APPT_DTTM - APPT_MADE_DTTM)
  
  waitTime <- data %>%
    filter(wait.time >= 0) %>%
    group_by(APPT_SOURCE_NEW, NEW_PT3) %>%
    dplyr::summarise(medWaitTime = round(median(wait.time, na.rm = TRUE))) %>%
    filter(NEW_PT3 == "TRUE") %>% collect() 
  waitTime$target <- 14
  
  waitTime$APPT_SOURCE_NEW[which(waitTime$APPT_SOURCE_NEW == "Other")] <- "Practice"
  
  newWaitTime <-
    ggplot(waitTime, aes(x=factor(APPT_SOURCE_NEW, levels = c("Practice","Access Center","My MountSinai/ MyChart","StayWell","Zocdoc", "FindADoc")), 
                         y=medWaitTime, group=APPT_SOURCE_NEW, fill=APPT_SOURCE_NEW)) +
    geom_bar(stat="identity", width = 0.8) +
    geom_hline(aes(yintercept=target), linetype="dashed", color = "red", size=1)+
    scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.3))+
    coord_flip() +
    scale_fill_MountSinai('pink')+
    labs(x=NULL, y=NULL, 
         title = "Wait Time* to New Appointment",
         # subtitle = paste0("Based on data from ",isolate(input$dateRange_access[1])," to ",isolate(input$dateRange_access[2]),
         #                   "\nWait Time = (Scheduled Appt Date - Appt Made Date)"),
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
  
  data.noShow <- dataArrivedNoShow_access() 
  
  # data.noShow <- data_arrived_noshow_subset %>% filter(CAMPUS== "MSUS" & CAMPUS_SPECIALTY=="Allergy") %>% collect()
  
  test_noshow <<- data.noShow
  
  noShows <- data.noShow %>%
    #filter(CAMPUS_SPECIALTY== "Cardiology") %>%
    filter(NEW_PT3 == "TRUE") %>%
    group_by(APPT_SOURCE_NEW, APPT_STATUS) %>%
    dplyr::summarise(Total = n()) %>% collect() %>%
    spread(APPT_STATUS, Total) 
  
  validate(need("No Show" %in% names(noShows), "There is no No Show for this selection."))
  
  
  # col_names <- c("APPT_SOURCE_NEW", "Arrived", "No Show" )
  # col_index <- which(!(col_names %in% names(noShows)))
  # column_to_add <- col_names[col_index]
  # 
  # 
  # 
  # if(length(column_to_add)>0){
  #   for (i in 1:length(column_to_add)){
  #      noShows[[column_to_add[i]]] <- 0
  # }}
  
  noShows[is.na(noShows)] <- 0
  
  noShows$`No Show Perc` <- round(noShows$`No Show`/(noShows$Arrived + noShows$`No Show`),2)
  
  noShows$APPT_SOURCE_NEW[which(noShows$APPT_SOURCE_NEW == "Other")] <- "Practice"
  
  
  noShows$APPT_SOURCE_NEW <- ifelse(noShows$APPT_SOURCE_NEW == "Other", "Practice", noShows$APPT_SOURCE_NEW)
  

  
  
  newNoShow <-
    
    ggplot(noShows, aes(x=factor(APPT_SOURCE_NEW, levels =  c("Practice","Access Center","My MountSinai/ MyChart","StayWell","Zocdoc", "FindADoc")), 
                        y=`No Show Perc`, group=APPT_SOURCE_NEW, fill=APPT_SOURCE_NEW)) +
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



}
