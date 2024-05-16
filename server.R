server <- function(input, output, session) {
  callModule(profvis_server, "profiler")
  
  # observeEvent(input$dropdownbutton3, {
  #   print("clcick button")
  #   saved_filter_choices <- return_saved_choices(df_choices, "FILTER_NAME")
  #   print("filter observe")
  #   updatePickerInput(session,
  #                     inputId = "filter_list",
  #                     selected = NULL,
  #                     choices = saved_filter_choices
  #   )
  # }, once = TRUE)
  observeEvent(input$update_filters1,{
    filter_name <- input$filter_list
    format <- "YYYY-MM-DD HH24:MI:SS"
    date_start <- input$dateRange[1]
    date_end <- input$dateRange[2]
    #filter_name <- "MSUS_CARDIO_ALLERGY"
    df_choices <- ambulatory_filters_tbl %>% filter(FILTER_NAME == filter_name) %>% collect()
    
    campus_choices <- return_saved_choices(df_choices, "CAMPUS")
    
    updated_campus_choices <- filters %>% select(CAMPUS) %>% mutate(CAMPUS = unique(CAMPUS)) %>% collect()
    updated_campus_choices <- sort(updated_campus_choices$CAMPUS, na.last = T)
    
    
    updatePickerInput(session,
                      inputId = "selectedCampus",
                      choices = updated_campus_choices,
                      selected = campus_choices
    )
    
    
    specialty_choices <- return_saved_choices(df_choices, "SPECIALTY")
    updated_specialty_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                            TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                            TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
                                select(CAMPUS_SPECIALTY) %>% 
                                mutate(CAMPUS_SPECIALTY = unique(CAMPUS_SPECIALTY)) %>% 
                                collect()
    updated_specialty_choices <- sort(updated_specialty_choices$CAMPUS_SPECIALTY, na.last = T)
    
    updatePickerInput(session,
                      inputId = "selectedSpecialty",
                      choices = updated_specialty_choices,
                      selected = specialty_choices
    )
    
    dept_choices <- return_saved_choices(df_choices, "DEPARTMENT")
    updated_dept_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                       CAMPUS_SPECIALTY %in% specialty_choices,
                                                            TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                            TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
      select(DEPARTMENT) %>% 
      mutate(DEPARTMENT = unique(DEPARTMENT)) %>% 
      collect()
    updated_dept_choices <- sort(updated_dept_choices$DEPARTMENT, na.last = T)
    
    updatePickerInput(session,
                      inputId = "selectedDepartment",
                      choices = updated_dept_choices,
                      selected = dept_choices
    )
    
    resource_choices <- return_saved_choices(df_choices, "RESOURCES")
    
    updatePickerInput(session,
                      inputId = "selectedResource",
                      choices = c("Provider","Resource"),
                      selected = resource_choices
    )
    
    provider_choices <- return_saved_choices(df_choices, "PROVIDER")
    updated_provider_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                       CAMPUS_SPECIALTY %in% specialty_choices,
                                                       DEPARTMENT %in% dept_choices,
                                                       RESOURCES %in% resource_choices,
                                                       TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                       TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
      select(PROVIDER) %>% 
      mutate(PROVIDER = unique(PROVIDER)) %>% 
      collect()
    updated_provider_choices <- sort(updated_provider_choices$PROVIDER, na.last = T)
    
    updatePickerInput(session,
                      inputId = "selectedProvider",
                      choices = updated_provider_choices,
                      selected = provider_choices
    )
    
    visit_method_choices <- return_saved_choices(df_choices, "VISIT_METHOD")
    
    if(length(provider_choices) >= 1000) {
      updated_visit_method_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                             CAMPUS_SPECIALTY %in% specialty_choices,
                                                             DEPARTMENT %in% dept_choices,
                                                             RESOURCES %in% resource_choices,
                                                             TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                             TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
        select(VISIT_METHOD) %>% 
        mutate(VISIT_METHOD = unique(VISIT_METHOD)) %>% 
        collect()
    } else {
      updated_visit_method_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                                 CAMPUS_SPECIALTY %in% specialty_choices,
                                                                 DEPARTMENT %in% dept_choices,
                                                                 RESOURCES %in% resource_choices,
                                                                 PROVIDER %in% provider_choices,
                                                                 TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                                 TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
        select(VISIT_METHOD) %>% 
        mutate(VISIT_METHOD = unique(VISIT_METHOD)) %>% 
        collect()
    }
    updated_visit_method_choices <- sort(updated_visit_method_choices$VISIT_METHOD, na.last = T)
    
    updatePickerInput(session,
                      inputId = "selectedVisitMethod",
                      choices = updated_visit_method_choices,
                      selected = visit_method_choices
    )
    
    visit_type_choices <- return_saved_choices(df_choices, "VISIT_TYPE")
    
    if(length(provider_choices) >= 1000) {
      updated_visit_type_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                                 CAMPUS_SPECIALTY %in% specialty_choices,
                                                                 DEPARTMENT %in% dept_choices,
                                                                 RESOURCES %in% resource_choices,
                                                                 VISIT_METHOD %in% visit_method_choices,
                                                                 TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                                 TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
        select(APPT_TYPE) %>% 
        mutate(APPT_TYPE = unique(APPT_TYPE)) %>% 
        collect()
    } else {
      updated_visit_type_choices <- historical.data %>% filter(CAMPUS %in% campus_choices,
                                                                 CAMPUS_SPECIALTY %in% specialty_choices,
                                                                 DEPARTMENT %in% dept_choices,
                                                                 RESOURCES %in% resource_choices,
                                                                 PROVIDER %in% provider_choices,
                                                                 VISIT_METHOD %in% visit_method_choices,
                                                                 TO_DATE(date_start, format) <= APPT_DATE_YEAR,
                                                                 TO_DATE(date_end, format) >= APPT_DATE_YEAR) %>%
        select(APPT_TYPE) %>% 
        mutate(APPT_TYPE = unique(APPT_TYPE)) %>% 
        collect()
    }
    updated_visit_type_choices <- sort(updated_visit_type_choices$APPT_TYPE, na.last = T)
    
    updatePickerInput(session,
                      inputId = "selectedPRCName",
                      choices = updated_visit_type_choices,
                      selected = visit_type_choices
    )
    
    days_choices <- return_saved_choices(df_choices, "DAYS")
    
    updatePickerInput(session,
                      inputId = "daysOfWeek",
                      choices = daysOfWeek.options,
                      selected = days_choices
    )
    
    holiday_choices <- return_saved_choices(df_choices, "HOLIDAY")
    
    updatePickerInput(session,
                      inputId = "excludeHolidays",
                      choices = unique(holid$holiday),
                      selected = holiday_choices
    )
    saved_filter_choices <- ambulatory_filters_tbl %>% summarise(choices = unique(FILTER_NAME)) %>% collect()
    saved_filter_choices <- sort(saved_filter_choices$choices, na.last = T)

    print("filter observe")
    updatePickerInput(session,
                      inputId = "filter_list",
                      selected = NULL,
                      choices = saved_filter_choices
    )
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  ##Test for volume
  dataArrived_volume <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_volume(volume_arrived_rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource,
                   input$selectedVisitMethod,
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  #callModule(profvis_server, "profiler")
  observeEvent(input$update_filters,{print(is.null(input$update_filters))})
  
  user <- reactive({
    #session$user
    "Filters"
  })
  
  filter_choices <- eventReactive(input$save_filters,{
    user <- user()
    filter_path <- paste0(filter_path, "/", user)
    file_path_sans_ext(list.files(path = filter_path, pattern = "*.csv"))
  })
  
  
  # observeEvent(input$remove_filters,{
  #   if(is.null(input$filter_list)){
  #     shinyalert("No preset selected.", type = "error")
  #     #showNotification("Please provider a name", duration = 5, type = "error")
  #   }else{
  #     
  #     user <- user()
  #     filter_path_full <- paste0(filter_path, "/", user,"/",input$filter_list,".csv")
  #     filter_path_half <- paste0(filter_path, "/", user)
  #     if (file.exists(filter_path_full)) {
  #       #Delete file if it exists
  #       file.remove(filter_path_full)
  #     }
  #     
  #     filter_choices <- file_path_sans_ext(list.files(path = filter_path_half, pattern = "*.csv"))
  #     updatePickerInput(session, "filter_list", choices = filter_choices)
  #   }
  #   
  #   print(input$filter_list)
  #   
  #   
  #   
  # })
  
  # observeEvent(input$sbm,{
  #   user <- user()
  #   filter_path_full <- paste0(filter_path, "/", user)
  #   dir.create(file.path(filter_path, user), showWarnings = FALSE)
  #   filter_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
  #   updatePickerInput(session, "filter_list", choices = filter_choices)
  # }, once = TRUE)
  
  
  ## Access Module
  selected_campus <- CampusServer("selectedCampus")
  selected_specialty <- SpecialtyServer("selectedSpecialty", data = filters, campus = selected_campus)
  selected_department <- DepartmentServer("selectedDepartment", data = filters,
                                          campus = selected_campus, specialty = selected_specialty)
  
  selected_resource <- ResourceServer("selectedResource")
  selected_provider <- ProviderServer("selectedProvider", data = filters,
                                      campus = selected_campus, 
                                      specialty = selected_specialty,
                                      department = selected_department, 
                                      resource = selected_resource)
  
  selected_visitmethod <- VisitMethodServer("selectedVisitMethod", data = filters,
                                            campus = selected_campus, 
                                            specialty = selected_specialty,
                                            department = selected_department, 
                                            resource = selected_resource,
                                            provider = selected_provider)
  
  selected_visittype <- VisitTypeServer("selectedPRCName", data = filters,
                                        campus = selected_campus, 
                                        specialty = selected_specialty,
                                        department = selected_department, 
                                        resource = selected_resource,
                                        provider = selected_provider,
                                        visit_method = selected_visitmethod)
  
  
  selected_dateRange <- DateServer("dateRange")
  selected_daysOfWeek <- WeekServer("daysOfWeek")
  selected_holiday <- HolidayServer("excludeHolidays")
  
  
  
  observeEvent(input$save_filters,{
    user <- user()
    
    print("collect")
    choices <- ambulatory_filters_tbl %>% summarise(choices_unique = unique(FILTER_NAME)) %>% collect()
    choices <- sort(choices$choices_unique, na.last = T)
    print("after collect")
   
    filter_name <- input$filter_name
    if(filter_name == ""){
      shinyalert("Please provide a name.", type = "error")
      #showNotification("Please provider a name", duration = 5, type = "error")
    } else if (filter_name %in% choices){
      shinyalert("The current name already exists, please provide a new one.", type = "error")
    } else{
      updateTextInput(session, "filter_name", value = "")
      print(filter_name)
      filter_path_full <- paste0(filter_path, "/", user)
      
      filter_df <- mapply(c, filter_name, input$selectedCampus, input$selectedSpecialty,
                          input$selectedDepartment, input$selectedResource,
                          input$selectedProvider, input$selectedVisitMethod,
                          input$selectedPRCName, input$daysOfWeek, input$excludeHolidays,
                          SIMPLIFY = TRUE)
      filter_df <- as.data.frame(t(filter_df), row.names = FALSE)
      colnames(filter_df) <- c("Name", "Campus", "Specialty", "Department", "Resource",
                               "Provider", "Visit Method", "Visit Type", "Days", "Holiday")
      # write.csv(filter_df, here::here(paste0(filter_path_full, "/" , input$filter_name, ".csv")), row.names = FALSE)
      # 
      # filter_list_choices <- file_path_sans_ext(list.files(path = filter_path_full, pattern = "*.csv"))
      write_filters_db(filter_df)
      
      filter_list_choices <- ambulatory_filters_tbl %>% summarise(choices = unique(FILTER_NAME)) %>% collect()
      filter_list_choices <- sort(filter_list_choices$choices, na.last = T)
      
      print("after collect")
      
      updatePickerInput(session,
                        inputId = "filter_list",
                        choices = filter_list_choices
      )
      
      filter_df
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
 
  
  # observeEvent(input$filter_list, {
  #   user <- user()
  #   
  #   filter_path <- paste0(filter_path, "/", user, "/", input$filter_list, ".csv")
  #   filter_df <- read_csv(filter_path)
  #   
  #   
  #   campus_selected <- unique(filter_df$Campus)
  #   specialty_selected <- unique(filter_df$Specialty)
  #   department_selected <- unique(filter_df$Department)
  #   resource_selected <- unique(filter_df$Resource)
  #   provider_selected <- unique(filter_df$Provider)
  #   visitMethod_selected <- unique(filter_df$`Visit Method`)
  #   visitType_selected <- unique(filter_df$`Visit Type`)
  #   day_selected <- unique(filter_df$Days)
  #   
  #   specialty_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% campus_selected, "Campus.Specialty"]))
  #   
  #   department_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% campus_selected &
  #                                                    kpi.all.data$Campus.Specialty %in% specialty_selected, "Department"]), na.last = TRUE)
  #   
  #   
  #   provider_choices <- sort(unique(kpi.all.data[
  #     kpi.all.data$Campus %in% campus_selected &
  #       kpi.all.data$Campus.Specialty %in% specialty_selected &
  #       kpi.all.data$Department %in% department_choices &
  #       kpi.all.data$Resource %in% resource_selected, "Provider"]), na.last = TRUE)
  #   
  #   visitMethod_choices <- sort(unique(kpi.all.data[
  #     kpi.all.data$Campus %in% campus_selected &
  #       kpi.all.data$Campus.Specialty %in% specialty_selected &
  #       kpi.all.data$Department %in% department_choices &
  #       kpi.all.data$Resource %in% resource_selected &
  #       kpi.all.data$Provider %in% provider_choices, "Visit.Method"]), na.last = TRUE)
  #   
  #   visitType_choices <- sort(unique(kpi.all.data[
  #     kpi.all.data$Campus %in% campus_selected &
  #       kpi.all.data$Campus.Specialty %in% specialty_selected &
  #       kpi.all.data$Department %in% department_choices &
  #       kpi.all.data$Resource %in% resource_selected &
  #       kpi.all.data$Provider %in% provider_choices &
  #       kpi.all.data$Visit.Method %in% visitMethod_choices, "Appt.Type"]), na.last = TRUE)
  #   
  #   
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedCampus",
  #                     selected = campus_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedSpecialty",
  #                     choices = specialty_choices,
  #                     selected = specialty_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedDepartment",
  #                     choices = department_choices,
  #                     selected = department_selected
  #   )
  #   
  #   updateCheckboxGroupButtons(session,
  #                              inputId = "selectedResource",
  #                              choices = c("Provider","Resource"),
  #                              checkIcon = list(
  #                                yes = icon("ok", lib = "glyphicon")),
  #                              selected = resource_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedProvider",
  #                     choices = provider_choices,
  #                     selected = provider_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedVisitMethod",
  #                     choices = visitMethod_choices,
  #                     selected = visitMethod_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "selectedPRCName",
  #                     choices = visitType_choices,
  #                     selected = visitType_selected
  #   )
  #   
  #   updatePickerInput(session,
  #                     inputId = "daysOfWeek",
  #                     choices = daysOfWeek.options,
  #                     selected = day_selected
  #   )
  #   
  #   
  # })
  
  
  observeEvent(input$selectedCampus,{
    if(is.null(input$filter_list) && !is.null(input$selectedCampus)){
      #specialty_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% input$selectedCampus, "Campus.Specialty"]))
      selected_campus <- input$selectedCampus
      
      specialty_choices <-  filters %>% filter(CAMPUS %in% selected_campus) %>% select( CAMPUS_SPECIALTY)  %>%
        mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
      specialty_choices <- sort(specialty_choices$CAMPUS_SPECIALTY, na.last = T)
      print("3")
      
      
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
    if(is.null(input$filter_list) && !is.null(input$selectedSpecialty)){
      # department_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))
      selected_campus <- input$selectedCampus
      selected_specialty <- input$selectedSpecialty
      
      department_choices <-  filters %>% filter(CAMPUS %in% selected_campus & 
                                                           CAMPUS_SPECIALTY %in% selected_specialty) %>% select(DEPARTMENT)  %>%
        mutate(DEPARTMENT= unique(DEPARTMENT)) %>% collect()
      department_choices <- sort(department_choices$DEPARTMENT, na.last = T)
      
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
    if(is.null(input$filter_list) && !is.null(input$selectedDepartment)){
      # provider_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
      #     kpi.all.data$Department %in% input$selectedDepartment &
      #     kpi.all.data$Resource %in% input$selectedResource, "Provider"]))
      
      selected_campus <- input$selectedCampus
      selected_specialty <- input$selectedSpecialty
      selected_department <- input$selectedDepartment
      selected_resource <- input$selectedResource
      
      provider_choices <-   #filters %>% 
        filters_table %>%
        filter(CAMPUS %in% selected_campus & 
                 CAMPUS_SPECIALTY %in% selected_specialty & 
                 DEPARTMENT %in% selected_department &
                 RESOURCES %in% selected_resource) %>% 
        #select(PROVIDER)  %>% 
        summarise(PROVIDER= unique(PROVIDER)) %>% collect()
      provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      
    
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  observeEvent(input$selectedCampus_slot,{
    if(is.null(input$filter_list) && !is.null(input$selectedCampus_slot)){
      #specialty_choices <- sort(unique(kpi.all.data[kpi.all.data$Campus %in% input$selectedCampus, "Campus.Specialty"]))
      selected_campus <- input$selectedCampus_slot
      
      specialty_choices <-  filters %>% filter(CAMPUS %in% selected_campus) %>% select( CAMPUS_SPECIALTY)  %>%
        mutate(CAMPUS_SPECIALTY= unique(CAMPUS_SPECIALTY)) %>% collect()
      specialty_choices <- sort(specialty_choices$CAMPUS_SPECIALTY, na.last = T)
      print("3")
      
      updatePickerInput(session,
                        inputId = "selectedSpecialty_slot",
                        choices = specialty_choices,
                        selected = specialty_choices
      )
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  observeEvent(input$selectedSpecialty_slot,{
    if(is.null(input$filter_list) && !is.null(input$selectedSpecialty_slot)){
      # department_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty, "Department"]))
      selected_campus <- input$selectedCampus_slot
      selected_specialty <- input$selectedSpecialty_slot
      
      department_choices <-  filters %>% filter(CAMPUS %in% selected_campus & 
                                                  CAMPUS_SPECIALTY %in% selected_specialty) %>% select(DEPARTMENT)  %>%
        mutate(DEPARTMENT= unique(DEPARTMENT)) %>% collect()
      department_choices <- sort(department_choices$DEPARTMENT, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedDepartment_slot",
                        choices = department_choices,
                        selected = department_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  observeEvent(input$selectedDepartment_slot,{
    if(is.null(input$filter_list) && !is.null(input$selectedDepartment_slot)){
      # provider_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
      #     kpi.all.data$Department %in% input$selectedDepartment &
      #     kpi.all.data$Resource %in% input$selectedResource, "Provider"]))
      
      selected_campus <- input$selectedCampus_slot
      selected_specialty <- input$selectedSpecialty_slot
      selected_department <- input$selectedDepartment_slot
      
      provider_choices <- filters %>% 
        filter(CAMPUS %in% selected_campus & 
                 CAMPUS_SPECIALTY %in% selected_specialty & 
                 DEPARTMENT %in% selected_department) %>% 
        summarise(PROVIDER= unique(PROVIDER)) %>% collect()
      provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      
      
      updatePickerInput(session,
                        inputId = "selectedProvider_slot",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  observeEvent(input$selectedResource, {
    if(is.null(input$filter_list) && !is.null(input$selectedResource)
    ){
      print("test provider")                              
      # provider_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
      #     kpi.all.data$Department %in% input$selectedDepartment &
      #     kpi.all.data$Resource %in% input$selectedResource, "Provider"]))
      selected_campus <- input$selectedCampus
      selected_specialty <- input$selectedSpecialty
      selected_department <- input$selectedDepartment
      selected_resource <- input$selectedResource
      
      provider_choices <-   #filters %>% 
        filters_table %>%
        filter(CAMPUS %in% selected_campus & 
                 CAMPUS_SPECIALTY %in% selected_specialty & 
                 DEPARTMENT %in% selected_department &
                 RESOURCES %in% selected_resource) %>% 
        #select(PROVIDER)  %>% 
        summarise(PROVIDER= unique(PROVIDER)) %>% collect()
      provider_choices <- sort(provider_choices$PROVIDER, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
    if (is.null(input$selectedResource)){
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = NA)
      
    }
    
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)

  
  observeEvent(list(input$selectedProvider, input$selectedResource),  {
    
    if((is.null(input$filter_list) && (!is.null(input$selectedProvider) &&
                                       !is.null(input$selectedResource))) ||
       
       (!is.null(input$filter_list) && (!is.null(input$selectedProvider) &&
                                        !is.null(input$selectedResource))))
      
    {
    
    
      # visit_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
      #     kpi.all.data$Department %in% input$selectedDepartment &
      #     kpi.all.data$Resource %in% input$selectedResource &
      #     kpi.all.data$Provider %in% input$selectedProvider, "Visit.Method"]))
      
      selected_campus <- input$selectedCampus
      selected_specialty <- input$selectedSpecialty
      selected_department <- input$selectedDepartment
      selected_resource <- input$selectedResource
      selected_provider <- input$selectedProvider
      
      
      if(length(selected_provider) >= 1000){
        visit_choices <- filters %>% 
                                filter(CAMPUS %in% selected_campus & 
                                       CAMPUS_SPECIALTY %in% selected_specialty & 
                                       DEPARTMENT %in% selected_department &
                                       RESOURCES %in% selected_resource) %>% 
                                select( VISIT_METHOD)  %>% 
                                mutate(VISIT_METHOD= unique(VISIT_METHOD)) %>% collect()
      } else{
        visit_choices <- filters %>% 
          filter(CAMPUS %in% selected_campus & 
                   CAMPUS_SPECIALTY %in% selected_specialty & 
                   DEPARTMENT %in% selected_department &
                   PROVIDER %in% selected_provider &
                   RESOURCES %in% selected_resource) %>% 
          select( VISIT_METHOD)  %>% 
          mutate(VISIT_METHOD= unique(VISIT_METHOD)) %>% collect()
        
      }
      
      
      visit_choices <- sort(visit_choices$VISIT_METHOD, na.last = T)
      
      updatePickerInput(session,
                        inputId = "selectedVisitMethod",
                        choices = visit_choices,
                        selected = visit_choices
      )
    } 
    
    if (is.null(input$selectedProvider))
      {
      
      updatePickerInput(session,
                        inputId = "selectedVisitMethod",
                        choices = NA
                        )
      
      
    }
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  observeEvent(list(input$selectedVisitMethod, input$selectedResource,
                    input$selectedProvider), {
    
    if((is.null(input$filter_list) && (!is.null(input$selectedVisitMethod) &&
                                      !is.null(input$selectedResource))) ||
       
       (!is.null(input$filter_list) && (!is.null(input$selectedVisitMethod) &&
                                       !is.null(input$selectedResource))))
       
       {
      if(!is.null(input$selectedProvider)){
                                      
      # default state
      # 1 & (0 | 1)
      # 1 & 1 TRUE 
      print("this observe is for PRC")
      # prc_choices <- sort(unique(kpi.all.data[
      #   kpi.all.data$Campus %in% input$selectedCampus &
      #     kpi.all.data$Campus.Specialty %in% input$selectedSpecialty &
      #     kpi.all.data$Department %in% input$selectedDepartment &
      #     kpi.all.data$Resource %in% input$selectedResource &
      #     kpi.all.data$Provider %in% input$selectedProvider &
      #     kpi.all.data$Visit.Method %in% input$selectedVisitMethod, "Appt.Type"]))
    
      
      
      selected_campus <- input$selectedCampus
      selected_specialty <- input$selectedSpecialty
      selected_department <- input$selectedDepartment
      selected_resource <- input$selectedResource
      selected_provider <- input$selectedProvider
      selected_visit_method <- input$selectedVisitMethod
      
      if(length(selected_provider) >= 1000){
        prc_choices <- filters %>% 
          filter(CAMPUS %in% selected_campus & 
                   CAMPUS_SPECIALTY %in% selected_specialty & 
                   DEPARTMENT %in% selected_department &
                   RESOURCES %in% selected_resource &
                   VISIT_METHOD %in% selected_visit_method) %>% 
          select(APPT_TYPE)  %>% 
          mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
      }else {
      
        prc_choices <- filters %>% 
          filter(CAMPUS %in% selected_campus & 
                   CAMPUS_SPECIALTY %in% selected_specialty & 
                   DEPARTMENT %in% selected_department &
                   PROVIDER %in% selected_provider &
                   RESOURCES %in% selected_resource &
                   VISIT_METHOD %in% selected_visit_method) %>% 
          select(APPT_TYPE)  %>% 
          mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
      }
      prc_choices <- sort(prc_choices$APPT_TYPE, na.last = T)
      
      
      updatePickerInput(session,
                        inputId = "selectedPRCName",
                        choices = prc_choices,
                        selected = prc_choices
      )
      }
    }
                      if (is.null(input$selectedVisitMethod)){
                        updatePickerInput(session,
                                          inputId = "selectedPRCName",
                                          choices = NA)
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
  dataAll <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(historical.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataAll_access <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access(historical.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShow <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(arrivedNoShow.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)

  })
  
  
  
  dataAll_access_new <- eventReactive(input$update_filter_access, {
    selected_campus <- selected_campus()
    selected_specialty <- selected_specialty()
    selected_department <-  selected_department()
    selected_resource <- selected_resource()
    selected_provider <- selected_provider()
    selected_visitmethod <- selected_visitmethod()
    selected_visittype <- selected_visittype()
    selected_dateRange <- selected_dateRange()
    min_date <- selected_dateRange[1]
    end_date <- selected_dateRange[2]
    selected_days <- selected_daysOfWeek()
    selected_holiday <- selected_holiday()
    
    validate(
      need(selected_campus != "", "Please select a Campus"),
      need(selected_specialty != "", "Please select a Specialty"),
      need(selected_department != "", "Please select a Department"),
      need(selected_resource != "", "Please select a Resource"),
      need(selected_provider != "", "Please select a Provider"),
      need(selected_visitmethod != "", "Please select a Visit Method"),
      need(selected_visittype != "", "Please select a Visit Type")
    )
    
    format <- "YYYY-MM-DD HH24:MI:SS"
    
    if(length(selected_provider) >= 1000){
      
      data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                         CAMPUS_SPECIALTY %in% selected_specialty,
                                         DEPARTMENT %in% selected_department,
                                         RESOURCES %in% selected_resource,
                                         VISIT_METHOD %in% selected_visitmethod,
                                         #PROVIDER %in% selected_provider,
                                         TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                         TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                         APPT_DAY %in% selected_days
                                         #HOLIDAY %in% selected_holiday
      )
      
    }
    else {
      
      data <- historical.data %>% filter(CAMPUS %in%  selected_campus, 
                                         CAMPUS_SPECIALTY %in% selected_specialty,
                                         DEPARTMENT %in% selected_department,
                                         RESOURCES %in% selected_resource,
                                         PROVIDER %in% selected_provider,
                                         VISIT_METHOD %in% selected_visitmethod,
                                         TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                         TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                         APPT_DAY %in% selected_days
                                         #HOLIDAY %in% selected_holiday
      )
    }
    
    if(length(selected_visittype) < 1000){
      
      data <- data %>% filter(APPT_TYPE %in% selected_visittype)
    }
  })
  
  dataArrived_access_new <- eventReactive(list(input$update_filter_access),{
    
    selected_campus <- selected_campus()
    selected_specialty <- selected_specialty()
    selected_department <-  selected_department()
    selected_resource <- selected_resource()
    selected_provider <- selected_provider()
    selected_visitmethod <- selected_visitmethod()
    selected_visittype <- selected_visittype()
    selected_dateRange <- selected_dateRange()
    min_date <- selected_dateRange[1]
    end_date <- selected_dateRange[2]
    selected_days <- selected_daysOfWeek()
    selected_holiday <- selected_holiday()
    
    validate(
      need(selected_campus != "", "Please select a Campus"),
      need(selected_specialty != "", "Please select a Specialty"),
      need(selected_department != "", "Please select a Department"),
      need(selected_resource != "", "Please select a Resource"),
      need(selected_provider != "", "Please select a Provider"),
      need(selected_visitmethod != "", "Please select a Visit Method"),
      need(selected_visittype != "", "Please select a Visit Type")
    )
    
    format <- "YYYY-MM-DD HH24:MI:SS"
    
    if(length(selected_provider) >= 1000){
      
      data <- arrived.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                           CAMPUS_SPECIALTY %in% selected_specialty,
                                           DEPARTMENT %in% selected_department,
                                           RESOURCES %in% selected_resource,
                                           VISIT_METHOD %in% selected_visitmethod,
                                           #PROVIDER %in% selected_provider,
                                           TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                           TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                           APPT_DAY %in% selected_days
                                           #HOLIDAY %in% selected_holiday
      )
      
    }
    else {
      
      data <- arrived.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                           CAMPUS_SPECIALTY %in% selected_specialty,
                                           DEPARTMENT %in% selected_department,
                                           RESOURCES %in% selected_resource,
                                           PROVIDER %in% selected_provider,
                                           VISIT_METHOD %in% selected_visitmethod,
                                           TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                           TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                           APPT_DAY %in% selected_days
                                           #HOLIDAY %in% selected_holiday
      )
    }
    
    if(length(selected_visittype) < 1000){
      
      data <- data %>% filter(APPT_TYPE %in% selected_visittype)
    }
  })
  
  
  dataArrivedNoShow_new <- eventReactive(list(input$update_filter_access),{
    
    selected_campus <- selected_campus()
    selected_specialty <- selected_specialty()
    selected_department <-  selected_department()
    selected_resource <- selected_resource()
    selected_provider <- selected_provider()
    selected_visitmethod <- selected_visitmethod()
    selected_visittype <- selected_visittype()
    selected_dateRange <- selected_dateRange()
    min_date <- selected_dateRange[1]
    end_date <- selected_dateRange[2]
    selected_days <- selected_daysOfWeek()
    selected_holiday <- selected_holiday()
    
    validate(
      need(selected_campus != "", "Please select a Campus"),
      need(selected_specialty != "", "Please select a Specialty"),
      need(selected_department != "", "Please select a Department"),
      need(selected_resource != "", "Please select a Resource"),
      need(selected_provider != "", "Please select a Provider"),
      need(selected_visitmethod != "", "Please select a Visit Method"),
      need(selected_visittype != "", "Please select a Visit Type")
    )
    
    format <- "YYYY-MM-DD HH24:MI:SS"
    
    if(length(selected_provider) >= 1000){
      
      data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                                 CAMPUS_SPECIALTY %in% selected_specialty,
                                                 DEPARTMENT %in% selected_department,
                                                 RESOURCES %in% selected_resource,
                                                 VISIT_METHOD %in% selected_visitmethod,
                                                 #PROVIDER %in% selected_provider,
                                                 TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                                 TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                                 APPT_DAY %in% selected_days
                                                 #HOLIDAY %in% selected_holiday
      )
      
    }
    else {
      
      data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in%  selected_campus, 
                                                 CAMPUS_SPECIALTY %in% selected_specialty,
                                                 DEPARTMENT %in% selected_department,
                                                 RESOURCES %in% selected_resource,
                                                 PROVIDER %in% selected_provider,
                                                 VISIT_METHOD %in% selected_visitmethod,
                                                 TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                                 TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                                 APPT_DAY %in% selected_days
                                                 #HOLIDAY %in% selected_holiday
      )
    }
    
    if(length(selected_visittype) < 1000){
      
      data <- data %>% filter(APPT_TYPE %in% selected_visittype)
    }
  })
  
  
  dataArrived_access_npr_new <- eventReactive(list(input$update_filter_access),{
    
    selected_campus <- selected_campus()
    selected_specialty <- selected_specialty()
    selected_department <-  selected_department()
    selected_resource <- selected_resource()
    selected_provider <- selected_provider()
    selected_visitmethod <- selected_visitmethod()
    selected_visittype <- selected_visittype()
    selected_dateRange <- selected_dateRange()
    min_date <- selected_dateRange[1]
    end_date <- selected_dateRange[2]
    selected_days <- selected_daysOfWeek()
    selected_holiday <- selected_holiday()
    
    validate(
      need(selected_campus != "", "Please select a Campus"),
      need(selected_specialty != "", "Please select a Specialty"),
      need(selected_department != "", "Please select a Department"),
      need(selected_resource != "", "Please select a Resource"),
      need(selected_provider != "", "Please select a Provider"),
      need(selected_visitmethod != "", "Please select a Visit Method"),
      need(selected_visittype != "", "Please select a Visit Type")
    )
    
    
    format <- "YYYY-MM-DD HH24:MI:SS"
    
    if(length(selected_provider) >= 1000){
      
      data <- arrived.data.rows.npr %>% filter(CAMPUS %in%  selected_campus, 
                                                 CAMPUS_SPECIALTY %in% selected_specialty,
                                                 DEPARTMENT %in% selected_department,
                                                 RESOURCES %in% selected_resource,
                                                 VISIT_METHOD %in% selected_visitmethod,
                                                 #PROVIDER %in% selected_provider,
                                                 TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                                 TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                                 APPT_DAY %in% selected_days
                                                 #HOLIDAY %in% selected_holiday
      )
      
    }
    else {
      
      data <- arrived.data.rows.npr %>% filter(CAMPUS %in%  selected_campus, 
                                                 CAMPUS_SPECIALTY %in% selected_specialty,
                                                 DEPARTMENT %in% selected_department,
                                                 RESOURCES %in% selected_resource,
                                                 PROVIDER %in% selected_provider,
                                                 VISIT_METHOD %in% selected_visitmethod,
                                                 TO_DATE(min_date, format) <= APPT_MADE_DATE_YEAR, 
                                                 TO_DATE(end_date, format) >= APPT_MADE_DATE_YEAR, 
                                                 APPT_DAY %in% selected_days
                                                 #HOLIDAY %in% selected_holiday
      )
    }
    
    if(length(selected_visittype) < 1000){
      
      data <- data %>% filter(APPT_TYPE %in% selected_visittype)
    }
  })
  
  
  dataArrivedNoShow_access <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access(arrivedNoShow.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  dataArrived <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(arrived.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  
  dataArrived_summary <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(arrived.data.rows.summary,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  
  dataArrived_access_npr <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access_npr(arrived.data.rows.npr,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  dataArrived_access <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access(arrived.data.rows,
                          input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                          input$selectedVisitMethod, input$selectedPRCName, 
                          input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  dataNoShow <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(noshow.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedRescheduled<- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(canceled.bumped.rescheduled.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataCanceledBumpedRescheduledAll <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(canceled.bumped.rescheduled.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  
  dataCanceled<- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(canceled.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataBumped<- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(bumped.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataRescheduled<- eventReactive(list(input$update_filters),{
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
  dataAllKpi <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(historical.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  dataArrivedNoShowKpi <- eventReactive(list(input$update_filters),{
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
  
  dataArrivedKpi <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(arrived.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  
  dataCanceledBumpedKpi <- eventReactive(list(input$update_filters),{
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
  
  dataCanceledKpi <- eventReactive(list(input$update_filters),{
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
  
  dataBumpedKpi <- eventReactive(list(input$update_filters),{
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
  
  dataUtilization <- eventReactive(list(input$update_filters,input$utilType),{
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
                     #input$excludeHolidays,
                     input$dateRangeUtil[1], input$dateRangeUtil[2], input$daysOfWeekUtil,  input$utilType)
  }) 
  
  # [2.3] All pre-processed data for access tabs --------------------------------------------------------------------------------------
  dataFutureSlot <- eventReactive(list(input$update_filters),{
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
  
  dataPastSlot <- eventReactive(list(input$update_filters),{
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
  
  
  dataAllSlot <- eventReactive(list(input$update_filter_slot),{
    print(paste0("Beginning of slot processing ", Sys.time()))
    validate(
      need(input$selectedCampus_slot != "", "Please select a Campus"),
      need(input$selectedSpecialty_slot != "", "Please select a Specialty"),
      need(input$selectedDepartment_slot != "", "Please select a Department"),
      #need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider_slot != "", "Please select a Provider"),
      #need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      #need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_slot(slot.data,
                     input$selectedCampus_slot, input$selectedSpecialty_slot, input$selectedDepartment_slot, input$selectedProvider_slot,
                     #input$selectedVisitMethod, input$selectedResource,
                     input$dateRangeslot[1], input$dateRangeslot[2], input$daysOfWeekslot, input$excludeHolidays)
    
    
    
  }, ignoreNULL = FALSE) 
  
  dataAllSlot_comp <- eventReactive(list(input$update_filter_slot),{
    validate(
      need(input$selectedCampus_slot != "", "Please select a Campus"),
      need(input$selectedSpecialty_slot != "", "Please select a Specialty"),
      need(input$selectedDepartment_slot != "", "Please select a Department"),
      #need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider_slot != "", "Please select a Provider"),
      #need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      #need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_slot(slot.data,
                     input$selectedCampus_slot, input$selectedSpecialty_slot, input$selectedDepartment_slot, input$selectedProvider_slot, 
                     #input$selectedResource, input$selectedVisitMethod,
                     input$dateRangeslot[1], input$dateRangeslot[2], input$daysOfWeekslot, input$excludeHolidays)
  }) 
  
  
  
  # [2.4] Arrived Population Data --------------------------------------------------------------------------------------
  
  dataArrivedPop <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(population_tbl,
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
      prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show", "Canceled"))) / 
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
  # slotData <- dataPastSlot()
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
    print("rendertext")
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
  
  # output$newpatients <- renderText({
  #   paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
  #          " for ", paste(sort(input$selectedCampus), collapse = ', '))
  # })
  
  output$newpatients <- renderText({
    paste0("Based on data from ", selected_dateRange()[1]," to ", selected_dateRange()[2], 
           " for ", paste(sort(selected_campus()), collapse = ', '))
  })
  
  output$slot_usage <- renderText({
    paste0("Based on data from ", input$dateRangeslot[1]," to ", input$dateRangeslot[2], 
           " for ", paste(sort(input$selectedCampus_slot), collapse = ', '))
  })
  
  output$cycle_time <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$room_time <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$room_time2 <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_opt_comp <- renderText({
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
    # data <- noshow.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy") 
    
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
  
  provScheduledAppts_num <- reactive({
    numerator <- as.integer(dataArrivedNoShow() %>% summarise(total = n()) %>% collect())
    denominator <- dataArrivedNoShow() %>% select(APPT_DATE_YEAR) %>% summarise(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator)
    prettyNum(round(numerator/denominator), big.mark = ",")
  })
  
  # Average Daily Appts Scheduled
  output$provScheduledAppts <- renderValueBox({
    provScheduledAppts_num() %>%
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      #provScheduledAppts_num(),
      subtitle = tags$p("Avg. Appointments Scheduled per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  
  dataIncompleteAppt<- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters(incomplete.appt.data.rows,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  # Average Daily Incomplete Appts
  output$provIncompleteAppts <- renderValueBox({
    
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      paste0(prettyNum(round(nrow(dataIncompleteAppt() %>% filter(Appt.Status != "Arrived")) / 
                               nrow(dataIncompleteAppt()), 2)*100, big.mark = ","),"%"),
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
      # stat_summary(fun = sum, vjust = 1, hjust =0, aes(label=ifelse(..y.. == 0,"",..y..), group =name), geom="text", color="black",
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
    data <- dataArrivedKpi()
    # kpiVolumeData <- aggregate(dataArrivedKpi()$uniqueId, by=list(dataArrivedKpi()$Appt.Year,dataArrivedKpi()$Appt.Quarter,
    #                                                               dataArrivedKpi()$Appt.Month, dataArrivedKpi()$Appt.Date, dataArrivedKpi()$Appt.MonthYear, dataArrivedKpi()$Appt.DateYear), FUN=NROW)
    
    kpiVolumeData <- data %>% group_by(APPT_YEAR, APPT_QUARTER, APPT_MONTH, APPT_DATE, APPT_MONTH_YEAR, APPT_DATE_YEAR) %>% summarise(total = n()) %>% collect()
    kpiVolumeData$APPT_YEAR <- as.character(kpiVolumeData$APPT_YEAR)
    print(str(kpiVolumeData))
    # arrived.data <- kpi.all.data[arrived.data.rows,]
    # kpiVolumeData <- aggregate(arrived.data$uniqueId, by=list(arrived.data$Appt.Year,arrived.data$Appt.Quarter,
    #                                                          arrived.data$Appt.Month, arrived.data$Appt.Date, arrived.data$Appt.MonthYear, arrived.data$Appt.DateYear), FUN=NROW)
    
    colnames(kpiVolumeData) <- c("Year","Quarter","Month","Date","YearMonth","DateYear","Volume")
    kpiVolumeData$DateYear <-as.Date(format(kpiVolumeData$DateYear, "%Y-%m-%d") , "%Y-%m-%d")
    
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
          graph_theme("none") + theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2) { # Quart
        ggplot(kpiVolumeDataQuarter, aes(x=interaction(Year,Quarter,lex.order = TRUE), y=Total,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Patients",
               title = "Historical Trend of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataQuarter$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
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
          graph_theme("none")+
          geom_point(size = 3.2)
        
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
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
      
    } else { 
      if(input$kpiFreq == 1){ # Year
        ggplot(kpiVolumeDataYear %>% mutate(Label = "Year"), aes(x=Label, y=Total, col=factor(Year),group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataYear$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top") + theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter 
        ggplot(kpiVolumeDataQuarter, aes(x=Quarter, y=Total, col=factor(Year),group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataQuarter$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(kpiVolumeDataMonth, aes(x = factor(x=Month, level= monthOptions), y=Total, col=factor(Year),group=Year)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Patients",
               title = "Comparison of Patient Volume by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(kpiVolumeDataMonth$Total)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(kpiVolumeData, aes(x=as.Date(DateYear,"%m-%d"), y=Volume, col=factor(Year),group=Year)) +
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
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    }
    
  })
  
  # Appt Status KPI ========================================================================
  output$kpiApptStatusGraph <- renderPlot({
    # statusData <- dataAll() %>% 
    #   group_by(Appt.Year, Appt.Quarter, Appt.Month, Appt.Date, Appt.Status, Appt.MonthYear, Appt.DateYear) %>%
    #   summarise(total = n()) %>%
      # `colnames<-` (c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count"))
    
    data <- dataAllKpi()
    # data_correct_ouptut <<- data
    
    #data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
    data <- data %>% 
      mutate(STATUS = ifelse((APPT_STATUS %in% c("Canceled") & (LEAD_DAYS < 1)),"No Show", APPT_STATUS),
             STATUS = ifelse((APPT_STATUS %in% c("Canceled") & is.na(LEAD_DAYS)),"Canceled", STATUS)) 

    statusData <- data %>% 
      group_by(APPT_YEAR, APPT_QUARTER, APPT_MONTH, APPT_DATE, STATUS, APPT_MONTH_YEAR, APPT_DATE_YEAR) %>%
      summarise(total = n()) %>% collect() %>%
      `colnames<-` (c("Year", "Quarter", "Month","Date","Status","YearMonth","DateYear","Count"))
    statusData$Year <- as.character(statusData$Year)
    
    
    # statusData <- kpi.all.data[all.data.rows,] %>%
    #   group_by(Appt.Year, Appt.Quarter, Appt.Month, Appt.Date, Appt.Status, Appt.MonthYear, Appt.DateYear) %>%
    #   summarise(total = n()) %>%
    #   `colnames<-` (c("Year","Quarter","Month","Date","Status","YearMonth","DateYear","Count"))
    
    statusDataYear <- statusData %>% group_by(Year,Status) %>% dplyr::summarise(Total = round(sum(Count)))
    statusDataYear <- reshape2::dcast(statusDataYear, Year ~ Status)
    column_names <- c("Bumped", "Arrived", "Canceled", "No Show", "Rescheduled")
    if(!identical(which(column_names %in% colnames(statusDataYear) == FALSE), integer(0))) { 
      
        index <- which(column_names %in% colnames(statusDataYear) == FALSE)
        columns_missing <- column_names[c(index)]
        statusDataYear[,columns_missing] <- NA
      }

    
    
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
    if(!identical(which(column_names %in% colnames(statusDataQuarter) == FALSE), integer(0))) { 
      
      index <- which(column_names %in% colnames(statusDataQuarter) == FALSE)
      columns_missing <- column_names[c(index)]
      statusDataQuarter[,columns_missing] <- NA
    }
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
    if(!identical(which(column_names %in% colnames(statusDataMonth) == FALSE), integer(0))) { 
      
      index <- which(column_names %in% colnames(statusDataMonth) == FALSE)
      columns_missing <- column_names[c(index)]
      statusDataMonth[,columns_missing] <- NA
    }
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
    if(!identical(which(column_names %in% colnames(statusDataDay) == FALSE), integer(0))) { 
      
      index <- which(column_names %in% colnames(statusDataDay) == FALSE)
      columns_missing <- column_names[c(index)]
      statusDataDay[,columns_missing] <- NA
    }
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
        ggplot(statusDataYear, aes(x=Year, y=value, col=variable, group=variable)) +
          geom_line() +
          geom_point() +
          facet_wrap(variable~., dir = "v", scales = "free")+
          labs(x = NULL, y = NULL,
               title = "Historical Trend of Scheduling Status by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(statusDataYear$value)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
        
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
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
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
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
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
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    } else {
      if(input$kpiFreq == 1){ # Year
        ggplot(statusDataYear %>% mutate(Label = "Year"), aes(x=Label, y=value, col=factor(Year),group=Year)) +
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
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter
        ggplot(statusDataQuarter, aes(x=Quarter, y=value, col=factor(Year),group=Year)) +
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
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        ggplot(statusDataMonth, aes(x=factor(Month, level = monthOptions), y=value, col=factor(Year),group=Year)) +
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
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        ggplot(statusDataDay, aes(x=as.Date(DateYear,"%m-%d"), y=value, col=factor(Year),group=Year)) +
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
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
        
      }
    }
    
  })
  
  
  dataAllKpi_access <- eventReactive(list(input$update_filters),{
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access(historical.data,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRangeKpi[1], input$dateRangeKpi[2], input$daysOfWeek, input$excludeHolidays)
  })
  
  # Access KPI ========================================================================
  ## Avg New Wait Time
  output$kpiNewWaitTimeGraph <- renderPlot({
    data <- dataAllKpi_access() 
    # data <- kpi.all.data %>% filter(Campus == "MSUS")
    
    
    # data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"), 2))
    data <- data %>% filter(NEW_PT2 == "NEW") %>% filter(WAIT_TIME >= 0)
    data_new_pt_test <- data %>% head(n = 1L) %>% collect()
    
    validate(need(nrow(data_new_pt_test) != 0, "There is no New Patient Data for the selected filters."))
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(APPT_MADE_YEAR) %>% dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% collect()
        data_filter$APPT_MADE_YEAR <- as.character(data_filter$APPT_MADE_YEAR)
        ggplot(data_filter, aes(x=APPT_MADE_YEAR, y=median, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Median Wait Time to New Appointment by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
               )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(APPT_MADE_YEAR, APPT_MADE_QUARTER) %>%
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% collect()
        data_filter$APPT_MADE_YEAR <- as.character(data_filter$APPT_MADE_YEAR)
        ggplot(data_filter, aes(x=interaction(APPT_MADE_YEAR,APPT_MADE_QUARTER,lex.order = TRUE), y=median,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Median Wait Time to New Appointment by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_MADE_MONTH_YEAR) %>% 
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% collect()
        ggplot(data_filter, aes(x=interaction(APPT_MADE_MONTH_YEAR,lex.order = TRUE), y=median,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Days", 
               title = "Median Wait Time to New Appointment by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else { # Day
        data_filter <- data %>% group_by(APPT_MADE_YEAR, APPT_MADE_DATE) %>%
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% collect()
        data_filter$APPT_MADE_YEAR <- as.character(data_filter$APPT_MADE_YEAR)
        data_filter$DateYear <- as.Date(with(data_filter, paste(APPT_MADE_YEAR, APPT_MADE_DATE,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=median, group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Days",
               title = "Median Wait Time to New Appointment by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(APPT_MADE_YEAR) %>% dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% 
          mutate(Label = "Year") %>% collect()
        data_filter$APPT_MADE_YEAR <- as.character(data_filter$APPT_MADE_YEAR)
        ggplot(data_filter, aes(x=Label, y=median, col=factor(APPT_MADE_YEAR),group=APPT_MADE_YEAR)) +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days", 
               title = "Median Wait Time to New Appointment by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(APPT_MADE_YEAR, APPT_MADE_QUARTER) %>% 
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter") %>% collect()
        ggplot(data_filter, aes(x=APPT_MADE_QUARTER, y=median, col=factor(APPT_MADE_YEAR),group=APPT_MADE_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days", 
               title = "Median Wait Time to New Appointment by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_MADE_YEAR, APPT_MADE_MONTH) %>% 
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>% 
          mutate(Label = "Month") %>% collect()
        #rename(Appt.Year = Year)
        
        ggplot(data_filter, aes(x = factor(APPT_MADE_MONTH, level = monthOptions), y=median, col=factor(APPT_MADE_YEAR),group=APPT_MADE_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Days",
               title = "Median Wait Time to New Appointment by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main") +
          scale_fill_manual(name = "Year")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(APPT_MADE_DATE_YEAR, APPT_MADE_YEAR) %>%
          dplyr::summarise(median = round(median(WAIT_TIME, na.rm=TRUE))) %>%
          mutate(Label = "Date") %>% collect() %>% mutate(APPT_MADE_DATE_YEAR = as.Date(format(APPT_MADE_DATE_YEAR, "%Y-%m-%d"), "%Y-%m-%d"))
        ggplot(data_filter, aes(x = APPT_MADE_DATE_YEAR, y=median, col=factor(APPT_MADE_YEAR),group=APPT_MADE_YEAR)) +
          geom_line() +
          labs(x = NULL, y = "Days",
               title = "Median Wait Time to New Appointment by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$median)*1.2))+
          coord_cartesian(clip = 'off') +
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+
          scale_color_MountSinai("main") +
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    }
    
  })
  
  
  # Day of Visit KPI ========================================================================
  ## Avg Check-in to Visit-end
  output$kpiCycleTimeGraph <- renderPlot({
    data <- dataArrivedKpi() %>% filter(CYCLETIME > 0)
    # data <- kpi.arrived.data %>% filter(cycleTime > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>%
          collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=APPT_YEAR, y=mean, group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Cycle Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=interaction(APPT_YEAR,APPT_QUARTER,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Cycle Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_MONTH_YEAR) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% collect()
        ggplot(data_filter, aes(x=interaction(APPT_MONTH_YEAR,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Cycle Time by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else { # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% collect()
        data_filter$DateYear <- as.Date(with(data_filter, paste(APPT_YEAR, APPT_DATE,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean,group=1)) +
          #ggplot(data_filter, aes(x=interaction(Appt.Year,as.Date(Appt.Date, format="%Y-%m-%d"),lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Time (min)",  
               title = "Average Cycle Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% 
          mutate(Label = "Year") %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=Label, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)",  
               title = "Average Cycle Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter") %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=APPT_QUARTER, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Cycle Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_YEAR, APPT_MONTH) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% 
          mutate(Label = "Month") %>% collect()
        ggplot(data_filter, aes(x = factor(APPT_MONTH, level = monthOptions), y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)",
               title = "Average Cycle Time by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE_YEAR) %>% 
          dplyr::summarise(mean = round(mean(CYCLETIME, na.rm=TRUE))) %>% 
          mutate(Label = "Date") %>% collect() %>% mutate(APPT_DATE_YEAR = as.Date(format(APPT_DATE_YEAR, "%Y-%m-%d"), "%Y-%m-%d"))
        ggplot(data_filter, aes(x = APPT_DATE_YEAR, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Cycle Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main") +
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
        
      }
    }
    
  })
  
  ## Check-in to Room-in Wait Time
  output$kpiWaitTimeGraph <- renderPlot({
    data <- dataArrivedKpi() %>% filter(CHECKINTOROOMIN > 0)
    # data <- kpi.arrived.data %>% filter(checkinToRoomin > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>%
          collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=APPT_YEAR, y=mean, group=1)) +
          #stat_summary(fun="mean", geom="line")+
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=interaction(APPT_YEAR,APPT_QUARTER,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_MONTH_YEAR) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% collect()
        ggplot(data_filter, aes(x=interaction(APPT_MONTH_YEAR,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else { # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% collect()
        data_filter$DateYear <- as.Date(with(data_filter, paste(APPT_YEAR, APPT_DATE,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% 
          mutate(Label = "Year") %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=Label, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter") %>% collect()
        ggplot(data_filter, aes(x=APPT_QUARTER, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_YEAR, APPT_MONTH) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% 
          mutate(Label = "Month") %>% collect()
        ggplot(data_filter, aes(x = factor(APPT_MONTH, level = monthOptions), y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE_YEAR) %>% 
          dplyr::summarise(mean = round(mean(CHECKINTOROOMIN, na.rm=TRUE))) %>% 
          mutate(Label = "Date") %>% collect() %>% mutate(APPT_DATE_YEAR = as.Date(format(APPT_DATE_YEAR, "%Y-%m-%d"), "%Y-%m-%d"))
        ggplot(data_filter, aes(x = APPT_DATE_YEAR, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Check-in to Room-in Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main") + 
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    }
    
  })
  
  
  
  output$kpiRoomTimeGraph <- renderPlot({
    data <- dataArrivedKpi() %>% filter(ROOMINTOVISITEND > 0)
    # data <- kpi.arrived.data %>% filter(checkinToRoomin > 0)
    
    if(input$kpiTrend ==1){ # Historical Trend
      if(input$kpiFreq == 1){ #Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>%
          collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=APPT_YEAR, y=mean, group=1)) +
          #stat_summary(fun="mean", geom="line")+
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2) { # Quarter
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=interaction(APPT_YEAR,APPT_QUARTER,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2])))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_MONTH_YEAR) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% collect()
        ggplot(data_filter, aes(x=interaction(APPT_MONTH_YEAR,lex.order = TRUE), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          geom_point(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+
          geom_point(size = 3.2)
        
      } else { # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% collect()
        data_filter$DateYear <- as.Date(with(data_filter, paste(APPT_YEAR, APPT_DATE,sep="-")), "%Y-%m-%d")
        ggplot(data_filter, aes(x= as.Date(DateYear,"%Y-%m-%d"), y=mean,group=1)) +
          geom_line(color="midnightblue") +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("none")+ 
          scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    } else { 
      if(input$kpiFreq == 1){ # Year
        data_filter <- data %>% group_by(APPT_YEAR) %>% dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% 
          mutate(Label = "Year") %>% collect()
        data_filter$APPT_YEAR <- as.character(data_filter$APPT_YEAR)
        ggplot(data_filter, aes(x=Label, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Year",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 2){ # Quarter 
        data_filter <- data %>% group_by(APPT_YEAR, APPT_QUARTER) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% 
          mutate(Label = "Quarter") %>% collect()
        ggplot(data_filter, aes(x=APPT_QUARTER, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Quarter",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme( axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 3){ # Month
        data_filter <- data %>% group_by(APPT_YEAR, APPT_MONTH) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% 
          mutate(Label = "Month") %>% collect()
        ggplot(data_filter, aes(x = factor(APPT_MONTH, level = monthOptions), y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          geom_point(size=4, alpha=0.5) +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end by Month",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
          scale_color_MountSinai("main")+
          geom_point(size = 3.2)
        
      } else if(input$kpiFreq == 4){ # Day
        data_filter <- data %>% group_by(APPT_YEAR, APPT_DATE_YEAR) %>% 
          dplyr::summarise(mean = round(mean(ROOMINTOVISITEND, na.rm=TRUE))) %>% 
          mutate(Label = "Date") %>% collect() %>% mutate(APPT_DATE_YEAR = as.Date(format(APPT_DATE_YEAR, "%Y-%m-%d"), "%Y-%m-%d"))
        ggplot(data_filter, aes(x = APPT_DATE_YEAR, y=mean, col=factor(APPT_YEAR),group=APPT_YEAR)) +
          geom_line() +
          labs(x = NULL, y = "Time (min)", 
               title = "Average Room-in to Visit-end Time by Day",
               subtitle = paste0("Based on data from ",isolate(input$dateRangeKpi[1])," to ",isolate(input$dateRangeKpi[2]))
          )+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(data_filter$mean)*1.2))+
          theme_new_line()+
          theme_bw()+
          graph_theme("top")+ 
          scale_color_MountSinai("main") + 
          scale_x_date(breaks = "day", date_labels = "%m-%d", date_breaks = "1 month",
                       date_minor_breaks = "1 day", expand = c(0, 0.6))+
          geom_point(size = 3.2)
      }
    }
    
  })
  ### Scheduling Tab -------------------------------------------------------------------------------------------------------------------
  # Scheduling Summary
  # Average Daily Appts Scheduled
  
  scheduledAppts_num <- reactive({
    data <- dataArrivedNoShow()
    numerator <- data %>% summarize(n()) %>% collect()
    denominator <- data %>% select(APPT_DATE_YEAR) %>% mutate(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator$APPT_DATE_YEAR)
    num <- prettyNum(round(numerator/denominator), big.mark = ",")
    num
  })
  output$scheduledAppts <- renderValueBox({
    scheduledAppts_num() %>% 
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      #num,
      subtitle = tags$p("Avg. Appointments Scheduled per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  incompleteAppts_num <- reactive({
    data <- dataArrivedNoShow()
    
    numerator <- data %>% filter(APPT_STATUS %in% c("Rescheduled", "Canceled", "Bumped", "No Show")) %>% summarize(n()) %>% collect()
    denominator <- data %>% summarize(n()) %>% collect()
    num <- prettyNum(round(numerator/denominator,2)*100, big.mark = ",")
  })
  # Average Daily Incomplete Appts
  output$incompleteAppts <- renderValueBox({
    incompleteAppts_num() %>% 
    valueBox(
      #prettyNum(round(nrow(dataNoShow() %>% filter(Appt.Status %in% c("No Show"))) / length(unique(dataArrived()$Appt.DateYear)),0), big.mark = ","),
      # prettyNum(round(nrow(dataArrivedNoShow() %>% filter(Appt.Status != "Arrived")) / 
      #                   nrow(dataArrivedNoShow()), 2)*100, big.mark = ","),
      subtitle = tags$p("% of Incomplete Appointments", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  output$schedulingStatusSummary <- renderPlot({
    
    data <- dataArrivedNoShow() %>% filter(APPT_STATUS %in% c("No Show", "Arrived"))

    # data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology")
    
    # total_arrived <- arrived.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology")%>% 
    #                                                          select(APPT_DATE_YEAR) %>% collect()
    
    # sameDay <- data %>%
    #   group_by(Appt.Status) %>%
    #   summarise(value = round(n()/length(unique(kpi.all.data[arrived.data.rows,]$Appt.DateYear)))) %>%
    #   arrange(desc(value)) 
    data <- data %>% select(APPT_STATUS, APPT_DATE_YEAR) %>% collect()
    total_arrived <- data %>% select(APPT_DATE_YEAR) %>% collect()
    
    sameDay <- data %>%
      group_by(APPT_STATUS) %>%
      summarise(value = ceiling(n()/length(unique(total_arrived$APPT_DATE_YEAR)))) %>%
      arrange(desc(value)) 
    
    data_incomplete <- dataCanceledBumpedRescheduledAll() %>% filter(LEAD_DAYS < 1) %>% select(APPT_STATUS, APPT_DATE_YEAR) %>% collect() %>%
                        group_by(APPT_STATUS) %>%
                        summarise(value = ceiling(n()/length(unique(total_arrived$APPT_DATE_YEAR)))) %>%
                        arrange(desc(value)) 
    
    sameDay <- bind_rows(sameDay, data_incomplete)
    
    
    
    sameDay$APPT_STATUS <- as.character(sameDay$APPT_STATUS)
    
    sameDay$APPT_STATUS[which(sameDay$APPT_STATUS == "Bumped")] <- "Same-day Bumped"
    sameDay$APPT_STATUS[which(sameDay$APPT_STATUS == "Canceled")] <- "Same-day Canceled \n(No Show)"
    sameDay$APPT_STATUS[which(sameDay$APPT_STATUS == "Rescheduled")] <- "Same-day Rescheduled"
    
    ggplot(sameDay, aes(reorder(APPT_STATUS, -value), value, fill=APPT_STATUS)) +
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
    #data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
    data <- data %>%
      group_by(APPT_DATE_YEAR, APPT_TM_HR, APPT_STATUS) %>%
      dplyr::summarise(total = n()) %>% collect() %>%
      group_by(APPT_TM_HR, APPT_STATUS) %>%
      dplyr::summarise(avg = round(mean(total)))
    
    data <- dcast(data, APPT_TM_HR ~ APPT_STATUS, sum)
    
    byTime.df <- as.data.frame(byTime.df[which(byTime.df$Time %in% unique(data$APPT_TM_HR)),])
    colnames(byTime.df) <- "Time"
    
    data <- as.data.frame(merge(byTime.df,data, by.x = c("Time"), by.y = c("APPT_TM_HR"), all.x = TRUE, all.y = TRUE))
    
    #data[is.na(data)] <- 0
    data <- reshape2::melt(data, id=c("Time"))
    
    data$variable <- as.character(data$variable)
    
    data$variable[which(data$variable == "Bumped")] <- "Same-day Bumped"
    data$variable[which(data$variable == "Canceled")] <- "Same-day Canceled (No Show)"
    data$variable[which(data$variable == "Rescheduled")] <- "Same-day Rescheduled"
    
    data <- data %>% filter(Time %in% timeOptionsHr_filter)
    
    #data <- data %>%
    # arrange(desc(variable))
    
    totals <- data %>%
      group_by(Time) %>%
      summarize(value = sum(value, na.rm = T))
    
    
    ggplot(data, aes(x=Time, y=value, fill=factor(variable, levels=c("Arrived", "No Show",
                                                                     "Same-day Bumped",
                                                                     "Same-day Canceled (No Show)", 
                                                                     "Same-day Rescheduled"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      #scale_fill_MountSinai(reverse = TRUE)+
      labs(x = NULL, y = "Pateints",
           title = "Average Patients by Status",
           subtitle = paste0("Based on scheduled appointment time from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
           )+
      scale_y_continuous(expand = c(0, 0), limits = c(0,max(totals$value)*1.5))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(legend.title = element_blank())+
      guides(colour = guide_legend(nrow = 1))+
      geom_text(aes(label=ifelse(value < max(value)*0.1," ",value)), color="white", 
                size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Time), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
  })
  
  # Arrived Patients 
  output$arrivedPts <- renderPlot({
    data <- dataArrived()
    # data <- arrived.data
    
    arrived <- data %>%
      group_by(APPT_DATE_YEAR, APPT_DAY, APPT_TM_HR) %>%
      dplyr::summarise(total = n()) %>% collect() %>%
      group_by(APPT_DAY, APPT_TM_HR) %>%
      dplyr::summarise(avg = round(mean(total),0))
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(arrived$Day)),]
    
    arrived <- as.data.frame(merge(byDayTime.df,arrived, by.x = c("Day","Time"), by.y = c("APPT_DAY","APPT_TM_HR"), all = TRUE))
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
      guides(colour = guide_legend(nrow = 1))+
      geom_point(size = 3.2) 
    
    
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
  # output$apptTypeControl <- renderUI({
  #   
  #   box(
  #     title = NULL,
  #     width = 12, 
  #     solidHeader = FALSE,
  #     pickerInput("selectedApptType", label=h4("Select Visit Type:"),
  #                 choices = c("New", "Established"),#sort(unique(dataAll()$Appt.Type)),
  #                 #choices = sort(unique(dataAll()$Appt.Type), na.last = TRUE),
  #                 # choices=sort(unique(dataAll()$Appt.Type)),
  #                 multiple=TRUE,
  #                 options = pickerOptions(
  #                   liveSearch = TRUE,
  #                   actionsBox = TRUE,
  #                   selectedTextFormat = "count > 1",
  #                   countSelectedText = "{0}/{1} Visit Types",
  #                   dropupAuto = FALSE),
  #                 selected = c("New", "Established")
  #                 #selected = unique(dataAll()$Appt.Type)
  #                 #selected = sort(unique(dataAll()$Appt.Type), na.last = TRUE)
  #     )
  #   )
  # })
  # 
  output$insuranceControl <- renderUI({
    
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedInsurance", label=h4("Select Payer Type:"),
                  #choices = sort(unique(dataAll()$Coverage)),
                  choices = sort(unique((dataAll() %>% select(COVERAGE) %>% collect())$COVERAGE), na.last = TRUE),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1",
                    countSelectedText = "{0}/{1} Payer Types",
                    dropupAuto = FALSE),
                  #selected = unique(dataAll()$Coverage)
                  selected = sort(unique((dataAll() %>% select(COVERAGE) %>% collect())$COVERAGE), na.last = TRUE)
      )
    )
  })
  
  # Arrived No Show Data with Additional Filters (Appointment Type and Insurance)
  dataArrivedNoShow_1 <- reactive({
    data <- dataArrivedNoShow()
    #data[,c("Coverage")][is.na(data[,c("Coverage")])] <- "NA"
    groupByFilters_1(data %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")),
                     #input$selectedApptType, 
                     input$selectedInsurance)
  })
  
  dataNoShow_1 <- reactive({
    data <- dataNoShow()
    #data[,c("Coverage")][is.na(data[,c("Coverage")])] <- "NA"
    groupByFilters_1(data %>% filter(APPT_STATUS %in% c("No Show", "Canceled")),
                     #input$selectedApptType,
                     input$selectedInsurance
    )
  })
  
  # Total No Shows per Day
  output$avgDailyNoShow_Count <- renderValueBox({
    data <- dataNoShow_1()
    numerator <- data %>%
      #filter(APPT_STATUS == "No Show") %>% 
      summarise(n()) %>% collect()
    denominator <- dataArrivedNoShow_1() %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>%
      select(APPT_DATE_YEAR) %>% mutate(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator$APPT_DATE_YEAR)
    
    valueBox(
      # prettyNum(round(nrow(dataNoShow_1() %>% filter(Appt.Status %in% c("No Show"))) / length(unique((dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived")))$Appt.DateYear)),0), big.mark = ","),
      prettyNum(ceiling(numerator/denominator),big.mark=","), 
      subtitle = tags$p("Avg. No Shows per Day", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # % No Shows per Day
  output$avgDailyNoShow_Perc <- renderValueBox({
    numerator <- dataNoShow_1() 
    numerator <- numerator %>% 
      #filter(APPT_STATUS %in% c("No Show", "Canceled")) %>% 
      summarise(n()) %>% collect()
    denominator <- dataArrivedNoShow_1() %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>% 
          summarise(n()) %>% collect()
    valueBox(
      # paste0(round((nrow(dataNoShow_1() %>% filter(Appt.Status %in% c("No Show"))) / 
      #                 nrow(dataArrivedNoShow_1() %>% filter(Appt.Status %in% c("Arrived", "No Show"))))*100,1), "%"),
      paste0(round((numerator / 
                      denominator)*100,1), "%"),
      subtitle = tags$p("No Show Rate (%)", style = "font-size: 130%;"), icon = NULL, color = "yellow"
    )
    
  })
  
  # Distribution of No Shows (%) by Lead Days 
  output$noShowLeadDays <- renderPlot({
    data <- dataArrivedNoShow_1() 
    #data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    data <- data %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>% 
                    select(APPT_DTTM, APPT_MADE_DTTM, APPT_STATUS, APPT_DATE_YEAR) %>% collect()
    # data <- kpi.all.data[arrivedNoShow.data.rows,] %>% filter(Campus == "MSUS")
    
    data$APPT_STATUS <- ifelse(data$APPT_STATUS == "Arrived","Arrived","No Show")
    
    noShows <- 
      data %>%
      mutate(apptLeadDays = as.numeric(round(difftime(APPT_DTTM, APPT_MADE_DTTM,  units = "days"),2))) %>%
      mutate(apptLeadDays = ifelse(is.na(apptLeadDays),0, apptLeadDays)) %>%
      mutate(apptLeadDays = ifelse(apptLeadDays > 30, "> 30 days",
                                       ifelse(apptLeadDays <= 30 & apptLeadDays >= 15, "15-30 days",
                                              ifelse(apptLeadDays <= 14 & apptLeadDays>= 8, "8-14 days",
                                                     ifelse(apptLeadDays <= 7 & apptLeadDays >= 1, "1-7 days",
                                                            ifelse(apptLeadDays < 0, "0 day","0 day")
                                                     )
                                              )
                                       )
                                )
      
      )
    
    noShows <- reshape2::dcast(noShows, apptLeadDays + APPT_DATE_YEAR ~ APPT_STATUS)
    noShows$noShow_perc <- round(noShows$`No Show`/ (noShows$`No Show` + noShows$Arrived),2)
    noShows$noShow_perc[!is.finite(noShows$noShow_perc)] <- 0
    
    status <- c('0 day','1-7 days','8-14 days','15-30 days', '> 30 days')
    
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
             title = "Average No Show Rate by Wait Time to Appointment",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1]),
                               " to ",isolate(input$dateRange[2])))+
        scale_y_continuous(labels=scales::percent_format(accuracy=1),limits = c(0,max(noShows_bar_tb$value)*1.2))+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")+
        theme(plot.caption = element_text(size=12, face="italic"),
              axis.text.x = element_text(size = 16, angle=0, hjust=0.5))+
        geom_text(aes(label=paste0(value*100,"%")), vjust = -1, hjust = -.5, color="black", fontface="bold",
                  position = position_dodge(1), size=5)+
        coord_flip()
    
    #grid.arrange(noShows_bar, noShows_box, ncol = 2)
    
  })
  
  
  
  # No Shows by Time of Day 
  output$avgNoShowCount <- renderPlot({
    data <- dataArrivedNoShow_1() %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>%
      select(APPT_STATUS, APPT_DAY, APPT_TM_HR, APPT_DATE_YEAR) %>% collect()
    # data <- arrivedNoShow.data
    
    data_arrived <- data %>% filter(APPT_STATUS %in% "Arrived") %>%
                    group_by(APPT_DAY, APPT_TM_HR) %>%
                      dplyr::summarise(total_arrived = n()) %>%
                    rename(Day = APPT_DAY,
                           Time = APPT_TM_HR)  
    data$APPT_STATUS <- ifelse(data$APPT_STATUS == "Arrived","Arrived","No Show")
    
    noShow_count <- data %>%
      filter(APPT_STATUS %in% "No Show") %>%
      group_by(APPT_DAY, APPT_TM_HR) %>%
      dplyr::summarise(Total = n()) 
    
    days <- unique(data[,c("APPT_DAY","APPT_DATE_YEAR")])
    days <- days %>% group_by(APPT_DAY) %>% dplyr::summarise(days = n())
    
    noShow_count$days <- days$days[match(noShow_count$APPT_DAY, days$APPT_DAY)]
    noShow_count$avgNoShows <- ceiling(noShow_count$Total/noShow_count$days)
    
    noShow_count.df <- byDayTime.df %>% filter(Day %in% unique(noShow_count$APPT_DAY))
    noShow_count.df <- merge(noShow_count.df, noShow_count, by.x = c("Day","Time"), by.y = c("APPT_DAY","APPT_TM_HR"), all = TRUE)
    
    noShow_count.df <- noShow_count.df %>% filter(Time %in% timeOptionsHr_filter)
    noShow_count.df <- left_join(noShow_count.df, data_arrived)
    noShow_count.df <- noShow_count.df %>% mutate(avgNoShows = ifelse((is.na(avgNoShows) & !is.na(total_arrived)), 0, avgNoShows))
    
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
      geom_text(aes(label= ifelse(is.na(avgNoShows),"",avgNoShows)), color="black", size=5, fontface="bold")
    
  })
  
  output$avgNoShowPercent <- renderPlot({
   
    data <- dataArrivedNoShow_1() 
    #data <- arrivedNoShow.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
    data <- data %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>%
       select(APPT_DATE_YEAR, APPT_DAY, APPT_TM_HR, APPT_STATUS) %>% collect()
    # data <- arrivedNoShow.data
    data$APPT_STATUS <- ifelse(data$APPT_STATUS == "Arrived","Arrived","No Show")
    
    noShow_perc <- data %>%
      group_by(APPT_DATE_YEAR, APPT_DAY, APPT_TM_HR, APPT_STATUS) %>%
      dplyr::summarise(Total = n())
    
    noShow_perc <- reshape2::dcast(noShow_perc, APPT_DAY +  APPT_TM_HR ~ APPT_STATUS, sum) 
    noShow_perc <- mutate(noShow_perc, percentage = round((`No Show` / (Arrived + `No Show`))*100,0))
    
    noShow_perc.df <- byDayTime.df %>% filter(Day %in% unique(noShow_perc$APPT_DAY))
    noShow_perc.df <- merge(noShow_perc.df, noShow_perc, by.x = c("Day","Time"), by.y = c("APPT_DAY","APPT_TM_HR"), all = TRUE)
    
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
    #data <- canceled.bumped.rescheduled.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "OB/GYN") 
    valueBox(
      prettyNum(nrow(data),big.mark=","), 
      subtitle = tags$p("Total Bumped/Canceled/Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
 
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyBumpedBox <- renderValueBox({
    
    
    data <- dataCanceledBumpedRescheduled()
    # data <- canceled.bumped.rescheduled.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "OB/GYN") 
    numerator <- data %>% filter(APPT_STATUS == "Bumped") %>% summarise(sum(TOTAL_APPTS)) %>% collect()
    denominator <- dataAll() %>% select(APPT_DATE_YEAR) %>% mutate(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator$APPT_DATE_YEAR)
    
    valueBox(
      # prettyNum(round(nrow(data %>% filter(Appt.Status == "Bumped"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","),
      prettyNum(round(numerator/denominator),big.mark=","), 
      subtitle = tags$p("Avg. Daily Bumped Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyCanceledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    numerator <- data %>% filter(APPT_STATUS == "Canceled") %>% summarise(sum(TOTAL_APPTS)) %>% collect()
    denominator <- dataAll() %>% select(APPT_DATE_YEAR) %>% mutate(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator$APPT_DATE_YEAR)

    valueBox(
      # prettyNum(round(nrow(data %>% filter(Appt.Status == "Canceled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      prettyNum(round(numerator/denominator),big.mark=","), 
      subtitle = tags$p("Avg. Daily Canceled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  # Avg Daily Canceled/Bumped/Rescheduled Appointments 
  output$avgDailyRescheduledBox <- renderValueBox({
    
    data <- dataCanceledBumpedRescheduled()
    numerator <- data %>% filter(APPT_STATUS == "Rescheduled") %>% summarise(sum(TOTAL_APPTS)) %>% collect()
    denominator <- dataAll() %>% select(APPT_DATE_YEAR) %>% mutate(APPT_DATE_YEAR = unique(APPT_DATE_YEAR)) %>% collect()
    denominator <- length(denominator$APPT_DATE_YEAR)
    
    valueBox(
      # prettyNum(round(nrow(data %>% filter(Appt.Status == "Rescheduled"))/length(unique(dataAll()$Appt.DateYear))),big.mark=","), 
      prettyNum(round(numerator/denominator),big.mark=","), 
      subtitle = tags$p("Avg. Daily Rescheduled Appointments", style = "font-size: 160%;"), icon = NULL,
      color = "yellow"
    )
  })
  
  
  ## Average Bumps/Canc/Resc Rate 
  output$avgBumpsCancRescRate <- renderPlot({
    data <- dataAll()
    # data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "OB/GYN") 
    
    apptsCanceled <- data %>%
      group_by(APPT_STATUS) %>%
      summarise(value = n()) %>% collect() %>%
      arrange(desc(value)) %>%
      mutate(percent = round((value/sum(value)), 2)) %>%
      select(APPT_STATUS, percent) %>%
      filter(APPT_STATUS %in% c("Bumped", "Canceled", "Rescheduled"))
    
    apptsCanceled$APPT_STATUS <- factor(apptsCanceled$APPT_STATUS, levels=sort(unique(apptsCanceled$APPT_STATUS)))
    
    ggplot(apptsCanceled, aes(APPT_STATUS, percent, fill=APPT_STATUS)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,(max(apptsCanceled$percent))*1.5))+
      scale_fill_manual(values=MountSinai_pal("all")(10))+
      labs(x=NULL, y=NULL,
           title = "Rate* of Scheduling Activities - \nBumped/Canceled/Rescheduled",
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
    #data <- dataAll() %>% filter(APPT_STATUS %in% c("Bumped","Canceled","Rescheduled"))
    
    data <- dataCanceledBumpedRescheduledAll()
    # data <-  historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy") %>% filter(APPT_STATUS %in% c("Bumped","Canceled","Rescheduled"))
    
    lead.days.df <- data %>%
      filter(LEAD_DAYS >= 0) %>% select(LEAD_DAYS, APPT_STATUS) %>% collect() %>%
      mutate(leadDays = ifelse(LEAD_DAYS > 30, "> 30 days",
                            ifelse(LEAD_DAYS <= 30 & LEAD_DAYS >= 15, "15-30 days",
                                 ifelse(LEAD_DAYS <= 14 & LEAD_DAYS >= 8, "8-14 days",
                                        ifelse(LEAD_DAYS < 8 & LEAD_DAYS >= 1, "1-7 days",
                                               ifelse(LEAD_DAYS < 0, "0 day","0 day")))))) %>%
      group_by(leadDays,APPT_STATUS) %>%
      dplyr::summarise(total = n()) %>%
      group_by(APPT_STATUS) %>%
      mutate(perc = total/sum(total))
    
    lead.days.df$APPT_STATUS <- factor(lead.days.df$APPT_STATUS, levels=sort(unique(lead.days.df$APPT_STATUS)))
    
    
    ggplot(lead.days.df, aes(x=APPT_STATUS, y=perc, fill=factor(leadDays, levels=c("0 day","1-7 days","8-14 days","15-30 days", "> 30 days"))))+
      geom_bar(position="stack",stat="identity", width=0.7)+
      scale_fill_manual(values=c("grey","#00aeef","#d80b8c","midnightblue", "#c7c6ef", "#212070"))+
      labs(x=NULL, y=NULL,
           title = "% of Bumped/Canceled/Rescheduled \nby Lead days to Appointment*",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*Time from scheduled appointment date to status changed.")+
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
    # data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy") %>% filter(APPT_STATUS %in% c("Bumped","Canceled","Rescheduled") & LEAD_DAYS < 1)
    #data <- dataAll() %>% filter(APPT_STATUS %in% c("Bumped","Canceled","Rescheduled") & LEAD_DAYS < 1)
    data <- dataCanceledBumpedRescheduledAll() %>% filter(LEAD_DAYS < 1)
    # data <- noShow.data %>% filter(Appt.Status %in% c("Bumped","Canceled","Rescheduled"))
    rows <- data %>% select(APPT_DATE_YEAR) %>% collect()
    rows <- length(unique(rows$APPT_DATE_YEAR))
   
    
    sameDay <- data %>%
      group_by(APPT_STATUS) %>%
      summarise(total = n()) %>% collect() %>%
      mutate(avg = ceiling(total/rows))
    
    
    sameDay$APPT_STATUS <- factor(sameDay$APPT_STATUS, levels=sort(unique(sameDay$APPT_STATUS)))
    
    ggplot(sameDay, aes(APPT_STATUS, avg, fill=APPT_STATUS)) +
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
    
    data <- dataBumped() %>% select(APPT_DTTM, APPT_CANCEL_DTTM, CANCEL_REASON) %>% collect()
    #data <- bumped.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy" ) %>% collect()
    
    total <- nrow(data)
    
    bumps <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(APPT_DTTM, APPT_CANCEL_DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 30, "> 30 days",
                            ifelse(leadDays <= 30 & leadDays >= 15, "15-30 days", 
                                 ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                        ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                               ifelse(leadDays < 0, "0 day","0 day")))))) %>%
      group_by(leadDays,CANCEL_REASON) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    bumps$percent <- round(bumps$total/total,2)*100
    
    top10 <- bumps %>% 
      group_by(CANCEL_REASON) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$CANCEL_REASON)
    
    if(input$percent == FALSE){
      
      graph <- 
        ggplot(bumps %>% filter(CANCEL_REASON %in% top10), 
               aes(CANCEL_REASON, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","15-30 days", "> 30 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days to Appointment",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#d80b8c", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Bumped Appointments\n by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(bumps %>% filter(CANCEL_REASON %in% top10), 
               aes(CANCEL_REASON, factor(leadDays, levels=c("0 day","1-7 days","8-14 days", "15-30 days","> 30 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Bumped Reasons by Lead Days to Appointment",
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
    
    data <- dataCanceled() %>% select(APPT_DTTM, APPT_CANCEL_DTTM, CANCEL_REASON) %>% collect()
    # data <- canceled.data
    
    total <- nrow(data)
    
    cancellations <- 
      data %>%
      mutate(leadDays = as.numeric(round(difftime(APPT_DTTM, APPT_CANCEL_DTTM,  units = "days"),2))) %>% #(REMOVE)
      mutate(leadDays = ifelse(is.na(leadDays),0,leadDays)) %>%
      mutate(leadDays = ifelse(leadDays > 30, "> 30 days",
                            ifelse(leadDays <= 30 & leadDays >= 15, "15-30 days", 
                                 ifelse(leadDays <= 14 & leadDays>= 8, "8-14 days",
                                        ifelse(leadDays < 8 & leadDays >= 1, "1-7 days",
                                               ifelse(leadDays < 0, "0 day","0 day")))))) %>%
      group_by(leadDays,CANCEL_REASON) %>%
      dplyr::summarise(total = n()) %>%
      arrange(leadDays,desc(total))
    
    cancellations$percent <- round(cancellations$total/total,2)*100
    cancellations$CANCEL_REASON[which(is.na(cancellations$CANCEL_REASON))] <- "No Reasons Recorded"
    
    top10 <- cancellations %>% 
      group_by(CANCEL_REASON) %>% 
      dplyr::summarise(total = sum(total)) %>% 
      arrange(desc(total)) %>%
      head(10)
    
    top10 <- as.vector(top10$CANCEL_REASON)
    
    
    if(input$percent == FALSE){
      
      graph <- 
        ggplot(cancellations %>% filter(CANCEL_REASON %in% top10), 
               aes(CANCEL_REASON, factor(leadDays, levels=c("0 day","1-7 days","8-14 days","15-30 days", "> 30 days")), fill = total))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days to Appointment",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        scale_fill_gradient(low = "white", high = "#00aeef", space = "Lab", na.value = "#dddedd", guide = "colourbar", 
                            name="Total Canceled Appointments\n by Wait Time to Appointment")+
        geom_text(aes(label= ifelse(is.na(total),"",total)), color="black", size=5)
      
    }else{
      
      graph <- 
        ggplot(cancellations %>% filter(CANCEL_REASON %in% top10), 
               aes(CANCEL_REASON, factor(leadDays, levels=c("0 day","1-7 days","8-14 days", "15-30 days", "> 30 days")), fill = percent))+
        geom_tile(color = "black")+
        coord_flip()+
        labs(x=NULL, y=NULL,
             title = "Top 10 Canceled Reasons by Lead Days to Appointment",
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
  # output$roomStat1 <- renderValueBox({
  #   valueBox(NULL,
  #            # paste0(input$setRooms," rooms available\n throughout",input$setHours," hours"),
  #            subtitle = tags$p(paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours"), style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "yellow"
  #   )
  # })
  # 
  # output$maxRoomsRequired <- renderValueBox({
  #   
  #   valueBox(NULL,
  #            subtitle = tags$p(paste0("Max # of rooms required during the day: ",
  #                                     max((dataUtilization() %>%
  #                                            filter(comparison == 0) %>%
  #                                            select(Appt.DateYear, timeOptionsHr_filter) %>%
  #                                            gather(Time, sum, 2:15) %>%
  #                                            group_by(Time) %>%
  #                                            summarise(avg = ceiling((sum(sum)/length(unique(Appt.DateYear)))/60)))$avg)), 
  #                              style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "aqua"
  #   )
  # })
  
  # output$avgUtilization <- renderValueBox({
  #   
  #   valueBox(NULL,
  #            subtitle = tags$p(paste0("Avg utilization per day: ",
  #                                     paste0(round((sum((dataUtilization() %>% filter(comparison == 0))$sum))/
  #                                                    (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setHours*input$setRooms))*100),"%")), 
  #                              style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
  #   )
  # })
  
  # output$maxUtilization <- renderValueBox({
  #   
  #   valueBox(NULL,
  #            subtitle = tags$p(paste0("Peak utilization during the day: ",
  #                                     max((dataUtilization() %>%
  #                                            filter(comparison == 0) %>%
  #                                            select(Appt.DateYear, timeOptionsHr_filter) %>%
  #                                            gather(Time, sum, 2:15) %>%
  #                                            group_by(Time) %>%
  #                                            summarise(avg = round((sum(sum)/ 
  #                                                                     (length(unique(dataUtilization()$Appt.DateYear))*(60*input$setRooms)))*100)))$avg),"%"), 
  #                              style = "font-size: 180%; font-weight: bold; text-align: center;"), icon = NULL, color = "fuchsia"
  #   )
  # })
  
  
  
  utilization_data <- reactive({
    
    data <- dataUtilization() 
    test_data <<- data
    #dataUtilization <- utilization.data  %>% filter(CAMPUS %in% default_campus, CAMPUS_SPECIALTY %in% default_specialty)

    dataUtilization <- data %>% select(SUM, APPT_DATE_YEAR, all_of(timeOptionsHr_filter)) %>% collect()
    
    # denominator <- data %>% select(APPT_DATE_YEAR) %>% 
    #   mutate(APPT_DATE_YEAR= unique(APPT_DATE_YEAR)) %>% summarise(total= n()) %>% collect()
    
    denominator <- length(unique(dataUtilization$APPT_DATE_YEAR))
    
    
    set_rooms <- input$setRoom
    set_hours <- input$setHours
    
    utilization_day <- dataUtilization  %>%
         select(APPT_DATE_YEAR, SUM, all_of(timeOptionsHr_filter)) %>%
      gather(Time, SUM, all_of(timeOptionsHr_filter)) %>%
      group_by(Time) %>%
      summarise(avg = round((sum(SUM, na.rm = T)/
                               (denominator *(60*set_rooms)))*100))
    
    
    daysOfWeek.Table <- 
      data %>%
      group_by(APPT_DAY, APPT_DATE_YEAR) %>%
      dplyr::summarise(total = n()) %>% #collect()
      group_by(APPT_DAY) %>%
      dplyr::summarise(count = n()) %>% 
      collect()
    
    
    # space.hour.day <- data %>% group_by(APPT_DAY) %>% 
    #   summarise(across(c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00",
    #                      "15:00", "16:00", "17:00", "18:00", "19:00", "20:00"), ~ sum(.x, na.rm = TRUE))) %>% collect()
    
    
    space.hour.day <- data %>% group_by(APPT_DAY) %>% 
      summarise(across(all_of(timeOptionsHr_filter), ~ sum(.x, na.rm = TRUE))) %>% collect()
    
    
    space.hour.day <- reshape2::melt(space.hour.day, id=c("APPT_DAY"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$APPT_DAY, space.hour.day$APPT_DAY)]
    
    space.hour.day$average <- round(space.hour.day$value/(space.hour.day$days*60), 1)
    
    space.hour.day[is.na(space.hour.day)] <- 0
    
    
    data <- data.frame( 
      
    `Avg utilization per day: `= paste0(round((sum(dataUtilization$SUM))/
                                                (denominator *(60*input$setHours*input$setRooms))*100),"%"),
    
      
    `Peak utilization during the day: `= paste0(
      max((dataUtilization  %>%
             select(APPT_DATE_YEAR, all_of(timeOptionsHr_filter)) %>%
             gather(Time, SUM, all_of(timeOptionsHr_filter)) %>%
             group_by(Time) %>%
             summarise(avg = round((sum(SUM, na.rm = T)/ 
                                      (denominator*(60*input$setRooms)))*100)))$avg),"%"),
    # `Max # of rooms required during the day: `= paste0(
    #     max((dataUtilization  %>%
    #            select(APPT_DATE_YEAR, all_of(timeOptionsHr_filter)) %>%
    #            gather(Time, SUM, 2:15) %>%
    #            group_by(Time) %>%
    #            summarise(avg = ceiling((sum(SUM, na.rm = T)/length(unique(APPT_DATE_YEAR)))/60)))$avg))#,
    
    `Max # of rooms required during the day: `= paste0(max(ceiling(space.hour.day$average)))
    
    
    #`% Utilization (Visits per Room): `= NA, 
    
    #`% Utilization (Visits Duration): `= NA
    )
    
    
    
    data <- as.data.frame(t(data))
    
    rownames(data) <- c("Avg utilization per day: ","Peak utilization during the day: ","Max # of rooms required during the day: "#,
                       #"% Utilization (Visits per Room): ", "% Utilization (Visits Duration): " 
                       )
    data <- rownames_to_column(data)
    #data$rowname <- NULL
    
  })
   
  
  output$roomStat1 <- function(){
    data <- utilization_data()
    
    
    header_above <- c("title" = length(data))
    names(header_above) <-  paste0("Analysis based on ",input$setRooms," rooms available\n throughout ",input$setHours," hours") 
    #names(header_above) <-  paste0("Analysis based on ",setRooms," rooms available\n throughout ",setHours," hours") 
    
    
    data %>%
      kable(booktabs = T,escape = F, col.names = NULL )  %>%
      kable_styling(bootstrap_options = "hover", full_width = T, position = "center", row_label_position = "c", font_size = 20) %>%
      add_header_above(header_above, background = "#dddedd", color = "black", font_size = 20, bold = T, align = "c", line = F) %>%
      collapse_rows(columns = 1:2, valign = "top") %>%
      row_spec(1:2,  background = "#DC298D", color = "white", font_size = 20, bold = T, align = "c")%>%
      row_spec(3,  background = "#06ABEB", color = "white", font_size = 20, bold = T, align = "c")#%>%
      #row_spec(4,  background = "#06ABEB", color = "white", font_size = 20, bold = T, align = "c")%>%
      #row_spec(5,  background = "#212070", color = "white", font_size = 20, bold = T, align = "c")
    
    
  }
  
  
  
  
  
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
    data <- dataUtilization() 
    
    # data <- as.data.frame(utilization.data[arrived.utilization.data.rows,])
    # data <- utilization.data %>% filter(CAMPUS %in% "MSUS" , CAMPUS_SPECIALTY== "Allergy")
    
    # Days of Week Table
    
    daysOfWeek.Table <- 
      data %>%
      group_by(APPT_DAY, APPT_DATE_YEAR) %>%
      dplyr::summarise(total = n()) %>%
      group_by(APPT_DAY) %>%
      dplyr::summarise(count = n()) %>% 
      collect()
    
    #c.start <- which(colnames(data)=="07:00")
    #c.end <- which(colnames(data)=="20:00")
    
    #space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$APPT_DAY),FUN = sum)
    #space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    
    
    space.hour.day <- data %>% group_by(APPT_DAY) %>% 
      summarise(across(c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00",
                         "15:00", "16:00", "17:00", "18:00", "19:00", "20:00"), ~ sum(.x, na.rm = TRUE))) %>% collect()
    
   
    
    space.hour.day <- reshape2::melt(space.hour.day, id=c("APPT_DAY"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$APPT_DAY, space.hour.day$APPT_DAY)]
    
    space.hour.day$average <- round(space.hour.day$value/(space.hour.day$days*60), 1)
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Req")
    
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- space.hour.day %>% mutate(Time= gsub('`', "", Time))
                                                  
      
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    
    if (input$utilType == "actual"){
      subtitle <- "actual"
    } else {
      subtitle <- "scheduled"
    }
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=ceiling(Average_Req), col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Average Space Required by Time of Day and Day of Week",
           subtitle = paste0("Based on ", subtitle, " appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
           )+
      scale_color_MountSinai("main")+
      geom_point(size = 3.2) +
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
      geom_text(aes(label= ifelse(is.na(Average_Req),"", ceiling(Average_Req))), color="black", size=5, fontface="bold")
    
    grid.arrange(graph, table, ncol = 1, heights = c(5,3))
    
  })
  
  # Average Utilization by Time of Day
  output$spaceUtil <- renderPlot({
    data <- dataUtilization() 
    # data <- utilization.data[arrived.utilization.data.rows,]
    #data <- utilization.data %>% filter(CAMPUS %in% default_campus, CAMPUS_SPECIALTY %in% "Allergy")
    
    test_util <<- data
    
    if (input$utilType == "actual"){

      check <- data %>% 
        group_by(SCHEDULE_TO_ACTUAL_CONVERSION) %>%
        summarise(total = n()) %>% 
        collect() %>%
        spread(key = SCHEDULE_TO_ACTUAL_CONVERSION, value = total)
      
      
        #index <- which(!(colnames(check) %in% c("TRUE", "FALSE")))


      if (!(colnames(check) %in% c("TRUE"))){
        check <- check %>% mutate(`TRUE` = 0)
      }
      
      check <- check %>%
        mutate(ratio = (`TRUE`/(`TRUE`+ `FALSE` )))
      
      variable_to_check <- check$ratio
        
      check_test <<- check
      validate(need(variable_to_check < 0.05, "Not enough EPIC timestamp data to generate graphs. Please refer to the handbook and reach out to the team." ))

    }
    
    # Days of Week Table
    daysOfWeek.Table <- 
      data %>%
      group_by(APPT_DAY, APPT_DATE_YEAR) %>%
      dplyr::summarise(total = n()) %>%
      group_by(APPT_DAY) %>%
      dplyr::summarise(count = n()) %>% 
      collect()
    
    #c.start <- which(colnames(data)=="07:00")
    #c.end <- which(colnames(data)=="20:00")
    
    #space.hour.day <- aggregate(data[c(c.start:c.end)], list(data$APPT_DAY),FUN = sum)
    #space.hour.day <- reshape2::melt(space.hour.day, id=c("Group.1"))
    #space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$APPT_DAY,space.hour.day$Group.1)]
    
    space.hour.day <- data %>% group_by(APPT_DAY) %>% 
      summarise(across(c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00",
                         "15:00", "16:00", "17:00", "18:00", "19:00", "20:00"), ~ sum(.x, na.rm = T))) %>% collect()
    
    space.hour.day <- reshape2::melt(space.hour.day, id=c("APPT_DAY"))
    space.hour.day$days <- daysOfWeek.Table$count[match(daysOfWeek.Table$APPT_DAY, space.hour.day$APPT_DAY)]
    
   
    
    space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*input$setRooms), 1)
    #space.hour.day$utilization <- round(space.hour.day$value/(space.hour.day$days*60*8), 1)
    
    names(space.hour.day) <- c("Day","Time","Total_Dur","Days","Average_Util")
    #space.hour.day$Average_Util <- space.hour.day$Average_Util*100
    
    space.hour.day <- space.hour.day %>% mutate(Time= gsub('`', "", Time))
    byDayTime.df <- byDayTime.df[which(byDayTime.df$Day %in% unique(space.hour.day$Day)),]
    
    space.hour.day <- as.data.frame(merge(byDayTime.df,space.hour.day, by.x = c("Day","Time"), by.y = c("Day","Time"), all = TRUE))
    space.hour.day[is.na(space.hour.day)] <- 0
    
    space.hour.day <- space.hour.day %>% filter(Time %in% timeOptionsHr_filter)
    #space.hour.day$target <- 80
    space.hour.day$target <- 0.8
    
    if (input$utilType == "actual"){
      subtitle <- "actual"
    } else {
      subtitle <- "scheduled"
    }
    
    graph <- ggplot(space.hour.day, aes(x=Time, y=Average_Util, col=factor(Day,level = daysOfWeek.options), group=Day))+
      geom_line(size=1.2)+
      labs(x=NULL, y="Utilization (%)", 
           title = "Average Space Utilization (%) by Time of Day and Day of Week",
           subtitle = paste0("Based on ", subtitle, " appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
           )+
      scale_color_MountSinai("main")+
      #geom_hline(yintercept = .8, color = "red", linetype="dashed")+
      geom_hline(aes(yintercept = .8), color = "red", linetype="dashed")+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour.day$Average_Util)*1.2))+
      geom_point(size = 3.2) +
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
    data <- dataUtilization() 
    
   
    
    # c.start <- which(colnames(data)=="H_07_00")
    # c.end <- which(colnames(data)=="H_20_00")
    # 
    # space.hour <- aggregate(data[c(c.start:c.end)], list(data$APPT_DATE_YEAR),FUN = sum)
    # space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- data %>% group_by(APPT_DATE_YEAR) %>% 
      summarise(across(c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00",
                         "15:00", "16:00", "17:00", "18:00", "19:00", "20:00"), ~ sum(.x, na.rm = T))) %>% collect()
    
    
    
    space.hour <- reshape2::melt(space.hour, id=c("APPT_DATE_YEAR"))
    space.hour <- space.hour %>% mutate(variable= gsub('`', "", variable))
    
    space.hour[is.na(space.hour)] <- 0
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = round(quantile(value, probs=0.5)/60,1),
        `70th Percentile`= round(quantile(value, probs=0.75)/60,1),
        `90th Percentile`= round(quantile(value, probs=0.90)/60,1))
    
    space.hour <- space.hour %>% rename(Time= variable)
    
    #colnames(space.hour)[1] <- "Time"
    
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    if (input$utilType == "actual"){
      subtitle <- "actual"
    } else {
      subtitle <- "scheduled"
    }
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(limits=c(0, max(space.hour$value)*1.2))+
      labs(x=NULL, y="Number of Rooms\n",
           title = "Space Required by Percentile by Time of Day",
           subtitle = paste0("Based on ", subtitle, " appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
           )+
      scale_color_MountSinai("main")+
      geom_point(size = 3.2) +
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
    data <- dataUtilization() 
    #data <- utilization.data %>% filter(comparison == 0)
    
    
    # c.start <- which(colnames(data)=="07:00")
    # c.end <- which(colnames(data)=="20:00")
    # 
    # space.hour <- aggregate(data[c(c.start:c.end)], list(data$Appt.DateYear),FUN = sum)
    # space.hour <- reshape2::melt(space.hour, id=c("Group.1"))
    
    space.hour <- data %>% group_by(APPT_DATE_YEAR) %>% 
      summarise(across(c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00",
                         "15:00", "16:00", "17:00", "18:00", "19:00", "20:00"), ~ sum(.x, na.rm = TRUE))) %>% collect()
    
    space.hour <- reshape2::melt(space.hour, id=c("APPT_DATE_YEAR"))
    
    space.hour <- space.hour %>% mutate(variable= gsub('`', "", variable))
    
    space.hour[is.na(space.hour)] <- 0
    
    space.hour <- space.hour %>%
      group_by(variable) %>%
      dplyr::summarise( 
        Median = round(quantile(value, probs=0.5)/(60*input$setRooms),2),
        `70th Percentile`= round(quantile(value, probs=0.75)/(60*input$setRooms),2),
        `90th Percentile`= round(quantile(value, probs=0.90)/(60*input$setRooms),2))
    
    # space.hour <- space.hour %>%
    #   group_by(variable) %>%
    #   dplyr::summarise(
    #     Median = round(quantile(value, probs=0.5)/(60*8),2),
    #     `70th Percentile`= round(quantile(value, probs=0.75)/(60*8),2),
    #     `90th Percentile`= round(quantile(value, probs=0.90)/(60*8), 2))
    
    
    space.hour <- space.hour %>% rename(Time= variable)
    #colnames(space.hour)[1] <- "Time"
    space.hour <- as.data.frame(reshape2::melt(space.hour, id=c("Time")))
    
    space.hour <- space.hour %>% filter(Time %in% timeOptionsHr_filter)
    
    if (input$utilType == "actual"){
      subtitle <- "actual"
    } else {
      subtitle <- "scheduled"
    }
    
    graph <- ggplot(space.hour, aes(x=Time, y=value, col=variable, group=variable))+
      geom_line(size=1.2)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,max(space.hour$value)*1.2))+
      labs(x=NULL, y="Utilization (%)", 
           title = "Space Utilization (%) by Percentile by Time of Day",
           subtitle = paste0("Based on ", subtitle, " scheduled appointment time and duration from ",isolate(input$dateRangeUtil[1])," to ",isolate(input$dateRangeUtil[2]))
           )+
      scale_color_MountSinai("main")+
      geom_point(size = 3.2) +
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
    
    data <- dataArrivedPop() 
    #data <-  arrived.data.rows %>%  filter(CAMPUS== "MSUS")
    
    data <- data %>%
      group_by(COVERAGE) %>%
      dplyr::summarise(`Total Arrived Patients` = n()) %>%
      mutate(Percent = round((`Total Arrived Patients`/sum(`Total Arrived Patients`, na.rm = T))*100, 1)) %>%
      rename(Payer = COVERAGE) %>% collect() %>%
      mutate(Percent = paste0(sprintf("%.1f", Percent), "%")) %>%
      arrange(desc(`Total Arrived Patients`))
    
    data$Payer[is.na(data$Payer)] <- "Unknown"
    data$`Total Arrived Patients` <- prettyNum(data$`Total Arrived Patients`, big.mark = ',')
    
    data %>%
      knitr::kable("html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover"), full_width=T, position="center", font_size = 15) %>%
      row_spec(0, bold=T, background = "#dddedd", color = "black") %>%
      column_spec(1, bold=T) %>%
      add_header_above(c("Payer Breakdown" = length(data)),
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
    #data <- population_tbl %>% filter(CAMPUS_SPECIALTY== "Allergy")
    #data <- population_data_filtered %>% filter(Campus.Specialty == "Allergy")
    newdata <- uniquePts_df_system(data)
  
    
    
    
    a_table <- newdata %>% 
      #filter(NEW_ZIP_CODE_LAYER_A != "EXCLUDE") %>%
      mutate(NEW_ZIP_CODE_LAYER_A= ifelse(is.null(NEW_ZIP_CODE_LAYER_A), "Out of NYS", NEW_ZIP_CODE_LAYER_A))%>%
      group_by(NEW_ZIP_CODE_LAYER_A) %>% summarise(total = n()) %>%
      arrange(-total) %>%
      mutate(perc = round(total/sum(total, na.rm = T), 2)*100) %>% collect()%>%
      adorn_totals("row") %>%
      mutate(perc = paste0(sprintf("%.1f", perc), "%")) %>%
      `colnames<-` (c("Zip Code Layer", "total", "perc")) %>%
      mutate(Layer = `Zip Code Layer`)
    
    b_table <- newdata %>%
      #filter(NEW_ZIP_CODE_LAYER_A != "EXCLUDE") %>%
      mutate(NEW_ZIP_CODE_LAYER_B= ifelse(is.null(NEW_ZIP_CODE_LAYER_B), "Other", NEW_ZIP_CODE_LAYER_B))%>%
      group_by(NEW_ZIP_CODE_LAYER_A, NEW_ZIP_CODE_LAYER_B) %>% summarise(total = n()) %>%
      arrange(-total) %>% collect()
    
    b_table <- b_table %>%
      mutate(perc = round(total/sum(b_table$total, na.rm = T),2)*100) %>% 
      mutate(perc = paste0(sprintf("%.1f", perc), "%")) %>% 
      `colnames<-` (c("Layer", "Zip Code Layer", "total", "perc")) %>%
      filter(`Layer` %in% c("Manhattan", "Out of NYS", "Long Island", "Northern New York"))
    
    zip_table <- bind_rows(a_table, b_table)
    zip_table <- zip_table[order(factor(zip_table$Layer, levels = unique(a_table$Layer))),]
    zip_table$Layer <- NULL
    
    
    
    
    # Table subtitle based on date range filter
    # manhattan_ref <- which(zip_table$`Zip Code Layer` == "Manhattan")
    # out_state_ref <-  which(zip_table$`Zip Code Layer` == "Out of NYS")
    # long_is_ref <-  which(zip_table$`Zip Code Layer` == "Long Island")
    # northern_ny_ref <-  which(zip_table$`Zip Code Layer` == "Northern New York")
  
    
    header_above <- c("Subtitle" = 3)
    names(header_above) <- paste0(c("Based on arrived data from "),c(isolate(input$dateRangepop[1])),c(" to "),c(isolate(input$dateRangepop[2])))
    total <-  which(zip_table$`Zip Code Layer` == "Total") - 1 
    # northern_ny_ref_1 <- northern_ny_ref + 1
    # northern <- c(northern_ny_ref_1:total)
    #names(header_above) <- paste0(c("Based on data from "),c("test"),c(" to "),c("test"))
    
    main_rows <- c("Manhattan" , "Brooklyn", "Queens", "Long Island", "Nassau", "Bronx" ,  "Out of NYS",
                   "Staten Island", "Westchester", "Northern New York")
    
    rows_for_indnent <- which(!zip_table$`Zip Code Layer` %in% main_rows)
    
    
    zip_table %>%
      kable(escape = F,
            col.names = c("Zip Code Layer", "Total Unique Patients", "Percent of Total")) %>%
      kable_styling(bootstrap_options = c("hover","bordered"), full_width = FALSE, position = "center", row_label_position = "l", font_size = 16) %>%
      add_header_above(header_above, color = "black", font_size = 16, align = "center", italic = TRUE) %>%
      add_header_above(c("Total Unique Patients by Zipcode" = length(zip_table)),
                       color = "black", font_size = 20, align = "center", line = FALSE) %>%
      add_indent(c(rows_for_indnent), level_of_indent = 2) %>%
      # add_indent(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3,
      #              out_state_ref+1, out_state_ref+2, out_state_ref+3, out_state_ref+4, out_state_ref+5,
      #              long_is_ref+1, long_is_ref+2,
      #              northern#northern_ny_ref+1#, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5
      # ),
      # level_of_indent = 2) %>%
      row_spec(0, background = "#d80b8c", color = "white", bold = T) %>%
      # row_spec(c(manhattan_ref+1, manhattan_ref+2, manhattan_ref+3, out_state_ref+1, out_state_ref+2,
      #            out_state_ref+3, out_state_ref+4, out_state_ref+5, long_is_ref+1, long_is_ref+2,
      #            northern#northern_ny_ref+1#\, northern_ny_ref+2, northern_ny_ref+3, northern_ny_ref+4, northern_ny_ref+5
      # ), font_size = 14) %>%
      row_spec(c(rows_for_indnent), font_size = 14) %>%
      row_spec(nrow(zip_table), background = "#d80b8c", color = "white", bold = T)
    
  }
  
  
  output$population1 <- renderLeaflet({
    
    # population.data <- dataArrivedPop()
    population.data <- dataArrivedPop()

    #population.data <- population_tbl %>% filter(CAMPUS == "MSUS", CAMPUS_SPECIALTY== "Internal Medicine")
    
    # newdata <- population.data %>% group_by(LATITUDE, LONGITUDE) %>% dplyr::summarise(total = round(n(),0))%>% collect()
    
    newdata <- population.data %>% group_by(NEW_ZIP, LATITUDE, LONGITUDE) %>% dplyr::summarise(total = round(n(),0))%>% collect() %>% filter(!is.na(NEW_ZIP))
    
    newdata_unique <- population.data %>% filter(!is.na(NEW_ZIP)) %>% select(NEW_ZIP, MRN) %>% distinct() %>% group_by(NEW_ZIP) %>% summarise(total_group = n()) %>% collect() 
    newdata_unique_total <- newdata_unique %>% mutate(total = sum(total_group)) %>% select(-total_group)
    
    newdata_join <- inner_join(newdata_unique, newdata_unique_total)
    newdata_join <- newdata_join %>% mutate(percent = round(total_group/total, 3)*100) %>% select(-total_group,-total)
    
    newdata <- inner_join(newdata, newdata_join)
    # newdata_coordinates <- geocode_zip(newdata$NEW_ZIP) %>% rename(NEW_ZIP = zipcode,
    #                                                                LATITUDE = lat,
    #                                                                LONGITUDE = lng)
    # 
    # newdata <- inner_join(newdata, newdata_coordinates) #%>% select(-NEW_ZIP)
    
    hist <- histogram(newdata$total, type = "irregular", grid = "quantiles", 
                      greedy = TRUE, breaks= 30,  plot = F)
    
    
    # Create a color palette with handmade bins.
    #mybins <- ceiling(seq(min(newdata$total), max(newdata$total), length.out=5))
    mybins <- round(hist$breaks,0)
    
    index =1 
    while (index < length(mybins)){
      if (mybins[index+1] - mybins[index] < 10){
        mybins <- mybins[- (index+1)]
      } else {
        index = index + 1
      }
      
    }
    
    
    mypalette <- colorBin(palette= MountSinai_palettes$dark, domain=quakes$mag, na.color="transparent", bins=mybins)
    
                                          
    # Create a color palette with handmade bins.
    #mybins <- ceiling(seq(min(newdata$total), max(newdata$total), length.out=10))
    #mypalette <- colorBin(palette= MountSinai_palettes$dark, domain=quakes$mag, na.color="transparent", bins=mybins)
    #mypalette <- colorFactor(palette= MountSinai_palettes$new , levels = newdata$bins)
    # Prepare the text for the tooltip:
    mytext <- paste(
      "Total Visits: ", format(newdata$total,big.mark=",",scientific=FALSE), "<br/>", 
      "% of Unique Patients: ", paste0(newdata$percent, "%"), "<br/>", 
      # "Longitude: ", newdata$LONGITUDE, sep="",
      "Zip Code:", newdata$NEW_ZIP, sep="") %>%
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
      addCircleMarkers(~LONGITUDE, ~LATITUDE, 
                       fillColor = ~ mypalette(total), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
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
    print("volume1")
    data <- dataArrived_summary()
     # data <- arrived.data.rows.summary %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")%>%
     #   filter(!holiday %in% c("Christmas"))
    # data <- setDT(data)
    # 
    # pts.count <- data[,list(Volume = .N), by = list(Appt.DateYear)]  
    # 
    
    # data <- data %>% select(UNIQUEID, APPT_DATE_YEAR) %>% collect()
    #data <- data %>% select(APPT_DATE_YEAR)
    
    pts.count <- data %>% group_by(APPT_DATE_YEAR) %>% summarise(total = sum(TOTAL_APPTS)) %>%
                      collect()

    
    # pts.count <- aggregate(data$UNIQUEID,
    #                        by=list(data$APPT_DATE_YEAR), FUN=NROW)
    
    # pts.count <- aggregate(arrived.data$uniqueId,
    #                        by=list(arrived.data$Appt.DateYear), FUN=NROW)
    
    names(pts.count) <- c("Date","Volume")
    pts.count$Date <- as.Date(pts.count$Date, format="%Y-%m-%d")
    
    pts.count <- pts.count[order(as.Date(pts.count$Date, format="%Y-%m-%d")),]
    
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
    print("volume2")
    data <- dataArrived_summary()
    #data <- arrived.data.rows.summary %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology")
    #data <- setDT(data)
    #data <- unique(data, by = "uniqueId")
    
    #pts.by.month <- data[,list(Volume = .N), by = list(Appt.MonthYear,Visit.Method)]  
    
    #data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD) #%>% collect()

    # pts.by.month <- aggregate(data$UNIQUEID,
    #                           by=list(data$APPT_MONTH_YEAR, data$VISIT_METHOD), FUN=NROW)
    
    pts.by.month <- data %>% group_by(APPT_MONTH_YEAR, VISIT_METHOD) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()

    names(pts.by.month) <- c("Month", "Visit.Method", "Volume")
    pts.by.month$Volume <- as.numeric(pts.by.month$Volume)
    #pts.by.month$Month <- as.yearmon(pts.by.month$Month, format="%Y-%m")
    #pts.by.month$Month <- as.Date(pts.by.month$Month, format="%Y-%m")
    
    pts.by.month <- pts.by.month[order(pts.by.month$Month, pts.by.month$Volume),]
    factor_levels <- sort(unique(pts.by.month$Visit.Method), na.last = T)
    
    
    #factor_levels = c("TELEPHONE VISITS", "VIDEO_TELEHEALTH VISITS","IN PERSON")
    
    #factor_levels = c("TELEPHONE VISITS", "VIDEO_TELEHEALTH VISITS","IN PERSON", "PORTING VV TH TP")
    pts.by.month$Visit.Method <- factor(pts.by.month$Visit.Method, levels = factor_levels)
    
    pts.by.month <- pts.by.month %>%
      group_by(Month) %>%
      mutate(percent = paste0("(",round(Volume/sum(Volume)*100,0), "%)"))
    
    # percent <- pts.by.month %>% 
    #   group_by(Month) %>%
    #   mutate(percent = paste0("(",round(Volume/sum(Volume)*100,0), "%)"))
    # 
    # pts.by.month <- full_join(pts.by.month,percent)
    
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
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Month), geom="text", color="black", 
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
    setDT(pts.by.month)
    
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
    plot <- g3 + g4 + plot_layout(ncol = 1, heights = c(10, 1 * length(unique(pts.by.month$Visit.Method))))
    
    plot
  })
  # Average Daily Patient Volume by Day of Week
  output$volume3 <- renderPlot({
    print("volume3")
    # data <- dataArrived()
    # #data <- kpi.all.data[arrived.data.rows,] %>% filter(Provider == "ABBOTT, ASHLEY")
    # 
    # data <- setDT(data)
    # 
    # pts.by.day <- data[,list(Volume = .N), by = list(Appt.Day,Visit.Method)]  
    # # pts.by.day <- aggregate(data$uniqueId, 
    # #                         by=list(data$Appt.Day, data$Visit.Method), FUN=NROW)
    # 
    # 
    # # pts.by.day <- aggregate(kpi.all.data[arrived.data.rows,]$uniqueId,
    # #                           by=list(kpi.all.data[arrived.data.rows,]$Appt.Day,kpi.all.data[arrived.data.rows,]$Visit.Method), FUN=NROW)
    # 
    # #names(pts.by.day) <- c("Day", "Volume")
    # names(pts.by.day) <- c("Day","Visit.Method", "Volume")
    # # totalDates <- seq(as.Date(min((kpi.all.data[arrived.data.rows,])$Appt.DTTM)),
    # #                                 as.Date(max((kpi.all.data[arrived.data.rows,])$Appt.DTTM)),by="days")
    # first_date <- arrived_first_date
    # second_date <- arrived_last_date
    # totalDates <- seq(first_date,second_date, by = "days")
    # totalDates <- as.data.frame(totalDates)
    # names(totalDates) <- c("Dates")
    # totalDates$day <- format(as.Date(totalDates$Dates, format="%Y-%m-%d"), "%a")
    # totalDates <- aggregate(totalDates$Dates,
    #                         by=list(totalDates$day), FUN=NROW)
    # names(totalDates) <- c("Day","Count")
    # 
    # pts.by.day$Day.Count <- totalDates$Count[match(pts.by.day$Day, totalDates$Day)]
    # pts.by.day$Avg.Volume <- as.numeric(round(pts.by.day$Volume/pts.by.day$Day.Count,1))
    # 
    # pts.by.day <- pts.by.day[order(pts.by.day$Day, pts.by.day$Avg.Volume),]
    # factor_levels <- unique(pts.by.day$Visit.Method)
    # 
    # pts.by.day$Visit.Method <- factor(pts.by.day$Visit.Method, levels = factor_levels)
    
    
    # data <- dataArrived_summary()
    data <- dataArrived()
    data_test <<- data
    
    
    # pts.by.day <- data %>% group_by(APPT_DAY, VISIT_METHOD) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()
    pts.by.day <- data %>% group_by(APPT_DAY, VISIT_METHOD) %>% summarise(total = n()) %>% collect()
    
    
    names(pts.by.day) <- c("Day","Visit.Method", "Volume")
    
    totalDates <- data %>% group_by(APPT_DAY, VISIT_METHOD) %>% summarise(Day.Count = count(unique(APPT_DATE_YEAR))) %>% collect()
    # totalDates <- as.data.frame(seq(as.Date(min(data$Appt.DTTM)), as.Date(max(data$Appt.DTTM)),by="days"))
    # names(totalDates) <- c("Dates")
    # totalDates$day <- format(as.Date(totalDates$Dates, format="%Y-%m-%d"), "%a")
    # totalDates <- aggregate(totalDates$Dates,
    #                         by=list(totalDates$day), FUN=NROW)
    # names(totalDates) <- c("Day", "Count")
    # 
    # 
    # totalDates <- data %>% group_by(Visit.Method) %>% summarise(Day = distinct(Appt.DateYear))
    
    #pts.by.day$Day.Count <- totalDates$Day[match(pts.by.day$Day, totalDates$APPT_DAY)]
    # pts.by.day <- inner_join(pts.by.day, totalDates, by = c("Day" = "APPT_DAY", "Visit.Method" = "VISIT_METHOD"))
    pts.by.day <- inner_join(pts.by.day, totalDates, by = c("Day" = "APPT_DAY", "Visit.Method" = "VISIT_METHOD"))
    
    pts.by.day$Avg.Volume <- as.numeric(ceiling(pts.by.day$Volume/pts.by.day$Day.Count))
    
    factor_levels <- sort(unique(pts.by.day$Visit.Method), na.last = T)

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
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",..y..), group = Day), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
  })
  
  # Daily Volume Distribution by Month
  output$volume4 <- renderPlot({
    
    print("volume4")
  
    data <- dataArrived_summary() #%>% select(APPT_MONTH_YEAR, APPT_DATE_YEAR) #%>% collect()
    # data <- arrived.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology") %>%
    #                                                         select(UNIQUEID, APPT_MONTH_YEAR, APPT_DATE)
    
    
    pts.dist.testing <<- data %>% group_by(APPT_MONTH_YEAR, APPT_DATE_YEAR) %>% summarise(total = sum(TOTAL_APPTS)) %>% show_query()
    pts.dist <- data %>% group_by(APPT_MONTH_YEAR, APPT_DATE_YEAR) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()
    
    # pts.dist <- aggregate(data$UNIQUEID,
    #                       by=list(data$APPT_MONTH_YEAR, data$APPT_DATE), FUN=NROW)

    # pts.dist <- aggregate(kpi.all.data[arrived.data.rows,]$uniqueId,
    #                       by=list(kpi.all.data[arrived.data.rows,]$Appt.MonthYear, kpi.all.data[arrived.data.rows,]$Appt.Date), FUN=NROW)

    names(pts.dist) <- c("Month","Date","Volume")
    pts.dist$Month <- as.yearmon(pts.dist$Month, format="%Y-%m")
    pts.dist$Month <- as.Date(pts.dist$Month, format="%Y-%m")
    pts.dist <- pts.dist[order(pts.dist$Month),]
    pts.dist <- pts.dist %>% ungroup()
   
    
    # data <- dataArrived()
    # #data <- data <- kpi.all.data[arrived.data.rows,] %>% filter(Provider == "ABBOTT, ASHLEY")
    # 
    # pts.dist <- aggregate(data$uniqueId, 
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    # 
    # names(pts.dist) <- c("Month","Date","Day","Volume")
    # 
    # pts.dist.summary <-
    #   pts.dist %>%
    #   group_by(Month) %>%
    #   summarise(Avg = round(mean(Volume),1), Median = median(Volume), Min = min(Volume), Max = max(Volume), N = n())
    # 
    # pts.dist.summary <- 
    #   pts.dist.summary[order(as.yearmon(pts.dist.summary$Month,format="%b-%Y")),]
    
    
    
    min_date <- min(pts.dist$Month)
    max_date <- max(pts.dist$Month)
    
    g1 <- ggplot(pts.dist, aes(x=Month, y=Volume, group=Month))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
      stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Month - All Visits"#,
           #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
           )+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5)) +
      # scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m",
      #              limits = c(as.Date("2021-01-01"), as.Date("2022-12-31"))
      #              )
      scale_x_date(breaks = seq.Date(from = as.Date(min_date), 
                                     to = as.Date(max_date), by = "month"), date_labels = "%Y-%m")
      
    #setDT(data)
    #pts.dist <- data[,list(Volume = .N), by = list(APPT_MONTH_YEAR, APPT_DATE)] 
    
    pts.dist <- data %>% group_by(APPT_MONTH_YEAR, APPT_DATE_YEAR) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()
    
    # pts.dist <- aggregate(dataArrived()$uniqueId, 
    #                       by=list(dataArrived()$Appt.MonthYear, dataArrived()$Appt.Date), FUN=NROW)
    
    # pts.dist <- aggregate(arrived.data$uniqueId,
    #                       by=list(arrived.data$Appt.MonthYear, arrived.data$Appt.Date), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Volume")
    
    pts.dist.summary <-
      pts.dist %>%
      group_by(Month) %>%
      dplyr::summarise(Avg = ceiling(mean(Volume)), Median = ceiling(median(Volume)), Min = ceiling(min(Volume)), Max = ceiling(max(Volume)), N = n())
    
    pts.dist.summary <- pts.dist.summary %>%
                        mutate(Month = as.Date(paste0(Month, "-01"))) %>%
                        complete(Month = seq.Date(min(Month), max(Month), by="month")) %>%
                        mutate(Month = format(Month, "%Y-%m"))
                      
    
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
    data_melt$Month <- as.yearmon(data_melt$Month, format="%Y-%m")
    data_melt$Month <- as.Date(data_melt$Month, format="%Y-%m")
    data_melt[is.na(data_melt)] <- 0
    
    month_order <- data.frame(month = unique(data_melt$Month))
    month_order <- month_order %>% arrange(month) %>% mutate(month_year = format(month, "%Y-%m"))
    month_order <- unique(month_order$month_year)
    
    data_melt$Month <- format(data_melt$Month, "%Y-%m")
    data_melt$Month <- factor(data_melt$Month, levels = month_order)

    g2 <- ggplot(data_melt, aes(x = Month, y = variable, label = value))+
      scale_color_MountSinai('dark' )+
      geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
      geom_hline(yintercept = hline_y, colour='black')+
      geom_vline(xintercept = 0, colour = 'black') +
      scale_x_discrete(position = "top") + 
      labs(y = NULL, x = NULL, fill = "AssociationListA")+
      theme_minimal() +
      table_theme() #+
      # scale_x_date(breaks = seq.Date(from = as.Date(min_date),
      #                                to = as.Date(max_date), by = "month"))+
      # theme(panel.grid.minor.x = element_blank()) +
      # theme(axis.text.x=element_blank())
    
    
    # g2 <- ggplot(data_melt, aes(x = Month, y = variable, label = value))+
    #   scale_color_MountSinai('dark' )+
    #   geom_text(size = 5, vjust = "center", hjust = "center", fontface  = "bold")+
    #   geom_hline(yintercept = hline_y, colour='black')+
    #   geom_vline(xintercept = 0, colour = 'black')
    #   labs(y = NULL, x = NULL, fill = "AssociationListA")+
    #   theme_minimal() +
    #   table_theme() +
    #   scale_x_date(breaks = seq.Date(from = as.Date(min_date), 
    #                                  to = as.Date(max_date), by = "month"))+
    #   theme(panel.grid.minor.x = element_blank()) +
    #   theme(axis.text.x=element_blank())
    
    g1 + g2 + plot_layout(ncol = 1, heights = c(7, 0.67 * length(unique(data_melt$variable))))
    
  })
  
  
  # Daily Volume Distribution by Day of Week
  output$volume5 <- renderPlot({
    
    print("volume5")
    data <- dataArrived_summary()
    #data <- data <- kpi.all.data[arrived.data.rows,] %>% filter(Provider == "ABBOTT, ASHLEY") %>% filter(Appt.DateYear >= "2021-01-01")
    #data <- data %>% select(APPT_MONTH_YEAR, APPT_DATE_YEAR, APPT_DAY) #%>% collect()
    #data <- setDT(data)
    
    # pts.dist <- data[,list(Volume = .N), by = list(APPT_MONTH_YEAR, APPT_DATE, APPT_DAY)]  
    
    pts.dist <- data %>% group_by(APPT_MONTH_YEAR, APPT_DATE_YEAR, APPT_DAY) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()
    
    # pts.dist <- aggregate(data$uniqueId, 
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    
    names(pts.dist) <- c("Month","Date","Day","Volume")
    
    g1 <- ggplot(pts.dist, aes(x=factor(Day, level = daysOfWeek.options), y=Volume))+
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+ 
      stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      labs(x = NULL, y = "Patients",
           title = "Daily Patient Volume Distribution by Day of Week - All Visits",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
           )+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(axis.text.x = element_text(size = 16, angle=0, hjust = 0.5))
    
    
    # data <- filter(arrived.data, Campus == "MSUS")
    # pts.dist <- aggregate(data$uniqueId,
    #                       by=list(data$Appt.MonthYear, data$Appt.Date, data$Appt.Day), FUN=NROW)
    
    
    # pts.dist <- data[,list(Volume = .N), by = list(APPT_MONTH_YEAR,APPT_DATE,APPT_DAY)]
    pts.dist <- data %>% group_by(APPT_MONTH_YEAR, APPT_DATE_YEAR, APPT_DAY) %>% summarise(total = sum(TOTAL_APPTS)) %>% collect()
    
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
    
    col.order <- toupper(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
    
    data_melt$Day <- factor(data_melt$Day, levels = col.order)
    
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
  
  output$volume_am_pm <- renderPlot({
    data <- dataArrived()
    #data <- arrived.data.rows %>% filter(CAMPUS == 'MSUS', CAMPUS_SPECIALTY == 'Allergy')
    
    data_process <- data %>%
                    group_by(APPT_DAY, AM_PM) %>% summarise(total = n()) %>% collect()
    
    total_dates <- data %>% group_by(APPT_DAY, AM_PM) %>% summarise(Day.Count = count(unique(APPT_DATE_YEAR))) %>% collect()
    
    data_process <- inner_join(data_process, total_dates) %>% mutate(total = ceiling(total/Day.Count))
    
    
    ggplot(data_process, aes(x = factor(APPT_DAY, levels = daysOfWeek.options), y = total, group = AM_PM, fill = AM_PM)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_fill_MountSinai('dark') +
      labs(x = NULL, y = "Patients",
           title = "Average Session* Breakdown",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*PM appointments occur after 12"
           )+
      scale_y_continuous(limits=c(0,(max(data_process$total, na.rm = TRUE))*2))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      theme(axis.text.x = element_text(size = 16, angle=0, hjust=0.5)) +
      geom_col(position = position_dodge())+
      # geom_text(data=subset(data_process, total > 0.15 * max(total)),aes(label=total), color="white", 
      #           size=5, fontface="bold", position = position_stack(vjust = 0.5))+
      geom_text(data=subset(data_process, total > 0.15 * max(total)),aes(label=total), color="white",
                size=5, fontface="bold", position = position_dodge(width = 0.9), vjust = 4.5)#+
      # stat_summary(fun = sum, vjust = 4, aes(label=ifelse(..y.. == 0,"",..y..), group = APPT_DAY), geom="text", color="black",
      #              size=5, fontface="bold.italic")
    
    
    
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
    #data <- dataArrived_access_npr()
    data <- dataArrived_access_npr_new()
    
    test_npr <<- data
    # data <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy"  )
     print("1")


    newpatients.ratio <- data %>%
      group_by(APPT_MADE_MONTH_YEAR,NEW_PT3) %>%
      dplyr::summarise(Total = sum(TOTAL_APPTS, na.rm = TRUE)) %>% collect() %>%
      mutate(NEW_PT3 = ifelse(is.na(NEW_PT3), "ESTABLISHED", NEW_PT3)) %>%
      spread(NEW_PT3, Total) 
    
    newpatients.ratio[is.na(newpatients.ratio)] <- 0
    
    
    print("1.5")
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
   
    newpatients.ratio <- newpatients.ratio %>% mutate(ratio = round(`NEW` / (`ESTABLISHED` + `NEW`),2))
    #newpatients.ratio$Appt.MonthYear <- as.Date(newpatients.ratio$Appt.MonthYear, format="%Y-%m") ## Create date-year column
    #newpatients.ratio[is.na(newpatients.ratio)] <- 0
    ggplot(newpatients.ratio, aes(x=APPT_MADE_MONTH_YEAR, y=ratio, group=1)) +
      # geom_bar(stat = "identity", width = 0.8, fill = "#221f72") +
      geom_line(size=1) +
      geom_line(color = "#221f72", size=1) +
      geom_point(color = "#221f72", size = 3.2) +
      labs(x=NULL, y=NULL,
           #title = "New Patient Ratio Trending over Time",
           title = "Monthly New Patient Ratio",
           subtitle = paste0("Based on arrived data from ", start_date," to ", end_date))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(newpatients.ratio$ratio)*1.5)
                         ) +
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                   size=5, fontface="bold.italic")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    geom_point(size = 3.2)+
      geom_point(color = "#221f72", size = 3.2) +
      scale_color_manual(values = c("#221f72", "#d80b8c"))
    # scale_x_date(breaks = "day", date_labels = "%Y-%m", date_breaks = "1 week",
    #              date_minor_breaks = "1 day", expand = c(0, 0.6))
    
  })
  
  
  # New Patient Ratio by Provideer
  output$newPtRatioByProv <- renderPlot({
    #data <- dataArrived_access_npr()
    data <- dataArrived_access_npr_new()
    #data <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS", CAMPUS_SPECIALTY %in% "Allergy")
    
    print("prov_running")
    newpatients.ratio <- data %>%
      group_by(PROVIDER, APPT_MADE_MONTH_YEAR,NEW_PT2) %>%
      dplyr::summarise(Total = sum(TOTAL_APPTS, na.rm = TRUE)) %>% collect() %>%
      mutate(NEW_PT2 = ifelse(is.na(NEW_PT2), "ESTABLISHED", NEW_PT2)) %>%
      group_by(PROVIDER,APPT_MADE_MONTH_YEAR,NEW_PT2) %>%
      dplyr::summarise(Total = n()) %>%
      spread(NEW_PT2, Total)
    
    newpatients.ratio[is.na(newpatients.ratio)] <- 0
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
    newpatients.ratio <-  newpatients.ratio %>% mutate(ratio = round(`NEW` / (`ESTABLISHED` + `NEW`), 2))
    #newpatients.ratio$Appt.MonthYear <- as.Date(paste0(newpatients.ratio$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
    
    ggplot(newpatients.ratio, aes(x=APPT_MADE_MONTH_YEAR, y=ratio, group = PROVIDER)) +
      geom_line(aes(color=PROVIDER), size=1) +
      # scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
      scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
      labs(x=NULL, y=NULL, 
           title = "New Patient Ratio Over Time by Provider",
           subtitle = paste0("Based on data from ", start_date," to ", end_date))+
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
    #data <- dataAll_access()
    data <-   dataAll_access_new()
    
    test_new <<- data
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(WAIT_TIME >= 0) %>%
      group_by(APPT_MADE_MONTH_YEAR, NEW_PT2) %>%
      dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
      filter(NEW_PT2 %in% c("NEW","ESTABLISHED")) %>% collect()
 
    
    waitTime$NEW_PT2 <- ifelse(waitTime$NEW_PT2 == "NEW", "New","Established")
    #waitTime$Appt.MonthYear <- as.Date(waitTime$Appt.MonthYear, format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(NEW_PT2, medWaitTime) 
    #waitTime$`New Patient Target <= 14` <- 14
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 2:3)
    target <- 14
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
    ggplot(waitTime, aes(x=APPT_MADE_MONTH_YEAR, y=value, group = variable, color=variable))+
      # geom_bar(stat = "identity", position = 'dodge')+
      geom_line(size=1) +
      geom_abline(slope=0, intercept=14,  col = "red",lty=2, size = 1) +
      #geom_line(aes(linetype = variable))+
      #scale_linetype_manual(values=c("solid", "solid", "dashed"))+
      #scale_size_manual(values=c(1, 1, 1.3))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      labs(x=NULL, y=NULL,
           #title = "Median Wait Time to New and Established Appointment Over Time",
           title = "Monthly Median Wait Time to New and Established Appointment",
           #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
           subtitle = paste0("Based on scheduled data from ",start_date," to ",end_date)#,
           #caption = "*New patients defined by CPT codes (level of service)."
      )+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      geom_label(aes(x = 0.8, y = target, label = paste0("Target: ", target," days")), fill = "white", fontface = "bold", color = "red", size=4)+
      #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
      scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_color_manual(values=c('#212070','#d80b8c')) +
      geom_point(size = 3.2)
    # stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..)), group = value), geom="text", color="black", 
    #              size=5, fontface="bold.italic")
    
  })
  
  output$newPtWaitTimeByDeptPercent <- renderPlot({
    #data <- dataAll_access()
    data <- dataAll_access_new()
    # data_test <<- data
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime_total <- data %>%
      filter(WAIT_TIME >= 0) %>%
      filter(NEW_PT2 == "NEW") %>%
      group_by(APPT_MADE_MONTH_YEAR) %>%
      dplyr::summarise(total_all = n()) %>%
      collect()
    
    waitTime_within_14_days <- data %>%
                                filter(WAIT_TIME >= 0, 
                                       WAIT_TIME < 14.0001, 
                                       NEW_PT2 == "NEW") %>%
                                group_by(APPT_MADE_MONTH_YEAR) %>%
                                dplyr::summarise(total = n()) %>%
                                collect()
    
    join_data <- inner_join(waitTime_total, waitTime_within_14_days)
    
    percent_within_14_days <- join_data %>% group_by(APPT_MADE_MONTH_YEAR) %>%
                              summarise(percent = round((total/total_all),2))
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
    ggplot(percent_within_14_days, aes(x=APPT_MADE_MONTH_YEAR, y=percent, group = 1))+
      # geom_bar(stat = "identity", position = 'dodge')+
      #geom_line(aes(linetype = variable))+
      scale_linetype_manual(values=c("solid"))+
      scale_size_manual(values=c(1))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      labs(x=NULL, y=NULL,
           #title = "Median Wait Time to New and Established Appointment Over Time",
           title = "Percent of New Patients Scheduled Within 14 Days",
           #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
           subtitle = paste0("Based on scheduled data from ",start_date," to ",end_date)#,
           #caption = "*New patients defined by CPT codes (level of service)."
      )+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
      #scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      scale_color_manual(values=c('#212070')) +
      geom_point(size = 3.2, color = '#d80b8c') +
      geom_line(size=1, color = '#d80b8c') +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(percent_within_14_days$percent)*1.5)
      ) +
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
    
  })
  
  
  # New Patient Wait Time
  output$newPtWaitTimeByProv <- renderPlot({
    #data <- dataAll_access()
    data <- dataAll_access_new()
    # data <- all.data %>% filter(Provider %in% c("BODDU, LAVANYA","CHUEY, JOHN N"))
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- data %>%
      filter(WAIT_TIME >= 0) %>%
      group_by(PROVIDER, APPT_MADE_MONTH_YEAR, NEW_PT2) %>%
      dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>% collect()
    
    waitTime$NEW_PT2 <- ifelse(waitTime$NEW_PT2 == "NEW", "New","Established")
    #waitTime$Appt.MonthYear <- as.Date(paste0(waitTime$Appt.MonthYear, "-01"), format="%Y-%m-%d") ## Create date-year column
    
    waitTime <- waitTime %>% spread(NEW_PT2, medWaitTime)
    waitTime[is.na(waitTime)] <- 0
    waitTime <- waitTime %>% gather(variable, value, 3:4)
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
    ggplot(waitTime %>% filter(variable == "Established"), aes(x=APPT_MADE_MONTH_YEAR, y=value, group=PROVIDER)) +
      geom_line(aes(color=PROVIDER), size=1) +
      # geom_hline(yintercept=14, linetype="dashed", color = "red", size=1)+
      scale_color_MountSinai("main",reverse = TRUE, labels = wrap_format(25))+
      labs(x=NULL, y=NULL, 
           title = "Median Wait Time to New and Established Appointment Over Time by Provider",
           #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
           subtitle = paste0("Based on data from ", start_date," to ",end_date)#,
           #caption = "*New patients defined by CPT codes (level of service)."
           )+
      theme_new_line()+
      theme_bw()+
      graph_theme("bottom")+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      scale_y_continuous(expand = c(0,0), limits = c(0,max(waitTime$value)*1.5))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      geom_point(aes(color=PROVIDER), size = 3.2)
  })
  
  
  output$newPtWaitTimeByProvPercent <- renderPlot({
    #data <- dataAll_access()
    data <- dataAll_access_new()
    # data_test <<- data
    # data <- kpi.all.data[all.data.rows,] %>% filter(Campus == "MSUS")
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime_total <- data %>%
      filter(WAIT_TIME >= 0) %>%
      filter(NEW_PT2 == "NEW") %>%
      group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
      dplyr::summarise(total_all = n()) %>%
      collect()
    
    waitTime_within_14_days <- data %>%
      filter(WAIT_TIME >= 0, 
             WAIT_TIME < 14.0001, 
             NEW_PT2 == "NEW") %>%
      group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
      dplyr::summarise(total = n()) %>%
      collect()
    
    join_data <- inner_join(waitTime_total, waitTime_within_14_days)
    
    percent_within_14_days <- join_data %>% group_by(APPT_MADE_MONTH_YEAR, PROVIDER) %>%
      summarise(percent = round((total/total_all),2))
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])
    
    ggplot(percent_within_14_days, aes(x=APPT_MADE_MONTH_YEAR, y=percent, group = PROVIDER))+
      # geom_bar(stat = "identity", position = 'dodge')+
      #geom_line(aes(linetype = variable))+
      scale_linetype_manual(values=c("solid"))+
      scale_size_manual(values=c(1))+
      # scale_x_date(breaks = "day", date_labels = "%Y-%m-%d", date_breaks = "1 week",
      #              date_minor_breaks = "1 day", expand = c(0, 0.6))+
      labs(x=NULL, y=NULL,
           #title = "Median Wait Time to New and Established Appointment Over Time",
           title = "Percent of New Patients Scheduled Within 14 Days",
           #subtitle = paste0("Based on scheduled data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))#,
           subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date)#,
           #caption = "*New patients defined by CPT codes (level of service)."
      )+
      theme_new_line()+
      theme_bw()+
      graph_theme("bottom")+
      #geom_text(aes(label = value), position = position_dodge(1), vjust = ifelse(waitTime$value >= 10 & waitTime$value <= 15,-3,-1), color = "black", size = 5, fontface="bold.italic")+
      #scale_y_continuous(limits = c(0,max(waitTime$value))*1.5)+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
      geom_point(aes(color=PROVIDER), size = 3.2) +
      geom_line(aes(color=PROVIDER), size=1) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0, 0), limits = c(0,max(percent_within_14_days$percent)*1.5)
      ) +
      stat_summary(fun = sum, vjust = -1, aes(label=ifelse(..y.. == 0,"",paste0(..y..*100,"%")), group = APPT_MADE_MONTH_YEAR), geom="text", color="black", 
                   size=5, fontface="bold.italic")
    
      
    
    
  })
  
  
  # New Patient Wait Time
  output$newPtApptSourceByDept <- renderPlot({
    data <- dataArrived_access_new()
    # data <- kpi.all.data[arrivedNoShow.data.rows,]
    
    print("2")
    newpatients.ratio <- data %>%
      filter(NEW_PT2 == "NEW") %>%
        group_by(SCHEDULE_GROUPING_MAPPED, NEW_PT2) %>%
      dplyr::summarise(Total = n()) %>% collect()
    
    start_date <- isolate(selected_dateRange()[1])
    end_date <- isolate(selected_dateRange()[2])

    #newpatients.ratio$APPT_SOURCE_NEW[which(newpatients.ratio$APPT_SOURCE_NEW == "Other")] <- "Practice"
    
    newpatients.ratio$ratio <- round(newpatients.ratio$Total / sum(newpatients.ratio$Total), 2)
    
    newRatio <-
      ggplot(newpatients.ratio, aes(x=factor(SCHEDULE_GROUPING_MAPPED
                                             #, levels = c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
                                             ), 
                                    y=ratio, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
      geom_bar(stat="identity", width = 0.8) +
      coord_flip() +
      scale_fill_MountSinai('purple')+
      labs(x=NULL, y=NULL,
           title = "New Patient Source",
           subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date,
                             "\nTotal New Patients = ",prettyNum(sum(newpatients.ratio$Total), big.mark = ','))
           )+
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
    
    
    #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
    
    waitTime <- dataAll_access_new() %>%
      filter(WAIT_TIME >= 0) %>%
      group_by(SCHEDULE_GROUPING_MAPPED, NEW_PT2) %>%
      dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
      filter(NEW_PT2 == "NEW") %>% collect()
    waitTime$target <- 14
    
    #waitTime$SCHEDULE_GROUPING_MAPPED[which(waitTime$SCHEDULE_GROUPING_MAPPED == "Other")] <- "Practice"
    
    newWaitTime <-
      ggplot(waitTime, aes(x=factor(SCHEDULE_GROUPING_MAPPED#, levels = c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
                                    ), 
                           y=medWaitTime, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
      geom_bar(stat="identity", width = 0.8) +
      geom_hline(aes(yintercept=target), linetype="dashed", color = "red", size=1)+
      scale_y_continuous(limits=c(0,max(waitTime$medWaitTime)*1.3))+
      coord_flip() +
      scale_fill_MountSinai('pink')+
      labs(x=NULL, y=NULL, 
           title = "Median Wait Time to New Appointment",
           subtitle = paste0("Based scheduled on data from ", start_date," to ", end_date,
                             "\nWait Time = (Scheduled Appt Date - Appt Made Date)"),
           # caption = "*Based on all of scheduled patients\n**New patients defined by CPT codes (level of service)."
           #caption = "*New patients defined by CPT codes (level of service)."
           )+
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
    
    data.noShow <- dataArrivedNoShow_new() %>% filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))
    # data.noShow <- arrivedNoShow.data
    
    print("3")
    noShows <- data.noShow %>%
      filter(NEW_PT2 == "NEW") %>%
      group_by(SCHEDULE_GROUPING_MAPPED, APPT_STATUS) %>%
      dplyr::summarise(Total = n()) %>% collect() %>%
      spread(APPT_STATUS, Total)
  
    noShows[is.na(noShows)] <- 0
    
    noShows$`No Show Perc` <- round((noShows$`No Show` + noShows$`Canceled`)/(noShows$Arrived + noShows$`No Show` + noShows$`Canceled`),2)
    noShows$SCHEDULE_GROUPING_MAPPED[which(noShows$SCHEDULE_GROUPING_MAPPED == "Other")] <- "Practice"
    
    
    #noShows$SCHEDULE_GROUPING_MAPPED <- ifelse(noShows$SCHEDULE_GROUPING_MAPPED == "Other", "Practice", noShows$SCHEDULE_GROUPING_MAPPED)
    
    
    
    newNoShow <-
      
      ggplot(noShows, aes(x=factor(SCHEDULE_GROUPING_MAPPED#, levels =  c("Practice","Access Center","My MountSinai/MyChart","StayWell","Zocdoc", "FindADoc")
                                   ), 
                          y=`No Show Perc`, group=SCHEDULE_GROUPING_MAPPED, fill=SCHEDULE_GROUPING_MAPPED)) +
      geom_bar(stat="identity", width = 0.8) +
      scale_y_continuous(limits=c(0,max(noShows$`No Show Perc`))*1.3)+
      coord_flip() +
      scale_fill_MountSinai('blue')+
      labs(x=NULL, y=NULL,
           title = "New Patient No Show Rate*",
           subtitle = paste0("Based on scheduled data from ", start_date," to ", end_date),
           caption = "*No Show Rate = (No Show + Same-day Canceled) / (Arrived + No Show + Same-day Canceled)"
           )+
      theme_new_line()+
      theme_bw()+
      theme(
        plot.title = element_text(hjust=0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust=0.5, size = 14, face = "italic"),
        plot.caption = element_text(hjust = 0.95, size = 10, face = "italic"),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = "12", vjust=0.5, angle = 0),
        axis.text.y = element_text(size = "14"))+
      geom_text(aes(label=paste0(`No Show Perc`*100,"%")), color="black", 
                size=5, position = position_dodge(1), hjust=-.5) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, max(newpatients.ratio$ratio)*1.3))
    
    
    grid.arrange(newRatio, newWaitTime, newNoShow, ncol=3)
    
  })
  
  # Booked and Filled Rate
  
  output$slotManageGraph <- renderPlotly({
    
    #data <- slot.data.subset[all.slot.rows,] %>% filter(Campus.Specialty == "Allergy")
    print(paste0("Start of slot graph function ", Sys.time()))
    data <- dataAllSlot()
    
    test_data <<- data
    
    need(nrow(data) > 0, "Data is not available for this selection" )
    #data <- slot.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    print(paste0("End of slot reactive ", Sys.time()))
    
    booked_filled <- data %>% select(AVAILABLE_HOURS, BOOKED_HOURS, ARRIVED_HOURS, APPT_WEEK) #%>% collect() #%>%
   
    
    booked_filled <- booked_filled %>%
      #mutate(Appt.Week = floor_date(as.Date(suppressWarnings(format(APPT_DTTM, "%Y-%m-%d")), "%Y-%m-%d"), unit="week", week_start = 1)) %>%
      group_by(APPT_WEEK) %>%
      summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm = T),1),
                `Booked Hours` = round(sum(BOOKED_HOURS, na.rm = T),1),
                `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm = T)),1) %>% collect() %>%
      mutate(`Booked Rate` = round(`Booked Hours`/`Available Hours`*100,1),
             `Filled Rate` = round(`Filled Hours`/`Available Hours`*100, 1)) %>%
      mutate(APPT_WEEK = as.Date(APPT_WEEK))
    
    booked_filled <- booked_filled[order(as.Date(booked_filled$APPT_WEEK, format="%Y-%m-%d")),]
    booked_filled <- booked_filled %>% select(APPT_WEEK, `Available Hours`, `Booked Hours`,
                                              `Filled Hours`, `Booked Rate`, `Filled Rate`)
    
    print(paste0("End of slot processing in function ", Sys.time()))
    
    
    
    
    #booked_filled$`Booked Rate` <- round(booked_filled$`Booked Hours`/booked_filled$`Available Hours`, 0)*100
    
    slot_fig <- plot_ly(booked_filled, x = ~APPT_WEEK,
                        textfont = list(color = '#000000', size = 16, family = "Calibri"))
    
    current_week <- floor_date(Sys.Date(), "weeks", week_start = 0)
    
    
    vline <- function(x = 0, color = "#a5a7a5") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash="solid")
      )
    }
    
    if(input$byRate == TRUE){
      y_axis <- "Booked Rate" 
    } else{
      y_axis <- "Available Hours"
    }

    if(input$byRate == TRUE){ # by Booked and Filled Rate
      
      slot_fig <- slot_fig %>% add_trace(y = ~`Booked Rate`, name = "Booked Rate (%)", mode = 'lines',
                                        # marker = list(color = "#d80b8c"), 
                                         line = list(color = "#f75dbe"))
      slot_fig <- slot_fig %>% add_trace(y = ~`Filled Rate`, name = "Filled Rate (%)", mode = 'lines',
                                         #marker = list(color = "#00aeef"),
                                         line = list(color = "#7030a0"))
      
     
      slot_fig %>% layout(
        #annotations = annon,
        #shapes=list(type='line', x0= max(dataAll()$Appt.DateYear + 2), x1= max(dataAll()$Appt.DateYear + 2), y0=50000, y1=50000, line=list(dash='dot', width=1)),
        title = "<b>Template Usage (%) by Week <b>", font=list(size=20),
        autosize = T, margin=list( l = 50, r = 50, b = 100, t = 130,  pad = 4),
        xaxis = list(
          #title = "Date",
          title = "Week Starts",
          fixedrange = T,
          #font = list(size = 14),
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          #rangeslider = list(type = "date"),
          fixedrange = F,
          mirror = TRUE,
          ticks = 'outside',
          showline = TRUE),
        yaxis = list(title = "Percent",
                     titlefont = list(size = 14),
                     tickfont = list(size = 12),
                     ticksuffix = "%",
                     mirror = TRUE,
                     range = c(0, max(booked_filled$`Booked Rate`, na.rm = T)*1.2),
                     ticks = 'outside',
                     showline = TRUE),
        hovermode = "x unified",
        hoverlabel = list(font=list(size=10), namelength = -1)) %>%
       layout(shapes = list(vline(current_week)))%>%
       layout(legend = list(orientation = 'h', xanchor = "center", x= 0.5, y = 1.1 ))
       
 
    } else{
      
      
      slot_fig <- slot_fig %>% add_trace(y = ~`Available Hours`, name = "Available Hours", type='scatter', mode = 'lines',
                                          line = list(color = "#A9A9A9", dash="dash"))
      slot_fig <- slot_fig %>% add_bars(y = ~`Booked Hours`, name = "Booked Hours",
                                        marker = list(color = "#f75dbe"))
      slot_fig <- slot_fig %>% add_bars(y = ~`Filled Hours`, name = "Filled Hours",
                                        marker = list(color = "#7030a0"))
      
      tick_text <- c(unique(booked_filled$APPT_WEEK))
      tick_values <- c(unique(booked_filled$APPT_WEEK))
      slot_fig %>% layout(
        #annotations = annon,
        #shapes=list(type='line', x0= max(dataAll()$Appt.DateYear + 2), x1= max(dataAll()$Appt.DateYear + 2), y0=50000, y1=50000, line=list(dash='dot', width=1)),
        title = "<b> Template Slot Usage by Week <b>", font=list(size=20),
        autosize = T, margin=list( l = 50, r = 50, b = 100, t = 130,  pad = 4),
        xaxis = list(
          autotick = F,
          ticktext = tick_text,
          tick_values = tick_values,
          tickmode = "array",
          #font = list(size = 14), #16
          title = "Week Starts", 
          titlefont = list(size = 14),
          tickfont = list(size = 12), #14
          #rangeslider = list(type = "date"),
          #tick0 = "2023-12-31",
          #dtick = 604800000, #set for 7 days
          fixedrange = T,
          mirror = TRUE,
          ticks = 'outside',
          showline = TRUE),
        yaxis = list(
          #font = list(size = 14),
          title = "Hours",
          tickfont = list(size = 12),
          titlefont = list(size = 14),
          mirror = TRUE,
          ticks = 'outside',
          range = c(0, max(booked_filled$`Available Hours`, na.rm = T)*1.2),
          showline = TRUE),
        hovermode = "x unified",
        hoverlabel = list(font=list(size=10), namelength = -1)) %>%
        layout(shapes = list(vline(current_week))) %>%
        layout(legend = list(orientation = 'h', xanchor = "center", x= 0.5, y = 1.1 ))
    }
    
    
    
  })
  
  
  # Slot Usage Table by Practice and Provider 
  slotUsageTb_data <- reactive({
    data <- dataAllSlot()
    #data <- slot.data %>% filter(CAMPUS %in% "MSUS" &CAMPUS_SPECIALTY %in% "Cardiology")
    
    need(nrow(data) > 0, "Data is not available for this selection" )
    
    if(input$byProvider2 == TRUE){
      
      summary.prov <- data %>%
        group_by(CAMPUS, CAMPUS_SPECIALTY, PROVIDER, SLOT_MONTH_YEAR) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm=TRUE),1),
                         `Booked Hours` = round(sum(BOOKED_HOURS, na.rm = TRUE), 1),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm = TRUE), 1)) %>% collect()%>%
        filter(!is.na(CAMPUS))
      
      
      
      summary.prov[is.na(summary.prov)] <- 0
      
      summary.prov <- summary.prov %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100,1), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100,1), "%")) %>%
        gather(variable, value, 5:9) %>%
        spread(SLOT_MONTH_YEAR, value)
      
      summary.prov[summary.prov == "Inf%"] <- "-"
      summary.prov[summary.prov == "NaN%"] <- "-"
      summary.prov[is.na(summary.prov)] <- "-"
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov <- summary.prov[order(match(summary.prov$variable, level.order)),]
      summary.prov <- summary.prov %>% arrange(CAMPUS, CAMPUS_SPECIALTY, PROVIDER)
      
      names(summary.prov)[names(summary.prov) == "CAMPUS_SPECIALTY"] <- "Specialty"
      names(summary.prov)[names(summary.prov) == "variable"] <- "Status"
      print("1")
      
      
      ###Monthly Averages
      
      
      summary.prov.month <- data %>%
        group_by(CAMPUS, CAMPUS_SPECIALTY, PROVIDER, SLOT_MONTH_YEAR) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm=TRUE), 1),
                         `Booked Hours` = round(sum(BOOKED_HOURS, na.rm=TRUE), 1),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm=TRUE), 1)) %>% collect()%>%
        filter(!is.na(CAMPUS))
      
      summary.prov.month[is.na(summary.prov.month)] <- 0
      
      summary.prov.month <- summary.prov.month %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100,1),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100,1)) %>%
        gather(variable, value, 5:9) %>%
        spread(SLOT_MONTH_YEAR, value)
      
      
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.prov.month <- summary.prov.month[order(match(summary.prov.month$variable, level.order)),]
      summary.prov.month <- summary.prov.month %>% arrange(CAMPUS, CAMPUS_SPECIALTY, PROVIDER)

      
      summary.prov.month <- data.frame(summary.prov.month[,1:4], `Monthly Average` = round(rowMeans(summary.prov.month[,-1:-4], na.rm = TRUE),1), check.names = FALSE)
      names(summary.prov.month)[names(summary.prov.month) == "variable"] <- "Status"
      names(summary.prov.month)[names(summary.prov.month) == "CAMPUS_SPECIALTY"] <- "Specialty"
      names(summary.prov.month)[names(summary.prov.month) == "CAMPUS"] <- "Campus"
      names(summary.prov.month)[names(summary.prov.month) == "PROVIDER"] <- "Provider"
      summary.prov.month[summary.prov.month == as.character("Inf")] <- "-"
      summary.prov.month[summary.prov.month == as.character("NaN")] <- "-"
      summary.prov.month[is.na(summary.prov.month)] <- "-"
      
      summary.prov.month$`Monthly Average` <- ifelse(summary.prov.month$Status %in% c("Booked Rate (%)", "Filled Rate (%)"), paste0(summary.prov.month$`Monthly Average`, "%"),summary.prov.month$`Monthly Average`)
      summary.prov.month[summary.prov.month == as.character("-%")] <- "-"
      
      
      names(summary.prov)[names(summary.prov) == "CAMPUS"] <- "Campus"
      names(summary.prov)[names(summary.prov) == "PROVIDER"] <- "Provider"
      
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
        group_by(CAMPUS, CAMPUS_SPECIALTY, SLOT_MONTH_YEAR) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm = TRUE), 1),
                         `Booked Hours` = round(sum(BOOKED_HOURS, na.rm = TRUE), 1),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm = TRUE), 1)) %>% collect()%>%
        filter(!is.na(CAMPUS))
      
      summary.dept[is.na(summary.dept)] <- 0
      
      summary.dept <- summary.dept %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100,1), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100,1), "%")) %>%
        gather(variable, value, 4:8) %>%
        spread(SLOT_MONTH_YEAR, value)
      
      summary.dept[summary.dept == "Inf%"] <- "-"
      summary.dept[summary.dept == "NaN%"] <- "-"
      summary.dept[is.na(summary.dept)] <- "-"
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept <- summary.dept[order(match(summary.dept$variable, level.order)),]
      summary.dept <- summary.dept %>% arrange(CAMPUS, CAMPUS_SPECIALTY)
      
      names(summary.dept)[names(summary.dept) == "CAMPUS_SPECIALTY"] <- "Specialty"
      names(summary.dept)[names(summary.dept) == "CAMPUS"] <- "Campus"
      names(summary.dept)[names(summary.dept) == "variable"] <- "Status"

      
      #Get Monthly Averages
      
      summary.dept.month <- data %>%
        group_by(CAMPUS, CAMPUS_SPECIALTY, SLOT_MONTH_YEAR) %>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm = TRUE), 1),
                         `Booked Hours` = round(sum(BOOKED_HOURS, na.rm = TRUE), 1),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm = TRUE), 1)) %>% collect()%>%
        filter(!is.na(CAMPUS))
      
      summary.dept.month[is.na(summary.dept.month)] <- 0
      
      summary.dept.month <- summary.dept.month %>%
        mutate(`Booked Rate (%)` = round((`Booked Hours`/`Available Hours`)*100,1),
               `Filled Rate (%)` = round((`Filled Hours`/`Available Hours`)*100,1)) %>%
        gather(variable, value, 4:8) %>%
        spread(SLOT_MONTH_YEAR, value)
      
      summary.dept.month[summary.dept.month == "Inf%"] <- "-"
      summary.dept.month[summary.dept.month == "NaN%"] <- "-"
      
      
      
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      summary.dept.month <- summary.dept.month[order(match(summary.dept.month$variable, level.order)),]
      summary.dept.month <- summary.dept.month %>% arrange(CAMPUS, CAMPUS_SPECIALTY)
      
      names(summary.dept.month)[names(summary.dept.month) == "CAMPUS_SPECIALTY"] <- "Specialty"
      names(summary.dept.month)[names(summary.dept.month) == "CAMPUS"] <- "Campus"
      names(summary.dept.month)[names(summary.dept.month) == "variable"] <- "Status"
      
      #summary.dept.month[summary.dept.month == as.character("Inf")] <- "-"
      #summary.dept.month[summary.dept.month == as.character("NaN")] <- "-"
      
      
      summary.dept.month <- data.frame(summary.dept.month[,1:3], `Monthly Average` = round(rowMeans(summary.dept.month[,-1:-3], na.rm = TRUE), 1), check.names = FALSE)
      summary.dept.month <- summary.dept.month %>% mutate(`Monthly Average` = ifelse(is.na(`Monthly Average`), "-", `Monthly Average`))
      
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
  # output$apptTypeControl2 <- renderUI({
  #   
  #   appt_choices <- dataAll() %>% filter(NEW_PT3 == "ESTABLISHED") %>% select(APPT_TYPE) %>%
  #     mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
  #   appt_choices <- sort(appt_choices$APPT_TYPE, na.last = T)
  #   
  #   #appt_choices <- sort(unique((dataAll() %>% filter(NEW_PT3 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE))
  #   box(
  #     title = NULL,
  #     width = 12, 
  #     solidHeader = FALSE,
  #     pickerInput("selectedApptType2", label = h4("Select Visit Type for Comparison:"), 
  #                 #choices = sort(unique((dataAll() %>% filter(NEW_PT2 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE)),
  #                 choices = appt_choices,
  #                 multiple=TRUE,
  #                 options = pickerOptions(
  #                   liveSearch = TRUE,
  #                   actionsBox = TRUE,
  #                   selectedTextFormat = "count > 1", 
  #                   countSelectedText = "{0}/{1} Types", 
  #                   dropupAuto = FALSE),
  #                 selected = appt_choices
  #                 #selected = sort(unique((dataAll() %>% filter(NEW_PT2 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE))
  #                 ))
  # })
  # 
  
  output$apptTypeControl3 <- renderUI({
  
    
    #data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
    appt_choices <- dataAll() %>% filter(NEW_PT3 == "ESTABLISHED") %>% select(APPT_TYPE) %>%
      mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()
    appt_choices <- sort(appt_choices$APPT_TYPE, na.last = T)
    
    #appt_choices <- sort(unique((data %>% filter(NEW_PT3 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE))
      
    box(
      title = NULL,
      width = 12, 
      solidHeader = FALSE,
      pickerInput("selectedApptType3", label = h4("Select Visit Type for Comparison:"), 
                  choices = appt_choices,
                  #choices = sort(unique((dataAll() %>% filter(NEW_PT2 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE)),
                  multiple=TRUE,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    actionsBox = TRUE,
                    selectedTextFormat = "count > 1", 
                    countSelectedText = "{0}/{1} Types", 
                    dropupAuto = FALSE),
                  selected = appt_choices
                  #selected = sort(unique((dataAll() %>% filter(NEW_PT2 == "ESTABLISHED") %>% select(APPT_TYPE) %>% collect())$APPT_TYPE))
                  ))
    
  })
  
  
  # Arrived Data with Additional Filters (Appointment Type)
  # dataNewComparison <- reactive({
  #   groupByFilters_3(dataArrived(),
  #                    input$selectedApptType2)
  # })
  
  # Arrived Data with Additional Filters (Appointment Type)
  # dataNewComparison2 <- reactive({
  #   groupByFilters_3(dataArrived(),
  #                    input$selectedApptType3)
  # })
  
  
  # (1) Cycle Times --------------------------------------------------------------------------
  
  output$cycleTimeCompNew <- renderValueBox({
    
    data_cycle <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW")
    
    data <- data_cycle %>% select(CYCLETIME) %>%
      summarise(CYCLETIME = ceiling(mean(CYCLETIME, na.rm = T))) %>%
      collect()
    
    data_median <- data_cycle %>% select(CYCLETIME) %>%
      summarise(CYCLETIME = ceiling(median(CYCLETIME, na.rm = T))) %>%
      collect()
    
    perc <- data_cycle %>% summarize(n()) %>% collect() /
            dataArrived() %>% filter(NEW_PT3 == "NEW")%>% summarize(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataArrived() %>% filter(cycleTime > 0, New.PT3 == TRUE))$cycleTime))," min"),
      value =  paste0("Average: ",data$CYCLETIME," min", " | Median: ", data_median$CYCLETIME, " min"),
      title = toupper(paste0("New Patients Check-in to Visit-end Time*")),
      # title = toupper("Average New Patients Check-in to Visit-end Time*"),
      subtitle = paste0("*Based on ",round(perc,2)*100,
                           "% of total arrived new patients based on visit timestamps" 
                    ),
   
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$cycleTimeCompOther <- renderValueBox({
    
    data_cycle <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED")
    
    
    data <- data_cycle %>% select(CYCLETIME) %>%
      summarise(CYCLETIME = mean(CYCLETIME, na.rm = T)) %>% collect()
    
    data_meadian <- data_cycle %>% select(CYCLETIME) %>%
      summarise(CYCLETIME = median(CYCLETIME, na.rm = T)) %>% collect()
    
    perc <- data_cycle %>% summarise(n()) %>% collect() / 
            dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED")%>% summarise(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE))$cycleTime))," min"),
      value =  paste0("Average: ",ceiling(data$CYCLETIME)," min", " | Median: ", data_meadian$CYCLETIME, " min"),
      title = toupper(
                     #ifelse(length(unique(dataArrived()$APPT_TYPE)) == 1,
                             #paste0("Average ", input$selectedApptType2," Appointments Check-in to Visit-end Time"),
                             "Established Patients Check-in to Visit-end Time*"),
      # subtitle = paste0("*Based on ",round(nrow(dataNewComparison() %>% filter(cycleTime > 0, New.PT3 == FALSE))/nrow(dataArrived()),2)*100,"% of total arrived established patients based on visit timestamps"),
      subtitle = paste0("*Based on ", round(perc,2)*100,"% of total arrived established patients based on visit timestamps"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$cycleTimeTrend <- renderPlot({
    # data_new <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>%
    #   select(CYCLETIME, NEW_PT3, APPT_TYPE) %>% collect()
    # # data_new <- arrived.data %>% filter(cycleTime > 0 & New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    # 
    # data_other <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>%
    #   select(CYCLETIME, NEW_PT3, APPT_TYPE) %>% collect()
    # # data_other <- arrived.data %>% filter(cycleTime > 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    # 
    # if(nrow(data_other) == 0){
    #   data <- data_new
    # } else{
    #   
    #   data <- rbind(data_new, data_other)
    # }
    
    #data <- arrived.data.rows %>% filter(CAMPUS %in% "MSUS"& CAMPUS_SPECIALTY %in% "Allergy")
    
    data <-  dataArrived()
    data <-  data %>% filter(CYCLETIME > 0, NEW_PT3 %in% c("NEW", "ESTABLISHED")) %>%
      select(CYCLETIME, NEW_PT3, APPT_TYPE, BIN_CYCLE) %>% collect() %>%
      mutate(NEW_PT3 = ifelse(NEW_PT3== "NEW", "NEW", APPT_TYPE)) %>%
      filter(!is.na(NEW_PT3))
 
    data <- data %>% select(CYCLETIME, NEW_PT3, BIN_CYCLE) %>%
      group_by(BIN_CYCLE, NEW_PT3) %>% summarise(total_bin = n()) %>% 
      ungroup() %>%
      mutate(total = sum (total_bin, na.rm = TRUE)) %>%
      group_by(BIN_CYCLE, NEW_PT3) %>%
      #group_by(BIN_CYCLE) %>%
      mutate(percent = total_bin / total) %>%
      mutate(BIN_CYCLE = as.numeric(BIN_CYCLE))

    
    
    
    
    main_rows <- seq(0, 480, by= 30)

    rows_to_be_included <- which(!main_rows %in% data$BIN_CYCLE)    
    
    if (length(rows_to_be_included)>0){
      for (i in rows_to_be_included){
        data[nrow(data) + 1 , 1] <- main_rows[i]
      }
      
    }
  
    data[, 3:length(data)][is.na(data[, 3:length(data)])] <- 0
    #data <- data %>% mutate(NEW_PT3 =ifelse(is.na(NEW_PT3), "NEW", NEW_PT3))
    
    data <- unique(data)
    data <- left_join(data, bin_mapping)
    
    data <- data[order(data$BIN_CYCLE),]
    
    data <- data %>%  group_by(BIN_CYCLE, NEW_PT3) %>%
      mutate(BIN_CYCLE = factor(BIN_CYCLE, levels = sort(BIN_CYCLE)))
    
    #data$BIN_CYCLE <- factor(data$BIN_CYCLE,levels = sort(data$BIN_CYCLE))
    x_label <- data %>% ungroup() %>% select(BIN_CYCLE, X_LABEL) %>% distinct()
    x_label <- x_label[order(x_label$BIN_CYCLE),]
    

    ggplot(aes(x = BIN_CYCLE , y = percent, fill=factor(NEW_PT3), color=factor(NEW_PT3)), data = data) +
      geom_bar(stat = 'identity') +
      scale_color_MountSinai()+
      scale_fill_MountSinai()+
      #geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Check-in to Visit-end Time* Comparison by Visit Type"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Visit-end Time is the minimum of Visit-end Time and Check-out"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      # scale_x_discrete()+
      scale_x_discrete(labels = x_label$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    

    
    # ggplot(data, aes(x=CYCLETIME, fill=NEW_PT3, color=NEW_PT3)) +
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  # position = "identity",
    #                  alpha = 0.8)+
    #   scale_color_MountSinai()+
    #   scale_fill_MountSinai()+
    #   labs(title = "Check-in to Visit-end Time* Comparison by Visit Type",
    #        y = "% of Patients",
    #        x = "Minutes",
    #        #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        caption = paste0("*Visit-end Time is the minimum of Visit-end Time and Check-out"))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("top")+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    
  })
  
  output$newCycleTimeBoxPlot <- renderPlot({
    
    # data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>% select(CYCLETIME, NEW_PT3) %>% collect()
    # # data <- arrived.data %>% filter(cycleTime > 0) %>% filter(New.PT3 == TRUE)
    # 
    # 
    # ggplot(data, aes(x=CYCLETIME)) + 
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  bins = 22,
    #                  color="#d80b8c", fill="#fcc9e9") +
    #   labs(title = "Distribution of NEW Appointment\nCheck-in to Visit-end Time*", 
    #        y = "% of Patients",
    #        x = "Minutes",
    #        caption = paste0("*Visit-end Time is the minimum of Visit-end Time and Check-out"),
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    
    
    
    data_cycle <- dataArrived() %>% 
      filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>% select(CYCLETIME, NEW_PT3, BIN_CYCLE) %>%
      group_by(BIN_CYCLE) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_CYCLE) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_CYCLE <- as.numeric(data_cycle$BIN_CYCLE)
    

    main_rows <- seq(0, max(data_cycle$BIN_CYCLE), by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_CYCLE)

       
    if (length(rows_to_be_included)>0){
       for (i in rows_to_be_included){
          data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
       }
    data_cycle[is.na(data_cycle)] <- 0
    }

    data_cycle <- left_join(data_cycle, bin_mapping)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_CYCLE),]
  
    data_cycle$BIN_CYCLE <- factor(data_cycle$BIN_CYCLE,levels = sort(data_cycle$BIN_CYCLE))

    graph <- ggplot(aes(x = BIN_CYCLE , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of NEW Appointments\nCheck-in to Visit-end Time**"),
           y = "% of Patients",
           x = "Minutes",
           #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("*Visit-end Time is the minimum of Visit-end Time and Check-out"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_discrete(labels = data_cycle$X_LABEL)+
      #scale_x_continuous(breaks = seq(0, 500, 30), limits = c(0, 500))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
    graph
    
    
  })
  
 
  
  output$establishedCycleTimeBoxPlot <- renderPlot({
  
    # data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>%
    #   select(CYCLETIME, NEW_PT3, APPT_TYPE) %>% collect()
    # # data <- arrived.data %>% filter(cycleTime > 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type %in% c("NEW PATIENT", "FOLLOW UP"))
    # 
    # #data <- data_other
    # 
    # if(length(unique(data$APPT_TYPE)) == 1){
    #   appt.type <- unique(data$APPT_TYPE)
    # } else{
    #   appt.type <- "Established*"
    # }
    # 
    # graph <- ggplot(data, aes(x=CYCLETIME)) +
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  bins = 22,
    #                  color="#d80b8c", fill="#fcc9e9") +
    #   labs(title = paste0("Distribution of ",appt.type," Appointments\nCheck-in to Visit-end Time**"),
    #        y = "% of Patients",
    #        x = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        caption = paste0("*Includes ", length(unique(data$APPT_TYPE)), " established visit types \n **Visit-end Time is the minimum of Visit-end Time and Check-out "))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")+
    #   theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    # 
    
     
    # appt.type.data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>%
    #   select(APPT_TYPE) %>% mutate(APPT_TYPE= unique(APPT_TYPE)) %>% collect()


    # appt.type.data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>%
    #   group_by(APPT_TYPE) %>% summarise(check = 1)  %>% collect()

    data_cycle <- dataArrived() %>% 
      filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>% select(CYCLETIME, NEW_PT3, BIN_CYCLE) %>%
      group_by(BIN_CYCLE) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_CYCLE) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_CYCLE <- as.numeric(data_cycle$BIN_CYCLE)
    
    main_rows <- seq(0, max(data_cycle$BIN_CYCLE), by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_CYCLE)

    
    if (length(rows_to_be_included)>0){
      for (i in rows_to_be_included){
          data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
    }
    
    data_cycle[is.na(data_cycle)] <- 0
    }

    
    data_cycle <- left_join(data_cycle, bin_mapping)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_CYCLE),]
    
    data_cycle$BIN_CYCLE <- factor(data_cycle$BIN_CYCLE,levels = sort(data_cycle$BIN_CYCLE))



    # if(length(unique(appt.type.data$APPT_TYPE)) == 1){
    #   appt.type <- unique(appt.type.data$APPT_TYPE)
    # } else{
    #   appt.type <- "Established*"
    # }

    graph <- ggplot(aes(x = BIN_CYCLE , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of Established Appointments\nCheck-in to Visit-end Time**"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           #caption = paste0("*Includes ", length(unique(appt.type.data$APPT_TYPE)), " established visit types \n **Visit-end Time is the minimum of Visit-end Time and Check-out "))+
           caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out "))+
           theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_discrete(labels = data_cycle$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
       #theme(axis.text.x = element_text(hjust = 3.5))

    
    graph
    
  })
  
 
  
  output$cycleTimeByHour <- renderPlot({
    
    #data <- arrived.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")  %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>% select(APPT_DAY, APPT_TM_HR, CYCLETIME) 
    
    data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>% select(APPT_DAY, APPT_TM_HR, CYCLETIME) 
    #data <- arrived.data.rows %>% filter(CYCLETIME > 0, NEW_PT3 == "NEW") %>% select(APPT_DAY, APPT_TM_HR, CYCLETIME)
    data_other <- dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED", CYCLETIME > 0) %>% select(APPT_DAY, APPT_TM_HR, CYCLETIME) 
    # data_other <- arrived.data.rows  %>% filter(NEW_PT3 == "ESTABLISHED", CYCLETIME > 0) %>% select(APPT_DAY, APPT_TM_HR, CYCLETIME)
   
    #names <- paste(unique(data_other$Appt.Type),sep="", collapse=", ")
    
    appt.type.choices <- dataArrived()  %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>% select(APPT_TYPE) %>% mutate(APPT_TYPE = unique(APPT_TYPE)) %>% collect()
    
    if(input$median2 == TRUE){
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(CYCLETIME)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter) %>% collect()
      
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(CYCLETIME)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter) %>% collect()
      
      
      input <- "Median"
    } else{
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(CYCLETIME, na.rm= TRUE)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)%>% collect()
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(CYCLETIME, na.rm= TRUE)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter) %>% collect()
      
      input <- "Average"
    }
    
    if(length(unique(appt.type.choices$APPT_TYPE)) == 1){
      appt.type <- unique(appt.type.choices$APPT_TYPE)
    } else{
      appt.type <- "Established*"
    }
    
    level_order <- rev(toupper(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
    
    new <- ggplot(data, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#d80b8c")+
      labs(title = paste0(input," NEW Appointments Check-in to Visit-end Time** by Hour\n"),
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
    
    other <- ggplot(data_other, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#00aeef")+
      labs(title = paste0(input," ",appt.type," Appointments Check-in to Visit-end Time** by Hour\n"), 
           y = NULL,
           caption = paste0("*Includes ", length(unique(appt.type.choices$APPT_TYPE)), " established visit types \n **Visit-end Time is the minimum of Visit-end Time and Check-out"),
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
      geom_text(aes(label= ifelse(is.na(avg),"", ceiling(avg))), color="black", size=5, fontface="bold")
    
    grid.arrange(new, other, ncol = 1)
    
  })
  
  
  output$newCycleTimeByProv <- renderPlot({
    data_test <- dataArrived()
    
    cycle.df <- dataArrived() %>% filter(CYCLETIME > 0 , NEW_PT3 == "NEW") %>%
      select(PROVIDER, CYCLETIME) 
      
      
      
    # data <- arrived.data.rows %>% filter(CYCLETIME > 0 , NEW_PT3 == "NEW") %>% 
    #   filter(CAMPUS %in% "MSUS", CAMPUS_SPECIALTY %in% "Cardiology")%>%
    #   select(PROVIDER, NEW_PT3, CYCLETIME) %>% collect() %>%
    #   group_by(PROVIDER, NEW_PT3)
      
    
    # cycle.df <- data %>%
    #   select(PROVIDER, NEW_PT3, CYCLETIME) %>% collect() %>%
    #   group_by(PROVIDER, NEW_PT3)
    # 
    # avg.cycleTime <- data.frame(New.PT3 = c("Avg"),
    #                             target =  round(mean(cycle.df$CYCLETIME)))
    
    # ggplot(cycle.df, aes(x = PROVIDER, y = CYCLETIME)) +
    #   geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
    #   stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
    #   scale_y_continuous(limits = c(0,quantile(cycle.df$CYCLETIME,0.75)*1.5))+
    #   # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
    #   # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
    #   labs(title = "Distribution of NEW Appointment Check-in to Visit-end Time** by Provider", 
    #        y = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        caption = "**Visit-end Time is the minimum of Visit-end Time and Check-out")+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")
    
    
    
    data_base <- cycle.df %>% group_by(PROVIDER) %>% summarise(min_value = min(CYCLETIME),                                
                                                               quartile_1st = quantile(CYCLETIME, 0.25),
                                                               median = median(CYCLETIME), 
                                                               mean = mean(CYCLETIME, na.rm= TRUE),
                                                               quartile_3rd = quantile(CYCLETIME, 0.75),
                                                               max_value = max(CYCLETIME)) %>% 
                                                               collect()
    
    
    
    
    ggplot(data_base,                              
           aes(x = PROVIDER,
               ymin = min_value,
               lower = quartile_1st,
               middle = median,
               upper = quartile_3rd,
               ymax = max_value)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
      #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
      # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = "Distribution of NEW Appointment Check-in to Visit-end Time** by Provider", 
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "**Visit-end Time is the minimum of Visit-end Time and Check-out")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
    
  })
  
  output$establishedCycleTimeByProv <- renderPlot({
  
    data <- dataArrived() %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>%
      select(PROVIDER, NEW_PT3, CYCLETIME, APPT_TYPE) 
    #data <- arrived.data.rows %>% filter(cycleTime > 0) %>% filter(CYCLETIME > 0, NEW_PT3 == "ESTABLISHED") %>% select(PROVIDER, NEW_PT3, CYCLETIME, APPT_TYPE)
    
    #data <- data_other
    
    if(length(unique(data$APPT_TYPE)) == 1){   # data$Appt.Type
      appt.type <- unique(data$APPT_TYPE)
    } else{
      appt.type <- "Established"
    }
    
    # cycle.df <- data %>%
    #   select(PROVIDER, CYCLETIME) %>%
      
      data_base <- data %>% group_by(PROVIDER) %>% summarise(min_value = min(CYCLETIME),                                
                                                        quartile_1st = quantile(CYCLETIME, 0.25),
                                                        median = median(CYCLETIME), 
                                                        mean = mean(CYCLETIME, na.rm= TRUE),
                                                        quartile_3rd = quantile(CYCLETIME, 0.75),
                                                        max_value = max(CYCLETIME)) %>% 
                                                        collect()




ggplot(data_base,                              
       aes(x = PROVIDER,
           ymin = min_value,
           lower = quartile_1st,
           middle = median,
           upper = quartile_3rd,
           ymax = max_value)) +
  geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
  #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
  #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
  # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
  # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
  labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Visit-end Time** by Provider"),
       y = "Minutes",
       subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
       # caption = paste0("*Includes ", length(unique(data$Appt.Type)), "established appointments")
       caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out")
  )+
  theme_new_line()+
  theme_bw()+
  graph_theme("none")
      
      
    
    # avg.cycleTime <- data.frame(New.PT3 = c("Avg"),
    #                             target =  ceiling(mean(cycle.df$CYCLETIME)))
    
    # ggplot(cycle.df, aes(x = PROVIDER, y = CYCLETIME)) +
    #   geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
    #   stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
    #   scale_y_continuous(limits = c(0,quantile(cycle.df$CYCLETIME, 0.75)*1.5))+
    #   # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
    #   # annotate("text",x=length(unique(cycle.df$Provider))/2,y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
    #   labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Visit-end Time** by Provider"),
    #        y = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        # caption = paste0("*Includes ", length(unique(data$Appt.Type)), "established appointments")
    #        caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out")
    #        )+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")
      #theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
  })
  
  # (2) Room-in Times ----------------------------------------------------------------------
  
  output$roomInTimeCompNew <- renderValueBox({
    print("1")
    
    data_room <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "NEW")
      
    data <-  data_room %>% select(CHECKINTOROOMIN) %>%
      summarise(CHECKINTOROOMIN = mean(CHECKINTOROOMIN, na.rm = T))  %>% collect()
    
    data_meadian <-  data_room %>% select(CHECKINTOROOMIN) %>%
      summarise(CHECKINTOROOMIN = median(CHECKINTOROOMIN, na.rm = T))  %>% collect()
    
    perc <-  data_room %>% summarise(n()) %>% collect() /
            dataArrived() %>% filter(NEW_PT3 == "NEW") %>% summarise(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE))$checkinToRoomin))," min"),
      value =  paste0("Average: ",ceiling(data)," min", " | Median: ", ceiling(data_meadian), " min"),
      title = toupper("New Appointments Check-in to Room-in Time"),
      # subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0))/nrow(dataArrived()),2)*100,"% of total arrived new patients based on visit timestamps"),
      subtitle = paste0("*Based on ",round(perc,2)*100,"% of total arrived new patients based on visit timestamps"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeCompOther <- renderValueBox({
    print("2")
    
   data_room <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "ESTABLISHED")
    
    data <- data_room %>% select(CHECKINTOROOMIN) %>%
      summarise(CHECKINTOROOMIN = mean(CHECKINTOROOMIN, na.rm = T)) %>% collect()
    
    data_median <- data_room %>% select(CHECKINTOROOMIN) %>%
      summarise(CHECKINTOROOMIN = median(CHECKINTOROOMIN, na.rm = T)) %>% collect()
    
    perc <- data_room %>%
            summarise(n()) %>% collect() / dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED") %>%
            summarise(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))$checkinToRoomin))," min"),
      value =  paste0("Average: ",ceiling(data)," min", " | Median: ", ceiling(data_median), " min"),
      title = toupper( 
              #ifelse(length(unique(dataArrived()$APPT_TYPE)) == 1,
                             #paste0("Avg. ", input$selectedApptType2," Appointments Check-in to Room-in Time"),
                            # paste0("Avg. ", unique(data_room$APPT_TYPE)," Appointments Check-in to Room-in Time"),
                             "Established Appointments Check-in to Room-in Time"),
 
      # subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))/nrow(dataArrived()),2)*100,"% of total arrived established patients based on visit timestamps"),
      subtitle = paste0("*Based on ",round(perc,2)*100,"% of total arrived established patients based on visit timestamps"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeTrend <- renderPlot({
    print("5")
    # data_new <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "NEW") %>%
    #   select(CHECKINTOROOMIN, NEW_PT3, APPT_TYPE) %>% collect()
    # # data_new <- arrived.data %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE)
    # 
    # data_other <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0,  NEW_PT3 == "ESTABLISHED") %>%
    #   select(CHECKINTOROOMIN, NEW_PT3, APPT_TYPE) %>% collect()
    # # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    # 
    # if(nrow(data_other) == 0){
    #   data <- data_new
    # } else{
    #   data <- rbind(data_new, data_other)
    # }
    # 
    # data <- data %>%
    #   mutate(NEW_PT3 = ifelse(NEW_PT3 == "NEW", "NEW", APPT_TYPE)) %>%
    #   filter(!is.na(NEW_PT3))
    # 
    # 
    # ggplot(data, aes(x=CHECKINTOROOMIN, fill=NEW_PT3, color=NEW_PT3)) +
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  # position = "identity",
    #                  alpha = 0.8)+
    #   scale_color_MountSinai()+
    #   scale_fill_MountSinai()+
    #   labs(title = "Check-in to Room-in Time Comparison by Appointment Type",
    #        y = "% of Patients",
    #        x = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("top")+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    
    
    data <- dataArrived() %>% filter(CHECKINTOROOMIN > 0, NEW_PT3 %in% c("NEW", "ESTABLISHED")) %>%
      select(CHECKINTOROOMIN, NEW_PT3, APPT_TYPE, BIN_ROOMIN) %>% collect() %>% 
      mutate(NEW_PT3 = ifelse(NEW_PT3== "NEW", "NEW", APPT_TYPE)) %>%
      filter(!is.na(NEW_PT3))
    
    

    data <- data %>% select(CHECKINTOROOMIN, NEW_PT3, BIN_ROOMIN) %>%
      group_by(BIN_ROOMIN, NEW_PT3) %>% summarise(total_bin = n()) %>% 
      ungroup() %>%
      mutate(total = sum (total_bin, na.rm = TRUE))  %>% group_by(BIN_ROOMIN, NEW_PT3) %>%
      mutate(percent = total_bin / total) %>%
      mutate(BIN_ROOMIN = as.numeric(BIN_ROOMIN))

    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data$BIN_ROOMIN)


    if (length(rows_to_be_included)>0){
      for (i in rows_to_be_included){
        data[nrow(data) + 1 , 1] <- main_rows[i]
      }
      
    }
    
    data[, 3:length(data)][is.na(data[, 3:length(data)])] <- 0
    data <- data %>% mutate(NEW_PT3 =ifelse(is.na(NEW_PT3), "NEW", NEW_PT3))
    
    data <- unique(data)
    
    bin_mapping_roomin <- bin_mapping %>% rename(BIN_ROOMIN = BIN_CYCLE)
    data <- left_join(data, bin_mapping_roomin)
    
    data <- data[order(data$BIN_ROOMIN),]

    data <- data %>%  group_by(BIN_ROOMIN, NEW_PT3) %>%
      mutate(BIN_ROOMIN = factor(BIN_ROOMIN, levels = sort(BIN_ROOMIN)))
    
    x_label <- data %>% ungroup() %>% select(BIN_ROOMIN, X_LABEL) %>% distinct()
    x_label <- x_label[order(x_label$BIN_ROOMIN),]
    
    #data$bin <- factor(data$bin,levels = sort(data$bin))
    
    

    ggplot(aes(x = BIN_ROOMIN , y = percent, fill=factor(NEW_PT3), color=factor(NEW_PT3)), data = data) +
      geom_bar(stat = 'identity') +
      scale_color_MountSinai()+
      scale_fill_MountSinai()+
      #geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Check-in to Room-in Time Comparison by Appointment Type"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      scale_x_discrete(labels = x_label$X_LABEL)+
      #scale_x_continuous(breaks = seq(0, 500, 30), limits = c(0, 500))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
  })
  
  output$newRoomInTimeBoxPlot <- renderPlot({
    print("3")
    # data <- data_test %>% filter(CHECKINTOROOMIN >= 0) %>%
    #   filter(NEW_PT3 == "NEW") %>% select(CHECKINTOROOMIN) %>% collect()
    


    data_cycle <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "NEW") %>%
      select(CHECKINTOROOMIN, BIN_ROOMIN) %>%
      group_by(BIN_ROOMIN) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_ROOMIN) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_ROOMIN <- as.numeric(data_cycle$BIN_ROOMIN)
    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_ROOMIN)
    
    if (length(rows_to_be_included > 0)){
    
    for (i in rows_to_be_included){
      data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
      
    }
    
    data_cycle[is.na(data_cycle)] <- 0
    }
    
    bin_mapping_roomin <- bin_mapping %>% rename(BIN_ROOMIN = BIN_CYCLE)
    data_cycle <- left_join(data_cycle, bin_mapping_roomin)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_ROOMIN),]
    
    data_cycle$BIN_ROOMIN <- factor(data_cycle$BIN_ROOMIN,levels = sort(data_cycle$BIN_ROOMIN))
    
   ggplot(aes(x = BIN_ROOMIN , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of NEW Appointment\nCheck-in to Room-in Time**"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "-")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
     scale_x_discrete(labels = data_cycle$X_LABEL)+
     scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
    
    
    # ggplot(data, aes(x=CHECKINTOROOMIN)) +
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  bins = 22,
    #                  color="#d80b8c", fill="#fcc9e9") +
    #   labs(title = "Distribution of NEW Appointment\nCheck-in to Room-in Time",
    #        y = "% of Patients",
    #        x = "Minutes",
    #        #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        caption = "-")+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    
  })
  
  output$establishedRoomInTimeBoxPlot <- renderPlot({
    print("4")
    # data <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "ESTABLISHED") %>%
    #   select(APPT_TYPE, CHECKINTOROOMIN) %>% collect()
    # # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    # 
    # #data <- data_other
    # 
    # if(length(unique(data$APPT_TYPE)) == 1){
    #   appt.type <- unique(data$APPT_TYPE)
    # } else{
    #   appt.type <- "Established*"
    # }
    # 
    # ggplot(data, aes(x=CHECKINTOROOMIN)) + 
    #   geom_histogram(aes(y = (..count..)/sum(..count..)),
    #                  bins = 22,
    #                  color="#d80b8c", fill="#fcc9e9") +
    #   labs(title = paste0("Distribution of ",appt.type,"Appointments\nCheck-in to Room-in Time"), 
    #        y = "% of Patients",
    #        x = "Minutes",
    #        #subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
    #        caption = paste0("*Includes ", length(unique(data$APPT_TYPE)), " established appointments"))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")+
    #   theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))+
    #   scale_x_continuous(breaks = seq(0, 500, 30), lim = c(0, 500))+
    #   scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    # 
    
    data_cycle <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "ESTABLISHED") %>%
      select(CHECKINTOROOMIN, BIN_ROOMIN) %>%
      group_by(BIN_ROOMIN) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_ROOMIN) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_ROOMIN <- as.numeric(data_cycle$BIN_ROOMIN)
    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_ROOMIN)

    
    if (length(rows_to_be_included > 0)){
      
      for (i in rows_to_be_included){
        data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
        
      }
      
      data_cycle[is.na(data_cycle)] <- 0
    }
    
    bin_mapping_roomin <- bin_mapping %>% rename(BIN_ROOMIN = BIN_CYCLE)
    data_cycle <- left_join(data_cycle, bin_mapping_roomin)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_ROOMIN),]

    data_cycle$BIN_ROOMIN <- factor(data_cycle$BIN_ROOMIN,levels = sort(data_cycle$BIN_ROOMIN))
    

    ggplot(aes(x = BIN_ROOMIN , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of ESTABLISHED Appointment\nCheck-in to Room-in Time**"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = "*Includes established appointments")+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_discrete(labels = data_cycle$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
   
    
  })
  
  output$roomInTimeByHour <- renderPlot({
    print("6")
    data <- dataArrived() %>% filter(CHECKINTOROOMIN > 0) %>% filter(NEW_PT3 == "NEW") %>%
      select(APPT_DAY, APPT_TM_HR, CHECKINTOROOMIN) %>% collect()
    # data <- arrived.data %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    
    data_other <- dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED", CHECKINTOROOMIN > 0) %>%
      select(APPT_DAY, APPT_TM_HR, CHECKINTOROOMIN, APPT_TYPE) %>% collect()
   
    # data_other <- arrived.data %>% filter(New.PT3 == FALSE, checkinToRoomin > 0)
    
    # names <- data_other %>% filter(APPT_TM_HR %in% timeOptionsHr_filter) %>% select(APPT_TYPE)
    # names <- sort(unique(names$APPT_TYPE), na.last = T)
    
    
    appt.type.choices <- dataArrived() %>% filter( CHECKINTOROOMIN > 0, NEW_PT3 == "ESTABLISHED") %>% 
      select(APPT_TYPE) %>% mutate(APPT_TYPE = unique(APPT_TYPE)) %>% collect()
    
    
    if(input$median3 == TRUE){
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(CHECKINTOROOMIN)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(CHECKINTOROOMIN)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      input <- "Median"
    } else{
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(CHECKINTOROOMIN)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(CHECKINTOROOMIN)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      input <- "Average"
    }
    
    if(length(appt.type.choices$APPT_TYPE) == 1){
      appt.type <- unique(appt.type.choices$APPT_TYPE)
    } else{
      appt.type <- "Established*"
    }
    
    level_order <- rev(toupper(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
    
    new <- ggplot(data, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#d80b8c")+
      labs(title = paste0(input," New Patients Check-in to Room-in Time by Hour\n"),
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
      geom_text(aes(label= ifelse(is.na(avg),"", ceiling(avg))), color="black", size=5, fontface="bold")
    
    
    other <- ggplot(data_other, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg))+ 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#00aeef")+
      labs(title = paste0(input," ",appt.type," Patients Check-in to Room-in Time by Hour\n"), 
           y = NULL,
           caption = paste0("*Includes ", length(unique(appt.type.choices$APPT_TYPE)), " established visit types"),
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
      geom_text(aes(label= ifelse(is.na(avg),"", ceiling(avg))), color="black", size=5, fontface="bold")
    
    grid.arrange(new, other, ncol = 1)
    
  })
  
  
  output$newRoomInTimeByProv <- renderPlot({
    print("7")
    data <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "NEW") %>%
      select(PROVIDER, CHECKINTOROOMIN) 
      
    # data <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    
    # roomin.df <- data %>%
    #   select(PROVIDER, NEW_PT3, CHECKINTOROOMIN) %>% collect() %>%
    #   group_by(PROVIDER, NEW_PT3)
    # 
    # avg.roomInTime <- data.frame(New.PT3 = c("Avg"),
    #                              target =  ceiling(mean(roomin.df$CHECKINTOROOMIN)))
    
    # ggplot(roomin.df, aes(x = PROVIDER, y = CHECKINTOROOMIN)) +
    #   geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
    #   stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
    #   scale_y_continuous(limits = c(0,quantile(roomin.df$CHECKINTOROOMIN,0.75)*1.5))+
    #   # geom_hline(yintercept= round(mean(roomin.df$checkinToRoomin)), linetype="dashed", color = "red")+
    #   # annotate("text",x=length(unique(roomin.df$Provider))/2,y=round(mean(roomin.df$checkinToRoomin))+3,size=5,color="red",label=c('Average'))+
    #   labs(title = "Distribution of NEW Appointment Check-in to Room-in Time by Provider", 
    #        y = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")
    
    data_base <- data %>% group_by(PROVIDER) %>% summarise(min_value = min(CHECKINTOROOMIN),                                
                                                               quartile_1st = quantile(CHECKINTOROOMIN, 0.25),
                                                               median = median(CHECKINTOROOMIN), 
                                                               mean = mean(CHECKINTOROOMIN, na.rm= TRUE),
                                                               quartile_3rd = quantile(CHECKINTOROOMIN, 0.75),
                                                               max_value = max(CHECKINTOROOMIN)) %>% 
                                                               collect()
    
    
    
    
    ggplot(data_base,                              
           aes(x = PROVIDER,
               ymin = min_value,
               lower = quartile_1st,
               middle = median,
               upper = quartile_3rd,
               ymax = max_value)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
      #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
      # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = "Distribution of NEW Appointment Check-in to Room-in Time by Provider",
             y = "Minutes",
             subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
        theme_new_line()+
        theme_bw()+
        graph_theme("none")
    
  })
  
  output$establishedRoomInTimeByProv <- renderPlot({
    print("8")
    data <- dataArrived() %>% filter(CHECKINTOROOMIN >= 0, NEW_PT3 == "ESTABLISHED") %>%
      select(PROVIDER, NEW_PT3, CHECKINTOROOMIN, APPT_TYPE) 
    # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
   #data <- data_other
    
    if(length(unique(data$APPT_TYPE)) == 1){
      appt.type <- unique(data$APPT_TYPE)
    } else{
      appt.type <- "Established"
    }
    
    # roomIn.df <- data %>%
    #   select(PROVIDER, NEW_PT3, CHECKINTOROOMIN) %>%
    #   group_by(PROVIDER, NEW_PT3)
    # 
    # avg.roomInTime <- data.frame(New.PT3 = c("Avg"),
    #                              target =  ceiling(mean(roomIn.df$CHECKINTOROOMIN)))
    # 
    # ggplot(roomIn.df, aes(x = PROVIDER, y = CHECKINTOROOMIN)) +
    #   geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA)+
    #   stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
    #   scale_y_continuous(limits = c(0,quantile(roomIn.df$CHECKINTOROOMIN,0.75)*1.5))+
    #   # geom_hline(yintercept= round(mean(roomIn.df$checkinToRoomin)), linetype="dashed", color = "red")+
    #   # annotate("text",x=length(unique(roomIn.df$Provider))/2,y=round(mean(roomIn.df$checkinToRoomin))+3,size=5,color="red",label=c('Average'))+
    #   labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Room-in Time by Provider"), 
    #        y = "Minutes",
    #        subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
    #        # caption = paste0("*Includes ", length(unique(data$Appt.Type)), "established appointments")
    #        )+
    #   theme_new_line()+
    #   theme_bw()+
    #   graph_theme("none")+
    #   theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
    data_base <- data %>% group_by(PROVIDER) %>% summarise(min_value = min(CHECKINTOROOMIN),                                
                                                               quartile_1st = quantile(CHECKINTOROOMIN, 0.25),
                                                               median = median(CHECKINTOROOMIN), 
                                                               mean = mean(CHECKINTOROOMIN, na.rm= TRUE),
                                                               quartile_3rd = quantile(CHECKINTOROOMIN, 0.75),
                                                               max_value = max(CHECKINTOROOMIN)) %>% 
                                                               collect()
    
    ggplot(data_base,                              
           aes(x = PROVIDER,
               ymin = min_value,
               lower = quartile_1st,
               middle = median,
               upper = quartile_3rd,
               ymax = max_value)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
      #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
      # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = paste0("Distribution of ",appt.type," Appointments Check-in to Room-in Time by Provider"),
           y = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
           # caption = paste0("*Includes ", length(unique(data$Appt.Type)), "established appointments")
           )+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
  })
  
  
  
  output$roomInTimeCompNew2 <- renderValueBox({
    print("1")
    
    data_room <- dataArrived() %>% filter(ROOMINTOVISITEND  >= 0, NEW_PT3 == "NEW")
    
    data <-  data_room %>% select(ROOMINTOVISITEND ) %>%
      summarise(ROOMINTOVISITEND  = mean(ROOMINTOVISITEND , na.rm = T))  %>% collect()
    
    data_median <-  data_room %>% select(ROOMINTOVISITEND ) %>%
      summarise(ROOMINTOVISITEND  = median(ROOMINTOVISITEND , na.rm = T))  %>% collect()
    
    perc <-  data_room %>% summarise(n()) %>% collect() /
      dataArrived() %>% filter(NEW_PT3 == "NEW") %>% summarise(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == TRUE))$checkinToRoomin))," min"),
      value =  paste0("Average: ",ceiling(data)," min", " | Median: ", ceiling(data_median), " min"),
      title = toupper("New Appointments Room-in to Visit-end* Time"),
      # subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0))/nrow(dataArrived()),2)*100,"% of total arrived new patients based on visit timestamps"),
      subtitle = paste0("*Based on ",round(perc,2)*100,"% of total arrived new patients based on visit timestamps"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$roomInTimeCompOther2 <- renderValueBox({
    print("2")
    
    data_room <- dataArrived() %>% filter(ROOMINTOVISITEND  >= 0, NEW_PT3 == "ESTABLISHED")
    
    data <- data_room %>% select(ROOMINTOVISITEND ) %>%
      summarise(ROOMINTOVISITEND  = mean(ROOMINTOVISITEND , na.rm = T)) %>% collect()
    
    data_median <- data_room %>% select(ROOMINTOVISITEND ) %>%
      summarise(ROOMINTOVISITEND  = median(ROOMINTOVISITEND , na.rm = T)) %>% collect()
    
    perc <- data_room %>%
      summarise(n()) %>% collect() / dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED") %>%
      summarise(n()) %>% collect()
    
    valueBoxSpark(
      # value =  paste0(round(mean((dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))$checkinToRoomin))," min"),
      value =  paste0("Average: ",ceiling(data)," min", " | Median: ", ceiling(data_median), " min"),
      title = toupper( 
        #ifelse(length(unique(dataArrived()$APPT_TYPE)) == 1,
        #paste0("Avg. ", input$selectedApptType2," Appointments Check-in to Room-in Time"),
        # paste0("Avg. ", unique(data_room$APPT_TYPE)," Appointments Check-in to Room-in Time"),
        "Established Appointments Room-in to Visit-end* Time"),
      
      # subtitle = paste0("*Based on ",round(nrow(dataArrived() %>% filter(checkinToRoomin >= 0, New.PT3 == FALSE))/nrow(dataArrived()),2)*100,"% of total arrived established patients based on visit timestamps"),
      subtitle = paste0("*Based on ",round(perc,2)*100,"% of total arrived established patients based on visit timestamps"),
      width = 6,
      color = "fuchsia"
    )
    
  })
  
  output$newRoomInTimeBoxPlot2 <- renderPlot({
    print("3")
    # data <- data_test %>% filter(CHECKINTOROOMIN >= 0) %>%
    #   filter(NEW_PT3 == "NEW") %>% select(CHECKINTOROOMIN) %>% collect()
    
    
    
    data_cycle <- dataArrived() %>% filter(ROOMINTOVISITEND >= 0, NEW_PT3 == "NEW") %>%
      select(ROOMINTOVISITEND, BIN_ROOMIN_VISIT_END) %>%
      group_by(BIN_ROOMIN_VISIT_END) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_ROOMIN_VISIT_END) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_ROOMIN_VISIT_END <- as.numeric(data_cycle$BIN_ROOMIN_VISIT_END)
    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_ROOMIN_VISIT_END)
    
    if (length(rows_to_be_included > 0)){
      
      for (i in rows_to_be_included){
        data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
        
      }
      
      data_cycle[is.na(data_cycle)] <- 0
    }
    
    bin_mapping_visit_end <- bin_mapping %>% rename(BIN_ROOMIN_VISIT_END = BIN_CYCLE)
    
    data_cycle <- left_join(data_cycle, bin_mapping_visit_end)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_ROOMIN_VISIT_END),]
    
    data_cycle$BIN_ROOMIN_VISIT_END <- factor(data_cycle$BIN_ROOMIN_VISIT_END,levels = sort(data_cycle$BIN_ROOMIN_VISIT_END))
    
    ggplot(aes(x = BIN_ROOMIN_VISIT_END , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of NEW Appointment\nRoom-in to Visit-end Time**"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_discrete(labels = data_cycle$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L))
    
  })
  
  
  output$establishedRoomInTimeBoxPlot2 <- renderPlot({
    print("4")
    
    data_cycle <- dataArrived() %>% filter(ROOMINTOVISITEND >= 0, NEW_PT3 == "ESTABLISHED") %>%
      select(ROOMINTOVISITEND, BIN_ROOMIN_VISIT_END) %>%
      group_by(BIN_ROOMIN_VISIT_END) %>% summarise(total_bin = n()) %>% collect() %>%
      mutate(total = sum (total_bin)) %>% group_by(BIN_ROOMIN_VISIT_END) %>% mutate(percent = total_bin / total)
    data_cycle$BIN_ROOMIN_VISIT_END <- as.numeric(data_cycle$BIN_ROOMIN_VISIT_END)
    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data_cycle$BIN_ROOMIN_VISIT_END)
    
    
    if (length(rows_to_be_included > 0)){
      
      for (i in rows_to_be_included){
        data_cycle[nrow(data_cycle) + 1 , 1] <- main_rows[i]
        
      }
      
      data_cycle[is.na(data_cycle)] <- 0
    }
    
    bin_mapping_visit_end <- bin_mapping %>% rename(BIN_ROOMIN_VISIT_END = BIN_CYCLE)
    data_cycle <- left_join(data_cycle, bin_mapping_visit_end)
    
    data_cycle <- data_cycle[order(data_cycle$BIN_ROOMIN_VISIT_END),]
    
    
    data_cycle$BIN_ROOMIN_VISIT_END <- factor(data_cycle$BIN_ROOMIN_VISIT_END,levels = sort(data_cycle$BIN_ROOMIN_VISIT_END))
    
    ggplot(aes(x = BIN_ROOMIN_VISIT_END , y = percent), data = data_cycle) +
      geom_bar(stat = 'identity') +
      geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Distribution of ESTABLISHED Appointment\nRoom-in to Visit-end Time**"),
           y = "% of Patients",
           x = "Minutes",
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])),
           caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out"))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")+
      scale_x_discrete(labels = data_cycle$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
    
    
  })
  
  output$roomInTimeTrend2 <- renderPlot({
    print("5")    
    
    data <- dataArrived() %>% filter(ROOMINTOVISITEND > 0, NEW_PT3 %in% c("NEW", "ESTABLISHED")) %>%
      select(ROOMINTOVISITEND, NEW_PT3, APPT_TYPE, BIN_ROOMIN_VISIT_END) %>% collect() %>% 
      mutate(NEW_PT3 = ifelse(NEW_PT3== "NEW", "NEW", APPT_TYPE)) %>%
      filter(!is.na(NEW_PT3))
    
    
    
    data <- data %>% select(ROOMINTOVISITEND, NEW_PT3, BIN_ROOMIN_VISIT_END) %>%
      group_by(BIN_ROOMIN_VISIT_END, NEW_PT3) %>% summarise(total_bin = n()) %>% 
      ungroup() %>%
      mutate(total = sum (total_bin, na.rm = TRUE))  %>% group_by(BIN_ROOMIN_VISIT_END, NEW_PT3) %>%
      mutate(percent = total_bin / total) %>%
      mutate(BIN_ROOMIN_VISIT_END = as.numeric(BIN_ROOMIN_VISIT_END))
    
    
    main_rows <- seq(0, 480, by= 30)
    
    rows_to_be_included <- which(!main_rows %in% data$BIN_ROOMIN_VISIT_END)
    
    
    if (length(rows_to_be_included)>0){
      for (i in rows_to_be_included){
        data[nrow(data) + 1 , 1] <- main_rows[i]
      }
      
    }
    
    data[, 3:length(data)][is.na(data[, 3:length(data)])] <- 0
    data <- data %>% mutate(NEW_PT3 =ifelse(is.na(NEW_PT3), "NEW", NEW_PT3))
    
    data <- unique(data)
    
    bin_mapping_visit_end <- bin_mapping %>% rename(BIN_ROOMIN_VISIT_END = BIN_CYCLE)
    data <- left_join(data, bin_mapping_visit_end)
    
    data <- data[order(data$BIN_ROOMIN_VISIT_END),]
    
    data <- data %>%  group_by(BIN_ROOMIN_VISIT_END, NEW_PT3) %>%
      mutate(BIN_ROOMIN_VISIT_END = factor(BIN_ROOMIN_VISIT_END, levels = sort(BIN_ROOMIN_VISIT_END)))
    
    x_label <- data %>% ungroup() %>% select(BIN_ROOMIN_VISIT_END, X_LABEL) %>% distinct()
    x_label <- x_label[order(x_label$BIN_ROOMIN_VISIT_END),]
    
    #data$bin <- factor(data$bin,levels = sort(data$bin))
    
    
    
    ggplot(aes(x = BIN_ROOMIN_VISIT_END , y = percent, fill=factor(NEW_PT3), color=factor(NEW_PT3)), data = data) +
      geom_bar(stat = 'identity') +
      scale_color_MountSinai()+
      scale_fill_MountSinai()+
      #geom_col(width = 1, fill="#fcc9e9", color = "#d80b8c") +
      labs(title = paste0("Room-in to Visit-end* Time Comparison by Appointment Type"),
           y = "% of Patients",
           x = "Minutes",
           caption = paste0("*Visit-end Time is the minimum of Visit-end Time and Check-out"), 
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("top")+
      scale_x_discrete(labels = x_label$X_LABEL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) #+
    #theme(axis.text.x = element_text(hjust = 3.5))
    
  })
  
  
  output$roomInTimeByHour2 <- renderPlot({
    print("6")
    data <- dataArrived() %>% filter(ROOMINTOVISITEND > 0) %>% filter(NEW_PT3 == "NEW") %>%
      select(APPT_DAY, APPT_TM_HR, ROOMINTOVISITEND) %>% collect()
    # data <- arrived.data %>% filter(checkinToRoomin > 0) %>% filter(New.PT3 == TRUE)
    
    data_other <- dataArrived() %>% filter(NEW_PT3 == "ESTABLISHED", ROOMINTOVISITEND > 0) %>%
      select(APPT_DAY, APPT_TM_HR, ROOMINTOVISITEND, APPT_TYPE) %>% collect()
    
    # data_other <- arrived.data %>% filter(New.PT3 == FALSE, checkinToRoomin > 0)
    
    # names <- data_other %>% filter(APPT_TM_HR %in% timeOptionsHr_filter) %>% select(APPT_TYPE)
    # names <- sort(unique(names$APPT_TYPE), na.last = T)
    
    
    appt.type.choices <- dataArrived() %>% filter( ROOMINTOVISITEND > 0, NEW_PT3 == "ESTABLISHED") %>% 
      select(APPT_TYPE) %>% mutate(APPT_TYPE = unique(APPT_TYPE)) %>% collect()
    
    
    if(input$median4 == TRUE){
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(ROOMINTOVISITEND)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = median(ROOMINTOVISITEND)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      input <- "Median"
    } else{
      
      data <- data %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(ROOMINTOVISITEND)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      
      data_other <- data_other %>%
        group_by(APPT_DAY, APPT_TM_HR) %>%
        summarise(avg = mean(ROOMINTOVISITEND)) %>%
        filter(APPT_TM_HR %in% timeOptionsHr_filter)
      
      input <- "Average"
    }
    
    if(length(appt.type.choices$APPT_TYPE) == 1){
      appt.type <- unique(appt.type.choices$APPT_TYPE)
    } else{
      appt.type <- "Established*"
    }
    
    level_order <- rev(toupper(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
    
    new <- ggplot(data, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg)) + 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#d80b8c")+
      labs(title = paste0(input," New Patients Room-in to Visit-end** Time by Hour\n"),
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
      geom_text(aes(label= ifelse(is.na(avg),"", ceiling(avg))), color="black", size=5, fontface="bold")
    
    
    other <- ggplot(data_other, aes(APPT_TM_HR, y = factor(APPT_DAY, level = level_order), fill = avg))+ 
      geom_tile(colour = "white") + 
      scale_fill_gradient(low="white", high="#00aeef")+
      labs(title = paste0(input," ",appt.type," Patients Room-in to Visit-end** Time by Hour\n"), 
           y = NULL,
           caption = paste0("*Includes ", length(unique(appt.type.choices$APPT_TYPE)), " established visit types\n **Visit-end Time is the minimum of Visit-end Time and Check-out"),
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
      geom_text(aes(label= ifelse(is.na(avg),"", ceiling(avg))), color="black", size=5, fontface="bold")
    
    grid.arrange(new, other, ncol = 1)
    
  })
  
  
  output$newRoomInTimeByProv2 <- renderPlot({
    print("7")
    data <- dataArrived() %>% filter(ROOMINTOVISITEND >= 0, NEW_PT3 == "NEW") %>%
      select(PROVIDER, ROOMINTOVISITEND) 
    
    # data <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(New.PT3 == TRUE) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology")
    
   
    data_base <- data %>% group_by(PROVIDER) %>% summarise(min_value = min(ROOMINTOVISITEND),                                
                                                           quartile_1st = quantile(ROOMINTOVISITEND, 0.25),
                                                           median = median(ROOMINTOVISITEND), 
                                                           mean = mean(ROOMINTOVISITEND, na.rm= TRUE),
                                                           quartile_3rd = quantile(ROOMINTOVISITEND, 0.75),
                                                           max_value = max(ROOMINTOVISITEND)) %>% 
      collect()
    
    
    
    
    ggplot(data_base,                              
           aes(x = PROVIDER,
               ymin = min_value,
               lower = quartile_1st,
               middle = median,
               upper = quartile_3rd,
               ymax = max_value)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
      #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
      # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = "Distribution of NEW Appointment Room-in to Visit-end** Time by Provider",
           y = "Minutes",
           caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out"),
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2])))+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")
    
  })
  
  output$establishedRoomInTimeByProv2 <- renderPlot({
    print("8")
    data <- dataArrived() %>% filter(ROOMINTOVISITEND >= 0, NEW_PT3 == "ESTABLISHED") %>%
      select(PROVIDER, NEW_PT3, ROOMINTOVISITEND, APPT_TYPE) 
    # data_other <- arrived.data %>% filter(checkinToRoomin >= 0) %>% filter(Campus == "MSUS", Campus.Specialty == "Cardiology", Appt.Type == "FOLLOW UP")
    
    #data <- data_other
    
    if(length(unique(data$APPT_TYPE)) == 1){
      appt.type <- unique(data$APPT_TYPE)
    } else{
      appt.type <- "Established"
    }
    

    data_base <- data %>% group_by(PROVIDER) %>% summarise(min_value = min(ROOMINTOVISITEND),                                
                                                           quartile_1st = quantile(ROOMINTOVISITEND, 0.25),
                                                           median = median(ROOMINTOVISITEND), 
                                                           mean = mean(ROOMINTOVISITEND, na.rm= TRUE),
                                                           quartile_3rd = quantile(ROOMINTOVISITEND, 0.75),
                                                           max_value = max(ROOMINTOVISITEND)) %>% 
      collect()
    
    ggplot(data_base,                              
           aes(x = PROVIDER,
               ymin = min_value,
               lower = quartile_1st,
               middle = median,
               upper = quartile_3rd,
               ymax = max_value)) +
      geom_boxplot(colour="black", fill="slategray1", outlier.shape=NA, stat = "identity")+
      #stat_summary(fun=mean, geom="point", shape=18, size=3, color="maroon1", fill="maroon1")+
      #scale_y_continuous(limits = c(0,  (data_base$quartile_3rd) *2))+
      # geom_hline(yintercept= round(mean(cycle.df$cycleTime)), linetype="dashed", color = "red")+
      # annotate("text",x=length(unique(cycle.df$Provider))/2, y=round(mean(cycle.df$cycleTime))+3,size=5,color="red",label=c('Average'))+
      labs(title = paste0("Distribution of ",appt.type," Appointments Room-in to Visit-end** Time by Provider"),
           y = "Minutes",
           caption = paste0("**Visit-end Time is the minimum of Visit-end Time and Check-out"),
           subtitle = paste0("Based on data from ",isolate(input$dateRange[1])," to ",isolate(input$dateRange[2]))
           # caption = paste0("*Includes ", length(unique(data$Appt.Type)), "established appointments")
      )+
      theme_new_line()+
      theme_bw()+
      graph_theme("none")#+
      #theme(plot.caption = element_text(hjust = 0, size = 12, face = "italic"))
    
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
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    
    paste0("Total Monthly Volume by ", name_1 , " and ", name_2)
  })
  
  output$vol_day_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    print(input$compare_filters)
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    paste0("Average Daily Volume by ", name_1 , " and ", name_2)
  })
  
  output$am_pm_breakdown_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    print(input$compare_filters)
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    paste0("Average Session* Daily Volume by ", name_1 , " and ", name_2)
  })
  
  output$am_pm_breakdown_title_month <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    print(input$compare_filters)
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    paste0("Total Session* Monthly Volume by ", name_1 , " and ", name_2)
  })
  
  output$npr_month_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    paste0("Monthly New Patient Ratio (%) by ", name_1 , " and ", name_2)
  })
  
  
  
  output$new_wait_month_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    paste0("Monthly Median New Patient Wait Time (Days) by ", name_1 , " and ", name_2)
  })
  
  
  
  output$new_patient_fill_rate_day_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT2"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    
    paste0("Average Daily Booked and Filled Rate by ", name_1 , " and ", name_2)
  })
  
  
  output$new_patient_fill_rate_month_title <- renderText({
    if(input$breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(input$breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(input$breakdown_filters == "NEW_PT2"){
      name_2 <- "Patient Status"
    }
    
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
    }
    if(input$compare_filters == "PROVIDER"){
      name_1 <- "Provider"
    }
    
    paste0("Total Monthly Booked and Filled Rate by ", name_1 , " and ", name_2)
  })
  
  rows_group <- reactive({
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      list <- list(0)
    }
    if(input$compare_filters == "DEPARTMENT"){
      list <- list(0,1)
    }
    if(input$compare_filters == "PROVIDER"){
      list <- list(0,1,2)
    }
    list
    
  })
  
  rows_group_opt <- reactive({
    if(input$compare_filters_opt == "CAMPUS_SPECIALTY"){
      list <- list(0)
    }
    if(input$compare_filters_opt == "DEPARTMENT"){
      list <- list(0,1)
    }
    if(input$compare_filters_opt == "PROVIDER"){
      list <- list(0,1,2)
    }
    list
    
  }) 
  
  rows_group_slot <- reactive({
    if(input$compare_filters == "CAMPUS_SPECIALTY"){
      list <- list(0,1)
    }
    if(input$compare_filters == "DEPARTMENT"){
      list <- list(0,1,2)
    }
    if(input$compare_filters == "PROVIDER"){
      list <- list(0,1,2,3)
    }
    list
    
  })
  
  
  vol_comp_day <- reactive({
    data <- dataArrived_summary() #%>% filter(Resource == "Provider")
    #data <- arrived.data.rows.summary %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% c( "Cardiology"))
    #compare_filters <- "Campus.Specialty"
    #breakdown_filters <- "Visit.Method"
    # data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD, APPT_TYPE, NEW_PT3, CAMPUS_SPECIALTY, DEPARTMENT, APPT_DATE_YEAR, PROVIDER) %>% collect()
    # 
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Appt.Type = APPT_TYPE,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         New.PT3 = NEW_PT3)

    #data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }

    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","DEPARTMENT",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    if(input$breakdown_filters == "NEW_PT3"){
      ### Group data by inputs and Month and Date.  Spread data to make columns FALSE and TRUE that determine number of new or established patients
      volume <- data %>% group_by(!!!syms(cols),APPT_DATE_YEAR, APPT_MONTH_YEAR) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        spread(!!breakdown_filters, total)
        

      volume[is.na(volume)] <- 0 ## NAs to 0
      
     

      #### Create "New" column to establish number of new patients. then group by Month and create "avg" column that is the sum of all new patients within 
      #### the month divided by the total number of days we have data in the month
      volume_new <- volume %>% group_by(across(!!tot_cols)) %>%
        mutate(New = ceiling(NEW)) %>% 
        group_by(across(!!tot_cols), APPT_MONTH_YEAR) %>%
        summarise(avg = ceiling(sum(New)/n())) #%>% collect() 
      
      drop <- c("ESTABLISHED","NEW", "<NA>")
      volume_new = volume_new[,!(names(volume_new) %in% drop)]
      
      #### Take volume_new and get the Appt.Month columns and make the values into columns themselves by widening the data
      #### also added a column named so it can be used as the "New" patients group
      volume_new <- volume_new %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
                    values_from = avg,
                    values_fill = 0) %>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      

      #### Create "Established" column to establish number of new patients. then group by Month and create "avg" column that is the sum of all new patients within 
      #### the month divided by the total number of days we have data in the month
      volume_est<- volume %>% group_by(across(!!tot_cols),APPT_DATE_YEAR) %>%
        mutate(New = ceiling(ESTABLISHED))%>% 
        group_by(across(!!tot_cols), APPT_MONTH_YEAR) %>%
        summarise(avg = ceiling(sum(New)/n())) 
      
      drop <- c("ESTABLISHED","NEW", "<NA>")
      volume_est = volume_est[,!(names(volume_est) %in% drop)]
      
      #### Take volume_new and get the Appt.Month columns and make the values into columns themselves by widening the data
      #### also added a column named so it can be used as the "Establihsed" patients group
      volume_est <- volume_est %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
                    values_from = avg,
                    values_fill = 0)%>%
        add_column(!!breakdown_filters := "Established") #%>%
        #relocate(all_of(breakdown_filters), .after = !!compare_filters)

      
      ### Join numbers for new and est patients
      volume <- full_join(volume_new,volume_est)
      # volume <- volume %>% relocate(breakdown_filters, .after = !!compare_filters)
      
      
      ### Get average total by adding all the numbers in the months columns grouped by the volume filters
      #### Also added a column named by the breakdown filter to store the number Avergae Total NUmber
      #tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
       # summarise_at(vars(-!!breakdown_filters), sum) #%>%
        #relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)

      volume <- full_join(volume,tot)
      volume <- volume %>% arrange(across(all_of(tot_cols)))
      
      # volume <-  volume %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
      

    }else {
      # 
      # if (compare_filters == "CAMPUS_SPEACILATY" && breakdown_filters == "VISIT_METHOD"){
      #   volume <- data %>% group_by(!!!syms(cols), APPT_DATE_YEAR, APPT_MONTH_YEAR)  %>%
      #     summarise(total = n()) %>%
      #     group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
      #     summarise(avg = round(sum(total)/n())) %>% collect() %>%
      #     pivot_wider(names_from = APPT_MONTH_YEAR,
      #                 values_from = avg,
      #                 values_fill = 0) 
      # } 
      # if (compare_filters == "CAMPUS_SPEACILATY"  && breakdown_filters == "APPT_TYPE"){
      #   volume <- data %>% group_by(CAMPUS_SPECIALTY, APPT_TYPE, APPT_DATE_YEAR, APPT_MONTH_YEAR)  %>%
      #     summarise(total = n()) %>%
      #     group_by(cols, APPT_MONTH_YEAR) %>%
      #     summarise(avg = round(sum(total)/n())) %>% collect() %>%
      #     pivot_wider(names_from = APPT_MONTH_YEAR,
      #                 values_from = avg,
      #                 values_fill = 0) 
      # }
      #### group Data by inputs and Month and data get the total for each day then group by the Month in order to sum all the vistis within the month and 
      #### divide it by the number of days within the month in the data, then we make a wider data sets with the months values made into a column
      volume <- data %>% group_by(!!!syms(cols),APPT_DATE_YEAR, APPT_MONTH_YEAR)  %>% 
        summarise(total = sum(TOTAL_APPTS)) %>%
        group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
        summarise(avg = ceiling(sum(total, na.rm = T)/n())) %>% collect() %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
                    values_from = avg,
                    values_fill = 0)

      
      volume[is.na(volume)] <- 0
      
      
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum)  %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      # volume <-  volume %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #             split( .[,tot_cols] ) %>%
      #             purrr::map_df(., janitor::adorn_totals)
      
      #test <- volume %>% group_by(CAMPUS_SPECIALTY, DEPARTMENT) %>% adorn_totals()
      
      
      volume <- full_join(volume, tot) 
      volume <- volume %>% arrange(across(all_of(tot_cols)))
      
      ### Get average total by adding all the numbers in the months columns grouped by the volume filters
      #### Also added a column named by the breakdown filter to store the number Avergae Total NUmber
      # tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
      #   summarise_at(vars(-!!breakdown_filters), sum) %>%
      #   relocate(all_of(breakdown_filters), .after = !!compare_filters)
      # 
      # volume <- full_join(volume,tot)
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])

      
      
    }
    volume <- setnames(volume, old = cols, new = cols_name)
    #volume$Total_YN <- ifelse(volume[[name_2]] == "Total", 1,0)
    volume_test <<- volume
    volume$Total_YN <- ifelse(volume[[all_of(name_2)]] == "Total", 1,0)

   
    months_df <- volume[,!(names(volume) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))
    #order_months <- months_df[months]

    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index,(length(volume)-1):length(volume))
    #index <- c(1:length(cols_name),index,length(volume))

    volume <- volume[index]

    
    # if(length(input$selectedSpecialty) > 1){
    #   ## Adding "All" for aggregate total comparison
    #   all <- volume %>% group_by(across(!!name_2)) %>% summarise_at(vars(names(order_months),Total), sum) %>%
    #                      filter(across(!!name_2) !="Total")
    #     
    #    
    #   all[cols_name[1:length(cols_name)-1]] <- ""
    #   
    #   all[[name_1]] <- "All"
    #   all$Total_YN <- 1
    #   
    #   
    #   all <- all %>% select(cols_name, everything())
    #   
    #   
    #   volume <- full_join(all, volume)
    # }
    
    #index <- months+length(cols_name)
    #index <- c(1:length(cols_name),index,length(volume))
    
    #volume <- volume[index]
    #month_names <- colnames(volume[setdiff(names(volume), c(cols_name, "Total", "Total_YN"))])
    #month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    #volume <- setnames(volume, old = month_names, new = month_names_new)
    
   
  
    
    # if(compare_filters == "Specialty"){
    #   col = 3
    # }
    # if(compare_filters == "Department"){
    #   col = 4
    # }
    # 
    # if(compare_filters == "Provider"){
    #   col = 5
    # }
    # 
    # volume <- cbind(volume[,1:(col-1)],round(volume[,col:length(volume)]))

    # months <- order(as.yearmon(colnames(volume[col:(length(volume)-1)]), "%Y-%b"))
    # 
    # volume <- cbind(volume[months],Total = volume[,length(volume)])
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
                            htmltools::em('Average Daily Volume = Total arrived visits by month and breakdown / Total number of days within the month.')),
                          
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
    
    # %>%
    #   formatStyle(
    #     'Total_YN',
    #     target = "row",
    #     fontWeight = styleEqual(1, "bold")
    #     #backgroundColor = styleEqual(c(1),c('grey'))
    #   ) 
    ) %>% 
      formatStyle(
        'Total_YN',
        target = "row",
        fontWeight = styleEqual(1, "bold")
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
    data <- dataArrived_summary() #%>% filter(Resource == "Provider")
    #data <- arrived.data.rows.summary %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% c("Allergy"))
    # compare_filters <- "Department"
    # breakdown_filters <- "Visit.Method"
    # 
    # data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD, APPT_TYPE, NEW_PT3, CAMPUS_SPECIALTY, DEPARTMENT, APPT_DATE_YEAR, PROVIDER) %>% collect()
    # 
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Appt.Type = APPT_TYPE,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         New.PT3 = NEW_PT3)
    # 
    # 
    # data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    if(breakdown_filters == "NEW_PT3"){
      
      #### Group data by inputs and Month.  Spread data to make TRUE and FALSe columns for new patietns
      volume <- data %>% group_by(!!!syms(cols),APPT_MONTH_YEAR) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        spread(!!breakdown_filters, total)
      
      volume[is.na(volume)] <- 0
      
      ### Getting total new patients for month
      volume_new <- volume %>% group_by(across(!!tot_cols),APPT_MONTH_YEAR) %>%
        mutate(total = ceiling(NEW))
      
      drop <- c("NEW","ESTABLISHED", "<NA>")
      volume_new = volume_new[,!(names(volume_new) %in% drop)]
      
      #### Pivoting new patients for each month
      volume_new <- volume_new %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
                    values_from = total,
                    values_fill = 0) %>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      
      #### Getting total number of establihsed patietns
      volume_est<- volume %>% group_by(across(!!tot_cols),APPT_MONTH_YEAR) %>%
        mutate(total = ceiling(ESTABLISHED))
      
      drop <- c("NEW","ESTABLISHED", "<NA>")
      volume_est = volume_est[,!(names(volume_est) %in% drop)]
      
      #### Pivot data established patietns for each month 
      volume_est <- volume_est %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
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
      volume <- volume %>% arrange(across(all_of(tot_cols)))
      
      # volume <-  volume %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      #### GEt rowSUms of all columns with months
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
      
      
    }else {
      
      #### Group data by inputs and Month get total for the month and pivot wider to moths are now columns
      volume <- data %>% group_by(!!!syms(cols),APPT_MONTH_YEAR) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        pivot_wider(names_from = APPT_MONTH_YEAR,
                    values_from = total,
                    values_fill = 0) 
      
      
      volume[is.na(volume)] <- 0
      
      #### Sum all cloumns with months to get the total for the month
      tot <- volume %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)


      volume <- full_join(volume,tot)
      volume <- volume %>% arrange(across(all_of(tot_cols)))
      
      # volume <-  volume %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      
      volume$Total <- rowSums(volume[setdiff(names(volume),cols)])
    }
    volume <- setnames(volume, old = cols, new = cols_name)
    
    #volume$Total_YN <- ifelse(volume[[name_2]] == "Total", 1,0)
    
    volume$Total_YN <- ifelse(volume[[all_of(name_2)]] == "Total", 1,0)
    
    months_df <- volume[,!(names(volume) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))
    order_months <- months_df[months]
    
    
    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index,(length(volume)-1):length(volume))
    
    volume <- volume[index]
    
    month_names <- colnames(volume[setdiff(names(volume), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    volume <- setnames(volume, old = month_names, new = month_names_new)
    
    
    # if(length(input$selectedSpecialty) > 1){
    #   all <- volume %>% group_by(across(!!name_2)) %>% summarise_at(vars(all_of(month_names_new),Total), sum) %>%
    #     filter(across(!!name_2) !="Total")
    #   
    #   all[cols_name[1:length(cols_name)-1]] <- ""
    #   all[[name_1]] <- "All"
    #   all$Total_YN <- 1
    #   all <- all %>% select(cols_name, everything())
    #   
    #   volume <- full_join(all, volume)
    # }
    
    # volume <- cbind(volume[,1:2],round(volume[,3:length(volume)]))
    # 
    # months <- order(as.yearmon(colnames(volume[3:(length(volume)-1)]), "%Y-%b"))
    # 
    # volume <- cbind(volume[months],Total_YN=volume[,length(volume)])
    
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
    data <- dataArrived_access_npr() #%>% filter(Resource == "Provider")
   #data <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% c("Allergy"))
    # compare_filters <- "Department"
    # breakdown_filters <- "Visit.Method"
    
    # data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD, APPT_TYPE, NEW_PT3, CAMPUS_SPECIALTY, DEPARTMENT, APPT_DATE_YEAR, PROVIDER) %>% collect()
    # 
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Appt.Type = APPT_TYPE,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         New.PT3 = NEW_PT3)
    
    
    #data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    data_test <<- data
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    
    
    if(breakdown_filters == "NEW_PT3"){
      
      ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
      newpatients.ratio <- data %>% group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        drop_na() %>%
        spread(!!breakdown_filters, total)
      
      newpatients.ratio[is.na(newpatients.ratio)] <- 0
      
      #### Calulate new patient ratio for the whole month sum of all new patients within the month divide by sum of new and established patients
      newpatients.ratio.new <- newpatients.ratio %>% group_by(across(!!tot_cols), APPT_MADE_MONTH_YEAR) %>%
        mutate(ratio = round(`NEW`/(`NEW`+ `ESTABLISHED`),2))
      
      
      
      drop <- c("ESTABLISHED","NEW", "<NA>")
      newpatients.ratio.new = newpatients.ratio.new[,!(names(newpatients.ratio.new) %in% drop)]
      
      #### Pivot the data so that month are now in a columns
      newpatients.ratio.new <- newpatients.ratio.new %>%
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                    values_from = ratio,
                    values_fill = 0)%>%
        add_column(!!breakdown_filters := "New") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)
      
      
      #### Calulate est patient ratio for the whole month sum of all est patients within the month divide by sum of new and established patients
      newpatients.ratio.est <- newpatients.ratio %>% group_by(across(!!tot_cols), APPT_MADE_MONTH_YEAR) %>%
        mutate(ratio = round(ESTABLISHED/(sum(NEW, na.rm = TRUE) + sum(ESTABLISHED, na.rm = TRUE)),2))
      
      
      
      drop <- c("ESTABLISHED","NEW", "<NA>")
      newpatients.ratio.est = newpatients.ratio.est[,!(names(newpatients.ratio.est) %in% drop)]
      
      #### Pivot data wider to months are their own columns
      newpatients.ratio.est <- newpatients.ratio.est %>%
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
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
      newpatients.ratio <- newpatients.ratio %>% arrange(across(all_of(tot_cols)))
      
      # newpatients.ratio <-  newpatients.ratio %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
    }else{
      
      ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
      newpatients.ratio <- data %>% group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR, NEW_PT3) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        drop_na() %>%
        spread(NEW_PT3, total)
      
      newpatients.ratio[is.na(newpatients.ratio)] <- 0
      
      
      newpatients.ratio.overtime <- data %>% group_by(!!!syms(cols), NEW_PT3) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        drop_na() %>%
        spread(NEW_PT3, total) %>%
        summarise(Total = round(`NEW`/(`NEW`+`ESTABLISHED`),2))
      
      newpatients.ratio.overtime[is.na(newpatients.ratio.overtime)] <- 0
      
      newpatients.ratio.overtime.total <- data %>% group_by(!!!syms(tot_cols), NEW_PT3) %>%
        summarise(total = sum(TOTAL_APPTS)) %>% collect() %>%
        drop_na() %>%
        spread(NEW_PT3, total) %>%
        summarise(Total = round(`NEW`/(`NEW`+`ESTABLISHED`),2)) %>%
        add_column(!!breakdown_filters := "Total") %>%
        select(all_of(cols),  everything())
      
      newpatients.ratio.overtime.total[is.na(newpatients.ratio.overtime.total)] <- 0
      
      
      newpatients.ratio.overtime <- bind_rows(newpatients.ratio.overtime, newpatients.ratio.overtime.total)  
      
      ### Calculate new patient ratio by breakdown
      newpatients.ratio <- newpatients.ratio %>% group_by(across(!!tot_cols), APPT_MADE_MONTH_YEAR) %>%
        mutate(ratio = round(`NEW`/(`NEW`+`ESTABLISHED`),2))
      
      
      drop <- c("ESTABLISHED","NEW", "<NA>")
      newpatients.ratio = newpatients.ratio[,!(names(newpatients.ratio) %in% drop)]
      
      
      newpatients.ratio <- newpatients.ratio %>%
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                    values_from = ratio,
                    values_fill = 0)
      
      
      ##### sum all the columns with months in them to get the Totoal for the month
      tot <- newpatients.ratio %>% group_by(across(all_of(tot_cols))) %>%
        summarise_at(vars(-!!breakdown_filters), sum) %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters)


      newpatients.ratio <- full_join(newpatients.ratio,tot)
      
      newpatients.ratio <- newpatients.ratio %>% arrange(across(all_of(tot_cols)))
      
      # newpatients.ratio <-  newpatients.ratio %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      newpatients.ratio <- left_join(newpatients.ratio, newpatients.ratio.overtime)
      
      
    }
    
    newpatients.ratio <- setnames(newpatients.ratio, old = cols, new = cols_name)
    
    #newpatients.ratio$Total_YN <- ifelse(newpatients.ratio[[name_2]] == "Total", 1,0)
    newpatients.ratio$Total_YN <- ifelse(newpatients.ratio[[all_of(name_2)]] == "Total", "1","0")
    
    months_df <- newpatients.ratio[,!(names(newpatients.ratio) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))
    
    
    index <- months+length(cols_name)
    
    
    if(breakdown_filters == "NEW_PT3"){
      index <- c(1:length(cols_name),index,length(newpatients.ratio))
    } else{
      index <- c(1:length(cols_name),index,(length(newpatients.ratio)-1):length(newpatients.ratio)) 
    }
    
    newpatients.ratio <- newpatients.ratio[index]
    
    month_names <- colnames(newpatients.ratio[setdiff(names(newpatients.ratio), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    newpatients.ratio <- setnames(newpatients.ratio, old = month_names, new = month_names_new)
    
    
    names_list <- colnames(newpatients.ratio[, !names(newpatients.ratio) %in% c(name_1,name_2)])
    myfun <- function(x) {
      if(is.numeric(x)){ 
        ifelse(is.na(x), x, scales::percent(x)) 
      } else x 
    }
    
    newpatients.ratio <- newpatients.ratio %>% mutate_each(funs(myfun))
    print(str(newpatients.ratio))
    # for (i in names_list){
    #   
    # }
    
    
    

    # months <- order(as.yearmon(colnames(newpatients.ratio[3:(length(newpatients.ratio))]), "%Y-%b"))
    # 
    # newpatients.ratio <- cbind(newpatients.ratio[months],Total_YN = newpatients.ratio[length(newpatients.ratio)])
    
    
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
        fontWeight = styleEqual("1", "bold")
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
    data <- dataAll_access()
    #data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Department"
    # breakdown_filters <- "New.PT3"
    
    
    # data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD, APPT_TYPE, NEW_PT3, CAMPUS_SPECIALTY, DEPARTMENT, APPT_DATE_YEAR, PROVIDER, APPT_DTTM, APPT_MADE_DTTM) %>% collect()
    # 
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Appt.Type = APPT_TYPE,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         Appt.DTTM = APPT_DTTM,
    #                         Appt.Made.DTTM = APPT_MADE_DTTM,
    #                         New.PT3 = NEW_PT3)
    # 
    # data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    # 
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters

    
    if(breakdown_filters == "NEW_PT3") {
      breakdown_filters <- "NEW_PT2"
    }
    
    start.time <- Sys.time()
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT2"){
      name_2 <- "New vs. Established"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    end.time <- Sys.time()
    time.taken <- round(end.time - start.time,2)
    time.taken
    
    
    
    
    
    if(breakdown_filters == "NEW_PT2"){
      
      
      #### Calculate wait time which is the difference between the the patients made the appt to the data of the appt
      #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
      
      #### Filter out for wait times grater than 0 and calculate the monthly median wait time 
      waitTime <- data %>%
        filter(WAIT_TIME >= 0) %>%
        group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
        filter(NEW_PT2 %in% c("NEW","ESTABLISHED")) %>% collect()
      
      
      #### convert to new and established and filter out established and drop the NEW.PT3 columns
      waitTime$NEW_PT2 <- ifelse(waitTime$NEW_PT2 == "NEW", "New","Established")
      
      
      #### Get the average daily for new patients and arrived patients 
      waitTime <- waitTime %>% 
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                    values_from = medWaitTime,
                    values_fill = 0) 
      
      #### Get total by summing all columns that are months
       # tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
       #   summarise_at(vars(-!!breakdown_filters), sum) 
       #  relocate(all_of(breakdown_filters), .after = !!compare_filters)
       #  
        tot <- data %>%
          filter(WAIT_TIME >= 0) %>%
          group_by(!!!syms(tot_cols), APPT_MADE_MONTH_YEAR) %>%
          dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
          collect() %>%
          add_column(!!breakdown_filters := "Total") %>%
          relocate(all_of(breakdown_filters), .after = !!compare_filters) %>%
          pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                      values_from = medWaitTime,
                      values_fill = 0)
      
      waitTime <- full_join(waitTime,tot)
      
      waitTime <- waitTime %>% arrange(across(all_of(tot_cols)))
      
      
      tot_over_time <- data %>%
        filter(WAIT_TIME >= 0) %>%
        group_by(!!!syms(cols)) %>%
        dplyr::summarise(Total = ceiling(median(WAIT_TIME))) %>%
        collect()
      
      tot_over_time$NEW_PT2 <- ifelse(tot_over_time$NEW_PT2 == "NEW", "New","Established")
      
      tot_all <- data %>%
        filter(WAIT_TIME >= 0) %>%
        group_by(!!!syms(tot_cols)) %>%
        dplyr::summarise(Total = ceiling(median(WAIT_TIME))) %>%
        collect() %>%
        add_column(!!breakdown_filters := "Total")
      
      total <- bind_rows(tot_over_time, tot_all)
      waitTime <- left_join(waitTime, total)
      
      # waitTime <-  waitTime %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      
    }else{
      ### Calculate wait time which is the difference between when the patient made the appt and the scheduled date
      #data$wait.time <- as.numeric(round(difftime(data$Appt.DTTM, data$Appt.Made.DTTM,  units = "days"),2))
      
      
      #### Filter out wait time that equals 0 and calculate the median wait time for NEw and est patients by month
      waitTime <- data %>%
        filter(WAIT_TIME >= 0, NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols),APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>% collect()
        #filter(NEW_PT3 %in% c("NEW","ESTABLISHED")) %>% collect()
      
      
      #### Change the TRUE and FALSE to New and Established and filter our new patients and drop the New.PT3 column
      #waitTime$NEW_PT3 <- ifelse(waitTime$NEW_PT3 == "NEW", "New","Established")
      #waitTime <- waitTime %>% filter(NEW_PT3 == "New")
      #drop <- c("NEW_PT3")
      #waitTime = waitTime[,!(names(waitTime) %in% drop)]
      
      #### Pivot the data so the months are in the columns and shows only new patient median time   
      waitTime <- waitTime %>%
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                    values_from = medWaitTime,
                    values_fill = 0)
      
      
      # tot <- waitTime %>% group_by(across(all_of(tot_cols))) %>%
      #   summarise_at(vars(-!!breakdown_filters), sum) %>%
      #   add_column(!!breakdown_filters := "Total") %>%
      #   relocate(all_of(breakdown_filters), .after = !!compare_filters)

      
      tot <- data %>%
        filter(WAIT_TIME >= 0, NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
        collect() %>%
        add_column(!!breakdown_filters := "Total") %>%
        relocate(all_of(breakdown_filters), .after = !!compare_filters) %>%
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                    values_from = medWaitTime,
                    values_fill = 0)
      
      
      waitTime <- full_join(waitTime,tot)
      waitTime <- waitTime %>% arrange(across(all_of(tot_cols)))
      
      # waitTime <-  waitTime %>% dplyr::arrange(across(all_of(tot_cols))) %>%
      #   split( .[,tot_cols] ) %>%
      #   purrr::map_df(., janitor::adorn_totals)
      
      
      
      tot_over_time <- data %>%
        filter(WAIT_TIME >= 0, NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols)) %>%
        dplyr::summarise(Total = ceiling(median(WAIT_TIME))) %>%
        collect()
      
      tot_all <- data %>%
        filter(WAIT_TIME >= 0, NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols)) %>%
        dplyr::summarise(Total = ceiling(median(WAIT_TIME))) %>%
        collect() %>%
        add_column(!!breakdown_filters := "Total")
      
      total <- bind_rows(tot_over_time, tot_all)
      waitTime <- left_join(waitTime, total)
      
      
    }
    waitTime <- setnames(waitTime, old = cols, new = cols_name)
    
    
    waitTime$Total_YN <- ifelse(waitTime[[all_of(name_2)]] == "Total", 1,0)
    
    months_df <- waitTime[,!(names(waitTime) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))
    order_months <- months_df[months]
    
    
    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index,(length(waitTime)-1):length(waitTime))
    
    waitTime <- waitTime[index]
    
    month_names <- colnames(waitTime[setdiff(names(waitTime), c(cols_name, "Total", "Total_YN"))])
    month_names_new <- as.character(lapply(month_names, function(x){paste(sapply(strsplit(x, "\\s+"), rev), collapse= '-')}))
    
    waitTime <- setnames(waitTime, old = month_names, new = month_names_new)
    
    
    # months <- order(as.yearmon(colnames(waitTime[3:(length(waitTime))]), "%Y-%b"))
    # 
    # waitTime <- waitTime[months]
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
                            # htmltools::em('Median New Patient Wait Time = median wait time of all arrived new patients within the month')
                            htmltools::em('Median New Patient Wait Time = median wait time of scheduled new patients within the month')
                            
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
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%') %>%
      formatStyle(columns = c("Total"), fontWeight = 'bold')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  
  no_Show_percentage <- reactive({
    

    
    # data <- arrivedNoShow.data.rows %>%
    #   filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")%>%
    #   filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))
    
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
      data <- dataArrivedNoShow() %>%
        filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))


    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
      breakdown_filters <- "NEW_PT2"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    
    data <- data %>% mutate(APPT_STATUS = ifelse(APPT_STATUS == "Arrived","Arrived","No Show"))
    
    
    # if(breakdown_filters == "NEW_PT2"){
    #   validate(
    #     need(input$breakdown_filters != "NEW_PT2", ("No show Rate cannot be viewed by New vs Established Patients"))
    #   )
    #   
    # } else{
      
      
      noShow_perc <- data %>%
        group_by(!!!syms(cols),APPT_STATUS, APPT_MONTH_YEAR) %>%
        dplyr::summarise(Total = n()) %>% collect() %>% 
        pivot_wider(names_from = APPT_STATUS, values_from = Total) %>%
        replace(is.na(.), 0) %>% 
        group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
        mutate(percentage = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%"))%>%
        mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))%>%
        select(-`No Show`,-Arrived) %>% 
        pivot_wider(names_from = APPT_MONTH_YEAR, values_from = percentage)%>%
        ungroup()
      
      
      
      tot <- data %>%
        group_by(!!!syms(tot_cols), APPT_STATUS, APPT_MONTH_YEAR) %>%
        dplyr::summarise(Total = n()) %>% 
        collect() %>%
        add_column(!!breakdown_filters := "Total") %>%
        pivot_wider(names_from = APPT_STATUS, values_from = Total) %>%
        replace(is.na(.), 0) %>% 
        group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
        mutate(percentage = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%")) %>%
        select(-`No Show`,-Arrived) %>% 
        mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))%>%
        pivot_wider(names_from = APPT_MONTH_YEAR, values_from = percentage)%>%
        ungroup() %>%
        select(all_of(cols),  everything())
      
      noShow_perc <- full_join(noShow_perc, tot)
      
      noShow_perc <- noShow_perc %>% arrange(across(all_of(tot_cols)))
      
      
      
      i1 <- as.yearmon(names(noShow_perc))
      noShow_perc <- noShow_perc[order(i1)]
      
      noShow_perc <- noShow_perc %>% select(cols, everything())
      
      tot_over_time <- data %>%
        group_by(!!!syms(cols), APPT_STATUS) %>%
        dplyr::summarise(Total = n()) %>% 
        collect() %>%
        #add_column(!!breakdown_filters := "Total") %>%
        pivot_wider(names_from = APPT_STATUS, values_from = Total) %>%
        replace(is.na(.), 0) %>% 
        group_by(!!!syms(cols)) %>%
        mutate(Total = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%")) %>%
        select(-`No Show`,-Arrived) %>% 
        #mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))%>%
        #pivot_wider(names_from = APPT_MONTH_YEAR, values_from = percentage)%>%
        ungroup() %>%
        select(all_of(cols),  everything())
      
      tot_all <- data %>%
        group_by(!!!syms(tot_cols), APPT_STATUS) %>%
        dplyr::summarise(Total = n()) %>% 
        collect() %>%
        add_column(!!breakdown_filters := "Total") %>%
        pivot_wider(names_from = APPT_STATUS, values_from = Total) %>%
        replace(is.na(.), 0) %>% 
        group_by(!!!syms(cols)) %>%
        mutate(Total = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%")) %>%
        select(-`No Show`,-Arrived) %>% 
        # mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))%>%
        # pivot_wider(names_from = APPT_MONTH_YEAR, values_from = percentage)%>%
        ungroup() %>%
        select(all_of(cols),  everything())
      
      total <- bind_rows(tot_over_time, tot_all)
      
      noShow_perc <- left_join(noShow_perc, total)
      
      

      
      
      
      noShow_perc$Total_YN <- ifelse(noShow_perc[[all_of(breakdown_filters)]] == "Total", 1,0)
      noShow_perc <- setnames(noShow_perc, old = cols, new = cols_name)
      
      
      month_names <- colnames(noShow_perc[,!(names(noShow_perc) %in% c(cols_name, "Total", "Total_YN"))])
      
      month_names_new <- format(as.Date(paste0(month_names, "-01"), format = "%b %Y-%d"), "%Y-%m")
      noShow_perc <- setnames(noShow_perc, old = month_names, new = month_names_new)
      
      noShow_perc
      
    #}
    
  })
  
  output[["new_no_show_rate_monthly"]] <- renderDT({
    num_of_cols <- length(no_Show_percentage())
    col_dissappear <- which(names(no_Show_percentage()) %in% c("Total_YN"))
    
    test_data <<- no_Show_percentage()
    
    dtable <-   datatable(no_Show_percentage(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            #htmltools::em('Median New Patient Wait Time = median wait time of scheduled new patients within the month')
                            
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly No Show Rate Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly No Show Rate Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(no_Show_percentage())[3] == "Provider", 4, 3)
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
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%') %>%
      formatStyle(columns = c("Total"), fontWeight = 'bold')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  wite_time_14days <- reactive({
    ## Percent of New Patients Scheduled Within 14 Days
    data <- dataAll_access()
    
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    data_test <<- data
    compare_filters_test <- compare_filters
    breakdown_filters_test <<- breakdown_filters
    
    if(breakdown_filters == "NEW_PT3") {
      breakdown_filters <- "NEW_PT2"
    }
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT2"){
      name_2 <- "New vs. Established"
      breakdown_filters <- "NEW_PT2"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    
    
    if(breakdown_filters == "NEW_PT2"){
      validate(
        need(input$breakdown_filters != "NEW_PT3", ("Percent of New Patients Scheduled Within 14 Days cannot be viewed by New vs Established Patients"))
      )
      
    } else{
      
      #data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
      
      
      waitTime_total <- data %>%
        filter(WAIT_TIME >= 0) %>%
        filter(NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(total_all = n()) %>%
        collect()
      
      waitTime_within_14_days <- data %>%
        filter(WAIT_TIME >= 0, 
               WAIT_TIME < 14.0001, 
               NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(total = n()) %>%
        collect()
      
      join_data <- inner_join(waitTime_total, waitTime_within_14_days)
      
      join_data[is.na(join_data)] <- 0
      
      percent_within_14_days <- join_data %>% 
        group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
        summarise(percent = paste0(round((total/total_all),2)*100, "%"))
      
      
      percent_within_14_days$APPT_MADE_MONTH_YEAR <- as.yearmon(percent_within_14_days$APPT_MADE_MONTH_YEAR, "%Y-%m")
      
      percent_within_14_days <- percent_within_14_days %>% 
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR, values_from = percent) %>%
        ungroup()
      
      
      
      percent_within_14_days <- percent_within_14_days %>% select(all_of(cols), everything())
      
      waitTime_tot <- data %>%
        filter(WAIT_TIME >= 0) %>%
        filter(NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(total_all = n()) %>%
        collect()
      
      waitTime_within_14_days_tot <- data %>%
        filter(WAIT_TIME >= 0, 
               WAIT_TIME < 14.0001, 
               NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols), APPT_MADE_MONTH_YEAR) %>%
        dplyr::summarise(total = n()) %>%
        collect()
      
      tot_join_data <- inner_join(waitTime_tot, waitTime_within_14_days_tot)
      
      tot_join_data[is.na(tot_join_data)] <- 0
      
      tot <- tot_join_data %>% 
        group_by(!!!syms(tot_cols), APPT_MADE_MONTH_YEAR) %>%
        summarise(percent = paste0(round((total/total_all),2)*100, "%"))%>%
        add_column(!!breakdown_filters := "Total") 
      
      
      tot$APPT_MADE_MONTH_YEAR <- as.yearmon(tot$APPT_MADE_MONTH_YEAR, "%Y-%m")
      
      tot <- tot %>% 
        pivot_wider(names_from = APPT_MADE_MONTH_YEAR, values_from = percent) %>%
        ungroup()
      
      
      percent_within_14_days <- full_join(percent_within_14_days, tot)
      
      
      waitTime_tot <- data %>%
        filter(WAIT_TIME >= 0) %>%
        filter(NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols)) %>%
        dplyr::summarise(total_all = n()) %>%
        collect()
      
      waitTime_within_14_days_tot <- data %>%
        filter(WAIT_TIME >= 0, 
               WAIT_TIME < 14.0001, 
               NEW_PT2 == "NEW") %>%
        group_by(!!!syms(cols)) %>%
        dplyr::summarise(total = n()) %>%
        collect()
      
      tot_join_data <- inner_join(waitTime_tot, waitTime_within_14_days_tot)
      
      tot_join_data[is.na(tot_join_data)] <- 0
      
      tot <- tot_join_data %>% 
        group_by(!!!syms(cols)) %>%
        summarise(Total = paste0(round((total/total_all),2)*100, "%"))
      
      
      waitTime_tot_overtime <- data %>%
        filter(WAIT_TIME >= 0) %>%
        filter(NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols)) %>%
        dplyr::summarise(total_all = n()) %>%
        collect()
      
      waitTime_within_14_days_tot_overtime <- data %>%
        filter(WAIT_TIME >= 0, 
               WAIT_TIME < 14.0001, 
               NEW_PT2 == "NEW") %>%
        group_by(!!!syms(tot_cols)) %>%
        dplyr::summarise(total = n()) %>%
        collect()
      
      tot_join_data <- inner_join(waitTime_tot_overtime, waitTime_within_14_days_tot_overtime)
      
      tot_join_data[is.na(tot_join_data)] <- 0
      
      tot_overtime <- tot_join_data %>% 
        group_by(!!!syms(tot_cols)) %>%
        summarise(Total = paste0(round((total/total_all),2)*100, "%")) %>%
        add_column(!!breakdown_filters := "Total")
      
      total <- bind_rows(tot, tot_overtime)
      
      
      percent_within_14_days <- left_join(percent_within_14_days, total)
      
      
      
      
      percent_within_14_days$Total_YN <- ifelse(percent_within_14_days[[all_of(breakdown_filters)]] == "Total", 1,0)
      
      percent_within_14_days <- setnames(percent_within_14_days, old = cols, new = cols_name)
      
      month_names <- colnames(percent_within_14_days[,!(names(percent_within_14_days) %in% c(cols_name, "Total", "Total_YN"))])
      
      month_names_new <- format(as.Date(paste0(month_names, "-01"), format = "%b %Y-%d"), "%Y-%m")
      percent_within_14_days <- setnames(percent_within_14_days, old = month_names, new = month_names_new)
      
      percent_within_14_days
    }
    
  })
  
  
  output[["new_patient_within_14days"]] <- renderDT({
    num_of_cols <- length(wite_time_14days())
    col_dissappear <- which(names(wite_time_14days()) %in% c("Total_YN"))
    
    dtable <-   datatable(wite_time_14days(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            
                          ),
                          options = list(
                            scrollX = TRUE,
                            columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
                            list(pageLength = 20, scrollY = "400px"),
                            dom = 'Bfrtip',
                            #buttons = c('csv','excel'),
                            buttons = list(
                              list(extend = 'csv', filename = 'Monthly Percent of New Patients Scheduled Within 14 Days Comaprsion'),
                              list(extend = 'excel', filename = 'Monthly Percent of New Patients Scheduled Within 14 Days Comaprsion')
                            ),
                            sDom  = '<"top">lrt<"bottom">ip',
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#dddedd', 'color': 'black'});",
                              "}"),
                            fixedColumns = list(leftColumns =
                                                  ifelse(colnames(wite_time_14days())[3] == "Provider", 4, 3)
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
      formatStyle(columns = c(1:num_of_cols), fontSize = '115%') %>%
      formatStyle(columns = c("Total"), fontWeight = 'bold')
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  am_pm_conparison_reactive <- reactive({
    data <- dataArrived()
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    data_test <<- dataArrived()
    
    compare_filters_test <<- input$compare_filters
    breakdown_filters_test <- input$breakdown_filters
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    am_pm_colname <- "AM_PM"
    am_pm_colname_updated <- "Session"
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c(name_1,name_2, am_pm_colname_updated)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c("Specialty",name_1,name_2, am_pm_colname_updated)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c("Specialty","Department",name_1,name_2, am_pm_colname_updated)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    
    
    am_pm <- data %>% group_by(!!!syms(cols),APPT_DATE_YEAR, APPT_MONTH_YEAR)  %>% 
      summarise(total = n()) %>%
      group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
      summarise(avg = ceiling(sum(total, na.rm = T)/n())) %>% collect() %>%
      pivot_wider(names_from = APPT_MONTH_YEAR,
                  values_from = avg,
                  values_fill = 0)
    
    tot <- am_pm %>% group_by(across(all_of(tot_cols))) %>%
      summarise_at(vars(-!!(c(breakdown_filters, am_pm_colname))), sum)  %>%
      add_column(!!am_pm_colname := "Total") %>%
      relocate(all_of(am_pm_colname), .after = !!compare_filters)
    
    
    am_pm <- full_join(am_pm, tot) 
    am_pm <- am_pm %>% arrange(across(all_of(tot_cols)))
    
    am_pm$Total <- rowSums(am_pm[setdiff(names(am_pm),cols)])
    
    # am_pm <- am_pm %>% group_by(!!!syms(cols)) %>% arrange(`AM_PM`)
    
    if(breakdown_filters == "NEW_PT3") {
      am_pm <- am_pm %>% mutate(NEW_PT3 = ifelse(NEW_PT3 == "NEW", "New", "Established"))
    }
    
    
    am_pm <- setnames(am_pm, old = cols, new = cols_name)
    
    am_pm$Total_YN <- ifelse(am_pm[["Session"]] == "Total", 1,0)
    
    
    months_df <- am_pm[,!(names(am_pm) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))

    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index,(length(am_pm)-1):length(am_pm))

    
    am_pm <- am_pm[index]
    

    am_pm <- am_pm %>% arrange(across(all_of(cols_name)))
    
    am_pm
    
  })
  
  output[["am_pm_breakdown_comparison"]] <- renderDT({
    

    col_dissappear <- which(names(am_pm_conparison_reactive()) %in% c("Total_YN"))
    num_of_cols <- length(am_pm_conparison_reactive()) 
    
    dtable <-   datatable(am_pm_conparison_reactive(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('*PM appointments occur after 12')
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
                                                  ifelse(colnames(am_pm_conparison_reactive())[3] == "Provider", 4, 3)
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
    
  })
  
  
  am_pm_conparison_month_reactive <- reactive({
    data <- dataArrived()
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    data_test <<- dataArrived()
    
    compare_filters_test <<- input$compare_filters
    breakdown_filters_test <- input$breakdown_filters
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT3"){
      name_2 <- "New vs. Established"
    }
    
    am_pm_colname <- "AM_PM"
    am_pm_colname_updated <- "Session"
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c(name_1,name_2, am_pm_colname_updated)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c("Specialty",name_1,name_2, am_pm_colname_updated)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters,breakdown_filters, am_pm_colname)
      cols_name <- c("Specialty","Department",name_1,name_2, am_pm_colname_updated)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    
    
    am_pm <- data %>% group_by(!!!syms(cols),APPT_MONTH_YEAR) %>%
      summarise(total = n()) %>% collect() %>%
      pivot_wider(names_from = APPT_MONTH_YEAR,
                  values_from = total,
                  values_fill = 0) 
    
    
    am_pm[is.na(am_pm)] <- 0
    
    #### Sum all cloumns with months to get the total for the month
    # tot <- data %>% group_by(APPT_MONTH_YEAR) %>%
    #   summarise(total = n()) %>% collect() %>%
    #   pivot_wider(names_from = APPT_MONTH_YEAR,
    #               values_from = total,
    #               values_fill = 0) 
    # 
    
    tot <- am_pm %>% group_by(across(all_of(tot_cols))) %>%
      summarise_at(vars(-!!(c(breakdown_filters, am_pm_colname))), sum)  %>%
      add_column(!!am_pm_colname := "Total") %>%
      relocate(all_of(am_pm_colname), .after = !!compare_filters)
    
    
    
    am_pm <- full_join(am_pm, tot) 
    am_pm <- am_pm %>% arrange(across(all_of(tot_cols)))
    
    am_pm$Total <- rowSums(am_pm[setdiff(names(am_pm),cols)])
    
    # am_pm <- am_pm %>% group_by(!!!syms(cols)) %>% arrange(`AM_PM`)
    
    if(breakdown_filters == "NEW_PT3") {
      am_pm <- am_pm %>% mutate(NEW_PT3 = ifelse(NEW_PT3 == "NEW", "New", "Established"))
    }
    
    
    am_pm <- setnames(am_pm, old = cols, new = cols_name)
    
    am_pm$Total_YN <- ifelse(am_pm[["Session"]] == "Total", 1,0)
    
    
    months_df <- am_pm[,!(names(am_pm) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%Y-%m"))
    
    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index,(length(am_pm)-1):length(am_pm))
    
    
    am_pm <- am_pm[index]
    
    # am_pm_order <- c("AM", "PM")
    # 
    # am_pm <- am_pm %>% mutate(`AM/PM` = factor(`AM/PM`, levels = am_pm_order))
    
    
    am_pm <- am_pm %>% arrange(across(all_of(cols_name)))
    
    am_pm
    
  })
  
  
  output[["am_pm_breakdown_comparison_month"]] <- renderDT({
    
    
    col_dissappear <- which(names(am_pm_conparison_month_reactive()) %in% c("Total_YN"))
    num_of_cols <- length(am_pm_conparison_month_reactive()) 
    
    dtable <-   datatable(am_pm_conparison_month_reactive(), 
                          class = 'cell-border stripe',
                          rownames = FALSE,
                          extensions = c('Buttons','Scroller'),
                          caption = htmltools::tags$caption(
                            style = 'caption-side: bottom; text-align: left;',
                            htmltools::em('*PM appointments occur after 12')
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
                                                  ifelse(colnames(am_pm_conparison_month_reactive())[3] == "Provider", 4, 3)
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
    
  })
  
  
  booked_and_filled_month <- reactive({
    data <- dataAllSlot_comp() #%>% select(APPT_MONTH_YEAR, VISIT_METHOD, CAMPUS_SPECIALTY, DEPARTMENT_NAME, APPT_DATE_YEAR, PROVIDER, APPT_DTTM, AVAILABLE_HOURS, BOOKED_HOURS, ARRIVED_HOURS) %>% collect()
    # data <- slot.data.subset[all.slot.rows,] %>% filter(Campus.Specialty %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Specialty"
    # breakdown_filters <- "Visit.Method"
    
    
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT_NAME,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         Appt.DTTM = APPT_DTTM,
    #                         `Available Hours` = AVAILABLE_HOURS, 
    #                         `Booked Hours` = BOOKED_HOURS, 
    #                         `Arrived Hours` = ARRIVED_HOURS)
    # 
    # data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT2"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME",breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME")
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- "Provider"
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT_NAME",compare_filters)
    }
    
    
    
    slot_metrics <- c("Available Hours", "Booked Hours", "Filled Hours", "Booked Rate (%)", "Filled Rate (%)")
    
    if(breakdown_filters != "VISIT_METHOD"){
      
      
      
      validate(
        need(input$breakdown_filters != "APPT_TYPE", ("Slot data can only be viewed by Visit Method")),
        need(input$breakdown_filters != "NEW_PT2", ("Slot data can only be viewed by Visit Method"))
      )
      
      
      
      
    }else{
      
      #### Group data by inputs and month, sum all available, booked, and filled hours for the whole month
      slot <- data %>% 
        group_by(!!!syms(cols), APPT_MONTH_YEAR)%>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm=TRUE), 0),
                         `Booked Hours` = round(sum(BOOKED_HOURS, na.rm=TRUE), 0),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm=TRUE), 0)) 
      
      slot[is.na(slot)] <- 0
      
      #### Add booked and filled rate columns( respective metric/Available Hours) multiply by 100 pivot the data so that the months are now columns
      #### and the metric oclumns become rows
      slot <- slot %>%
        filter(`Available Hours` > 0) %>%
        filter(!is.na(`Available Hours`)) %>%
        mutate(`Booked Rate (%)` = paste0(round((`Booked Hours`/`Available Hours`)*100), "%"),
               `Filled Rate (%)` = paste0(round((`Filled Hours`/`Available Hours`)*100), "%")) %>%
        collect() %>% 
        gather(variable, value, !!slot_metrics) %>% 
        spread(APPT_MONTH_YEAR, value) %>%
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
    print(Sys.time())
    
    data <- dataAllSlot_comp()
    
    print(Sys.time())
    #data <- slot.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% c("Allergy", "Cardiology"))
    # compare_filters <- "Specialty"
    # breakdown_filters <- "Visit.Method"
    
    #data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    
    compare_filters <- input$compare_filters
    breakdown_filters <- input$breakdown_filters
    
    
    
    if(breakdown_filters == "VISIT_METHOD"){
      name_2 <- "Visit Method"
    }
    if(breakdown_filters == "APPT_TYPE"){
      name_2 <- "Vist Type"
    }
    if(breakdown_filters == "NEW_PT2"){
      name_2 <- "Patient Status"
    }
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters,breakdown_filters)
      cols_name <- c(name_1,name_2)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME",breakdown_filters)
      cols_name <- c("Specialty",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME")
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- "Provider"
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT_NAME",compare_filters,breakdown_filters)
      cols_name <- c("Specialty","Department",name_1,name_2)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT_NAME",compare_filters)
    }
    
    
    
    slot_metrics <- c("Available Hours", "Booked Hours", "Filled Hours", "Booked Rate (%)", "Filled Rate (%)")
    
    if(breakdown_filters != "VISIT_METHOD"){
      
      validate(
        need(input$breakdown_filters != "APPT_TYPE", ("Slot data can only be viewed by Visit Method")),
        need(input$breakdown_filters != "NEW_PT2", ("Slot data can only be viewed by Visit Method"))
      )
      
      
      
    }else{
      print(Sys.time())
      
      #### Group data by inputs, month, and date sum all available, booked, and filled hours for the whole month
      slot <- data %>% 
        group_by(!!!syms(cols), APPT_MONTH_YEAR, APPT_DATE_YEAR)%>%
        dplyr::summarise(`Available Hours` = round(sum(AVAILABLE_HOURS, na.rm=TRUE), 0),
                         `Booked Hours` = round(sum(BOOKED_HOURS , na.rm=TRUE), 0),
                         `Filled Hours` = round(sum(ARRIVED_HOURS, na.rm=TRUE), 0) 
        ) 

      slot[is.na(slot)] <- 0
      print("test1")
      print(Sys.time())
      
      
      ### Group by the month and get the monthl avaerage for each month by summing the column and dividing by number of rows within the month
      ### then gather all created columns make them categories for the STatus column
      ### Spread data to make monhts in Appt.Month into columns
      slot <- slot %>%  group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
        filter(`Available Hours` > 0) %>%
        filter(!is.na(`Available Hours`)) %>%
        summarise(
          `Booked Rate (%)` = paste0(round(sum(`Booked Hours`)/sum(`Available Hours`)*100),"%"),
          `Filled Rate (%)` = paste0(round(sum(`Filled Hours`)/sum(`Available Hours`)*100),"%"),
          `Available Hours` = ceiling(sum(`Available Hours`)/n()),
          `Booked Hours` = ceiling(sum(`Booked Hours`)/n()),
          `Filled Hours` = ceiling(sum(`Filled Hours`)/n())
        ) %>% collect() %>% 
        gather(variable, value, !!slot_metrics) %>%
        spread(APPT_MONTH_YEAR, value) %>%
        rename(Status = variable)
      
      print("test2")
      print(Sys.time())
      
      
      #### Add booked and filled rate columns( respective metric/Available Hours) multiply by 100 pivot the data so that the months are now columns
      #### and the metric oclumns become rows
      
      
      #### Order that the status appears
      level.order <- c("Available Hours", "Booked Hours","Filled Hours","Booked Rate (%)","Filled Rate (%)")
      
      #slot <- group_by(!!!syms(cols)) %>% arrange(level.order, .by_group = T)
      #slot <- arrange(slot, level.order,  group_by = !!!syms(cols) )
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
  
  
  
  
### optimization 
  
  
  output$practiceName_opt_comp <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  output$practiceName_appt_length_breakdown <- renderText({
    paste0("Based on data from ", input$dateRange[1]," to ", input$dateRange[2], 
           " for ", paste(sort(input$selectedCampus), collapse = ', '))
  })
  
  dataArrived_test_schedule <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_schedule(arrived.data.rows.schedule,
                   input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                   input$selectedVisitMethod, input$selectedPRCName, 
                   input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  dataArrived_access_schedule <- eventReactive(list(input$update_filters),{
    
    validate(
      need(input$selectedCampus != "", "Please select a Campus"),
      need(input$selectedSpecialty != "", "Please select a Specialty"),
      need(input$selectedDepartment != "", "Please select a Department"),
      need(input$selectedResource != "", "Please select a Resource"),
      need(input$selectedProvider != "", "Please select a Provider"),
      need(input$selectedVisitMethod != "", "Please select a Visit Method"),
      need(input$selectedPRCName != "", "Please select a Visit Type")
    )
    groupByFilters_access_schedule(arrived.data.rows.schedule.access,
                          input$selectedCampus, input$selectedSpecialty, input$selectedDepartment, input$selectedResource, input$selectedProvider,
                          input$selectedVisitMethod, input$selectedPRCName, 
                          input$dateRange[1], input$dateRange[2], input$daysOfWeek, input$excludeHolidays)
    
  })
  
  schedule_opt <- reactive({
    
    time_1 <- Sys.time()
    # Volume Data

    # data <- kpi.all.data %>% filter(Campus.Specialty=="Cardiology" & Appt.Status == "Arrived") %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    # compare_filters <- "Provider"
    
    data <- dataArrived_summary()
    
    #data <- arrived.data.rows.summary %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
    #data <- data %>% select(APPT_MONTH_YEAR, VISIT_METHOD, APPT_TYPE, NEW_PT2, CAMPUS_SPECIALTY, DEPARTMENT, APPT_DATE_YEAR, PROVIDER, APPT_DTTM, APPT_MADE_DTTM) %>% collect()
    
   
    # data <- data %>% rename(Appt.MonthYear = APPT_MONTH_YEAR,
    #                         Visit.Method = VISIT_METHOD,
    #                         Appt.Type = APPT_TYPE,
    #                         Campus.Specialty = CAMPUS_SPECIALTY,
    #                         Department = DEPARTMENT,
    #                         Appt.DateYear = APPT_DATE_YEAR,
    #                         Provider = PROVIDER,
    #                         Appt.DTTM = APPT_DTTM,
    #                         Appt.Made.DTTM = APPT_MADE_DTTM,
    #                         New.PT3 = NEW_PT2)
    
    
    #data  <- data %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    compare_filters <- input$compare_filters_opt
    
    
    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- c(compare_filters)
      cols_name <- c(name_1)
      tot_cols <- c(compare_filters)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY",compare_filters)    
      cols_name <- c("Specialty",name_1)
      tot_cols <- c("CAMPUS_SPECIALTY",compare_filters)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- compare_filters
      cols <- c("CAMPUS_SPECIALTY","DEPARTMENT",compare_filters)
      cols_name <- c("Specialty","DEPARTMENT", name_1)
      tot_cols <- c("CAMPUS_SPECIALTY", "DEPARTMENT",compare_filters)
    }
    
    #### group Data by inputs and Month and data get the total for each day then group by the Month in order to sum all the vistis within the month and 
    #### divide it by the number of days within the month in the data, then we make a wider data sets with the months values made into a column
    
    print("2")
    volume <- data %>% group_by(!!!syms(cols),APPT_DATE_YEAR, APPT_MONTH_YEAR) %>%
      summarise(total = SUM(TOTAL_APPTS)) %>%  # it was n()
      group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
      summarise(avg = ceiling(sum(total)/n())) %>%
      collect() %>%
      mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))
    
   
     year <- max(year(volume$APPT_MONTH_YEAR))-1
    
     volume <- volume %>%
      pivot_wider(names_from = APPT_MONTH_YEAR,
                  values_from = avg,
                  values_fill = 0) 
    
    # estimate the total 
    volume <- volume %>% 
      group_by(!!!syms(cols))%>%
      mutate(Total = ceiling(rowMeans(across(where(is.numeric)), na.rm=TRUE)))
    
    
    
    # campus <- input$selectedCampus 
    # specialty <- input$selectedSpecialty
    # department <- input$selectedDepartment
    # resources <- input$selectedResource 
    # provider <- input$selectedProvider
    # 
    # 
    # if(compare_filters == "CAMPUS_SPECIALTY"){
    #   volume_dynamics <- arrived.data.rows.summary %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty ) 
    #   
    # }
    # if(compare_filters == "DEPARTMENT"){
    #   volume_dynamics <- arrived.data.rows.summary %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department )
    # }
    # if(compare_filters == "PROVIDER"){
    #   volume_dynamics <- arrived.data.rows.summary %>%
    #     filter(CAMPUS %in% campus  &
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department &
    #              RESOURCES %in% resources &
    #              PROVIDER %in% provider)
    # }
    # 
    # 
    # volume_dynamics <- volume_dynamics %>% 
    #   group_by(!!!syms(cols),APPT_DATE_YEAR, APPT_MONTH_YEAR) %>%
    #   summarise(total = SUM(TOTAL_APPTS)) %>% 
    #   collect()%>%
    #   mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))%>%
    #   filter(year(APPT_MONTH_YEAR) %in% year)  %>%
    #   group_by(!!!syms(cols)) %>%
    #   summarise(`Dynamic Target` = ceiling(sum(total)/n()))
    # 
    # volume <- left_join(volume, volume_dynamics, by= cols )
    
    volume[is.na(volume)] <- 0
    
    volume$Metrics <- "Average Daily Volume"
    #volume <- volume %>% select(all_of(cols), Metrics, `Dynamic Target`, everything(), Total)
    volume <- volume %>% select(all_of(cols), Metrics, everything(), Total)
    

    data_access <- dataArrived_access_npr()
    
    # data_access <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
    newpatients.ratio <- data_access %>% group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR, NEW_PT3) %>%
      summarise(total = SUM(TOTAL_APPTS)) %>% collect()%>%  # it was n()
      mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))%>%
      drop_na() %>%
      spread(NEW_PT3, total)
    
    newpatients.ratio[is.na(newpatients.ratio)] <- 0
    
   
    
    col_names <- c(cols, "APPT_MADE_MONTH_YEAR",  "NEW", "ESTABLISHED")
    
    col_index <- which(!(col_names %in% colnames(newpatients.ratio)))
    
    col_to_add <- col_names[col_index]
    
    if (length(col_to_add)>0){
      
      newpatients.ratio[[col_to_add]] <- NA
    }
    
    
    ### Calculate new patient ratio by breakdown
    newpatients.ratio <- newpatients.ratio %>% group_by(APPT_MADE_MONTH_YEAR) %>%
      mutate(ratio = paste0(round(`NEW`/(`NEW`+`ESTABLISHED`), 2)*100, "%")) 
    
   
    
    #newpatients.ratio <- newpatients.ratio %>% group_by(Appt.MonthYear) %>%
     # mutate(ratio = paste0(round(`TRUE`/(sum(`TRUE`, na.rm = TRUE) + sum(`FALSE`, na.rm = TRUE)),2)*100, "%"))  
    
   # `Booked Rate (%)` = paste0(round(sum(`Booked Hours`)/sum(`Available Hours`)*100),"%"),
    
    year <- max(year(newpatients.ratio$APPT_MADE_MONTH_YEAR))-1
    
    drop <- c("ESTABLISHED","NEW", "<NA>")
    newpatients.ratio = newpatients.ratio[,!(names(newpatients.ratio) %in% drop)]
    newpatients.ratio <- newpatients.ratio %>% spread(APPT_MADE_MONTH_YEAR, ratio)
    
    
    total <- data_access %>% group_by(!!!syms(cols), NEW_PT3) %>%
      summarise(total = SUM(TOTAL_APPTS)) %>%
      collect()%>% 
      drop_na() %>%
      spread(NEW_PT3, total) %>%
      replace(is.na(.), 0) 

    col_index <- which(!(c(cols, "NEW", "ESTABLISHED") %in% colnames(total)))
    col_to_add <- col_names[col_index]
    
    if (length(col_to_add)>0){
      total[[col_to_add]] <- 0
    }
    
    
    total <- total %>%
      mutate(Total = paste0(round(`NEW`/(`NEW`+`ESTABLISHED`), 2)*100, "%"))%>%
      select(-c("NEW", "ESTABLISHED"))
 
    newpatients.ratio <- left_join(newpatients.ratio, total, by = cols)
    

    # if(compare_filters == "CAMPUS_SPECIALTY"){
    #   
    #   newpatients.ratio.dynamcis <- arrived.data.rows.npr %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty ) 
    #   
    # }
    # if(compare_filters == "DEPARTMENT"){
    #   newpatients.ratio.dynamcis <- arrived.data.rows.npr %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department )
    # }
    # if(compare_filters == "PROVIDER"){
    #   newpatients.ratio.dynamcis <- arrived.data.rows.npr %>%
    #     filter(CAMPUS %in% campus  &
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department &
    #              RESOURCES %in% resources &
    #              PROVIDER %in% provider)
    # }
    # 
    # 
    # 
    # # data_access <- arrived.data.rows.npr %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy"  )
    # ### Get total of new patients to arrive per month and spread that to TRUE and FALSE columns
    # newpatients.ratio.dynamcis <- newpatients.ratio.dynamcis %>% 
    #   group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR, NEW_PT3) %>%
    #   summarise(total = SUM(TOTAL_APPTS)) %>% 
    #   collect()%>%
    #   mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))%>%
    #   filter(year(APPT_MADE_MONTH_YEAR) %in% year) %>%
    #   drop_na() %>%
    #   spread(NEW_PT3, total)%>%
    #   replace(is.na(.), 0)
    #   
    # 
    # col_index <- which(!(col_names %in% colnames(newpatients.ratio.dynamcis)))
    # col_to_add <- col_names[col_index]
    # 
    # if (length(col_to_add)>0){
    #   
    #   newpatients.ratio.dynamcis[[col_to_add]] <- NA
    # }
    
    
    
    ### Calculate new patient ratio by breakdown
    # newpatients.ratio.dynamcis <- newpatients.ratio.dynamcis %>%
    #   group_by(!!!syms(cols)) %>%
    #   summarise(`ESTABLISHED` =sum(`ESTABLISHED`, na.rm = T), 
    #             `NEW` = sum(`NEW`, na.rm = T))%>%
    #   mutate(`Dynamic Target` = paste0(round(`NEW`/(`NEW`+`ESTABLISHED`), 2)*100, "%")) %>%
    #   select(c(cols, "Dynamic Target" )) 
    # 
    # newpatients.ratio <- left_join(newpatients.ratio, newpatients.ratio.dynamcis, by = cols )
    
    
    newpatients.ratio[is.na(newpatients.ratio)] <- "0%"
    #newpatients.ratio[is.na(newpatients.ratio)] <- 0
    
    
    newpatients.ratio$Metrics <- "New Patient Ratio"
    #newpatients.ratio <- newpatients.ratio %>% select(cols, Metrics, `Dynamic Target`, everything(), Total)
    newpatients.ratio <- newpatients.ratio %>% select(cols, Metrics, everything(), Total)
    
    #dataAll <- dataAll()
    #dataAll <- dataAll() %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    compare_filters <- input$compare_filters_opt
    
    #dataAll <- kpi.all.data[all.data.rows,] %>% filter(Campus.Specialty=="Allergy") %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))

    ### Calculate wait time which is the difference between when the patient made the appt and the scheduled date
    
    #waitTime <- data %>% mutate(wait.time= as.numeric(round(difftime(APPT_DTTM, APPT_MADE_DTTM,  units = "days"),2)))
    #waitTime <- dataAll %>% mutate(wait.time= as.numeric(round(difftime(Appt.DTTM, Appt.Made.DTTM,  units = "days"),2)))
    #waitTime <- waitTime  %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    #### Filter out wait time that equals 0 and calculate the median wait time for NEw and est patients by month
    data_access <-  dataAll_access()
    #data_access <- arrived.data.rows %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")
    
   
    waitTime <- data_access %>%
      filter(WAIT_TIME >= 0, NEW_PT3 == "NEW") %>% 
      group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>% 
      dplyr::summarise(medWaitTime = ceiling(median(WAIT_TIME))) %>%
      #filter(NEW_PT3 %in% c("NEW","ESTABLISHED")) %>% 
      collect() %>%
      mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))
    
    
    #### Change the TRUE and FALSE to New and Established and filter our new patients and drop the New.PT3 column
    # waitTime$NEW_PT3 <- ifelse(waitTime$NEW_PT3 == "NEW", "New","Established")
    # waitTime <- waitTime %>% filter(NEW_PT3 == "New") 
    # drop <- c("NEW_PT3")
    # waitTime = waitTime[,!(names(waitTime) %in% drop)]
    
    #### Pivot the data so the months are in the columns and shows only new patient median time   
    # waitTime <- waitTime %>%
    #   mutate(`medWaitTime` = case_when(
    #     `medWaitTime` <= 14 ~  cell_spec(paste0(medWaitTime," Days") , color = "green"),
    #     `medWaitTime` > 14 ~  cell_spec(paste0(medWaitTime," Days") , color = "red")
    #   )) %>%
    #  
    waitTime <- waitTime %>%
      pivot_wider(names_from = APPT_MADE_MONTH_YEAR,
                  values_from = medWaitTime)
    
    total <- data_access %>%
      filter(WAIT_TIME >= 0, NEW_PT3 == "NEW") %>% 
      group_by(!!!syms(cols)) %>% 
      dplyr::summarise(Total = ceiling(median(WAIT_TIME))) %>%
      collect() 
    
    waitTime <- left_join(waitTime, total, by = cols)
    
    
    
    # if(compare_filters == "CAMPUS_SPECIALTY"){
    #   
    #   waitTime.dynamic <- arrived.data.rows %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty ) 
    #   
    # }
    # if(compare_filters == "DEPARTMENT"){
    #   waitTime.dynamic <- arrived.data.rows %>%
    #     filter(CAMPUS %in% campus  & 
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department )
    # }
    # if(compare_filters == "PROVIDER"){
    #   waitTime.dynamic <- arrived.data.rows %>%
    #     filter(CAMPUS %in% campus  &
    #              CAMPUS_SPECIALTY %in% specialty &
    #              DEPARTMENT %in% department &
    #              RESOURCES %in% resources &
    #              PROVIDER %in% provider)
    # }
    # 
    # 
    # waitTime.dynamic <- waitTime.dynamic %>%
    #   filter(WAIT_TIME >= 0, NEW_PT3 == "NEW") %>% 
    #   collect() %>%
    #   mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))%>%
    #   filter(year(APPT_MADE_MONTH_YEAR) %in% year) %>%
    #   group_by(!!!syms(cols)) %>% 
    #   dplyr::summarise(`Dynamic Target` = ceiling(0.90* median(WAIT_TIME))) 
    # 
    # 
    # waitTime <- left_join(waitTime, waitTime.dynamic, by= cols)
 
    waitTime$Metrics <- "New Patient Wait Time (Days)"
    #waitTime <- waitTime %>% select(cols, Metrics, `Dynamic Target`, everything(), Total)
    waitTime <- waitTime %>% select(cols, Metrics, everything(), Total)
   
    
    
   
    # Process slot data
    #data_slot <- slot.data.subset %>% filter(Campus.Specialty== "Cardiology") %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    # slot <- dataAllSlot_comp() %>% rename(DEPARTMENT= DEPARTMENT_NAME)
    # 
    # #slot <- data_slot_test
    # #data_slot <- dataAllSlot_comp() %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    # #data_slot <-  slot.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology")
    # compare_filters <- input$compare_filters_opt
    # 
    # 
    # 
    # #data_slot <- slot.data.subset[all.slot.rows,] %>% filter(Campus.Specialty=="Allergy") %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    # #data_slot <- slot.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Cardiology") %>% rename(DEPARTMENT= DEPARTMENT_NAME)
    # 
    # 
    # #slot <- data_slot %>% mutate(Appt.MonthYear = as.yearmon(Appt.MonthYear, "%Y-%m"))
    # slot <- slot %>% group_by(!!!syms(cols), APPT_MONTH_YEAR, APPT_DATE_YEAR)%>%
    #   dplyr::summarise(AVAILABLE_HOURS = round(sum(AVAILABLE_HOURS, na.rm=TRUE),1),
    #                    BOOKED_HOURS = round(sum(BOOKED_HOURS, na.rm=TRUE), 1),
    #                    FILLED_HOURS = round(sum(ARRIVED_HOURS, na.rm=TRUE), 1)) #%>% collect()
    #                     
    # print("9")
    # suppressWarnings({
    #   ### Group by the month and get the monthl avaerage for each month by summing the column and dividing by number of rows within the month
    #   ### then gather all created columns make them categories for the STatus column
    #   ### Spread data to make monhts in Appt.Month into columns
    #   slot <- slot %>%  group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
    #     filter(AVAILABLE_HOURS > 0) %>%
    #     filter(!is.na(AVAILABLE_HOURS)) %>%
    #     summarise(
    #       `Booked Rate` = round(sum(BOOKED_HOURS, na.rm = T)/sum(AVAILABLE_HOURS, na.rm = T),2 ),
    #       `Filled Rate` = round(sum(FILLED_HOURS, na.rm = T)/sum(AVAILABLE_HOURS, na.rm = T),2 ),
    #     ) %>% collect() %>%
    #     mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m"))
    # })
    # slot[is.na(slot)] <- 0
    # slot_metrics <- c( "Booked Rate", "Filled Rate")
    # 
    # print("9.1")
    #   
    # slot <- slot %>%
    #   mutate_if(is.numeric,function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) %>%
    #   mutate(`Booked Rate` = formattable::percent(`Booked Rate`, digits = 0),
    #          `Filled Rate` = formattable::percent(`Filled Rate`, digits = 0)) %>%
    # mutate(`Booked Rate` = case_when(
    #   `Booked Rate` >= 0.95 ~ cell_spec(`Booked Rate`, color = "green"),
    #   `Booked Rate` < 0.95 ~ cell_spec(`Booked Rate`, color = "red")
    # )) %>%
    # mutate(`Filled Rate` = case_when(
    #        `Filled Rate` >= 0.85 ~ cell_spec(`Filled Rate`, color = "green"),
    #        `Filled Rate` < 0.85 ~ cell_spec(`Filled Rate`, color = "red"))) %>%
    #   gather(variable, value, !!slot_metrics) %>%
    #   spread(APPT_MONTH_YEAR, value) %>%
    #   rename(Metrics = variable)
   
   
print("10")
    #slot <- slot %>%  group_by(across(!!cols), Appt.MonthYear) %>%
     # summarise(
      #  `Booked Rate (%)` = paste0(round(sum(`Booked Hours`)/sum(`Available Hours`)*100),"%"),
       # `Filled Rate (%)` = paste0(round(sum(`Filled Hours`)/sum(`Available Hours`)*100),"%"),
       # )%>%
      #gather(variable, value, !!slot_metrics) %>%
      #spread(Appt.MonthYear, value) %>%
      #rename(Metrics = variable)
    


     data_noShow <- dataArrivedNoShow() %>%
       filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))%>%
       mutate(APPT_STATUS = ifelse(APPT_STATUS == "Arrived","Arrived","No Show"))

     # data_noShow  <- arrivedNoShow.data.rows %>%
     #   filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")%>%
     #    filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled"))%>%
     #  mutate(APPT_STATUS = ifelse(APPT_STATUS == "Arrived","Arrived","No Show"))


  noShow_perc <-  data_noShow %>%
  group_by(!!!syms(cols),APPT_STATUS, APPT_MONTH_YEAR) %>%
  dplyr::summarise(Total = n()) %>% collect() %>%
    pivot_wider(names_from = APPT_STATUS, values_from = Total)

  noShow_perc[is.na(noShow_perc)] <- 0

  noShow_perc <- noShow_perc %>% group_by(!!!syms(cols), APPT_MONTH_YEAR) %>%
  mutate(percentage = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%"))



 noShow_perc$APPT_MONTH_YEAR <- as.yearmon(noShow_perc$APPT_MONTH_YEAR, "%Y-%m")
 year <- max(year(noShow_perc$APPT_MONTH_YEAR))-1
 
 noShow_perc <- noShow_perc %>% select(-`No Show`,-Arrived) %>% 
   pivot_wider(names_from = APPT_MONTH_YEAR, values_from = percentage)%>%
   ungroup()
 
# added total no-show %
 total <-  data_noShow %>%
   group_by(!!!syms(cols), APPT_STATUS) %>%
   dplyr::summarise(Total = n()) %>%
   collect()%>%
   pivot_wider(names_from = APPT_STATUS, values_from = Total)%>%
   replace(is.na(.), 0)%>%
   group_by(!!!syms(cols)) %>%
   mutate(Total = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%"))%>%
   select(-c(`No Show`,Arrived))
 
   noShow_perc <- left_join(noShow_perc, total, by = cols )
 
 
 # ## Added dynamic no-shows
 # if(compare_filters == "CAMPUS_SPECIALTY"){
 #   
 #   dynamic_noshow <- arrivedNoShow.data.rows %>%
 #     filter(CAMPUS %in% campus  & 
 #              APPT_YEAR %in% year &
 #              CAMPUS_SPECIALTY %in% specialty ) 
 #   
 # }
 # if(compare_filters == "DEPARTMENT"){
 #   dynamic_noshow <- arrivedNoShow.data.rows %>%
 #     filter(CAMPUS %in% campus  & 
 #              APPT_YEAR %in% year &
 #              CAMPUS_SPECIALTY %in% specialty &
 #              DEPARTMENT %in% department )
 # }
 # if(compare_filters == "PROVIDER"){
 #   dynamic_noshow <- arrivedNoShow.data.rows %>%
 #     filter(CAMPUS %in% campus  &
 #              APPT_YEAR %in% year &
 #              CAMPUS_SPECIALTY %in% specialty &
 #              DEPARTMENT %in% department &
 #              RESOURCES %in% resources &
 #              PROVIDER %in% provider)
 # }
 # 
 # 
 #   
 # dynamic_noshow  <- dynamic_noshow %>%
 #   #filter(CAMPUS %in% "MSUS"  & CAMPUS_SPECIALTY %in% "Allergy")%>%
 #   filter(APPT_STATUS %in% c("Arrived", "No Show", "Canceled")) %>%
 #   mutate(APPT_STATUS = ifelse(APPT_STATUS == "Arrived","Arrived","No Show")) %>%
 #   group_by(!!!syms(cols),APPT_STATUS, APPT_MONTH_YEAR) %>%
 #   dplyr::summarise(Total = n()) %>% 
 #   collect() %>% 
 #   pivot_wider(names_from = APPT_STATUS, values_from = Total)%>%
 #   replace(is.na(.), 0) %>%
 #   mutate(APPT_MONTH_YEAR = as.yearmon(APPT_MONTH_YEAR, "%Y-%m")) %>%
 #   #filter(year(APPT_MONTH_YEAR) == year)%>%
 #   group_by(!!!syms(cols)) %>%
 #   summarise(Arrived =sum(Arrived, na.rm = T), 
 #             `No Show` = sum(`No Show`, na.rm = T))%>%
 #   mutate(`Dynamic Target` = paste0(round((`No Show` / (Arrived + `No Show`))*100,0), "%"))%>%
 #   select(-`No Show`,-Arrived)
 # 
 #   
 #   noShow_perc <- left_join(noShow_perc, dynamic_noshow, by = cols )
   
  
   noShow_perc[is.na(noShow_perc)] <- "0%"

 noShow_perc$Metrics <- "No Show Rate"
 #noShow_perc <- noShow_perc %>% select(all_of(cols), Metrics, `Dynamic Target`, everything(), Total)
 noShow_perc <- noShow_perc %>% select(all_of(cols), Metrics, everything(), Total)

print("11")

    ## Percent of New Patients Scheduled Within 14 Days
data <- dataAll_access()

#data <- historical.data %>% filter(CAMPUS %in% "MSUS" & CAMPUS_SPECIALTY %in% "Allergy")

waitTime_total <- data %>%
  filter(WAIT_TIME >= 0) %>%
  filter(NEW_PT2 == "NEW") %>%
  group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
  dplyr::summarise(total_all = n()) %>%
  collect()

waitTime_within_14_days <- data %>%
  filter(WAIT_TIME >= 0, 
         WAIT_TIME < 14.0001, 
         NEW_PT2 == "NEW") %>%
  group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
  dplyr::summarise(total = n()) %>%
  collect()

join_data <- inner_join(waitTime_total, waitTime_within_14_days)

join_data[is.na(join_data)] <- 0

percent_within_14_days <- join_data %>% 
  group_by(!!!syms(cols), APPT_MADE_MONTH_YEAR) %>%
  summarise(percent = paste0(round((total/total_all),2)*100, "%"))


percent_within_14_days$APPT_MADE_MONTH_YEAR <- as.yearmon(percent_within_14_days$APPT_MADE_MONTH_YEAR, "%Y-%m")
percent_within_14_days <- percent_within_14_days %>% 
  pivot_wider(names_from = APPT_MADE_MONTH_YEAR, values_from = percent) %>%
  ungroup()


# Added total
total_wait <- data %>%
  filter(WAIT_TIME >= 0) %>%
  filter(NEW_PT2 == "NEW") %>%
  group_by(!!!syms(cols)) %>%
  dplyr::summarise(total_all = n()) %>%
  collect()

total_14_days <- data %>%
  filter(WAIT_TIME >= 0, 
         WAIT_TIME < 14.0001, 
         NEW_PT2 == "NEW") %>%
  group_by(!!!syms(cols)) %>%
  dplyr::summarise(total = n()) %>%
  collect()

total <- inner_join(total_wait, total_14_days)

total[is.na(total)] <- 0

total <- total %>% 
  group_by(!!!syms(cols)) %>%
  summarise(Total = paste0(round((total/total_all),2)*100, "%"))

percent_within_14_days <- left_join(percent_within_14_days, total, by= cols)


# # Added dynamic 
# if(compare_filters == "CAMPUS_SPECIALTY"){
#   
#   data.dynamcis <- historical.data %>%
#     filter(CAMPUS %in% campus  & 
#              CAMPUS_SPECIALTY %in% specialty ) 
#   
# }
# if(compare_filters == "DEPARTMENT"){
#   data.dynamcis <- historical.data %>%
#     filter(CAMPUS %in% campus  & 
#              CAMPUS_SPECIALTY %in% specialty &
#              DEPARTMENT %in% department )
# }
# if(compare_filters == "PROVIDER"){
#   data.dynamcis <- historical.data %>%
#     filter(CAMPUS %in% campus  &
#              CAMPUS_SPECIALTY %in% specialty &
#              DEPARTMENT %in% department &
#              RESOURCES %in% resources &
#              PROVIDER %in% provider)
# }
# 
# 
# waitTime.dynamic <- data.dynamcis %>%
#   filter(WAIT_TIME >= 0, NEW_PT2 == "NEW") %>%
#   collect()%>%
#   mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))%>%
#   filter(year(APPT_MADE_MONTH_YEAR) %in% year) %>%
#   group_by(!!!syms(cols)) %>%
#   dplyr::summarise(total_all = n()) 
# 
# waitTime_within_14_days.dynamic <- data.dynamcis %>%
#   filter(WAIT_TIME >= 0, 
#          WAIT_TIME < 14.0001, 
#          NEW_PT2 == "NEW") %>%
#   collect()%>%
#   mutate(APPT_MADE_MONTH_YEAR = as.yearmon(APPT_MADE_MONTH_YEAR, "%Y-%m"))%>%
#   filter(year(APPT_MADE_MONTH_YEAR) %in% year) %>%
#   group_by(!!!syms(cols)) %>%
#   dplyr::summarise(total = n())
# 
# percent_within_14_days.dynamics <- inner_join(waitTime.dynamic, waitTime_within_14_days.dynamic)
# 
# percent_within_14_days.dynamics[is.na(percent_within_14_days.dynamics)] <- 0
# 
# percent_within_14_days.dynamics <- percent_within_14_days.dynamics %>% 
#   group_by(!!!syms(cols)) %>%
#   summarise(`Dynamic Target` = paste0(round((total/total_all),2)*100, "%"))
# 
# 
# percent_within_14_days <- left_join(percent_within_14_days, percent_within_14_days.dynamics, by = cols)
percent_within_14_days$Metrics <- "Percent of New Patients Scheduled Within 14 Days"
#percent_within_14_days <- percent_within_14_days %>% select(all_of(cols), Metrics, `Dynamic Target`, everything(), Total)
percent_within_14_days <- percent_within_14_days %>% select(all_of(cols), Metrics, everything(), Total)



    
    ### Bind datas by row to create the final table
    # opt_table <- plyr:: rbind.fill(volume, slot, newpatients.ratio, waitTime )  
    opt_table <- plyr:: rbind.fill(volume, newpatients.ratio, waitTime, noShow_perc, percent_within_14_days )  

    opt_table[is.na(opt_table)] <- 0
    

    i1 <- as.yearmon(names(opt_table))
    opt_table <- opt_table[order(i1)]
    
    print("11")
    
    # months_df <-  opt_table[,!(names( opt_table) %in% c(cols, "Metrics", "Dynamic Target"))]

    # months <- order(as.yearmon(colnames(months_df), "%b %Y"))
    # order_months <- months_df[months]
    # 
    # 
    # 
    # 
    # index <- months+length(cols)+2
    # #index <- c(1:length(cols_name),index,length(opt_table))
    # index <- c(1:length(cols)+2, index)
    # index <- sort(index, decreasing = F)

    #opt_table <- opt_table[index]
    #opt_table <- opt_table %>% select(cols, "Metrics", "Dynamic Target", everything(), Total)
    opt_table <- opt_table %>% select(cols, "Metrics", everything(), Total)
    
    opt_table <- opt_table %>% add_column(`System Target` = "TBD", .after = "Metrics") 
    opt_table <- opt_table %>% mutate(`System Target`= case_when(Metrics=="Booked Rate"~ ">= 95%", 
                                                        Metrics=="Filled Rate"~ ">= 85%",
                                                        Metrics=="New Patient Wait Time (Days)"~ "14",
                                                        Metrics=="Average Daily Volume"~ "Variable",
                                                        Metrics == "New Patient Ratio" ~ "25%",
                                                        Metrics == "No Show Rate" ~ "10%",
                                                        Metrics == "Percent of New Patients Scheduled Within 14 Days" ~ "60%",
                                                        TRUE ~ "TBD"))
    
                                                             
    
   
    print("12") 
    time_2 <- Sys.time()
    
    print(time_2 - time_1)
    metric_order <- c("Average Daily Volume",
                      c("Booked Rate", "Filled Rate", "New Patient Ratio", "New Patient Wait Time (Days)", "No Show Rate", "Percent of New Patients Scheduled Within 14 Days") , as.vector(unique(opt_table$Metrics)))
  

    opt_table <- opt_table[order(match(opt_table$Metrics, metric_order )),]
    
     
    
   

 # opt_table <- as.datatable(formattable(opt_table, list(
 #    `Jan 2021` = formatter("span",
 #                          style = x ~ style(color = 'white',
 #                                           'background-color' =
 #                                             ifelse(opt_table$Metrics == "Booked Rate (%)",
 #                                                  ifelse( x> 0.95,"green", "red"), "gray")) ))))

 #opt_table <- opt_table %>% mutate(`Jan 2021`=ifelse(Metrics == "Booked Rate (%)", paste0(`Jan 2021`*100, "%"), `Jan 2021`))
    
 
   
    
  })
  
  
  
  
  
  output$opt_day_title <- renderText({
    
    if(input$compare_filters_opt == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
    }
    if(input$compare_filters_opt == "DEPARTMENT"){
      name_1 <- input$compare_filters_opt
    }
    if(input$compare_filters_opt == "PROVIDER"){
      name_1 <- input$compare_filters_opt
    }
    paste0("Schedule Optimization by ", name_1 )
  })
  
  
  
  
  output$opt_comparison_tb_kable <- function(){
    data <- schedule_opt()
    
    compare_filters <- input$compare_filters_opt
    

    if(compare_filters == "CAMPUS_SPECIALTY"){
      name_1 <- "Specialty"
      cols <- name_1
      cols_name <- name_1
      pack_rows_name <- "CAMPUS_SPECIALTY"
      print(pack_rows_name)
      data <- data %>% arrange(CAMPUS_SPECIALTY)
    }
    if(compare_filters == "DEPARTMENT"){
      name_1 <- "Department"
      cols <- c("Specialty",compare_filters)    
      cols_name <- c("Specialty",name_1)
      pack_rows_name <- c("CAMPUS_SPECIALTY", "DEPARTMENT")
      data <- data %>% arrange(CAMPUS_SPECIALTY, DEPARTMENT)
    }
    if(compare_filters == "PROVIDER"){
      name_1 <- "Provider"
      cols <- c("Specialty","DEPARTMENT",compare_filters)
      cols_name <- c("Specialty","DEPARTMENT", name_1)
      pack_rows_name <- c("CAMPUS_SPECIALTY", "DEPARTMENT", "PROVIDER")
      data <- data %>% arrange(CAMPUS_SPECIALTY, DEPARTMENT, PROVIDER)
    }
    
  
    
    # col_names <- c(cols, "Metrics", "System Target", "Dynamic Target",                  
    #                colnames(data)[(length(cols)+4):length(data)])
    
    col_names <- c(cols, "Metrics", "System Target",                 
                   colnames(data)[(length(cols)+3):length(data)])

    header_above <- c("title" = length(data))
    names(header_above) <- paste0("Schedule Optimization by ", name_1)
    
    month_columns <- colnames(data)[(length(cols)+4):length(data)-1]
    
    options(knitr.kable.NA = '-')
    
    pack_rows_name <- table(pack_rows_name)
    border_column <- length(cols)+2 
    
    border_column <- length(cols)+2 
    
    data %>%
      kable(booktabs = T,escape = F, #align = c(rep("r",3),rep("c",length(metrics_dept_output)-4)),
            col.names = col_names) %>%
      kable_styling(bootstrap_options = "hover", full_width = FALSE, position = "center", row_label_position = "c", font_size = 16) %>%
      add_header_above(header_above, background = "white", color = "black", font_size = 20, bold = T, align = "c", line = F) %>%
      row_spec(0,  background = "#212070", color = "white")%>%
      column_spec(1, border_left = "2px solid #dddedd", border_right = FALSE)%>%
      column_spec(border_column, border_left = FALSE, border_right = "2px solid #dddedd" )%>%
      column_spec(length(data), border_left = "2px solid #dddedd", border_right = "2px solid #dddedd" )%>%
      #row_spec(nrow(data), hline_after = TRUE, extra_css = "border-bottom: 1px solid;")%>%
      collapse_rows(columns = 1:3, valign = "top")
      
   
      
    
 
    
    # data %>%
    #   kable("html", booktabs = T,escape = F) %>%
    #   collapse_rows(columns = "Department", valign = "top")
    
    
    
    # data_test %>%
    #     kable(booktabs = T,escape = F) %>%
    #   kableExtra::collapse_rows(columns = 4:5, valign = "top")

  }
  
  output[["opt_comparison_tb"]] <- renderDT({
    
    #col_dissappear <- which(names( schedule_opt()) %in% c("Total_YN"))
    
    data <- schedule_opt()
    #data <- opt_table
    
    num_of_cols <- length(data)
    
    
    
    if(compare_filters == "Campus.Specialty"){
      name_1 <- "Specialty"
      cols_name <- c(name_1)
    }
    if(compare_filters == "Department"){
      name_1 <- compare_filters
      cols_name <- c("Specialty",name_1)
    }
    if(compare_filters == "Provider"){
      name_1 <- compare_filters
      cols_name <- c("Specialty","Department", name_1)
    }
    
    filled_rate_rows <- which(data$Metrics == "Filled Rate (%)")
    filled_rate_rows <- data.frame(which(data  == "Filled Rate (%)" & data >= 0.85, arr.ind=TRUE))
    
    green_booked_rate <- data.frame(which(data[,(length(cols_name)+3):length(data)] >= 0.85, arr.ind=TRUE) + length(cols_name)+2)
     
    
    #green_booked_rate <- green_booked_rate %>% filter(row %in% test)
    green_booked_rate <- green_booked_rate %>% filter(row %in% filled_rate_rows)
    
    dtable <-   datatable( data, 
                           class = 'cell-border stripe',
                           rownames = FALSE,
                           extensions = c('Buttons','Scroller'),
                           caption = htmltools::tags$caption(
                             style = 'caption-side: bottom; text-align: left;'
                             #htmltools::em('Average Daily Volume = Total arrived visits by month and breakdown / Total number of days within the month.')
                           ),
                           options = list(
                             scrollX = TRUE,
                             #columnDefs = list(list(visible = F, targets = as.list(col_dissappear-1))),
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
                                                   ifelse(colnames( data)[3] == "Provider", 4, 3)
                             ),
                             #fixedColumns = list(leftColumns = 2),
                             #rowsGroup = rows_group_opt(),
                             headerCallback = DT::JS(
                               "function(thead) {",
                               "  $(thead).css('font-size', '115%');",
                               "}"
                             )
                           )
                           
                           # %>%
                           #   formatStyle(
                           #     'Total_YN',
                           #     target = "row",
                           #     fontWeight = styleEqual(1, "bold")
                           #     #backgroundColor = styleEqual(c(1),c('grey'))
                           #   ) 
    ) %>% formatStyle(columns = c(1:num_of_cols), fontSize = '115%')
    
    
    path <- here::here("www")
    
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)  
  
  
  
  
  output$no_show_comp_title_daily <- renderText({
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
    paste0("Average Daily No Show Rate (%) by ", name_1 , " and ", name_2)
  })
  
  
  no_show_rate_daily <- reactive({
    
    data <- dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- kpi.all.data[arrivedNoShow.data.rows,] %>% filter(Appt.Status %in% c("Arrived", "No Show")) %>% filter(Campus == "MSUS")
    # compare_filters <- "Campus.Specialty"
    # breakdown_filters <- "Visit.Method"
    
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    
    
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
    }else {
      
      noShow_perc <- data %>%
        group_by(across(cols),Appt.Status, Appt.MonthYear) %>%
        dplyr::summarise(Total = n())
  
      noShow_perc_days <- data %>%
        group_by(across(cols), Appt.MonthYear) %>%
        dplyr::summarise(total_days = length(unique(Appt.DateYear)))
  

      noShow_perc <- noShow_perc %>% pivot_wider(names_from = Appt.Status, values_from = Total)
      
      noShow_perc <- merge(noShow_perc,noShow_perc_days)
      
      noShow_perc <- noShow_perc %>% group_by(across(cols),Appt.MonthYear) %>% mutate(percentage = round((`No Show` / (Arrived + `No Show`))/total_days*100,2))
      noShow_perc <- noShow_perc %>% mutate(percentage = ifelse(percentage >= 1, round((`No Show` / (Arrived + `No Show`))/total_days*100,0), round((`No Show` / (Arrived + `No Show`))/total_days*100,2)))
      
      noShow_perc <- noShow_perc %>% select(-`No Show`,-Arrived, -total_days) %>% pivot_wider(names_from = Appt.MonthYear, values_from = percentage)
    }
    
    noShow_perc <- setnames(noShow_perc, old = cols, new = cols_name)
    
    
    months_df <- noShow_perc[,!(names(noShow_perc) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%b %Y"))
    order_months <- months_df[months]
    
    
    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index)
    
    noShow_perc <- noShow_perc[index]
    
  })
  
  output[["no_show_comp_daily"]] <- renderDT({
    data_table <- no_show_rate_daily()
    #data_table <- noShow_perc
    num_of_cols <- length(data)
    #col_dissappear <- which(names(data_table) %in% c("Total_YN"))
    dtable <-   datatable(data_table, 
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
                            columnDefs = list(list(visible = F#, targets = as.list(col_dissappear-1)
                            )
                            ),
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
                                                  ifelse(colnames(data_table)[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group()
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
  
  

  
  
  output$no_show_comp_title <- renderText({
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
    paste0("Monthly No Show Rate (%) by ", name_1 , " and ", name_2)
  })
  
  no_show_rate_month <- reactive({
    
    data <- dataArrivedNoShow() %>% filter(Appt.Status %in% c("Arrived", "No Show"))
    # data <- kpi.all.data[arrivedNoShow.data.rows,] %>% filter(Appt.Status %in% c("Arrived", "No Show")) %>% filter(Campus == "MSUS")
    # compare_filters <- "Campus.Specialty"
    # breakdown_filters <- "Visit.Method"
    
    
    data$Appt.MonthYear <- as.yearmon(data$Appt.MonthYear, "%Y-%m")
    data$Appt.Status <- ifelse(data$Appt.Status == "Arrived","Arrived","No Show")
    
    
    
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
      }else {
        
        noShow_perc <- data %>%
          group_by(across(cols),Appt.Status, Appt.MonthYear) %>%
          dplyr::summarise(Total = n())
        
        noShow_perc <- noShow_perc %>% pivot_wider(names_from = Appt.Status, values_from = Total)
        
        noShow_perc <- noShow_perc %>% group_by(across(cols),Appt.MonthYear) %>% mutate(percentage = round((`No Show` / (Arrived + `No Show`))*100,0))
        
        noShow_perc <- noShow_perc %>% select(-`No Show`,-Arrived) %>% pivot_wider(names_from = Appt.MonthYear, values_from = percentage)
      }
    
    noShow_perc <- setnames(noShow_perc, old = cols, new = cols_name)
    
    
    months_df <- noShow_perc[,!(names(noShow_perc) %in% c(cols_name, "Total", "Total_YN"))]
    months <- order(as.yearmon(colnames(months_df), "%b %Y"))
    order_months <- months_df[months]
    
    
    index <- months+length(cols_name)
    index <- c(1:length(cols_name),index)
    
    noShow_perc <- noShow_perc[index]
    
  })
  
  
  
  
  output[["no_show_comp_month"]] <- renderDT({
    data_table <- no_show_rate_month()
    #data_table <- noShow_perc
    num_of_cols <- length(data)
    #col_dissappear <- which(names(data_table) %in% c("Total_YN"))
    dtable <-   datatable(data_table, 
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
                            columnDefs = list(list(visible = F#, targets = as.list(col_dissappear-1)
                            )
                            ),
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
                                                  ifelse(colnames(data_table)[3] == "Provider", 4, 3)
                            ),
                            rowsGroup = rows_group()
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
  
  output$visit_type_summary <- renderValueBox({
    data <- dataAll()
    
    unique_visit_types <- data %>% select(APPT_TYPE) %>% distinct() %>% collect()
    
    number_visit_types <- nrow(unique_visit_types)
    
    valueBoxSpark(
      value = paste0(number_visit_types),
      title = "Unique Visit Types",
      subtitle = NULL,
      width = 6,
      color = "fuchsia"
    )
  })
  
  output$visit_type_summary_new <- renderValueBox({
    data <- dataAll()
    
    unique_visit_types <- data %>% filter(NEW_PT2 == "NEW") %>% select(APPT_TYPE) %>% distinct() %>% collect()
    
    number_visit_types <- nrow(unique_visit_types)
    
    valueBoxSpark(
      value = paste0(number_visit_types),
      title = "Unique New Visit Types",
      subtitle = NULL,
      width = 6,
      color = "fuchsia"
    )
  })
  
  output$visit_type_summary_est <- renderValueBox({
    data <- dataAll()
    
    unique_visit_types <- data %>% filter(NEW_PT2 == "ESTABLISHED") %>% select(APPT_TYPE) %>% distinct() %>% collect()
    
    number_visit_types <- nrow(unique_visit_types)
    
    valueBoxSpark(
      value = paste0(number_visit_types),
      title = "Unique Established Visit Types",
      subtitle = NULL,
      width = 6,
      color = "fuchsia"
    )
  })
  
  output$appt_length_breakdown_tb_kable <- function() {
    
    data <- dataAll()
    
    data <- data %>% select(CAMPUS, CAMPUS_SPECIALTY, DEPARTMENT, NEW_PT2, APPT_TYPE, APPT_DUR) %>% 
      group_by(CAMPUS, CAMPUS_SPECIALTY, DEPARTMENT, NEW_PT2, APPT_TYPE, APPT_DUR) %>% 
      summarise(total_appt = n()) %>% collect() %>%
      pivot_wider(names_from = APPT_DUR, values_from = total_appt, names_sort = TRUE) %>%
      arrange(CAMPUS, CAMPUS_SPECIALTY, DEPARTMENT, NEW_PT2, APPT_TYPE) %>%
      rename(Campus = CAMPUS,
             Specialty = CAMPUS_SPECIALTY,
             `New vs. Established` = NEW_PT2,
             `Visit Type` = APPT_TYPE,
             Department = DEPARTMENT) %>%
      mutate(`New vs. Established` = ifelse(`New vs. Established` == "NEW", "New", "Established")) %>%
      ungroup()
    
    data <- data %>% mutate(`Total Volume` = rowSums(data[,6:length(data)], na.rm = TRUE))
    
    options(knitr.kable.NA = '-')
    
    total_rows <- nrow(data)
    
    data %>%
      kable(booktabs = T,escape = F) %>%
      kable_styling(bootstrap_options = "bordered", full_width = FALSE, position = "center", row_label_position = "c", font_size = 16) %>%
      row_spec(0,  background = "#212070", color = "white")%>%
      column_spec(1, border_left = "2px solid #dddedd", border_right = FALSE)%>%
      column_spec(length(data), border_left = "2px solid #dddedd", border_right = "2px solid #dddedd" )%>%
      column_spec(5, border_left = "2px solid #dddedd")%>%
      #row_spec(total_rows,  extra_css = "border-bottom: 2px solid #dddedd") %>%
      collapse_rows(columns = 1:4, valign = "top") 

    
    
  }
  
} # Close server 

#shinyApp(ui, server)


#shinyApp(ui, server, options = list(launch.browser = T,browser = "C:/Program Files/Google/Chrome/Application/chrome.exe"))
