
# Define server logic
server <- function(input, output, session) {
  
  # Set the CRAN mirror
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
  
  
  tryCatch({
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "himanshucmc-9d06ab2bebfc.json")
    gs4_auth(path = "himanshucmc-9d06ab2bebfc.json",
             scopes = "https://www.googleapis.com/auth/spreadsheets",
             cache = TRUE,
             use_oob = FALSE)
    
  }, error = function(e) {
    print(paste("Error: ", e$message))
    # Optionally, return a user-friendly message or take corrective action
  })
  
  path= "https://docs.google.com/spreadsheets/d/1FwfxfMlQRBppNx8P9rJDjGPX1LqPKU44hdlfUHKoXxo/edit?gid=330088273#gid=330088273"
  dfo <- read_sheet(path, sheet = "CMCBMT Data till 2023")
  
  
  dff <- data.frame(
    Sl_No = dfo$`Sl No`,
    Name = dfo$NAME,
    Patient_Gender = dfo$`Patient Gender`,
    Disease = dfo$Disease,
    Status_at_last_FU = dfo$`Status at last FU`,
    Status_if_alive = dfo$`Status if alive`,
    time = dfo$Time,
    OS = dfo$OS,
    EFS = dfo$EFS,
    PFS = dfo$PFS,
    GRFS = dfo$GRFS,
    Date_of_BMT = as.Date(dfo$Date_of_BMT),
    Type_of_transplant = dfo$Type_of_transplant,
    Diagnosis = dfo$Diagnosis_new,
    Type_of_Transplant_4 = dfo$Type_of_Transplant_4,
    Age = dfo$`Age at Tx`
  )
  
  dff <- dff %>%
    mutate(CAT = if_else(Age < 18, "Pediatrics", "Adult")) %>%
    mutate(Time = pmin(time, 365*5))
  
  dff$year <- year(dff$Date_of_BMT)
  dff <- dff %>%
    mutate(period = cut(year, 
                        breaks = c(2007, 2014, 2019, 2024, 2029), 
                        labels = c("2008-2014", "2015-2019", "2020-2024", "2025-2029")))
  
  
  df <- reactive({
    dp <- dff%>%
      filter(Date_of_BMT >= input$date_range[1] & Date_of_BMT <= input$date_range[2])
    return(dp)
  })
  
  
  ###########################################################################################
  
  ###########################################################################################

  ###########################################################################################
  
  ###########################################################################################
  
  fit_ci <- reactive({
    
    df <- df()
    
    if (input$Category == "Overall") {
      cuminc(ftime = df$Time, fstatus = df$OS)
    } else if (input$Category == "Disease") {
      df$Disease <- as.factor(df$Disease)
      cuminc(ftime = df$Time, fstatus = df$OS, group = df$Disease)
    } else if (input$Category == "Type of transplant") {
      df$Type_of_transplant <- as.factor(df$Type_of_transplant)
      cuminc(ftime = df$Time, fstatus = df$OS, group = df$Type_of_transplant)
    } else if (input$Category == "CAT") {
      df$CAT <- as.factor(df$CAT)
      cuminc(ftime = df$Time, fstatus = df$OS, group = df$CAT)
    }
  })
  
  output$cumulative <- renderPlot({
    
   df <- df()
    
    fit_ci <- fit_ci()
    
    x_ticks <- seq(0, 365*5, by = 365)
    
    plot(fit_ci,
         xlab = "Time",
         xlim = range(x_ticks),
         xaxt = "n",
         ylab = "Cumulative Incidence",
         main = "Cumulative Incidence by Disease Type",
         col = c("red", "blue"),  
         lty = 1:2, 
         lwd = 2,
         cex.main = 1.5,        
         cex.lab = 1.5,           
         cex.axis = 1.5)
    
    axis(1, at = x_ticks, cex.axis = 1.2)
    
  })
  
  output$estimates <- renderPrint({
    
    fit_ci <- fit_ci()
    
    time_points <- c(30, 60, 100, 365, 730)
    ci_estimates <- timepoints(fit_ci, times = time_points)
    
    print(ci_estimates)
  })
  
  ###########################################################################################
 
  ###########################################################################################
 
  fit_ci_pfs <- reactive({
    
    df <- df()
    
    if (input$Category_pfs == "Overall") {
      cuminc(ftime = df$Time, fstatus = df$EFS)
    } else if (input$Category_pfs == "Disease") {
      df$Disease <- as.factor(df$Disease)
      cuminc(ftime = df$Time, fstatus = df$EFS, group = df$Disease)
    } else if (input$Category_pfs == "Type of transplant") {
      df$Type_of_transplant <- as.factor(df$Type_of_transplant)
      cuminc(ftime = df$Time, fstatus = df$EFS, group = df$Type_of_transplant)
    } else if (input$Category_pfs == "CAT") {
      df$CAT <- as.factor(df$CAT)
      cuminc(ftime = df$Time, fstatus = df$EFS, group = df$CAT)
    }
  })
  
  output$cumulative_pfs <- renderPlot({
    
    df <- df()
    
    fit_ci_pfs <- fit_ci_pfs()
    
    x_ticks <- seq(0, 365*5, by = 365)
    
    plot(fit_ci_pfs,
         xlab = "Time",
         xlim = range(x_ticks),
         xaxt = "n",
         ylab = "Cumulative Incidence",
         main = "Cumulative Incidence by Disease Type",
         col = brewer.pal(8, "Set1"),  
         lty = 1:2,
         lwd = 2,
         cex.main = 1.5,        
         cex.lab = 1.5,           
         cex.axis = 1.5
         )
    
    axis(1, at = x_ticks, cex.axis = 1.2)
    
  })
  
  output$estimates_pfs <- renderPrint({
    
    fit_ci_pfs <- fit_ci_pfs()
    
    time_points <- c(30, 60, 100, 365, 730)
    ci_estimates <- timepoints(fit_ci_pfs, times = time_points)
    
    print(ci_estimates)
  })
  
  ###########################################################################################
  
  ###########################################################################################
  
  # Output: OS survival plot
  fit_os <- reactive({
    
    df <- df()
    
    if (input$os_stratify == "Overall") {
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$os_stratify == "Disease") {
      df$Disease <- as.factor(df$Disease)
      survfit(Surv(time = Time, event = OS) ~ Disease, data = df)
    } else if (input$os_stratify == "Type of transplant") {
      df$Type_of_transplant <- as.factor(df$Type_of_transplant)
      survfit(Surv(time = Time, event = OS) ~ Type_of_transplant, data = df)
    } else if (input$os_stratify == "4 Types of transplant") {
      df$Type_of_Transplant_4 <- as.factor(df$Type_of_Transplant_4)
      survfit(Surv(time = Time, event = OS) ~ Type_of_Transplant_4, data = df)
    } else if (input$os_stratify == "CAT") {
      df$CAT <- as.factor(df$CAT)
      survfit(Surv(time = Time, event = OS) ~ CAT, data = df)
    }
  })
  
  output$os_survival_plot <- renderPlot({
    
    df <- df()
    
    fit_os <- fit_os()
    ggsurvplot(fit_os,
               data = df,
               pval = TRUE,
               pval.size = 9,
               conf.int = FALSE,
               cumevents = TRUE,
               cumevents.table.pos = "in",
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen"))
    )
  })
  
  output$survTable_OS <- render_gt({
    surv_model_os <- fit_os()
    tbl <- tbl_survfit(surv_model_os, 
                       times = c(90, 365, 365*3, 365*5)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$os_summary <- renderPrint({
    print(fit_os(), print.rmean = TRUE)
    summary(fit_os())
  })
  
  ###########################################################################################
  
  ###########################################################################################
  
  # Output: EFS survival plot
  fit_efs <- reactive({
    
    df <- df()
    
    if (input$efs_stratify == "Overall") {
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$efs_stratify == "Disease") {
      df$Disease <- as.factor(df$Disease)
      survfit(Surv(time = Time, event = EFS) ~ Disease, data = df)
    } else if (input$efs_stratify == "Type of transplant") {
      df$Type_of_transplant <- as.factor(df$Type_of_transplant)
      survfit(Surv(time = Time, event = EFS) ~ Type_of_transplant, data = df)
    } else if (input$efs_stratify == "CAT") {
      df$CAT <- as.factor(df$CAT)
      survfit(Surv(time = Time, event = EFS) ~ CAT, data = df)
    }
  })
  
  output$efs_survival_plot <- renderPlot({
    
    df <- df()
    
    ggsurvplot(fit_efs(),
               data = df,
               pval = TRUE,
               pval.size = 9,
               conf.int = TRUE,
               cumevents = TRUE,
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen")))
  })
  
  output$survTable_EFS <- render_gt({
    surv_model_efs <- fit_efs()
    tbl <- tbl_survfit(surv_model_efs, 
                       times = c(90, 365, 365*5, 365*10)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$efs_summary <- renderPrint({
    print(fit_efs(), print.rmean = TRUE)
    summary(fit_efs())
  })
  
  ###########################################################################################
  
  ###########################################################################################
  
  # Output: Specific disease survival plot
  fit_specific <- reactive({
    
    df <- df()
    
    df$Diagnosis <- as.factor(df$Diagnosis)
    
    if (input$specific_disease_stratify == "Overall") {
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "ALL") {
      df <- df %>%
        filter(Diagnosis == "ALL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML") {
      df <- df %>%
        filter(Diagnosis == "AML")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    }  else if (input$specific_disease_stratify == "AML Relapse CR1") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR1")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    }   else if (input$specific_disease_stratify == "AML Relapse CR2") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR2")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML Relapse CR3") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR3")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML with HLH") {
      df <- df %>%
        filter(Diagnosis == "AML with HLH")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "CML") {
      df <- df %>%
        filter(Diagnosis == "CML")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "DLBCL") {
      df <- df %>%
        filter(Diagnosis == "DLBCL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HD") {
      df <- df %>%
        filter(Diagnosis == "HD")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HL") {
      df <- df %>%
        filter(Diagnosis == "HL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MCL") {
      df <- df %>%
        filter(Diagnosis == "MCL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MDS") {
      df <- df %>%
        filter(Diagnosis == "MDS")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MM/PCD") {
      df <- df %>%
        filter(Diagnosis == "MM/PCD")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MPN") {
      df <- df %>%
        filter(Diagnosis == "MPN")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "N K T-Cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "N K T-Cell Lymphoma")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Neuroblastoma") {
      df <- df %>%
        filter(Diagnosis == "Neuroblastoma")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "NHL") {
      df <- df %>%
        filter(Diagnosis == "NHL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse AML") {
      df <- df %>%
        filter(Diagnosis == "Relapse AML")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse HL") {
      df <- df %>%
        filter(Diagnosis == "Relapse HL")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "SAA") {
      df <- df %>%
        filter(Diagnosis == "SAA")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "T cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "T cell Lymphoma")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "TM") {
      df <- df %>%
        filter(Diagnosis == "TM")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "WAS") {
      df <- df %>%
        filter(Diagnosis == "WAS")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Yalk Cell Tumor") {
      df <- df %>%
        filter(Diagnosis == "Yalk Cell Tumor")
      survfit(Surv(time = Time, event = OS) ~ 1, data = df)
    } 
  })
  
  output$Specific_disease_survival_plot <- renderPlot({
    
    df <- df()
    
    ggsurvplot(fit_specific(),
               data = df,
               pval = FALSE,
               conf.int = FALSE,
               cumevents = TRUE,
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen")))
  })
  
  output$survTable_SO <- render_gt({
    surv_model_SO <- fit_specific()
    tbl <- tbl_survfit(surv_model_SO, 
                       times = c(90, 365, 365*5, 365*10)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$specific_disease_summary <- renderPrint({
    print(fit_specific(), print.rmean = TRUE)
    summary(fit_specific())
  })
  
  
  
  # Output: EFS Specific disease survival plot
  fit_specific_EFS <- reactive({
    
    df <- df()
    
    df$Diagnosis <- as.factor(df$Diagnosis)
    
    if (input$specific_disease_stratify == "Overall") {
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "ALL") {
      df <- df %>%
        filter(Diagnosis == "ALL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML") {
      df <- df %>%
        filter(Diagnosis == "AML")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    }  else if (input$specific_disease_stratify == "AML Relapse CR1") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR1")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    }   else if (input$specific_disease_stratify == "AML Relapse CR2") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR2")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML Relapse CR3") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR3")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML with HLH") {
      df <- df %>%
        filter(Diagnosis == "AML with HLH")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "CML") {
      df <- df %>%
        filter(Diagnosis == "CML")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "DLBCL") {
      df <- df %>%
        filter(Diagnosis == "DLBCL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HD") {
      df <- df %>%
        filter(Diagnosis == "HD")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HL") {
      df <- df %>%
        filter(Diagnosis == "HL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MCL") {
      df <- df %>%
        filter(Diagnosis == "MCL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MDS") {
      df <- df %>%
        filter(Diagnosis == "MDS")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MM/PCD") {
      df <- df %>%
        filter(Diagnosis == "MM/PCD")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MPN") {
      df <- df %>%
        filter(Diagnosis == "MPN")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "N K T-Cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "N K T-Cell Lymphoma")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Neuroblastoma") {
      df <- df %>%
        filter(Diagnosis == "Neuroblastoma")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "NHL") {
      df <- df %>%
        filter(Diagnosis == "NHL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse AML") {
      df <- df %>%
        filter(Diagnosis == "Relapse AML")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse HL") {
      df <- df %>%
        filter(Diagnosis == "Relapse HL")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "SAA") {
      df <- df %>%
        filter(Diagnosis == "SAA")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "T cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "T cell Lymphoma")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "TM") {
      df <- df %>%
        filter(Diagnosis == "TM")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "WAS") {
      df <- df %>%
        filter(Diagnosis == "WAS")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Yalk Cell Tumor") {
      df <- df %>%
        filter(Diagnosis == "Yalk Cell Tumor")
      survfit(Surv(time = Time, event = EFS) ~ 1, data = df)
    } 
  })
  
  output$Specific_disease_survival_plot_EFS <- renderPlot({
    
    df <- df()
    
    ggsurvplot(fit_specific_EFS(),
               data = df,
               pval = FALSE,
               conf.int = FALSE,
               cumevents = TRUE,
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen")))
  })
  
  output$survTable_SEFS <- render_gt({
    surv_model_SEFS <- fit_specific_EFS()
    tbl <- tbl_survfit(surv_model_SEFS, 
                       times = c(90, 365, 365*5, 365*10)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$specific_disease_summary_EFS <- renderPrint({
    print(fit_specific_EFS(), print.rmean = TRUE)
    summary(fit_specific_EFS())
   })
  
  
  # Output: PFS Specific disease survival plot
  fit_specific_PFS <- reactive({
    
    df <- df()
    
    df$Diagnosis <- as.factor(df$Diagnosis)
    
    if (input$specific_disease_stratify == "Overall") {
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "ALL") {
      df <- df %>%
        filter(Diagnosis == "ALL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML") {
      df <- df %>%
        filter(Diagnosis == "AML")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    }  else if (input$specific_disease_stratify == "AML Relapse CR1") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR1")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    }   else if (input$specific_disease_stratify == "AML Relapse CR2") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR2")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML Relapse CR3") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR3")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML with HLH") {
      df <- df %>%
        filter(Diagnosis == "AML with HLH")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "CML") {
      df <- df %>%
        filter(Diagnosis == "CML")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "DLBCL") {
      df <- df %>%
        filter(Diagnosis == "DLBCL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HD") {
      df <- df %>%
        filter(Diagnosis == "HD")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HL") {
      df <- df %>%
        filter(Diagnosis == "HL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MCL") {
      df <- df %>%
        filter(Diagnosis == "MCL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MDS") {
      df <- df %>%
        filter(Diagnosis == "MDS")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MM/PCD") {
      df <- df %>%
        filter(Diagnosis == "MM/PCD")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MPN") {
      df <- df %>%
        filter(Diagnosis == "MPN")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "N K T-Cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "N K T-Cell Lymphoma")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Neuroblastoma") {
      df <- df %>%
        filter(Diagnosis == "Neuroblastoma")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "NHL") {
      df <- df %>%
        filter(Diagnosis == "NHL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse AML") {
      df <- df %>%
        filter(Diagnosis == "Relapse AML")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse HL") {
      df <- df %>%
        filter(Diagnosis == "Relapse HL")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "SAA") {
      df <- df %>%
        filter(Diagnosis == "SAA")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "T cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "T cell Lymphoma")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "TM") {
      df <- df %>%
        filter(Diagnosis == "TM")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "WAS") {
      df <- df %>%
        filter(Diagnosis == "WAS")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Yalk Cell Tumor") {
      df <- df %>%
        filter(Diagnosis == "Yalk Cell Tumor")
      survfit(Surv(time = Time, event = PFS) ~ 1, data = df)
    } 
  })
  
  output$Specific_disease_survival_plot_PFS <- renderPlot({
    
    df <- df()
    
    ggsurvplot(fit_specific_PFS(),
               data = df,
               pval = FALSE,
               conf.int = FALSE,
               cumevents = TRUE,
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen")))
  })
  
  output$survTable_SPFS <- render_gt({
    surv_model_SPFS <- fit_specific_PFS()
    tbl <- tbl_survfit(surv_model_SPFS, 
                       times = c(90, 365, 365*5, 365*10)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$specific_disease_summary_PFS <- renderPrint({
    print(fit_specific_PFS(), print.rmean = TRUE)
    summary(fit_specific_PFS())
  })
  
  # Output: GRFS Specific disease survival plot
  fit_specific_GRFS <- reactive({
    
    df <- df()
    df$Diagnosis <- as.factor(df$Diagnosis)
    
    if (input$specific_disease_stratify == "Overall") {
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "ALL") {
      df <- df %>%
        filter(Diagnosis == "ALL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML") {
      df <- df %>%
        filter(Diagnosis == "AML")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    }  else if (input$specific_disease_stratify == "AML Relapse CR1") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR1")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    }   else if (input$specific_disease_stratify == "AML Relapse CR2") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR2")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML Relapse CR3") {
      df <- df %>%
        filter(Diagnosis == "AML Relapse CR3")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "AML with HLH") {
      df <- df %>%
        filter(Diagnosis == "AML with HLH")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "CML") {
      df <- df %>%
        filter(Diagnosis == "CML")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "DLBCL") {
      df <- df %>%
        filter(Diagnosis == "DLBCL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HD") {
      df <- df %>%
        filter(Diagnosis == "HD")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "HL") {
      df <- df %>%
        filter(Diagnosis == "HL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MCL") {
      df <- df %>%
        filter(Diagnosis == "MCL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MDS") {
      df <- df %>%
        filter(Diagnosis == "MDS")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MM/PCD") {
      df <- df %>%
        filter(Diagnosis == "MM/PCD")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "MPN") {
      df <- df %>%
        filter(Diagnosis == "MPN")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "N K T-Cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "N K T-Cell Lymphoma")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Neuroblastoma") {
      df <- df %>%
        filter(Diagnosis == "Neuroblastoma")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "NHL") {
      df <- df %>%
        filter(Diagnosis == "NHL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse AML") {
      df <- df %>%
        filter(Diagnosis == "Relapse AML")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Relapse HL") {
      df <- df %>%
        filter(Diagnosis == "Relapse HL")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "SAA") {
      df <- df %>%
        filter(Diagnosis == "SAA")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "T cell Lymphoma") {
      df <- df %>%
        filter(Diagnosis == "T cell Lymphoma")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "TM") {
      df <- df %>%
        filter(Diagnosis == "TM")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "WAS") {
      df <- df %>%
        filter(Diagnosis == "WAS")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } else if (input$specific_disease_stratify == "Yalk Cell Tumor") {
      df <- df %>%
        filter(Diagnosis == "Yalk Cell Tumor")
      survfit(Surv(time = Time, event = GRFS) ~ 1, data = df)
    } 
    
  })
  
  output$Specific_disease_survival_plot_GRFS <- renderPlot({
    
    df <- df()
    
    ggsurvplot(fit_specific_GRFS(),
               data = df,
               pval = TRUE,
               pval.size = 9,
               conf.int = FALSE,
               cumevents = TRUE,
               surv.median.line = "hv",
               break.time.by = 365,
               font.legend = c(18, "plain", "black"),
               font.x = c(18, "bold.italic", "darkred"),
               font.y = c(18, "bold.italic", "darkred"),
               font.tickslab = c(16, "plain", "darkgreen"),
               tables.theme = theme_survminer(font.x = c(18, "bold.italic", "darkred"),
                                              font.y = c(18, "bold.italic", "darkred"),
                                              font.tickslab = c(16, "plain", "darkgreen"))
               )
  })
  
  output$survTable_SGRFS <- render_gt({
    surv_model_SGRFS <- fit_specific_GRFS()
    tbl <- tbl_survfit(surv_model_SGRFS, 
                       times = c(90, 365, 365*5, 365*10)) 
    tbl %>% as_gt() %>%
      tab_options(
        table.width = "100%"   # Full width adjustment
      )
  })
  
  output$specific_disease_summary_GRFS <- renderPrint({
    print(fit_specific_GRFS(), print.rmean = TRUE)
    summary(fit_specific_GRFS())
  })
  ###########################################################################################
  
  ###########################################################################################

  # Trends 
  dfo$Date_of_BMT = as.Date(dfo$Date_of_BMT)
  dfo$year <- year(dfo$Date_of_BMT)
  dfo <- dfo %>%
    mutate(period = cut(year, 
                        breaks = c(2007, 2014, 2019, 2025), 
                        labels = c("2008-2014", "2015-2019", "2020-2025")))
  
  dfo <- dfo %>%
    mutate(CAT = if_else(`Age at Tx` < 18, "Pediatrics", "Adult"))
  
  # Output: 
  dfo$time = pmin(dfo$Time, 365*5)
  output$trends_survival_plot <- renderPlot({
    
    if (input$trends_stratify == "Overall") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
                     data = dfo,
                     start.time = 0)
        
        ggsurvplot(fit,
                   data = dfo,
                   title = "Survival trends",  # Add your desired title here
                   subtitle = "Overall",  # Optional subtitle
                   font.title = c(16, "bold", "darkblue"),  # Customize title font
                   font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
                   pval = TRUE,
                   conf.int = FALSE, fun = "pct",
                   cumevents = TRUE,
                   cumcensor = FALSE,
                   risk.table = FALSE,
                   surv.median.line = "hv",
                   font.x = c(14, "bold.italic", "red"),
                   font.y = c(14, "bold.italic", "darkred"),
                   font.tickslab = c(12, "plain", "darkgreen"),
                   break.time.by = 365)
    } else if (input$trends_stratify == "Malignant") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Disease == "Malignant", ],
               start.time = 0)
	ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "after transplant for patients with Malignant disease",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Non Malignant") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Disease == "Non Malignant", ],
               start.time = 0)
	ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "after transplant for patients with Non malignant disease",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Paediatric") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$CAT == "Pediatrics", ],
               start.time = 0)
	ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for Pediatrics patients",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Adult") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$CAT == "Adult", ],
               start.time = 0)
	ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for Adult patients",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Allo") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_transplant == "Allo", ],
               start.time = 0)
	ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients after Allogeneic tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Auto") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_transplant == "Auto", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients after Autologous tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Allo=1") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_Transplant_4 == "Allo=1", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients with Allo tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Auto=2") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_Transplant_4 == "Auto=2", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients with Auto tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "Haplo=4") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_Transplant_4 == "Haplo=4", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients with Haplo tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "MUD=3") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$Type_of_Transplant_4 == "MUD=3", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients with MUD tranplant",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } else if (input$trends_stratify == "MAC") {
      fit <- survfit(Surv(time = time, event = OS) ~ period, 
               data = dfo[dfo$`Type of Cond. Regmn.` == "MAC", ],
               start.time = 0)

ggsurvplot(fit,
           data = dfo,
           title = "Survival trends",  # Add your desired title here
           subtitle = "for patients with MAC condition regimen",  # Optional subtitle
           font.title = c(16, "bold", "darkblue"),  # Customize title font
           font.subtitle = c(14, "italic", "purple"),  # Customize subtitle font
           pval = TRUE,
           conf.int = FALSE, fun = "pct",
           cumevents = TRUE,
           cumcensor = FALSE,
           risk.table = FALSE,
           surv.median.line = "hv",
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"),
           break.time.by = 365)
    } 

      })
      
      output$trends_surv_gttable <- render_gt({
        
        if (input$trends_stratify == "Overall") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo,
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Malignant") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Disease == "Malignant", ],
                         start.time = 0)
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Non Malignant") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Disease == "Non Malignant", ],
                         start.time = 0)
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Paediatric") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$CAT == "Pediatrics", ],
                         start.time = 0)
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Adult") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$CAT == "Adult", ],
                         start.time = 0)
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Allo") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_transplant == "Allo", ],
                         start.time = 0)
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Auto") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_transplant == "Auto", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Allo=1") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Allo=1", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Auto=2") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Auto=2", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "Haplo=4") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Haplo=4", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "MUD=3") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "MUD=3", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } else if (input$trends_stratify == "MAC") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$`Type of Cond. Regmn.` == "MAC", ],
                         start.time = 0)
          
          surv_model_SGRFS <- fit
          tbl <- tbl_survfit(surv_model_SGRFS, 
                             times = c(90, 365, 365*5, 365*10)) 
          tbl %>% as_gt() %>%
            tab_options(
              table.width = "100%"   # Full width adjustment
            )
        } 
      })
      
      output$trendssummary <- renderPrint({
        
        if (input$trends_stratify == "Overall") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo,
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Malignant") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Disease == "Malignant", ],
                         start.time = 0)
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Non Malignant") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Disease == "Non Malignant", ],
                         start.time = 0)
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Paediatric") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$CAT == "Pediatrics", ],
                         start.time = 0)
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Adult") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$CAT == "Adult", ],
                         start.time = 0)
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Allo") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_transplant == "Allo", ],
                         start.time = 0)
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Auto") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_transplant == "Auto", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Allo=1") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Allo=1", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Auto=2") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Auto=2", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "Haplo=4") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "Haplo=4", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "MUD=3") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$Type_of_Transplant_4 == "MUD=3", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } else if (input$trends_stratify == "MAC") {
          fit <- survfit(Surv(time = time, event = OS) ~ period, 
                         data = dfo[dfo$`Type of Cond. Regmn.` == "MAC", ],
                         start.time = 0)
          
          print(fit, print.rmean = TRUE)
          summary(fit)
        } 
      })
  ###########################################################################################
  
  ###########################################################################################
    
}



