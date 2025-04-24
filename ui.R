
library(survival)
library(survminer)
library(shiny)
library(shinydashboard)
library(shinymanager)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(shiny)
library(gargle)
library(lubridate)
library(cmprsk)
library(RColorBrewer)
library(gtsummary)
library(gt)
library(cardx)

gslink <- "https://docs.google.com/spreadsheets/d/1FwfxfMlQRBppNx8P9rJDjGPX1LqPKU44hdlfUHKoXxo/edit?gid=330088273#gid=330088273"

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

# Define UI
ui <- fluidPage(
                dashboardPage(
                  dashboardHeader(title = "BMT Data Dashboard"),
                  dashboardSidebar(
                    sidebarMenu(
                      
                      menuItem("Cumulative Incidence",
                               tabName = "cumu",
                               icon = icon("line-chart")),
                      
                      menuItem("OS Survival Curve",
                               tabName = "os_survival", icon =
                                 icon("line-chart")),
                      
                      menuItem("EFS Survival Curve", 
                               tabName = "efs_survival", 
                               icon = icon("line-chart")),
                      
                      menuItem("Specific Disease", 
                               tabName = "specific_disease",
                               icon = icon("line-chart")),
                      
                      menuItem("Survival Trends", 
                               tabName = "TRENDS",
                               icon = icon("line-chart"))
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      
                      ###########################################################################################
                      
                     tabItem(tabName = "cumu",
                              fluidRow(
                                
                                # Include Material Icons
                                tags$head(
                                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
                                ),
                                tags$div(
                                  style = "display: flex; align-items: center;",  # Align items in a row
                                  tags$a(
                                    href = gslink,
                                    "Open Google Sheets", 
                                    target = "_blank",
                                    style = "margin-left: auto;"), # Opens in a new tab
                                  tags$span(class = "material-icons", 
                                            style = "font-size: 24px; margin-left: 12px;", 
                                            "table_chart")  # Google Sheets icon
                                ),
                               
                                box(title = "Date Filter", width = 12, status = "primary",
                                    
                                    dateRangeInput("date_range", "Select Date Range:",
                                                   start = as.Date(Sys.Date()) - years(100), 
                                                   end = Sys.Date(),
                                                   format =  "dd-mm-yyyy")),
                                
                                
                                box(title = "Cumulative incidence", width = 12, status = "primary",
                                    
                                    selectInput("Category", "Stratify by", 
                                                choices = c("Disease", 
                                                            "Type of transplant", 
                                                            "Overall",
                                                            "CAT")),
                                    plotOutput("cumulative")),
                                
                                
                                box(title = "Cumulative Incidence Estimates", width = 12, status = "primary",
                                    
                                    verbatimTextOutput("estimates")),
                                
                                box(title = "Cumulative incidence Progression", width = 12, status = "primary",
                                    
                                    selectInput("Category_pfs", "Stratify by", 
                                                choices = c("Disease", 
                                                            "Type of transplant", 
                                                            "Overall",
                                                            "CAT")),
                                    plotOutput("cumulative_pfs")),
                                
                                
                                box(title = "Cumulative Incidence Estimates Progression", width = 12, status = "primary",
                                    
                                    verbatimTextOutput("estimates_pfs"))
                                
                              )),
                      
                      ###########################################################################################
                      
                      tabItem(tabName = "os_survival",
                              fluidRow(
                                
                                # Include Material Icons
                                tags$head(
                                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
                                ),
                                tags$div(
                                  style = "display: flex; align-items: center;",  # Align items in a row
                                  tags$a(
                                    href = gslink,
                                    "Open Google Sheets", 
                                    target = "_blank",
                                    style = "margin-left: auto;"), # Opens in a new tab
                                  tags$span(class = "material-icons", 
                                            style = "font-size: 24px; margin-left: 12px;", 
                                            "table_chart")  # Google Sheets icon
                                ),
                                
                                # Add Date Range Filter for OS Survival Tab
                                box(title = "Date Filter", width = 12, status = "primary",
                                    
                                    dateRangeInput("date_range", "Select Date Range:",
                                                   start = as.Date(Sys.Date()) - years(100), 
                                                   end = Sys.Date(),
                                                   format =  "dd-mm-yyyy")),
                                
                                
                                box(title = "OS Survival Curve", width = 12, status = "primary",
                                    
                                    selectInput("os_stratify", "Stratify by", 
                                                choices = c("Disease", 
                                                            "Type of transplant",
                                                            "4 Types of transplant",
                                                            "Overall",
                                                            "CAT")),
                                    plotOutput("os_survival_plot",
                                               height = "600px")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_OS")
                                ),
                                
                                box(title = "OS summary", width = 12, status = "primary",
                                    
                                    verbatimTextOutput("os_summary"))
                              )),
                      
                      ###########################################################################################
                      
                      tabItem(tabName = "efs_survival",
                              fluidRow(
                                
                                # Include Material Icons
                                tags$head(
                                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
                                ),
                                tags$div(
                                  style = "display: flex; align-items: center;",  # Align items in a row
                                  tags$a(
                                    href = gslink,
                                    "Open Google Sheets", 
                                    target = "_blank",
                                    style = "margin-left: auto;"), # Opens in a new tab
                                  tags$span(class = "material-icons", 
                                            style = "font-size: 24px; margin-left: 12px;", 
                                            "table_chart")  # Google Sheets icon
                                ),
                                
                                # Add Date Range Filter for EFS Survival Tab
                                box(title = "Date Filter", width = 12, status = "primary",
                                    
                                    dateRangeInput("date_range", "Select Date Range:",
                                                   start = as.Date(Sys.Date()) - years(100),
                                                   end = Sys.Date(),
                                                   format =  "dd-mm-yyyy")),
                                
                                
                                box(title = "EFS Survival Curve", width = 12, status = "primary",
                                    
                                    selectInput("efs_stratify", "Stratify by", 
                                                choices = c("Disease", 
                                                            "Type of transplant", 
                                                            "Overall",
                                                            "CAT")),
                                    plotOutput("efs_survival_plot")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_EFS")
                                ),
                                
                                box(title = "EFS summary", width = 12, status = "primary",
                                    
                                    verbatimTextOutput("efs_summary"))
                              )),
                      
                      ###########################################################################################
                      
                      tabItem(tabName = "specific_disease",
                              fluidRow(
                                
                                # Include Material Icons
                                tags$head(
                                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
                                ),
                                tags$div(
                                  style = "display: flex; align-items: center;",  # Align items in a row
                                  tags$a(
                                    href = gslink,
                                    "Open Google Sheets", 
                                    target = "_blank",
                                    style = "margin-left: auto;"), # Opens in a new tab
                                  tags$span(class = "material-icons", 
                                            style = "font-size: 24px; margin-left: 12px;", 
                                            "table_chart")  # Google Sheets icon
                                ),
                                
                                # Add Date Range Filter for Specific Disease Tab
                                box(title = "Date Filter", width = 12, status = "primary",
                                    
                                    dateRangeInput("date_range", "Select Date Range:",
                                                   start = as.Date(Sys.Date()) - years(100), 
                                                   end = Sys.Date(),
                                                   format =  "dd-mm-yyyy")),
                                
                                
                                box(title = "Specific Disease Curve", width = 12, status = "primary",
                                    
                                    selectInput("specific_disease_stratify", "Stratify by", 
                                                choices = c("Overall",
                                                            "ALL", "AML", 
                                                            "AML Relapse CR1", 
                                                            "AML Relapse CR2", 
                                                            "AML Relapse CR3", 
                                                            "AML with HLH", "CML",
                                                            "DLBCL", "HD", "HL",
                                                            "MCL", "MDS", "MM/PCD", "MPN",
                                                            "N K T-Cell Lymphoma", 
                                                            "Neuroblastoma", "NHL",
                                                            "Relapse AML", 
                                                            "Relapse HL", "SAA", 
                                                            "T cell Lymphoma", "TM", 
                                                            "WAS", "Yalk Cell Tumor"),
                                                selectize =  TRUE),
                                    plotOutput("Specific_disease_survival_plot", 
                                               height = "600px")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_SO")
                                  ),
                                
                                box(title = "Specific disease summary", width = 12, 
                                    status = "primary",
                                    verbatimTextOutput("specific_disease_summary")),
                                
                                box(title = "Specific Disease Curve EFS", width = 12, status = "primary",
                                    
                                    selectInput("specific_disease_stratify", "Stratify by", 
                                                choices = c("Overall",
                                                            "ALL", "AML", 
                                                            "AML Relapse CR1", 
                                                            "AML Relapse CR2", 
                                                            "AML Relapse CR3", 
                                                            "AML with HLH", "CML",
                                                            "DLBCL", "HD", "HL",
                                                            "MCL", "MDS", "MM/PCD", "MPN",
                                                            "N K T-Cell Lymphoma", 
                                                            "Neuroblastoma", "NHL",
                                                            "Relapse AML", 
                                                            "Relapse HL", "SAA", 
                                                            "T cell Lymphoma", "TM", 
                                                            "WAS", "Yalk Cell Tumor"),
                                                selectize =  TRUE),
                                    plotOutput("Specific_disease_survival_plot_EFS", 
                                               height = "600px")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_SEFS")
                                ),
                                
                                box(title = "Specific disease summary EFS", width = 12, 
                                    status = "primary",
                                    
                                    verbatimTextOutput("specific_disease_summary_EFS")),
                                
                                box(title = "Specific Disease Curve PFS", width = 12, status = "primary",
                                    
                                    selectInput("specific_disease_stratify", "Stratify by", 
                                                choices = c("Overall",
                                                            "ALL", "AML", 
                                                            "AML Relapse CR1", 
                                                            "AML Relapse CR2", 
                                                            "AML Relapse CR3", 
                                                            "AML with HLH", "CML",
                                                            "DLBCL", "HD", "HL",
                                                            "MCL", "MDS", "MM/PCD", "MPN",
                                                            "N K T-Cell Lymphoma", 
                                                            "Neuroblastoma", "NHL",
                                                            "Relapse AML", 
                                                            "Relapse HL", "SAA", 
                                                            "T cell Lymphoma", "TM", 
                                                            "WAS", "Yalk Cell Tumor"),
                                                selectize =  TRUE),
                                    plotOutput("Specific_disease_survival_plot_PFS", 
                                               height = "600px")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_SPFS")
                                ),
                                
                                box(title = "Specific disease summary PFS", width = 12, 
                                    status = "primary",
                                    
                                    verbatimTextOutput("specific_disease_summary_PFS")),
                                
                                box(title = "Specific Disease Curve GRFS", width = 12, status = "primary",
                                    
                                    selectInput("specific_disease_stratify", "Stratify by", 
                                                choices = c("Overall",
                                                            "ALL", "AML", 
                                                            "AML Relapse CR1", 
                                                            "AML Relapse CR2", 
                                                            "AML Relapse CR3", 
                                                            "AML with HLH", "CML",
                                                            "DLBCL", "HD", "HL",
                                                            "MCL", "MDS", "MM/PCD", "MPN",
                                                            "N K T-Cell Lymphoma", 
                                                            "Neuroblastoma", "NHL",
                                                            "Relapse AML", 
                                                            "Relapse HL", "SAA", 
                                                            "T cell Lymphoma", "TM", 
                                                            "WAS", "Yalk Cell Tumor"),
                                                selectize =  TRUE),
                                    plotOutput("Specific_disease_survival_plot_GRFS", 
                                               height = "600px")),
                                
                                mainPanel(
                                  h3("Survival table"),
                                  gt_output("survTable_SGRFS")
                                ),
                                
                                box(title = "Specific disease summary GRFS", width = 12, 
                                    status = "primary",
                                    
                                    verbatimTextOutput("specific_disease_summary_GRFS"))
                                
                              )),
                     
                      ###########################################################################################
                     
                     tabItem(tabName = "TRENDS",
                             fluidRow(
                               
                               # Include Material Icons
                               tags$head(
                                 tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
                               ),
                               tags$div(
                                 style = "display: flex; align-items: center;",  # Align items in a row
                                 tags$a(
                                   href = gslink,
                                   "Open Google Sheets", 
                                   target = "_blank",
                                   style = "margin-left: auto;"), # Opens in a new tab
                                 tags$span(class = "material-icons", 
                                           style = "font-size: 24px; margin-left: 12px;", 
                                           "table_chart")  # Google Sheets icon
                               ),
                               
                               box(title = "Trends in Survival", width = 12, status = "primary",
                                   
                                   selectInput("trends_stratify", "Stratify by", 
                                               choices = c("Overall",
                                                           "Malignant", "Non Malignant",
                                                           "Paediatric", "Adult",
                                                           "Allo", "Auto",
                                                           "Allo=1", "Auto=2", "Haplo=4", "MUD=3",
                                                           "MAC"),
                                               selectize =  TRUE),
                                   plotOutput("trends_survival_plot", 
                                              height = "600px")),
                               
                               mainPanel(
                                 h3("Survival table"),
                                 gt_output("trends_surv_gttable")
                               ),
                               
                               box(title = "Trends summary", width = 12, 
                                   status = "primary",
                                   verbatimTextOutput("trendssummary"))
                               
                             ))
                     ###########################################################################################
                     
                     
                    )
                  )
                )
)

