# Respiratory Geospatial Dashboard
#
# This is the main Shiny application file for the pediatric respiratory
# health geospatial analysis dashboard.

library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# Source modules
source("modules/overview_module.R")
source("modules/map_module.R")
source("modules/trajectories_module.R")
source("modules/risk_module.R")
source("modules/data_module.R")

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Pediatric Respiratory Health Dashboard",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geospatial Map", tabName = "map", icon = icon("map")),
      menuItem("Lung Function", tabName = "trajectories", icon = icon("chart-line")),
      menuItem("Risk Prediction", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      
      hr(),
      
      # Global filters
      div(style = "padding: 15px;",
          h4("Global Filters"),
          
          sliderInput("age_range",
                      "Age Range (years):",
                      min = 2, max = 18,
                      value = c(2, 18)),
          
          selectInput("sex_filter",
                      "Sex:",
                      choices = c("All", "Male", "Female"),
                      selected = "All"),
          
          selectInput("asthma_filter",
                      "Asthma Diagnosis:",
                      choices = c("All", "Yes", "No"),
                      selected = "All")
      )
    )
  ),
  
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        .box.box-success {
          border-top-color: #00a65a;
        }
        .box.box-warning {
          border-top-color: #f39c12;
        }
        .box.box-danger {
          border-top-color: #dd4b39;
        }
        .info-box {
          min-height: 90px;
        }
        .info-box-icon {
          height: 90px;
          line-height: 90px;
        }
        .info-box-content {
          padding-top: 10px;
          padding-bottom: 10px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              overviewUI("overview")
      ),
      
      # Map Tab
      tabItem(tabName = "map",
              mapUI("map")
      ),
      
      # Trajectories Tab
      tabItem(tabName = "trajectories",
              trajectoriesUI("trajectories")
      ),
      
      # Risk Tab
      tabItem(tabName = "risk",
              riskUI("risk")
      ),
      
      # Data Tab
      tabItem(tabName = "data",
              dataUI("data")
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Load or generate data
  data <- reactive({
    # Try to load package data, otherwise generate synthetic
    if (exists("respiratory_data", envir = .GlobalEnv)) {
      return(get("respiratory_data", envir = .GlobalEnv))
    }
    
    # Generate synthetic data if not available
    if (file.exists("../../data/respiratory_data.rda")) {
      load("../../data/respiratory_data.rda")
      return(respiratory_data)
    }
    
    # Fallback: generate on the fly
    source("../../R/data_processing.R", local = TRUE)
    generate_synthetic_data(n_patients = 5000)
  })
  
  # Filtered data based on global filters
  filtered_data <- reactive({
    df <- data()
    
    # Apply age filter
    df <- df[df$age_years >= input$age_range[1] & 
               df$age_years <= input$age_range[2], ]
    
    # Apply sex filter
    if (input$sex_filter != "All") {
      df <- df[df$sex == input$sex_filter, ]
    }
    
    # Apply asthma filter
    if (input$asthma_filter != "All") {
      df <- df[df$asthma_diagnosis == input$asthma_filter, ]
    }
    
    return(df)
  })
  
  # Call module servers
  overviewServer("overview", filtered_data)
  mapServer("map", filtered_data)
  trajectoriesServer("trajectories", filtered_data)
  riskServer("risk", filtered_data)
  dataServer("data", filtered_data)
}

# Run the application
shinyApp(ui = ui, server = server)
