# Map Module
# Interactive geospatial map with patient locations and environmental overlays

mapUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Map controls
    column(
      width = 3,
      box(
        title = "Map Controls",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        
        selectInput(ns("map_layer"),
                    "Map Layer:",
                    choices = c(
                      "Patient Locations" = "patients",
                      "Asthma Prevalence Heatmap" = "heatmap",
                      "Exacerbation Density" = "exacerbations",
                      "PM2.5 Exposure" = "pm25"
                    ),
                    selected = "patients"),
        
        checkboxInput(ns("show_stations"),
                      "Show Air Quality Stations",
                      value = TRUE),
        
        checkboxInput(ns("show_hotspots"),
                      "Show Hotspot Analysis",
                      value = FALSE),
        
        hr(),
        
        sliderInput(ns("time_filter"),
                    "Date Range:",
                    min = as.Date("2020-01-01"),
                    max = as.Date("2021-12-31"),
                    value = c(as.Date("2020-01-01"), as.Date("2021-12-31"))),
        
        hr(),
        
        actionButton(ns("reset_view"), "Reset Map View", class = "btn-info")
      ),
      
      box(
        title = "Legend",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        htmlOutput(ns("map_legend"))
      )
    ),
    
    # Map display
    column(
      width = 9,
      box(
        title = "Geospatial Analysis",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        leafletOutput(ns("main_map"), height = 600)
      )
    ),
    
    # Spatial statistics
    column(
      width = 12,
      box(
        title = "Spatial Statistics",
        status = "success",
        solidHeader = TRUE,
        width = 6,
        verbatimTextOutput(ns("spatial_stats"))
      ),
      
      box(
        title = "Environmental Summary",
        status = "warning",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("env_summary_plot"), height = 250)
      )
    )
  )
}

mapServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive for air quality stations
    stations_data <- reactive({
      # Use real Perth air quality monitoring stations
      if (file.exists("../../R/real_data_import.R")) {
        source("../../R/real_data_import.R", local = TRUE)
        return(get_perth_air_quality_stations())
      }
      
      # Fallback to basic generation
      if (file.exists("../../R/data_processing.R")) {
        source("../../R/data_processing.R", local = TRUE)
        return(generate_air_quality_stations(15))
      }
      
      # Return empty dataframe with correct structure
      return(data.frame(
        station_id = character(),
        station_name = character(),
        latitude = numeric(),
        longitude = numeric(),
        pm25_avg = numeric(),
        stringsAsFactors = FALSE
      ))
    })
    
    # Filter data by time
    time_filtered_data <- reactive({
      df <- data()
      if ("visit_date" %in% names(df)) {
        df <- df[df$visit_date >= input$time_filter[1] & 
                   df$visit_date <= input$time_filter[2], ]
      }
      return(df)
    })
    
    # Create base map
    output$main_map <- renderLeaflet({
      leaflet() %>%
        setView(lng = 115.85, lat = -31.95, zoom = 11) %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "Light"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
    
    # Update map based on selections
    observe({
      df <- time_filtered_data()
      
      if (nrow(df) == 0) return()
      
      # Get unique patients for mapping
      map_df <- df %>%
        distinct(patient_id, .keep_all = TRUE)
      
      # Clear existing layers
      leafletProxy("main_map") %>%
        clearMarkers() %>%
        clearHeatmap() %>%
        clearShapes()
      
      # Add layer based on selection
      if (input$map_layer == "patients") {
        # Color by asthma status
        pal <- colorFactor(
          palette = c("#2E86AB", "#A23B72"),
          domain = map_df$asthma_diagnosis
        )
        
        popup_content <- paste0(
          "<b>Patient ID:</b> ", map_df$patient_id, "<br>",
          "<b>Age:</b> ", map_df$age_years, " years<br>",
          "<b>Sex:</b> ", map_df$sex, "<br>",
          "<b>Asthma:</b> ", map_df$asthma_diagnosis, "<br>",
          "<b>FEV1:</b> ", round(map_df$fev1, 2), " L<br>",
          "<b>PM2.5:</b> ", round(map_df$pm25, 1), " μg/m³"
        )
        
        leafletProxy("main_map") %>%
          addCircleMarkers(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(asthma_diagnosis),
            fillColor = ~pal(asthma_diagnosis),
            radius = 5,
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 1,
            popup = popup_content,
            clusterOptions = markerClusterOptions()
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = map_df$asthma_diagnosis,
            title = "Asthma Diagnosis",
            opacity = 0.8
          )
          
      } else if (input$map_layer == "heatmap") {
        # Create asthma prevalence heatmap
        asthma_numeric <- as.numeric(map_df$asthma_diagnosis == "Yes")
        
        leafletProxy("main_map") %>%
          addHeatmap(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            intensity = asthma_numeric,
            blur = 20,
            max = 0.5,
            radius = 25
          )
          
      } else if (input$map_layer == "exacerbations") {
        # Heatmap by exacerbation count
        leafletProxy("main_map") %>%
          addHeatmap(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            intensity = ~exacerbation_count,
            blur = 20,
            max = max(map_df$exacerbation_count, na.rm = TRUE),
            radius = 25
          )
          
      } else if (input$map_layer == "pm25") {
        # PM2.5 exposure
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = map_df$pm25
        )
        
        leafletProxy("main_map") %>%
          addCircleMarkers(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(pm25),
            fillColor = ~pal(pm25),
            radius = 5,
            fillOpacity = 0.7,
            stroke = FALSE
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = map_df$pm25,
            title = "PM2.5 (μg/m³)",
            opacity = 0.8
          )
      }
      
      # Add air quality stations if selected
      if (input$show_stations) {
        stations <- stations_data()
        
        if (nrow(stations) > 0) {
          popup_content <- paste0(
            "<b>Station:</b> ", stations$station_name, "<br>",
            "<b>Type:</b> ", stations$station_type, "<br>",
            "<b>PM2.5:</b> ", stations$pm25_avg, " μg/m³<br>",
            "<b>PM10:</b> ", stations$pm10_avg, " μg/m³<br>",
            "<b>NO2:</b> ", stations$no2_avg, " ppb"
          )
          
          leafletProxy("main_map") %>%
            addAwesomeMarkers(
              data = stations,
              lng = ~longitude,
              lat = ~latitude,
              icon = awesomeIcons(
                icon = "cloud",
                library = "fa",
                markerColor = "blue"
              ),
              popup = popup_content,
              group = "Air Quality Stations"
            )
        }
      }
    })
    
    # Reset view
    observeEvent(input$reset_view, {
      leafletProxy("main_map") %>%
        setView(lng = 115.85, lat = -31.95, zoom = 11)
    })
    
    # Spatial statistics
    output$spatial_stats <- renderPrint({
      df <- time_filtered_data() %>%
        distinct(patient_id, .keep_all = TRUE)
      
      cat("Spatial Analysis Summary\n")
      cat("========================\n\n")
      
      cat("Total unique locations:", nrow(df), "\n")
      cat("Latitude range:", round(min(df$latitude), 4), "to", round(max(df$latitude), 4), "\n")
      cat("Longitude range:", round(min(df$longitude), 4), "to", round(max(df$longitude), 4), "\n\n")
      
      # Calculate basic spatial stats
      if (nrow(df) > 10) {
        # Simple clustering analysis
        asthma_rate_by_area <- df %>%
          mutate(
            lat_bin = cut(latitude, breaks = 5),
            lon_bin = cut(longitude, breaks = 5)
          ) %>%
          group_by(lat_bin, lon_bin) %>%
            summarise(
            n_patients = n(),
            asthma_rate = mean(asthma_diagnosis == "Yes"),
            avg_pm25 = mean(pm25),
            .groups = "drop"
          )
        
        cat("Asthma rate by geographic area:\n")
        print(asthma_rate_by_area)
      }
    })
    
    # Environmental summary plot
    output$env_summary_plot <- renderPlotly({
      df <- time_filtered_data()
      
      env_summary <- df %>%
        distinct(patient_id, .keep_all = TRUE) %>%
        group_by(asthma_diagnosis) %>%
        summarise(
          avg_pm25 = mean(pm25, na.rm = TRUE),
          avg_pollen = mean(pollen_index, na.rm = TRUE),
          avg_temp = mean(temperature, na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(env_summary, aes(x = asthma_diagnosis)) +
        geom_bar(aes(y = avg_pm25, fill = "PM2.5"), stat = "identity", alpha = 0.7) +
        geom_point(aes(y = avg_pollen * 2, color = "Pollen"), size = 4) +
        scale_y_continuous(
          name = "PM2.5 (μg/m³)",
          sec.axis = sec_axis(~./2, name = "Pollen Index")
        ) +
        scale_fill_manual(values = c("PM2.5" = "#E74C3C")) +
        scale_color_manual(values = c("Pollen" = "#27AE60")) +
        labs(title = "Environmental Exposures by Asthma Status",
             x = "Asthma Diagnosis") +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Map legend
    output$map_legend <- renderUI({
      HTML("
        <div style='font-size: 12px;'>
          <p><strong>Data Sources:</strong></p>
          <ul>
            <li>Individual patients: Synthetic data (calibrated to ABS/AIHW)</li>
            <li>Air quality stations: Real WA monitoring network</li>
          </ul>
          <p><strong>Map Legend:</strong></p>
          <ul>
            <li><span style='color: #2E86AB;'>●</span> No Asthma</li>
            <li><span style='color: #A23B72;'>●</span> Asthma Diagnosed</li>
            <li><span style='color: blue;'>☁</span> Air Quality Station</li>
          </ul>
          <p><em>Click on markers for details. Use cluster markers to zoom.</em></p>
        </div>
      ")
    })
  })
}
