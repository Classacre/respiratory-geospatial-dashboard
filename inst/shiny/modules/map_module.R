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
                      "PM2.5 Exposure" = "pm25",
                      "Risk Tier" = "risk_tier",
                      "Healthcare Access" = "healthcare_access",
                      "Opportunity Index" = "opportunity"
                    ),
                    selected = "patients"),

        checkboxInput(ns("show_stations"),
                      "Show Air Quality Stations",
                      value = TRUE),

        checkboxInput(ns("show_hotspots"),
                      "Show Hotspot Analysis",
                      value = FALSE),

        checkboxInput(ns("show_significance"),
                      "Show Monte Carlo Significance",
                      value = FALSE),

        hr(),

        sliderInput(ns("time_filter"),
                    "Date Range:",
                    min = as.Date("2020-01-01"),
                    max = as.Date("2021-12-31"),
                    value = c(as.Date("2020-01-01"), as.Date("2021-12-31"))),

        # Time animation controls
        conditionalPanel(
          condition = paste0("input['", ns("map_layer"), "'] == 'patients'"),
          hr(),
          h4("Time Animation"),
          checkboxInput(ns("animate_time"), "Enable Animation", value = FALSE),
          sliderInput(ns("animation_speed"), "Animation Speed (sec/frame):",
                      min = 0.5, max = 3, value = 1, step = 0.5),
          actionButton(ns("play_animation"), "Play", icon = icon("play")),
          actionButton(ns("pause_animation"), "Pause", icon = icon("pause")),
          actionButton(ns("reset_animation"), "Reset", icon = icon("undo"))
        ),

        hr(),

        # Alert threshold settings
        h4("Alert System"),
        numericInput(ns("alert_threshold"),
                     "Alert Threshold (cases per 1000):",
                     value = 100,
                     min = 0,
                     max = 500),
        checkboxInput(ns("show_alerts"), "Show Alert Areas", value = FALSE),

        hr(),

        actionButton(ns("reset_view"), "Reset Map View", class = "btn-info")
      ),

      box(
        title = "Alert Status",
        status = "warning",
        solidHeader = TRUE,
        width = NULL,
        uiOutput(ns("alert_panel"))
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
    ),

    # Cumulative tracking panel
    column(
      width = 12,
      conditionalPanel(
        condition = paste0("input['", ns("animate_time"), "'] == true"),
        box(
          title = "Cumulative Case Tracking",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput(ns("cumulative_plot"), height = 200)
        )
      )
    )
  )
}

mapServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # Reactive values for animation state
    animation_state <- reactiveValues(
      is_playing = FALSE,
      current_frame = 1,
      max_frames = 12  # Monthly frames
    )

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

    # Generate healthcare facilities data
    healthcare_facilities <- reactive({
      data.frame(
        facility_id = 1:8,
        facility_name = c(
          "Perth Children's Hospital",
          "Sir Charles Gairdner Hospital",
          "Royal Perth Hospital",
          "Fiona Stanley Hospital",
          "St John of God Subiaco",
          "Joondalup Health Campus",
          "Armadale Hospital",
          "Midland Hospital"
        ),
        latitude = c(-31.944, -31.969, -31.954, -32.073, -31.948,
                     -31.742, -32.148, -31.892),
        longitude = c(115.819, 115.821, 115.869, 115.845, 115.824,
                      115.768, 116.012, 116.007),
        type = c("Children's", "General", "General", "General", "Private",
                 "General", "General", "General")
      )
    })

    # Calculate healthcare access
    healthcare_access_data <- reactive({
      df <- data() %>%
        distinct(patient_id, .keep_all = TRUE)

      if (nrow(df) == 0) return(NULL)

      # Calculate travel times
      access_data <- calculate_travel_time(
        df,
        healthcare_facilities(),
        avg_speed_kmh = 40,
        mode = "driving"
      )

      # Identify deserts
      desert_analysis <- identify_deserts(access_data)
      return(desert_analysis)
    })

    # Calculate risk stratification
    risk_stratification_data <- reactive({
      df <- data() %>%
        distinct(patient_id, .keep_all = TRUE)

      if (nrow(df) == 0) return(NULL)

      # Check if required columns exist
      env_vars <- c("pm25", "pollen_index")
      demo_vars <- c("age_years", "ses_score")
      clin_vars <- c("exacerbation_count", "hospitalization_count")

      available_env <- intersect(env_vars, names(df))
      available_demo <- intersect(demo_vars, names(df))
      available_clin <- intersect(clin_vars, names(df))

      if (length(available_env) == 0 && length(available_demo) == 0 && length(available_clin) == 0) {
        return(NULL)
      }

      risk_result <- stratify_risk(
        df,
        environmental_vars = if (length(available_env) > 0) available_env else NULL,
        demographic_vars = if (length(available_demo) > 0) available_demo else NULL,
        clinical_vars = if (length(available_clin) > 0) available_clin else NULL
      )

      return(risk_result$data)
    })

    # Calculate opportunity index
    opportunity_data <- reactive({
      df <- data() %>%
        distinct(patient_id, .keep_all = TRUE)

      if (nrow(df) == 0) return(NULL)

      # Check if we have distance to station, calculate if not
      if (!"distance_to_station" %in% names(df)) {
        stations <- stations_data()
        if (nrow(stations) > 0) {
          df$distance_to_station <- sapply(1:nrow(df), function(i) {
            patient_coord <- c(df$longitude[i], df$latitude[i])
            station_coords <- as.matrix(stations[, c("longitude", "latitude")])
            min(geosphere::distHaversine(patient_coord, station_coords) / 1000)
          })
        } else {
          df$distance_to_station <- runif(nrow(df), 1, 20)
        }
      }

      # Calculate opportunity index
      opp_result <- calculate_opportunity_index(df)
      return(opp_result)
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

    # Animation frame data
    animation_data <- reactive({
      if (!input$animate_time) return(NULL)

      df <- data()
      if ("visit_date" %in% names(df)) {
        # Create monthly frames
        df$year_month <- format(df$visit_date, "%Y-%m")
        months <- sort(unique(df$year_month))

        frame_idx <- min(animation_state$current_frame, length(months))
        current_month <- months[frame_idx]

        df <- df[df$year_month <= current_month, ]
      }
      return(df)
    })

    # Cumulative case counts for animation
    cumulative_cases <- reactive({
      df <- data()
      if (!"visit_date" %in% names(df)) return(NULL)

      df$year_month <- format(df$visit_date, "%Y-%m")
      months <- sort(unique(df$year_month))

      cumulative <- data.frame(
        month = months,
        total_cases = sapply(months, function(m) {
          sum(df$year_month <= m, na.rm = TRUE)
        }),
        new_cases = sapply(months, function(m) {
          sum(df$year_month == m, na.rm = TRUE)
        })
      )

      return(cumulative)
    })

    # Alert areas detection
    alert_areas <- reactive({
      df <- time_filtered_data() %>%
        distinct(patient_id, .keep_all = TRUE)

      if (nrow(df) == 0) return(NULL)

      # Create grid and calculate rates
      lat_breaks <- seq(min(df$latitude), max(df$latitude), length.out = 8)
      lon_breaks <- seq(min(df$longitude), max(df$longitude), length.out = 8)

      df$lat_bin <- cut(df$latitude, breaks = lat_breaks, include.lowest = TRUE)
      df$lon_bin <- cut(df$longitude, breaks = lon_breaks, include.lowest = TRUE)

      grid_summary <- df %>%
        group_by(lat_bin, lon_bin) %>%
        summarise(
          n = n(),
          asthma_cases = sum(asthma_diagnosis == "Yes", na.rm = TRUE),
          rate_per_1000 = asthma_cases / n * 1000,
          lat_center = mean(latitude, na.rm = TRUE),
          lon_center = mean(longitude, na.rm = TRUE),
          .groups = "drop"
        )

      # Identify alert areas
      grid_summary$alert <- grid_summary$rate_per_1000 > input$alert_threshold

      return(grid_summary)
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

    # Animation observer
    observe({
      if (animation_state$is_playing && input$animate_time) {
        invalidateLater(input$animation_speed * 1000, session)

        if (animation_state$current_frame < animation_state$max_frames) {
          animation_state$current_frame <- animation_state$current_frame + 1
        } else {
          animation_state$is_playing <- FALSE
        }
      }
    })

    # Play button
    observeEvent(input$play_animation, {
      animation_state$is_playing <- TRUE
    })

    # Pause button
    observeEvent(input$pause_animation, {
      animation_state$is_playing <- FALSE
    })

    # Reset button
    observeEvent(input$reset_animation, {
      animation_state$is_playing <- FALSE
      animation_state$current_frame <- 1
    })

    # Update map based on selections
    observe({
      # Use animation data if enabled, otherwise use time-filtered data
      if (input$animate_time && !is.null(animation_data())) {
        df <- animation_data()
      } else {
        df <- time_filtered_data()
      }

      if (nrow(df) == 0) return()

      # Get unique patients for mapping
      map_df <- df %>%
        distinct(patient_id, .keep_all = TRUE)

      # Clear existing layers
      leafletProxy("main_map") %>%
        clearMarkers() %>%
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
        # Create asthma prevalence heatmap using circle markers
        asthma_numeric <- as.numeric(map_df$asthma_diagnosis == "Yes")
        
        # Use color intensity for asthma prevalence
        pal <- colorNumeric(
          palette = c("blue", "yellow", "red"),
          domain = asthma_numeric
        )

        leafletProxy("main_map") %>%
          addCircleMarkers(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(asthma_numeric),
            fillColor = ~pal(asthma_numeric),
            radius = 8,
            fillOpacity = 0.6,
            stroke = FALSE,
            popup = paste0(
              "<b>Asthma:</b> ", map_df$asthma_diagnosis, "<br>",
              "<b>Patient ID:</b> ", map_df$patient_id
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = asthma_numeric,
            title = "Asthma (1=Yes, 0=No)",
            opacity = 0.8
          )

      } else if (input$map_layer == "exacerbations") {
        # Exacerbation density using circle markers
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = map_df$exacerbation_count
        )

        leafletProxy("main_map") %>%
          addCircleMarkers(
            data = map_df,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(exacerbation_count),
            fillColor = ~pal(exacerbation_count),
            radius = ~sqrt(exacerbation_count + 1) * 3,
            fillOpacity = 0.6,
            stroke = FALSE,
            popup = paste0(
              "<b>Exacerbations:</b> ", map_df$exacerbation_count, "<br>",
              "<b>Patient ID:</b> ", map_df$patient_id
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = map_df$exacerbation_count,
            title = "Exacerbation Count",
            opacity = 0.8
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

      } else if (input$map_layer == "risk_tier") {
        # Risk tier visualization
        risk_data <- risk_stratification_data()

        if (!is.null(risk_data) && "risk_tier" %in% names(risk_data)) {
          pal <- colorFactor(
            palette = c("#2ECC71", "#F1C40F", "#E74C3C"),
            domain = risk_data$risk_tier
          )

          leafletProxy("main_map") %>%
            addCircleMarkers(
              data = risk_data,
              lng = ~longitude,
              lat = ~latitude,
              color = ~pal(risk_tier),
              fillColor = ~pal(risk_tier),
              radius = 6,
              fillOpacity = 0.8,
              stroke = TRUE,
              weight = 1,
              popup = paste0(
                "<b>Risk Tier:</b> ", risk_data$risk_tier, "<br>",
                "<b>Risk Score:</b> ", round(risk_data$composite_risk_score, 2)
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = risk_data$risk_tier,
              title = "Risk Tier",
              opacity = 0.8
            )
        }

      } else if (input$map_layer == "healthcare_access") {
        # Healthcare access layer - simplified version
        access_analysis <- healthcare_access_data()

        if (!is.null(access_analysis)) {
          # Show travel time as circle colors
          access_data <- access_analysis$data
          if (!is.null(access_data) && "travel_time_min" %in% names(access_data)) {
            pal <- colorNumeric(
              palette = c("green", "yellow", "red"),
              domain = access_data$travel_time_min
            )
            
            leafletProxy("main_map") %>%
              addCircleMarkers(
                data = access_data,
                lng = ~longitude,
                lat = ~latitude,
                color = ~pal(travel_time_min),
                fillColor = ~pal(travel_time_min),
                radius = 5,
                fillOpacity = 0.7,
                stroke = FALSE,
                popup = paste0(
                  "<b>Travel Time:</b> ", round(access_data$travel_time_min, 1), " min<br>",
                  "<b>Nearest Facility:</b> ", access_data$nearest_facility
                )
              ) %>%
              addLegend(
                position = "bottomright",
                pal = pal,
                values = access_data$travel_time_min,
                title = "Travel Time (min)",
                opacity = 0.8
              )
          }

          # Add facility markers
          facilities <- healthcare_facilities()
          leafletProxy("main_map") %>%
            addAwesomeMarkers(
              data = facilities,
              lng = ~longitude,
              lat = ~latitude,
              icon = awesomeIcons(
                icon = "hospital",
                library = "fa",
                markerColor = "red"
              ),
              popup = paste0("<b>", facilities$facility_name, "</b><br>Type: ", facilities$type),
              group = "Healthcare Facilities"
            )
        }

      } else if (input$map_layer == "opportunity") {
        # Opportunity index layer
        opp_data <- opportunity_data()

        if (!is.null(opp_data)) {
          pal <- colorNumeric(
            palette = "RdYlGn",
            domain = opp_data$opportunity_index,
            reverse = FALSE
          )

          leafletProxy("main_map") %>%
            addCircleMarkers(
              data = opp_data,
              lng = ~longitude,
              lat = ~latitude,
              color = ~pal(opportunity_index),
              fillColor = ~pal(opportunity_index),
              radius = 6,
              fillOpacity = 0.8,
              stroke = FALSE,
              popup = paste0(
                "<b>Opportunity Index:</b> ", round(opp_data$opportunity_index, 1), "<br>",
                "<b>Quintile:</b> ", opp_data$opportunity_quintile, "<br>",
                "<b>SES Component:</b> ", round(opp_data$ses_component, 1), "<br>",
                "<b>Environment Component:</b> ", round(opp_data$environment_component, 1)
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pal,
              values = opp_data$opportunity_index,
              title = "Opportunity Index",
              opacity = 0.8
            )
        }
      }

      # Add hotspot analysis if selected
      if (input$show_hotspots) {
        if (input$show_significance && requireNamespace("spatial_analysis", quietly = TRUE)) {
          # Use Monte Carlo hotspot test
          hotspot_result <- monte_carlo_hotspot_test(
            map_df,
            outcome_col = "exacerbation_count",
            n_simulations = 199
          )

          sig_data <- hotspot_result$data[hotspot_result$data$hotspot_class != "Not Significant", ]

          if (nrow(sig_data) > 0) {
            sig_colors <- c("Hotspot (High-High)" = "#D73027",
                           "Coldspot (Low-Low)" = "#4575B4")

            leafletProxy("main_map") %>%
              addCircleMarkers(
                data = sig_data,
                lng = ~longitude,
                lat = ~latitude,
                color = ~sig_colors[hotspot_class],
                fillColor = ~sig_colors[hotspot_class],
                radius = 8,
                fillOpacity = 0.9,
                stroke = TRUE,
                weight = 2,
                popup = paste0(
                  "<b>Hotspot Class:</b> ", sig_data$hotspot_class, "<br>",
                  "<b>P-value:</b> ", format.pval(sig_data$p_value, eps = 0.001)
                ),
                group = "Hotspots"
              )
          }
        } else {
          # Simple hotspot detection
          hotspots <- detect_hotspots(map_df, outcome_col = "exacerbation_count")
          sig_hotspots <- hotspots[hotspots$hotspot_class != "Not Significant", ]

          if (nrow(sig_hotspots) > 0) {
            hotspot_colors <- c("Hotspot (High-High)" = "red",
                               "Coldspot (Low-Low)" = "blue")

            leafletProxy("main_map") %>%
              addCircleMarkers(
                data = sig_hotspots,
                lng = ~longitude,
                lat = ~latitude,
                color = ~hotspot_colors[hotspot_class],
                fillColor = ~hotspot_colors[hotspot_class],
                radius = 8,
                fillOpacity = 0.8,
                stroke = TRUE,
                weight = 2,
                group = "Hotspots"
              )
          }
        }
      }

      # Add alert areas if selected
      if (input$show_alerts) {
        alerts <- alert_areas()
        if (!is.null(alerts)) {
          alert_regions <- alerts[alerts$alert, ]

          if (nrow(alert_regions) > 0) {
            leafletProxy("main_map") %>%
              addRectangles(
                data = alert_regions,
                lng1 = ~lon_center - 0.015,
                lat1 = ~lat_center - 0.015,
                lng2 = ~lon_center + 0.015,
                lat2 = ~lat_center + 0.015,
                fillColor = "red",
                fillOpacity = 0.3,
                color = "darkred",
                weight = 2,
                popup = paste0(
                  "<b>ALERT:</b> High asthma rate detected<br>",
                  "<b>Rate:</b> ", round(alert_regions$rate_per_1000, 1), " per 1000<br>",
                  "<b>Cases:</b> ", alert_regions$asthma_cases, " / ", alert_regions$n
                ),
                group = "Alerts"
              )
          }
        }
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

    # Alert panel output
    output$alert_panel <- renderUI({
      alerts <- alert_areas()
      if (is.null(alerts)) return(NULL)

      n_alerts <- sum(alerts$alert, na.rm = TRUE)

      if (n_alerts > 0) {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          strong(paste(n_alerts, "alert areas detected!")),
          br(),
          paste("Threshold:", input$alert_threshold, "per 1000")
        )
      } else {
        div(
          class = "alert alert-success",
          icon("check-circle"),
          "No alert areas detected."
        )
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

      # Animation status
      if (input$animate_time) {
        cat("Animation Status:\n")
        cat("  Frame:", animation_state$current_frame, "/", animation_state$max_frames, "\n")
        cat("  Status:", ifelse(animation_state$is_playing, "Playing", "Paused"), "\n\n")
      }

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

    # Cumulative case tracking plot
    output$cumulative_plot <- renderPlotly({
      cum_data <- cumulative_cases()
      if (is.null(cum_data)) return(NULL)

      p <- ggplot(cum_data, aes(x = month)) +
        geom_bar(aes(y = new_cases, fill = "New Cases"), stat = "identity", alpha = 0.6) +
        geom_line(aes(y = total_cases, color = "Cumulative", group = 1), size = 1.5) +
        geom_point(aes(y = total_cases, color = "Cumulative"), size = 3) +
        scale_fill_manual(values = c("New Cases" = "steelblue")) +
        scale_color_manual(values = c("Cumulative" = "darkred")) +
        labs(
          title = "Cumulative Case Tracking",
          x = "Month",
          y = "Number of Cases"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
            <li>Healthcare facilities: Perth metro hospitals/clinics</li>
          </ul>
          <p><strong>Map Legend:</strong></p>
          <ul>
            <li><span style='color: #2E86AB;'>●</span> No Asthma</li>
            <li><span style='color: #A23B72;'>●</span> Asthma Diagnosed</li>
            <li><span style='color: blue;'>☁</span> Air Quality Station</li>
            <li><span style='color: red;'>✚</span> Healthcare Facility</li>
          </ul>
          <p><strong>New Features:</strong></p>
          <ul>
            <li>▶ Time-series animation</li>
            <li>⚠ Alert system for high-rate areas</li>
            <li>🏘 Risk tier visualization</li>
            <li>🏥 Healthcare access overlay</li>
          </ul>
          <p><em>Click on markers for details. Use cluster markers to zoom.</em></p>
        </div>
      ")
    })
  })
}
