#' Geospatial Utility Functions
#'
#' Helper functions for creating maps, handling spatial data, and
#' generating visualizations for the Shiny dashboard.

#' Create Perth Metro Basemap
#'
#' Creates a leaflet map centered on Perth with appropriate zoom and tiles.
#'
#' @return A leaflet map object
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap()
#' }
create_perth_basemap <- function() {
  leaflet::leaflet() %>%
    leaflet::setView(lng = 115.85, lat = -31.95, zoom = 11) %>%
    leaflet::addTiles(group = "OpenStreetMap") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Light") %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter, group = "Dark") %>%
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Light", "Dark"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
}

#' Add Patient Markers to Map
#'
#' Adds patient location markers to a leaflet map.
#'
#' @param map A leaflet map object
#' @param data Data frame with patient locations
#' @param color_by Variable to color markers by (default: "asthma_diagnosis")
#' @param radius Marker radius (default: 4)
#' @return Updated leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap() %>%
#'   add_patient_markers(patient_data, color_by = "asthma_diagnosis")
#' }
add_patient_markers <- function(map, data, color_by = "asthma_diagnosis", radius = 4) {

  # Define color palette
  if (is.factor(data[[color_by]]) || is.character(data[[color_by]])) {
    pal <- leaflet::colorFactor(
      palette = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D"),
      domain = data[[color_by]]
    )
  } else {
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = data[[color_by]]
    )
  }

  # Create popup content
  popup_content <- paste0(
    "<b>Patient ID:</b> ", data$patient_id, "<br>",
    "<b>Age:</b> ", data$age_years, " years<br>",
    "<b>Sex:</b> ", data$sex, "<br>",
    "<b>Asthma:</b> ", data$asthma_diagnosis, "<br>",
    "<b>FEV1:</b> ", round(data$fev1, 2), " L<br>",
    "<b>FVC:</b> ", round(data$fvc, 2), " L"
  )

  map %>%
    leaflet::addCircleMarkers(
      data = data,
      lng = ~longitude,
      lat = ~latitude,
      color = ~pal(data[[color_by]]),
      fillColor = ~pal(data[[color_by]]),
      radius = radius,
      fillOpacity = 0.7,
      stroke = TRUE,
      weight = 1,
      popup = popup_content,
      clusterOptions = leaflet::markerClusterOptions()
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = data[[color_by]],
      title = color_by,
      opacity = 0.8
    )
}

#' Add Heatmap Layer
#'
#' Adds a heatmap layer showing density of outcomes.
#'
#' @param map A leaflet map object
#' @param data Data frame with coordinates and intensity values
#' @param intensity_col Column with intensity values (default: "exacerbation_count")
#' @param blur Blur amount (default: 15)
#' @param max Maximum intensity (default: 0.5)
#' @param radius Radius (default: 25)
#' @return Updated leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap() %>%
#'   add_heatmap_layer(patient_data, "exacerbation_count")
#' }
add_heatmap_layer <- function(map, data, intensity_col = "exacerbation_count",
                               blur = 15, max = 0.5, radius = 25) {

  # Prepare intensity values
  intensity <- data[[intensity_col]]
  if (is.null(intensity)) {
    intensity <- 1
  }

  map %>%
    leaflet.extras::addHeatmap(
      data = data,
      lng = ~longitude,
      lat = ~latitude,
      intensity = ~intensity,
      blur = blur,
      max = max,
      radius = radius
    )
}

#' Add Air Quality Stations
#'
#' Adds markers for air quality monitoring stations.
#'
#' @param map A leaflet map object
#' @param stations Data frame with station locations
#' @return Updated leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap() %>%
#'   add_air_quality_stations(station_data)
#' }
add_air_quality_stations <- function(map, stations) {

  popup_content <- paste0(
    "<b>Station:</b> ", stations$station_name, "<br>",
    "<b>PM2.5:</b> ", stations$pm25_avg, " μg/m³<br>",
    "<b>PM10:</b> ", stations$pm10_avg, " μg/m³<br>",
    "<b>NO2:</b> ", stations$no2_avg, " ppb"
  )

  map %>%
    leaflet::addAwesomeMarkers(
      data = stations,
      lng = ~longitude,
      lat = ~latitude,
      icon = leaflet::awesomeIcons(
        icon = "cloud",
        library = "fa",
        markerColor = "blue"
      ),
      popup = popup_content,
      group = "Air Quality Stations"
    )
}

#' Create Choropleth Map Data
#'
#' Prepares data for choropleth mapping by aggregating into regions.
#'
#' @param data Data frame with coordinates and values
#' @param value_col Column to aggregate
#' @param n_bins Number of grid bins (default: 15)
#' @return Data frame ready for choropleth
#' @export
#'
#' @examples
#' \dontrun{
#' choro_data <- create_choropleth_data(patient_data, "asthma_rate")
#' }
create_choropleth_data <- function(data, value_col, n_bins = 15) {

  # Create grid
  lat_breaks <- seq(min(data$latitude), max(data$latitude), length.out = n_bins + 1)
  lon_breaks <- seq(min(data$longitude), max(data$longitude), length.out = n_bins + 1)

  # Assign bins
  data$lat_bin <- cut(data$latitude, breaks = lat_breaks, include.lowest = TRUE)
  data$lon_bin <- cut(data$longitude, breaks = lon_breaks, include.lowest = TRUE)

  # Aggregate
  aggregated <- aggregate(
    data[[value_col]] ~ lat_bin + lon_bin,
    data = data,
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      count = length(x),
      sum = sum(x, na.rm = TRUE)
    )
  )

  # Extract values
  aggregated$value <- aggregated[[value_col]][, "mean"]
  aggregated$count <- aggregated[[value_col]][, "count"]

  # Add center coordinates
  aggregated$lat_center <- sapply(as.character(aggregated$lat_bin), function(b) {
    nums <- as.numeric(strsplit(gsub("\\(|\\)|\\[|\\]", "", b), ",")[[1]])
    mean(nums)
  })

  aggregated$lon_center <- sapply(as.character(aggregated$lon_bin), function(b) {
    nums <- as.numeric(strsplit(gsub("\\(|\\)|\\[|\\]", "", b), ",")[[1]])
    mean(nums)
  })

  return(aggregated)
}

#' Create Perth Metro Bounding Box
#'
#' Returns the bounding box for Perth metropolitan area.
#'
#' @return List with xmin, xmax, ymin, ymax
#' @export
get_perth_bbox <- function() {
  list(
    xmin = 115.7,
    xmax = 116.1,
    ymin = -32.5,
    ymax = -31.6
  )
}

#' Generate Color Palette for Map
#'
#' Creates a color palette function for mapping.
#'
#' @param data_values Vector of values to map
#' @param palette_name Color palette name (default: "viridis")
#' @param reverse Reverse palette (default: FALSE)
#' @return Color palette function
#' @export
#'
#' @examples
#' \dontrun{
#' pal <- generate_color_palette(data$pm25, "YlOrRd")
#' }
generate_color_palette <- function(data_values, palette_name = "viridis",
                                    reverse = FALSE) {

  if (is.factor(data_values) || is.character(data_values)) {
    leaflet::colorFactor(
      palette = if (palette_name == "viridis") {
        viridis::viridis(length(unique(data_values)))
      } else {
        RColorBrewer::brewer.pal(length(unique(data_values)), palette_name)
      },
      domain = data_values,
      reverse = reverse
    )
  } else {
    leaflet::colorNumeric(
      palette = if (palette_name == "viridis") {
        viridis::viridis(100)
      } else {
        RColorBrewer::brewer.pal(9, palette_name)
      },
      domain = data_values,
      reverse = reverse
    )
  }
}

#' Add Polygons to Map
#'
#' Adds polygon layers (e.g., for choropleth) to a leaflet map.
#'
#' @param map A leaflet map object
#' @param polygon_data Data frame with polygon coordinates
#' @param fill_col Column for fill color
#' @param palette Color palette function
#' @param popup_col Column for popup content
#' @return Updated leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap() %>%
#'   add_polygon_layer(grid_data, "value", color_palette, "count")
#' }
add_polygon_layer <- function(map, polygon_data, fill_col, palette,
                               popup_col = NULL) {

  # Create popup content
  if (!is.null(popup_col)) {
    popup_content <- paste0(
      "<b>Value:</b> ", round(polygon_data[[fill_col]], 2), "<br>",
      "<b>Count:</b> ", polygon_data[[popup_col]]
    )
  } else {
    popup_content <- paste0("<b>Value:</b> ", round(polygon_data[[fill_col]], 2))
  }

  map %>%
    leaflet::addRectangles(
      data = polygon_data,
      lng1 = ~lon_center - 0.005,
      lat1 = ~lat_center - 0.005,
      lng2 = ~lon_center + 0.005,
      lat2 = ~lat_center + 0.005,
      fillColor = ~palette(polygon_data[[fill_col]]),
      fillOpacity = 0.6,
      color = "white",
      weight = 1,
      popup = popup_content
    )
}

#' Filter Data by Map Bounds
#'
#' Filters data to only include points within map bounds.
#'
#' @param data Data frame with coordinates
#' @param bounds List with west, east, south, north
#' @return Filtered data frame
#' @export
#'
#' @examples
#' \dontrun{
#' filtered <- filter_by_bounds(data, list(west=115.7, east=116.0, south=-32.3, north=-31.8))
#' }
filter_by_bounds <- function(data, bounds) {
  data[data$longitude >= bounds$west &
         data$longitude <= bounds$east &
         data$latitude >= bounds$south &
         data$latitude <= bounds$north, ]
}

#' Calculate Map Center and Zoom
#'
#' Calculates appropriate center and zoom for a set of coordinates.
#'
#' @param data Data frame with coordinates
#' @return List with center coordinates and zoom level
#' @export
#'
#' @examples
#' \dontrun{
#' view <- calculate_map_view(patient_data)
#' }
calculate_map_view <- function(data) {
  center_lat <- mean(range(data$latitude, na.rm = TRUE))
  center_lon <- mean(range(data$longitude, na.rm = TRUE))

  # Calculate zoom based on spread
  lat_range <- diff(range(data$latitude, na.rm = TRUE))
  lon_range <- diff(range(data$longitude, na.rm = TRUE))

  # Approximate zoom calculation
  max_range <- max(lat_range, lon_range)
  zoom <- floor(log2(360 / max_range)) + 1
  zoom <- min(max(zoom, 3), 18)  # Clamp between 3 and 18

  list(
    center = list(lat = center_lat, lng = center_lon),
    zoom = zoom
  )
}

#' Calculate Travel Time to Nearest Healthcare Facility
#'
#' Estimates travel time from each patient location to the nearest
#' healthcare facility using Euclidean distance and average speed assumptions.
#'
#' @param patients Data frame with patient coordinates
#' @param facilities Data frame with facility coordinates
#' @param patient_lat_col Patient latitude column name (default: "latitude")
#' @param patient_lon_col Patient longitude column name (default: "longitude")
#' @param facility_lat_col Facility latitude column name (default: "latitude")
#' @param facility_lon_col Facility longitude column name (default: "longitude")
#' @param avg_speed_kmh Average travel speed in km/h (default: 40)
#' @param mode Travel mode ("driving", "walking", "transit") for speed adjustment
#' @return Data frame with travel times added
#' @export
#'
#' @examples
#' \dontrun{
#' patients <- data.frame(
#'   patient_id = 1:10,
#'   latitude = runif(10, -32.5, -31.6),
#'   longitude = runif(10, 115.7, 116.1)
#' )
#' facilities <- data.frame(
#'   facility_id = 1:3,
#'   facility_name = c("Hospital A", "Clinic B", "ER C"),
#'   latitude = c(-31.95, -32.1, -31.8),
#'   longitude = c(115.85, 115.9, 115.75)
#' )
#' result <- calculate_travel_time(patients, facilities)
#' }
calculate_travel_time <- function(patients, facilities,
                                   patient_lat_col = "latitude",
                                   patient_lon_col = "longitude",
                                   facility_lat_col = "latitude",
                                   facility_lon_col = "longitude",
                                   avg_speed_kmh = 40,
                                   mode = "driving") {
  # Input validation
  if (!patient_lat_col %in% names(patients) || !patient_lon_col %in% names(patients)) {
    stop("Patient coordinate columns not found")
  }
  if (!facility_lat_col %in% names(facilities) || !facility_lon_col %in% names(facilities)) {
    stop("Facility coordinate columns not found")
  }

  # Adjust speed by mode
  speed <- switch(mode,
                  "driving" = avg_speed_kmh,
                  "walking" = 5,
                  "transit" = avg_speed_kmh * 0.6,
                  avg_speed_kmh)

  patient_coords <- as.matrix(patients[, c(patient_lon_col, patient_lat_col)])
  facility_coords <- as.matrix(facilities[, c(facility_lon_col, facility_lat_col)])

  # Calculate distances and find nearest facility for each patient
  n_patients <- nrow(patients)
  travel_times <- numeric(n_patients)
  nearest_facility_idx <- integer(n_patients)
  distances_km <- numeric(n_patients)

  for (i in seq_len(n_patients)) {
    # Calculate distances to all facilities
    dists <- geosphere::distHaversine(patient_coords[i, ], facility_coords) / 1000

    # Find nearest
    nearest_idx <- which.min(dists)
    nearest_facility_idx[i] <- nearest_idx
    distances_km[i] <- dists[nearest_idx]

    # Calculate travel time in minutes
    travel_times[i] <- (distances_km[i] / speed) * 60
  }

  # Add results to patient data
  result <- patients
  result$distance_to_facility_km <- round(distances_km, 2)
  result$travel_time_minutes <- round(travel_times, 1)
  result$nearest_facility_idx <- nearest_facility_idx

  # Add facility name if available
  if ("facility_name" %in% names(facilities)) {
    result$nearest_facility <- facilities$facility_name[nearest_facility_idx]
  }

  return(result)
}

#' Identify Healthcare Deserts
#'
#' Identifies areas with poor healthcare access based on travel time
#' thresholds and facility density.
#'
#' @param data Data frame with patient locations and travel times
#' @param travel_time_col Name of travel time column (default: "travel_time_minutes")
#' @param distance_col Name of distance column (default: "distance_to_facility_km")
#' @param time_threshold Time threshold in minutes for "desert" classification (default: 30)
#' @param distance_threshold Distance threshold in km (default: 15)
#' @param lat_col Latitude column name (default: "latitude")
#' @param lon_col Longitude column name (default: "longitude")
#' @return List containing desert classification and summary statistics
#' @export
#'
#' @references
#' Syed, S. T., Gerber, B. S., & Sharp, L. K. (2013). Traveling towards disease:
#' transportation barriers to health care access. Journal of Community Health,
#' 38(5), 976-993.
#'
#' @examples
#' \dontrun{
#' # First calculate travel times
#' patients_with_travel <- calculate_travel_time(patients, facilities)
#'
#' # Then identify deserts
#' desert_analysis <- identify_deserts(patients_with_travel)
#' }
identify_deserts <- function(data, travel_time_col = "travel_time_minutes",
                              distance_col = "distance_to_facility_km",
                              time_threshold = 30,
                              distance_threshold = 15,
                              lat_col = "latitude",
                              lon_col = "longitude") {
  # Input validation
  if (!travel_time_col %in% names(data)) {
    stop("Travel time column not found in data")
  }
  if (!distance_col %in% names(data)) {
    stop("Distance column not found in data")
  }

  # Classify healthcare access
  data$access_category <- ifelse(
    data[[travel_time_col]] <= time_threshold / 2,
    "Good Access",
    ifelse(
      data[[travel_time_col]] <= time_threshold,
      "Moderate Access",
      ifelse(
        data[[travel_time_col]] <= time_threshold * 2,
        "Poor Access",
        "Healthcare Desert"
      )
    )
  )

  data$is_desert <- data$access_category == "Healthcare Desert"
  data$is_underserved <- data[[travel_time_col]] > time_threshold

  # Summary statistics
  access_summary <- data %>%
    dplyr::group_by(access_category) %>%
    dplyr::summarise(
      n = dplyr::n(),
      pct = dplyr::n() / nrow(data) * 100,
      mean_travel_time = mean(.data[[travel_time_col]], na.rm = TRUE),
      mean_distance = mean(.data[[distance_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Geographic clustering of deserts
  desert_locations <- data[data$is_desert, c(lat_col, lon_col)]

  if (nrow(desert_locations) > 5) {
    # Simple clustering to identify desert regions
    coords <- as.matrix(desert_locations)
    # Calculate centroid of desert areas
    desert_centroid <- c(
      lat = mean(coords[, lat_col], na.rm = TRUE),
      lon = mean(coords[, lon_col], na.rm = TRUE)
    )
  } else {
    desert_centroid <- NULL
  }

  # Risk factors in desert areas
  desert_risk <- NULL
  if (sum(data$is_desert) > 0) {
    risk_vars <- c("asthma_diagnosis", "exacerbation_count", "ses_score", "pm25")
    available_vars <- intersect(risk_vars, names(data))

    if (length(available_vars) > 0) {
      desert_risk <- data %>%
        dplyr::group_by(is_desert) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(available_vars),
            ~mean(as.numeric(as.character(.)), na.rm = TRUE),
            .names = "mean_{.col}"
          ),
          .groups = "drop"
        )
    }
  }

  structure(list(
    data = data,
    access_summary = access_summary,
    desert_centroid = desert_centroid,
    n_deserts = sum(data$is_desert),
    pct_deserts = sum(data$is_desert) / nrow(data) * 100,
    desert_risk_comparison = desert_risk,
    parameters = list(
      time_threshold = time_threshold,
      distance_threshold = distance_threshold
    )
  ), class = "healthcare_desert_analysis")
}

#' Add Healthcare Access Layer to Map
#'
#' Adds a visualization of healthcare access to a leaflet map.
#'
#' @param map A leaflet map object
#' @param access_data Data frame with access analysis results
#' @param lat_col Latitude column name (default: "latitude")
#' @param lon_col Longitude column name (default: "longitude")
#' @param access_col Access category column name (default: "access_category")
#' @return Updated leaflet map
#' @export
#'
#' @examples
#' \dontrun{
#' map <- create_perth_basemap() %>%
#'   add_healthcare_access_layer(access_analysis$data)
#' }
add_healthcare_access_layer <- function(map, access_data,
                                         lat_col = "latitude",
                                         lon_col = "longitude",
                                         access_col = "access_category") {

  # Define color palette for access categories
  access_colors <- c(
    "Good Access" = "#2ECC71",
    "Moderate Access" = "#F1C40F",
    "Poor Access" = "#E67E22",
    "Healthcare Desert" = "#E74C3C"
  )

  pal <- leaflet::colorFactor(
    palette = access_colors,
    domain = access_data[[access_col]]
  )

  # Create popup content
  popup_content <- paste0(
    "<b>Access Level:</b> ", access_data[[access_col]], "<br>",
    "<b>Travel Time:</b> ", round(access_data$travel_time_minutes, 1), " min<br>",
    "<b>Distance:</b> ", round(access_data$distance_to_facility_km, 2), " km"
  )

  if ("nearest_facility" %in% names(access_data)) {
    popup_content <- paste0(
      popup_content, "<br><b>Nearest Facility:</b> ", access_data$nearest_facility
    )
  }

  map %>%
    leaflet::addCircleMarkers(
      data = access_data,
      lng = as.formula(paste0("~", lon_col)),
      lat = as.formula(paste0("~", lat_col)),
      color = ~pal(access_data[[access_col]]),
      fillColor = ~pal(access_data[[access_col]]),
      radius = 5,
      fillOpacity = 0.7,
      stroke = TRUE,
      weight = 1,
      popup = popup_content
    ) %>%
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = access_data[[access_col]],
      title = "Healthcare Access",
      opacity = 0.8
    )
}

#' Print Method for Healthcare Desert Analysis
#'
#' @param x Object of class "healthcare_desert_analysis"
#' @param ... Additional arguments
#' @export
print.healthcare_desert_analysis <- function(x, ...) {
  cat("Healthcare Desert Analysis Results\n")
  cat("==================================\n\n")

  cat("Thresholds:\n")
  cat("  Time threshold:", x$parameters$time_threshold, "minutes\n")
  cat("  Distance threshold:", x$parameters$distance_threshold, "km\n\n")

  cat("Summary:\n")
  cat("  Healthcare deserts:", x$n_deserts, "(", round(x$pct_deserts, 1), "%)\n\n")

  cat("Access Distribution:\n")
  print(x$access_summary)

  if (!is.null(x$desert_risk_comparison)) {
    cat("\nRisk Comparison (Desert vs Non-Desert):\n")
    print(x$desert_risk_comparison)
  }
}
