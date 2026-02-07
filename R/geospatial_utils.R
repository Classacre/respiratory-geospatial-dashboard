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
