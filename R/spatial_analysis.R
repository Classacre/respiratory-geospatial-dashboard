#' Spatial Analysis Functions for Respiratory Health Data
#'
#' This file contains functions for spatial autocorrelation analysis,
#' hotspot detection, and spatial clustering of respiratory health outcomes.

#' Calculate Spatial Weights Matrix
#'
#' Creates a spatial weights matrix based on inverse distance between points.
#'
#' @param data Data frame with latitude and longitude columns
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param bandwidth Distance bandwidth in km (default: 10)
#' @return A spatial weights matrix
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(latitude = c(-31.9, -32.0), longitude = c(115.8, 115.9))
#' W <- calculate_spatial_weights(data)
#' }
calculate_spatial_weights <- function(data, lat_col = "latitude", lon_col = "longitude",
                                       bandwidth = 10) {
  coords <- as.matrix(data[, c(lon_col, lat_col)])
  
  # Calculate distance matrix in km
  n <- nrow(coords)
  dist_matrix <- matrix(0, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        dist_matrix[i, j] <- geosphere::distHaversine(coords[i, ], coords[j, ]) / 1000
      }
    }
  }
  
  # Create weights using Gaussian kernel
  W <- exp(-0.5 * (dist_matrix / bandwidth)^2)
  diag(W) <- 0
  
  # Row standardize
  row_sums <- rowSums(W)
  W <- W / ifelse(row_sums > 0, row_sums, 1)
  
  return(W)
}

#' Calculate Moran's I Statistic
#'
#' Computes Moran's I statistic for spatial autocorrelation.
#'
#' @param x Numeric vector of values
#' @param W Spatial weights matrix
#' @return A list with Moran's I statistic and p-value
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(50)
#' W <- matrix(runif(2500), 50, 50)
#' diag(W) <- 0
#' result <- calculate_morans_i(x, W)
#' }
calculate_morans_i <- function(x, W) {
  n <- length(x)
  x_std <- scale(x)
  
  # Calculate Moran's I
  numerator <- sum(W * outer(x_std, x_std, "*"))
  denominator <- sum(x_std^2)
  W_sum <- sum(W)
  
  moran_i <- (n / W_sum) * (numerator / denominator)
  
  # Expected value under null
  E_I <- -1 / (n - 1)
  
  # Variance (simplified approximation)
  S1 <- sum((W + t(W))^2) / 2
  S2 <- sum((rowSums(W) + colSums(W))^2)
  b2 <- sum(x_std^4) / (sum(x_std^2)^2 / n)
  
  var_I <- (n * ((n^2 - 3*n + 3) * S1 - n * S2 + 3 * W_sum^2) -
              b2 * ((n^2 - n) * S1 - 2*n*S2 + 6*W_sum^2)) /
    ((n-1) * (n-2) * (n-3) * W_sum^2) - E_I^2
  
  # Z-score and p-value
  z_score <- (moran_i - E_I) / sqrt(var_I)
  p_value <- 2 * pnorm(-abs(z_score))
  
  return(list(
    moran_i = as.numeric(moran_i),
    expected_i = E_I,
    variance = var_I,
    z_score = z_score,
    p_value = p_value
  ))
}

#' Detect Spatial Hotspots
#'
#' Identifies spatial hotspots using local spatial statistics.
#'
#' @param data Data frame with coordinates and outcome variable
#' @param outcome_col Name of outcome column
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param bandwidth Distance bandwidth in km (default: 5)
#' @param threshold Significance threshold (default: 0.05)
#' @return Data frame with hotspot classifications
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   latitude = runif(100, -32.5, -31.6),
#'   longitude = runif(100, 115.7, 116.1),
#'   asthma_rate = rpois(100, 5)
#' )
#' hotspots <- detect_hotspots(data, "asthma_rate")
#' }
detect_hotspots <- function(data, outcome_col, lat_col = "latitude",
                            lon_col = "longitude", bandwidth = 5, threshold = 0.05) {
  coords <- as.matrix(data[, c(lon_col, lat_col)])
  x <- data[[outcome_col]]
  
  n <- nrow(data)
  local_stats <- numeric(n)
  
  # Calculate local statistic for each point
  for (i in 1:n) {
    distances <- geosphere::distHaversine(coords[i, ], coords) / 1000
    weights <- exp(-0.5 * (distances / bandwidth)^2)
    weights[i] <- 0  # Exclude self
    
    if (sum(weights) > 0) {
      local_stats[i] <- sum(weights * (x - mean(x))) / sd(x)
    }
  }
  
  # Classify hotspots
  data$local_stat <- local_stats
  data$hotspot_class <- "Not Significant"
  data$hotspot_class[local_stats > qnorm(1 - threshold)] <- "Hotspot (High-High)"
  data$hotspot_class[local_stats < qnorm(threshold)] <- "Coldspot (Low-Low)"
  
  return(data)
}

#' Create Spatial Interpolation Grid
#'
#' Creates a regular grid for spatial interpolation/prediction.
#'
#' @param bbox Bounding box as list with xmin, xmax, ymin, ymax
#' @param resolution Grid resolution in km (default: 1)
#' @return Data frame with grid coordinates
#' @export
#'
#' @examples
#' \dontrun{
#' bbox <- list(xmin = 115.7, xmax = 116.1, ymin = -32.5, ymax = -31.6)
#' grid <- create_interpolation_grid(bbox, resolution = 0.5)
#' }
create_interpolation_grid <- function(bbox, resolution = 1) {
  # Convert km to approximate degrees
  km_to_deg <- 1 / 111
  
  lon_seq <- seq(bbox$xmin, bbox$xmax, by = resolution * km_to_deg)
  lat_seq <- seq(bbox$ymin, bbox$ymax, by = resolution * km_to_deg)
  
  grid <- expand.grid(longitude = lon_seq, latitude = lat_seq)
  
  return(grid)
}

#' Inverse Distance Weighting Interpolation
#'
#' Performs IDW interpolation of values to a grid.
#'
#' @param data Data frame with coordinates and values
#' @param value_col Name of value column to interpolate
#' @param grid Grid data frame with coordinates
#' @param power IDW power parameter (default: 2)
#' @param max_dist Maximum distance to consider (km, default: 20)
#' @return Grid with interpolated values
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   longitude = c(115.8, 115.9, 116.0),
#'   latitude = c(-32.0, -31.9, -32.1),
#'   value = c(10, 15, 12)
#' )
#' grid <- create_interpolation_grid(list(xmin=115.7, xmax=116.1, ymin=-32.5, ymax=-31.6), 1)
#' result <- idw_interpolate(data, "value", grid)
#' }
idw_interpolate <- function(data, value_col, grid, power = 2, max_dist = 20) {
  values <- data[[value_col]]
  coords_data <- as.matrix(data[, c("longitude", "latitude")])
  coords_grid <- as.matrix(grid[, c("longitude", "latitude")])
  
  n_grid <- nrow(grid)
  interpolated <- numeric(n_grid)
  
  for (i in 1:n_grid) {
    distances <- geosphere::distHaversine(coords_grid[i, ], coords_data) / 1000
    
    # Filter by max distance
    valid <- distances < max_dist & distances > 0
    
    if (sum(valid) > 0) {
      w <- 1 / (distances[valid]^power)
      interpolated[i] <- sum(w * values[valid]) / sum(w)
    } else {
      interpolated[i] <- NA
    }
  }
  
  grid$interpolated <- interpolated
  return(grid)
}

#' Calculate Distance to Nearest Facility
#'
#' Calculates the distance from each patient to the nearest facility.
#'
#' @param patients Data frame with patient coordinates
#' @param facilities Data frame with facility coordinates
#' @param patient_lat_col Patient latitude column name
#' @param patient_lon_col Patient longitude column name
#' @param facility_lat_col Facility latitude column name
#' @param facility_lon_col Facility longitude column name
#' @return Vector of distances in km
#' @export
#'
#' @examples
#' \dontrun{
#' patients <- data.frame(lat = c(-32.0), lon = c(115.8))
#' facilities <- data.frame(lat = c(-31.9, -32.1), lon = c(115.9, 115.7))
#' dists <- distance_to_nearest(patients, facilities, "lat", "lon", "lat", "lon")
#' }
distance_to_nearest <- function(patients, facilities,
                                 patient_lat_col = "latitude",
                                 patient_lon_col = "longitude",
                                 facility_lat_col = "latitude",
                                 facility_lon_col = "longitude") {
  patient_coords <- as.matrix(patients[, c(patient_lon_col, patient_lat_col)])
  facility_coords <- as.matrix(facilities[, c(facility_lon_col, facility_lat_col)])
  
  distances <- apply(patient_coords, 1, function(p) {
    min(geosphere::distHaversine(p, facility_coords) / 1000)
  })
  
  return(distances)
}

#' Aggregate Data by Geographic Bins
#'
#' Aggregates point data into geographic bins for choropleth mapping.
#'
#' @param data Data frame with coordinates
#' @param value_col Name of value column to aggregate
#' @param lat_col Latitude column name
#' @param lon_col Longitude column name
#' @param n_bins Number of bins per dimension (default: 10)
#' @param fun Aggregation function (default: mean)
#' @return Data frame with binned data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   lat = runif(100, -32.5, -31.6),
#'   lon = runif(100, 115.7, 116.1),
#'   value = rnorm(100)
#' )
#' binned <- aggregate_by_bins(data, "value", "lat", "lon")
#' }
aggregate_by_bins <- function(data, value_col, lat_col = "latitude",
                               lon_col = "longitude", n_bins = 10,
                               fun = mean) {
  # Create bins
  lat_breaks <- seq(min(data[[lat_col]]), max(data[[lat_col]]), length.out = n_bins + 1)
  lon_breaks <- seq(min(data[[lon_col]]), max(data[[lon_col]]), length.out = n_bins + 1)
  
  data$lat_bin <- cut(data[[lat_col]], breaks = lat_breaks, include.lowest = TRUE)
  data$lon_bin <- cut(data[[lon_col]], breaks = lon_breaks, include.lowest = TRUE)
  
  # Aggregate
  aggregated <- aggregate(
    data[[value_col]] ~ lat_bin + lon_bin,
    data = data,
    FUN = function(x) if (length(x) > 0) fun(x, na.rm = TRUE) else NA
  )
  
  names(aggregated)[3] <- value_col
  
  # Add bin centers
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
