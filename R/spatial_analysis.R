#' Monte Carlo Simulation for Hotspot Significance Testing
#'
#' Tests significance of spatial clusters using Monte Carlo simulation.
#' Based on Roberts et al. (2006) Alameda County study methodology.
#'
#' @param data Data frame with coordinates and outcome variable
#' @param outcome_col Name of outcome column
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param n_simulations Number of Monte Carlo simulations (default: 999)
#' @param bandwidth Distance bandwidth in km (default: 5)
#' @param significance_level Significance level for p-values (default: 0.05)
#' @param seed Random seed for reproducibility (default: 42)
#' @return List containing hotspot classifications, p-values, and simulation results
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   latitude = runif(100, -32.5, -31.6),
#'   longitude = runif(100, 115.7, 116.1),
#'   asthma_rate = rpois(100, 5)
#' )
#' result <- monte_carlo_hotspot_test(data, "asthma_rate", n_simulations = 499)
#' }
monte_carlo_hotspot_test <- function(data, outcome_col, lat_col = "latitude",
                                      lon_col = "longitude", n_simulations = 999,
                                      bandwidth = 5, significance_level = 0.05,
                                      seed = 42) {
  # Input validation
  if (!outcome_col %in% names(data)) {
    stop("outcome_col not found in data")
  }
  if (!lat_col %in% names(data) || !lon_col %in% names(data)) {
    stop("lat_col or lon_col not found in data")
  }
  if (n_simulations < 99) {
    warning("At least 99 simulations recommended for reliable p-values")
  }

  set.seed(seed)

  coords <- as.matrix(data[, c(lon_col, lat_col)])
  x <- data[[outcome_col]]
  n <- nrow(data)

  # Calculate observed local statistics
  observed_stats <- calculate_local_g_statistic(coords, x, bandwidth)

  # Monte Carlo simulation - random permutations
  simulated_max_stats <- numeric(n_simulations)
  simulated_min_stats <- numeric(n_simulations)

  for (i in seq_len(n_simulations)) {
    # Permute outcome values
    x_permuted <- sample(x, replace = FALSE)
    permuted_stats <- calculate_local_g_statistic(coords, x_permuted, bandwidth)
    simulated_max_stats[i] <- max(permuted_stats, na.rm = TRUE)
    simulated_min_stats[i] <- min(permuted_stats, na.rm = TRUE)
  }

  # Calculate p-values
  p_values_upper <- sapply(observed_stats, function(stat) {
    (sum(simulated_max_stats >= stat) + 1) / (n_simulations + 1)
  })

  p_values_lower <- sapply(observed_stats, function(stat) {
    (sum(simulated_min_stats <= stat) + 1) / (n_simulations + 1)
  })

  # Classify hotspots based on significance
  hotspot_class <- rep("Not Significant", n)
  hotspot_class[p_values_upper < significance_level] <- "Hotspot (High-High)"
  hotspot_class[p_values_lower < significance_level] <- "Coldspot (Low-Low)"

  # Add results to data
  result_data <- data
  result_data$local_stat <- observed_stats
  result_data$p_value <- pmin(p_values_upper, p_values_lower)
  result_data$hotspot_class <- hotspot_class
  result_data$significance <- cut(
    result_data$p_value,
    breaks = c(0, 0.01, 0.05, 0.1, 1),
    labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not Significant"),
    include.lowest = TRUE
  )

  # Summary statistics
  summary_stats <- list(
    n_hotspots = sum(hotspot_class == "Hotspot (High-High)"),
    n_coldspots = sum(hotspot_class == "Coldspot (Low-Low)"),
    n_significant = sum(hotspot_class != "Not Significant"),
    significance_level = significance_level,
    n_simulations = n_simulations
  )

  structure(list(
    data = result_data,
    observed_stats = observed_stats,
    simulated_max_stats = simulated_max_stats,
    simulated_min_stats = simulated_min_stats,
    p_values = pmin(p_values_upper, p_values_lower),
    hotspot_class = hotspot_class,
    summary = summary_stats,
    parameters = list(
      bandwidth = bandwidth,
      n_simulations = n_simulations,
      significance_level = significance_level
    )
  ), class = "monte_carlo_hotspot")
}

#' Calculate Local G Statistic for Hotspot Detection
#'
#' Internal function to calculate local G statistic for each location.
#'
#' @param coords Matrix of coordinates (longitude, latitude)
#' @param x Vector of values
#' @param bandwidth Distance bandwidth in km
#' @return Vector of local G statistics
#' @keywords internal
calculate_local_g_statistic <- function(coords, x, bandwidth) {
  n <- nrow(coords)
  local_stats <- numeric(n)

  for (i in seq_len(n)) {
    distances <- geosphere::distHaversine(coords[i, ], coords) / 1000
    weights <- exp(-0.5 * (distances / bandwidth)^2)
    weights[i] <- 0  # Exclude self

    if (sum(weights) > 0) {
      local_stats[i] <- sum(weights * (x - mean(x))) / sd(x)
    } else {
      local_stats[i] <- 0
    }
  }

  return(local_stats)
}

#' Plot Monte Carlo Hotspot Results
#'
#' Visualizes the Monte Carlo simulation results with significance levels.
#'
#' @param x Object of class "monte_carlo_hotspot"
#' @param ... Additional arguments passed to plot
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- monte_carlo_hotspot_test(data, "asthma_rate")
#' plot(result)
#' }
plot.monte_carlo_hotspot <- function(x, ...) {
  if (!inherits(x, "monte_carlo_hotspot")) {
    stop("Object must be of class 'monte_carlo_hotspot'")
  }

  data <- x$data

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["longitude"]],
                                           y = .data[["latitude"]],
                                           color = significance)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = c("p < 0.01" = "#D73027",
                 "p < 0.05" = "#FC8D59",
                 "p < 0.10" = "#FEE090",
                 "Not Significant" = "#E0E0E0"),
      name = "Significance"
    ) +
    ggplot2::labs(
      title = "Monte Carlo Hotspot Significance",
      subtitle = paste0(x$summary$n_simulations, " simulations, ",
                        "bandwidth = ", x$parameters$bandwidth, " km"),
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Getis-Ord Gi* Statistic for Hotspot Detection
#'
#' Calculates the Getis-Ord Gi* statistic for identifying spatial clusters.
#' Gi* identifies statistically significant hotspots (high values surrounded by
#' high values) and coldspots (low values surrounded by low values).
#'
#' @param data Data frame with coordinates and outcome variable
#' @param outcome_col Name of outcome column
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param bandwidth Distance bandwidth in km (default: 5)
#' @param significance_level Significance level (default: 0.05)
#' @return Data frame with Gi* statistics, z-scores, and classifications
#' @export
#'
#' @references
#' Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use
#' of distance statistics. Geographical Analysis, 24(3), 189-206.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   latitude = runif(100, -32.5, -31.6),
#'   longitude = runif(100, 115.7, 116.1),
#'   asthma_rate = rpois(100, 5)
#' )
#' gi_result <- calculate_getis_ord_gi(data, "asthma_rate")
#' }
calculate_getis_ord_gi <- function(data, outcome_col, lat_col = "latitude",
                                    lon_col = "longitude", bandwidth = 5,
                                    significance_level = 0.05) {
  # Input validation
  if (!outcome_col %in% names(data)) {
    stop("outcome_col not found in data")
  }

  coords <- as.matrix(data[, c(lon_col, lat_col)])
  x <- data[[outcome_col]]
  n <- nrow(data)

  # Calculate Gi* statistic for each location
  gi_stats <- numeric(n)
  z_scores <- numeric(n)
  expected_gi <- numeric(n)
  variance_gi <- numeric(n)

  global_mean <- mean(x)
  global_var <- var(x)

  for (i in seq_len(n)) {
    # Calculate spatial weights based on distance
    distances <- geosphere::distHaversine(coords[i, ], coords) / 1000
    weights <- exp(-0.5 * (distances / bandwidth)^2)

    # Gi* includes self (unlike Gi)
    W_i <- sum(weights)
    S1_i <- sum(weights^2)

    # Calculate Gi*
    gi_stats[i] <- sum(weights * x)

    # Expected value under null hypothesis
    expected_gi[i] <- W_i * global_mean

    # Variance
    variance_gi[i] <- W_i * (n - W_i) * global_var / (n - 1)

    # Standardized z-score
    if (variance_gi[i] > 0) {
      z_scores[i] <- (gi_stats[i] - expected_gi[i]) / sqrt(variance_gi[i])
    } else {
      z_scores[i] <- 0
    }
  }

  # Classify based on z-scores
  classification <- rep("Not Significant", n)
  classification[z_scores > qnorm(1 - significance_level / 2)] <- "Hotspot (High-High)"
  classification[z_scores < qnorm(significance_level / 2)] <- "Coldspot (Low-Low)"

  # Add results to data
  result <- data
  result$gi_star <- gi_stats
  result$gi_expected <- expected_gi
  result$gi_variance <- variance_gi
  result$gi_zscore <- z_scores
  result$gi_pvalue <- 2 * pnorm(-abs(z_scores))
  result$gi_classification <- classification

  return(result)
}

#' Spatial Scan Statistic (Kulldorff's Method)
#'
#' Implements Kulldorff's spatial scan statistic for detecting clusters
#' of events. Uses a circular scanning window to identify areas with
#' statistically significant excess risk.
#'
#' @param data Data frame with coordinates and case/control counts
#' @param case_col Name of column with case counts
#' @param population_col Name of column with population counts (optional)
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param max_radius Maximum scan radius in km (default: 10)
#' @param n_simulations Number of Monte Carlo simulations (default: 999)
#' @param seed Random seed for reproducibility (default: 42)
#' @return List containing detected clusters and scan statistics
#' @export
#'
#' @references
#' Kulldorff, M. (1997). A spatial scan statistic. Communications in
#' Statistics-Theory and Methods, 26(6), 1481-1496.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   latitude = runif(100, -32.5, -31.6),
#'   longitude = runif(100, 115.7, 116.1),
#'   cases = rpois(100, 5),
#'   population = rpois(100, 100) + 50
#' )
#' scan_result <- spatial_scan_statistic(data, "cases", "population")
#' }
spatial_scan_statistic <- function(data, case_col, population_col = NULL,
                                    lat_col = "latitude", lon_col = "longitude",
                                    max_radius = 10, n_simulations = 999,
                                    seed = 42) {
  # Input validation
  if (!case_col %in% names(data)) {
    stop("case_col not found in data")
  }
  if (!is.null(population_col) && !population_col %in% names(data)) {
    stop("population_col not found in data")
  }

  set.seed(seed)

  coords <- as.matrix(data[, c(lon_col, lat_col)])
  cases <- data[[case_col]]

  if (is.null(population_col)) {
    population <- rep(1, nrow(data))
  } else {
    population <- data[[population_col]]
  }

  total_cases <- sum(cases)
  total_pop <- sum(population)

  # Define candidate circles (centered at each point with varying radii)
  n_circles <- nrow(data)
  radii <- seq(1, max_radius, length.out = 5)

  best_log_likelihood <- -Inf
  best_cluster <- NULL

  # Evaluate each potential cluster
  for (i in seq_len(n_circles)) {
    for (radius in radii) {
      # Find points within circle
      distances <- geosphere::distHaversine(coords[i, ], coords) / 1000
      in_circle <- distances <= radius

      if (sum(in_circle) < 3) next  # Skip very small clusters

      c_z <- sum(cases[in_circle])
      n_z <- sum(population[in_circle])

      if (n_z == 0 || c_z == 0) next

      # Calculate log-likelihood ratio
      if (c_z / n_z > total_cases / total_pop) {
        log_lr <- c_z * log(c_z / n_z) +
          (total_cases - c_z) * log((total_cases - c_z) / (total_pop - n_z)) -
          total_cases * log(total_cases / total_pop)
      } else {
        log_lr <- 0
      }

      if (log_lr > best_log_likelihood) {
        best_log_likelihood <- log_lr
        best_cluster <- list(
          center_idx = i,
          center_lat = data[[lat_col]][i],
          center_lon = data[[lon_col]][i],
          radius = radius,
          cases = c_z,
          population = n_z,
          expected_cases = total_cases * n_z / total_pop,
          relative_risk = (c_z / n_z) / (total_cases / total_pop),
          log_likelihood = log_lr
        )
      }
    }
  }

  # Monte Carlo simulation for p-value
  simulated_max_ll <- numeric(n_simulations)

  for (sim in seq_len(n_simulations)) {
    # Permute cases
    permuted_cases <- sample(cases, replace = FALSE)
    sim_best_ll <- -Inf

    for (i in seq_len(n_circles)) {
      for (radius in radii) {
        distances <- geosphere::distHaversine(coords[i, ], coords) / 1000
        in_circle <- distances <= radius

        if (sum(in_circle) < 3) next

        c_z <- sum(permuted_cases[in_circle])
        n_z <- sum(population[in_circle])

        if (n_z == 0 || c_z == 0) next

        if (c_z / n_z > total_cases / total_pop) {
          log_lr <- c_z * log(c_z / n_z) +
            (total_cases - c_z) * log((total_cases - c_z) / (total_pop - n_z)) -
            total_cases * log(total_cases / total_pop)
        } else {
          log_lr <- 0
        }

        sim_best_ll <- max(sim_best_ll, log_lr)
      }
    }

    simulated_max_ll[sim] <- sim_best_ll
  }

  # Calculate p-value
  p_value <- (sum(simulated_max_ll >= best_log_likelihood) + 1) / (n_simulations + 1)

  if (!is.null(best_cluster)) {
    best_cluster$p_value <- p_value
    best_cluster$significant <- p_value < 0.05
  }

  structure(list(
    primary_cluster = best_cluster,
    log_likelihood = best_log_likelihood,
    p_value = p_value,
    simulated_max_ll = simulated_max_ll,
    parameters = list(
      max_radius = max_radius,
      n_simulations = n_simulations,
      total_cases = total_cases,
      total_population = total_pop
    )
  ), class = "spatial_scan")
}

#' Variogram Analysis for Spatial Correlation
#'
#' Calculates and analyzes the spatial variogram to understand the
#' spatial correlation structure of the data.
#'
#' @param data Data frame with coordinates and outcome variable
#' @param outcome_col Name of outcome column
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @param max_dist Maximum distance for variogram calculation in km (default: 20)
#' @param n_lags Number of distance lags (default: 15)
#' @return List containing variogram data and fitted model parameters
#' @export
#'
#' @references
#' Cressie, N. (1993). Statistics for Spatial Data. Wiley.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   latitude = runif(100, -32.5, -31.6),
#'   longitude = runif(100, 115.7, 116.1),
#'   pm25 = rnorm(100, 12, 3)
#' )
#' vgm_result <- variogram_analysis(data, "pm25")
#' }
variogram_analysis <- function(data, outcome_col, lat_col = "latitude",
                                lon_col = "longitude", max_dist = 20,
                                n_lags = 15) {
  # Input validation
  if (!outcome_col %in% names(data)) {
    stop("outcome_col not found in data")
  }

  coords <- as.matrix(data[, c(lon_col, lat_col)])
  z <- data[[outcome_col]]
  n <- nrow(data)

  # Calculate pairwise distances and semivariances
  distances <- matrix(0, n, n)
  semivariances <- matrix(0, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        distances[i, j] <- geosphere::distHaversine(coords[i, ], coords[j]) / 1000
        semivariances[i, j] <- 0.5 * (z[i] - z[j])^2
      }
    }
  }

  # Create distance bins
  lag_breaks <- seq(0, max_dist, length.out = n_lags + 1)
  lag_centers <- (lag_breaks[-1] + lag_breaks[-(n_lags + 1)]) / 2

  # Calculate empirical variogram
  variogram_data <- data.frame(
    distance = numeric(n_lags),
    gamma = numeric(n_lags),
    npairs = numeric(n_lags)
  )

  for (i in seq_len(n_lags)) {
    in_lag <- distances > lag_breaks[i] & distances <= lag_breaks[i + 1]
    if (sum(in_lag) > 0) {
      variogram_data$distance[i] <- lag_centers[i]
      variogram_data$gamma[i] <- mean(semivariances[in_lag], na.rm = TRUE)
      variogram_data$npairs[i] <- sum(in_lag)
    }
  }

  # Remove empty bins
  variogram_data <- variogram_data[variogram_data$npairs > 0, ]

  # Fit theoretical variogram models
  # Spherical model
  fit_spherical <- fit_variogram_model(variogram_data, "spherical")

  # Exponential model
  fit_exponential <- fit_variogram_model(variogram_data, "exponential")

  # Select best fit
  if (fit_spherical$sse < fit_exponential$sse) {
    best_model <- fit_spherical
    model_type <- "spherical"
  } else {
    best_model <- fit_exponential
    model_type <- "exponential"
  }

  # Calculate range (distance at which spatial correlation becomes negligible)
  range_km <- best_model$range

  # Calculate nugget-to-sill ratio (indicates proportion of variance at small scales)
  nugget_ratio <- best_model$nugget / (best_model$nugget + best_model$sill)

  structure(list(
    empirical = variogram_data,
    fitted_model = best_model,
    model_type = model_type,
    parameters = list(
      nugget = best_model$nugget,
      sill = best_model$sill,
      range = range_km,
      nugget_ratio = nugget_ratio,
      partial_sill = best_model$sill
    ),
    spatial_dependence = list(
      has_structure = nugget_ratio < 0.5,
      range_km = range_km,
      strength = ifelse(range_km > 0, 1 / range_km, 0)
    )
  ), class = "variogram_analysis")
}

#' Fit Variogram Model
#'
#' Internal function to fit theoretical variogram models to empirical data.
#'
#' @param variogram_data Data frame with distance and gamma values
#' @param model_type Type of model ("spherical" or "exponential")
#' @return List with fitted parameters
#' @keywords internal
fit_variogram_model <- function(variogram_data, model_type) {
  # Initial parameter estimates
  nugget_init <- min(variogram_data$gamma)
  sill_init <- max(variogram_data$gamma) - nugget_init
  range_init <- max(variogram_data$distance) / 2

  # Model fitting using least squares
  fit_function <- function(params) {
    nugget <- params[1]
    sill <- params[2]
    range <- params[3]

    if (model_type == "spherical") {
      gamma_pred <- nugget + sill * ifelse(
        variogram_data$distance <= range,
        1.5 * (variogram_data$distance / range) - 0.5 * (variogram_data$distance / range)^3,
        1
      )
    } else {  # exponential
      gamma_pred <- nugget + sill * (1 - exp(-3 * variogram_data$distance / range))
    }

    sum((variogram_data$gamma - gamma_pred)^2)
  }

  # Optimize parameters
  optim_result <- optim(
    c(nugget_init, sill_init, range_init),
    fit_function,
    method = "L-BFGS-B",
    lower = c(0, 0, 0.1),
    upper = c(max(variogram_data$gamma), max(variogram_data$gamma) * 2, max(variogram_data$distance))
  )

  list(
    nugget = optim_result$par[1],
    sill = optim_result$par[2],
    range = optim_result$par[3],
    sse = optim_result$value
  )
}

#' Dasymetric Mapping for Refined Spatial Interpolation
#'
#' Performs dasymetric mapping to refine spatial interpolation using
#' population density and land use data. This improves upon simple IDW
#' interpolation by incorporating ancillary data about population distribution.
#'
#' @param data Data frame with coordinates and values to interpolate
#' @param value_col Name of value column to interpolate
#' @param grid Grid data frame with coordinates
#' @param land_use Data frame with land use information (optional)
#' @param population_col Name of population column in land_use (default: "population")
#' @param land_type_col Name of land type column in land_use (default: "land_type")
#' @param power IDW power parameter (default: 2)
#' @param max_dist Maximum distance to consider (km, default: 20)
#' @param residential_weight Weight for residential areas (default: 1.0)
#' @param commercial_weight Weight for commercial areas (default: 0.3)
#' @param industrial_weight Weight for industrial areas (default: 0.1)
#' @return Grid with dasymetrically interpolated values
#' @export
#'
#' @references
#' Mennis, J. (2003). Generating surface models of population using
#' dasymetric mapping. The Professional Geographer, 55(1), 31-42.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   longitude = c(115.8, 115.9, 116.0),
#'   latitude = c(-32.0, -31.9, -32.1),
#'   value = c(10, 15, 12)
#' )
#' grid <- create_interpolation_grid(get_perth_bbox(), 1)
#' result <- dasymetric_mapping(data, "value", grid)
#' }
dasymetric_mapping <- function(data, value_col, grid, land_use = NULL,
                                population_col = "population",
                                land_type_col = "land_type",
                                power = 2, max_dist = 20,
                                residential_weight = 1.0,
                                commercial_weight = 0.3,
                                industrial_weight = 0.1) {
  # Input validation
  if (!value_col %in% names(data)) {
    stop("value_col not found in data")
  }

  values <- data[[value_col]]
  coords_data <- as.matrix(data[, c("longitude", "latitude")])
  coords_grid <- as.matrix(grid[, c("longitude", "latitude")])

  n_grid <- nrow(grid)
  interpolated <- numeric(n_grid)

  # Default land use weights
  land_weights <- list(
    "residential" = residential_weight,
    "commercial" = commercial_weight,
    "industrial" = industrial_weight,
    "park" = 0.2,
    "water" = 0.0,
    "other" = 0.5
  )

  for (i in seq_len(n_grid)) {
    distances <- geosphere::distHaversine(coords_grid[i, ], coords_data) / 1000

    # Filter by max distance
    valid <- distances < max_dist & distances > 0

    if (sum(valid) > 0) {
      # Calculate IDW weights
      idw_weights <- 1 / (distances[valid]^power)

      # Apply dasymetric weighting if land use data provided
      if (!is.null(land_use)) {
        # Find nearest land use cell for each data point
        dasym_weights <- sapply(which(valid), function(j) {
          # Find closest land use point
          if (land_type_col %in% names(land_use)) {
            land_type <- land_use[[land_type_col]][j]
            if (land_type %in% names(land_weights)) {
              return(land_weights[[land_type]])
            }
          }
          return(1.0)
        })
        idw_weights <- idw_weights * dasym_weights
      }

      # Normalize weights
      idw_weights <- idw_weights / sum(idw_weights)

      interpolated[i] <- sum(idw_weights * values[valid])
    } else {
      interpolated[i] <- NA
    }
  }

  grid$interpolated <- interpolated
  grid$method <- "dasymetric"

  return(grid)
}

#' Print Method for Monte Carlo Hotspot Results
#'
#' @param x Object of class "monte_carlo_hotspot"
#' @param ... Additional arguments
#' @export
print.monte_carlo_hotspot <- function(x, ...) {
  cat("Monte Carlo Hotspot Test Results\n")
  cat("=================================\n")
  cat("Simulations:", x$summary$n_simulations, "\n")
  cat("Significance level:", x$parameters$significance_level, "\n")
  cat("Bandwidth:", x$parameters$bandwidth, "km\n\n")

  cat("Summary:\n")
  cat("  Hotspots (High-High):", x$summary$n_hotspots, "\n")
  cat("  Coldspots (Low-Low):", x$summary$n_coldspots, "\n")
  cat("  Total significant:", x$summary$n_significant, "\n")
}

#' Print Method for Spatial Scan Results
#'
#' @param x Object of class "spatial_scan"
#' @param ... Additional arguments
#' @export
print.spatial_scan <- function(x, ...) {
  cat("Spatial Scan Statistic Results\n")
  cat("=============================\n")

  if (!is.null(x$primary_cluster)) {
    cluster <- x$primary_cluster
    cat("Primary Cluster:\n")
    cat("  Center: (", round(cluster$center_lat, 4), ", ", 
        round(cluster$center_lon, 4), ")\n", sep = "")
    cat("  Radius:", round(cluster$radius, 2), "km\n")
    cat("  Cases:", cluster$cases, "\n")
    cat("  Expected:", round(cluster$expected_cases, 2), "\n")
    cat("  Relative Risk:", round(cluster$relative_risk, 2), "\n")
    cat("  P-value:", format.pval(cluster$p_value, eps = 0.001), "\n")
    cat("  Significant:", ifelse(cluster$significant, "Yes", "No"), "\n")
  } else {
    cat("No significant clusters detected.\n")
  }
}

#' Print Method for Variogram Analysis
#'
#' @param x Object of class "variogram_analysis"
#' @param ... Additional arguments
#' @export
print.variogram_analysis <- function(x, ...) {
  cat("Variogram Analysis Results\n")
  cat("==========================\n")
  cat("Model type:", x$model_type, "\n\n")

  cat("Parameters:\n")
  cat("  Nugget:", round(x$parameters$nugget, 4), "\n")
  cat("  Sill:", round(x$parameters$sill, 4), "\n")
  cat("  Range:", round(x$parameters$range, 2), "km\n")
  cat("  Nugget ratio:", round(x$parameters$nugget_ratio, 3), "\n\n")

  cat("Spatial dependence:\n")
  cat("  Has structure:", x$spatial_dependence$has_structure, "\n")
  cat("  Effective range:", round(x$spatial_dependence$range_km, 2), "km\n")
}
