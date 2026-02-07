#' Respiratory Geospatial Dashboard
#'
#' A comprehensive Shiny dashboard for pediatric respiratory health analysis
#' with geospatial components.
#'
#' @name respiratorygeospatial
#' @docType package
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import plotly
#' @import DT
#' @import dplyr
#' @import ggplot2
NULL

#' Run the Shiny Dashboard
#'
#' Launches the respiratory geospatial dashboard.
#'
#' @param ... Additional arguments passed to shiny::runApp
#' @return A Shiny application object
#' @export
#'
#' @examples
#' \dontrun{
#' run_dashboard()
#' }
run_dashboard <- function(...) {
  app_dir <- system.file("shiny", package = "respiratorygeospatial")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.")
  }
  shiny::runApp(app_dir, ...)
}
