#'
#' Shiny App for visualizing Mapped Point Information.
#'
#' Interactive maps and plots for visualizing mapped point information, e.g. HYPE MapOutput files or model performances at observation sites.
#' 
#' @param results.dir Optional string, path to a directory containing e.g. MapOutput files that should be loaded on app initialization.
#' @param file.pattern Optional string, filename pattern to select files in \code{results.dir} that should be loaded on app initialization. See \code{\link{list.files}}.
#' @param sites Optional string, path to GIS file for outlet points that should be loaded on app initialization. Typically a GeoPackage (.gpkg) or Shapefile (.shp).
#' @param sites.subid.column Optional integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs) that should be used on app intialization.
#' @param output.dir Optional string, path to a default output directory to save captured map images.
#' 
#' @details
#' \code{VisualizeMapPoints} is a Shiny app that provides interactive maps, plots, and tables for visualizing mapped point information. The interactive Leaflet map is generated using \code{\link{PlotMapPoints}}.
#' The app can be launched with or without the input arguments. All necessary input buttons and menus are provided within the app interface. For convenience, however, the input arguments can be provided in order to quickly launch the
#' app with desired settings.
#' 
#' @return
#' \code{VisualizeMapPoints} returns a Shiny application object.
#' 
#' @seealso
#' \code{\link{ReadMapOutput}}; \code{\link{PlotMapPoints}}
#'
#' @examples 
#' \dontrun{
#' VisualizeMapPoints(
#'   results.dir = system.file("demo_model", "results", package = "HYPEtools"),
#'   sites = system.file("demo_model", "gis", "Nytorp_centroids.gpkg", package = "HYPEtools"),
#'   sites.subid.column = 25,
#'   bg = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools")
#' )
#' }
#' 
#' @export

# wrapper for shiny::shinyApp()
VisualizeMapPoints<- function(results.dir = NULL, file.pattern = "^map.*\\.(txt|csv)$", sites = NULL, sites.subid.column = 1, bg = NULL, output.dir = NULL) {
  
  # Search for App Directory
  appDir <- system.file("visualize_map_points", package = "HYPEtools")

  # Input argument checks
  if(!is.null(results.dir)){stopifnot(length(results.dir) == 1)}
  if(!is.null(output.dir)){stopifnot(length(output.dir) == 1)}

  # Check/Load Dependencies - do this here so that these packages are not required for the base HYPEtools installation
  if (!all(
    requireNamespace("DT", quietly = TRUE),
    requireNamespace("htmlwidgets", quietly = TRUE),
    requireNamespace("leaflet", quietly = TRUE),
    requireNamespace("leaflet.extras", quietly = TRUE),
    requireNamespace("mapview", quietly = TRUE),
    requireNamespace("plotly", quietly = TRUE),
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("shiny", quietly = TRUE),
    requireNamespace("shinyalert", quietly = TRUE),
    requireNamespace("shinyFiles", quietly = TRUE),
    requireNamespace("shinyWidgets", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop('To use the Shiny app features, please ensure that the following packages are installed: c("DT", "htmlwidgets", "leaflet", "leaflet.extras", "mapview", "plotly", "sf", "shiny", "shinyalert", "shinyFiles", "shinyWidgets")', call.=FALSE)
  }

  # Pass options to Shiny
  shiny::shinyOptions(results.dir = results.dir,
               file.pattern = file.pattern,
               sites = sites,
               sites.subid.column = sites.subid.column,
               bg = bg,
               output.dir = output.dir)
  
  # Run Shiny App
  shiny::runApp(appDir)
}

# Alias
#' @rdname VisualizeMapPoints
#' @export
VisualiseMapPoints <- VisualizeMapPoints