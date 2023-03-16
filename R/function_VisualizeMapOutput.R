#'
#' Shiny App for visualizing HYPE MapOutputs.
#'
#' Interactive maps and plots for visualizing MapOutput files.
#' 
#' @param results.dir Optional string, path to a directory containing MapOutput files that should be loaded on app initialization.
#' @param file.pattern Optional string, filename pattern to select files in \code{results.dir} that should be loaded on app initialization. See \code{\link{list.files}}.
#' @param map Optional string, path to GIS file for subbasin polygons that should be loaded on app initialization. Typically a GeoPackage (.gpkg) or Shapefile (.shp).
#' @param map.subid.column Optional integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs) that should be used on app intialization.
#' @param output.dir Optional string, path to a default output directory to save captured map images.
#' 
#' @details
#' \code{VisualizeMapOutput} is a Shiny app that provides interactive maps, plots, and tables for visualizing HYPE MapOutput files. The interactive Leaflet map is generated using \code{\link{PlotMapOutput}}.
#' The app can be launched with or without the input arguments. All necessary input buttons and menus are provided within the app interface. For convenience, however, the input arguments can be provided in order to quickly launch the
#' app with desired settings.
#' 
#' @return
#' \code{VisualizeMapOutput} returns a Shiny application object.
#' 
#' @seealso
#' \code{\link{ReadMapOutput}}; \code{\link{PlotMapOutput}}
#'
#' @examples 
#' \donttest{
#' VisualizeMapOutput(results.dir = system.file("demo_model", "results", package = "HYPEtools"),
#' map = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools"),
#' map.subid.column = 25)
#' }
#' 
#' @export

# wrapper for shiny::shinyApp()
VisualizeMapOutput <- function(results.dir = NULL, file.pattern = "^map.*\\.(txt|csv)$", map = NULL, map.subid.column = 1, output.dir = NULL) {
  
  # Search for App Directory
  appDir <- system.file("visualize_map_output", package = "HYPEtools")

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
    stop("To use the Shiny app features, please ensure that the following packages are installed: DT, htmlwidgets, leaflet, leaflet.extras, mapview, plotly, shiny, shinyalert, shinyFiles, shinyWidgets", call.=FALSE)
  }
  
  # Pass options to Shiny
  shiny::shinyOptions(results.dir = results.dir,
               file.pattern = file.pattern,
               map = map,
               map.subid.column = map.subid.column,
               output.dir = output.dir)
  
  # Run Shiny App
  shiny::runApp(appDir)
}

# Alias
#' @rdname VisualizeMapOutput
#' @export
VisualiseMapOutput <- VisualizeMapOutput