#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#'
#' @import shiny
#' @import shinyFiles
#' @importFrom graphics hist

# wrapper for shiny::shinyApp()
launchApp <- function(results.dir = NULL, map = NULL, map.subid.column = 1, ...) {
  
  # Search for App Directory
  appDir <- system.file("shiny", package = "HYPEtools")
  if (appDir == "") {
    stop("Could not find Shiny App directory. Try re-installing `HYPEtools`.", call. = FALSE)
  }
  
  # Pass options to Shiny
  shinyOptions(results.dir = results.dir,
               map = map,
               map.subid.column = map.subid.column,
               ...)
  
  # Run Shiny App
  runApp(appDir)
}
