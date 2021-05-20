#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function(option.x, option.map, option.var.name) {
  
  # Search for App Directory
  appDir <- system.file("shiny", package = "HYPEtools")
  if (appDir == "") {
    stop("Could not find Shiny App directory. Try re-installing `HYPEtools`.", call. = FALSE)
  }
  
  # Pass options to Shiny
  shinyOptions(option.x = option.x,
               option.map = option.map,
               option.var.name = option.var.name)
  
  # Run Shiny App
  runApp(appDir)
}
