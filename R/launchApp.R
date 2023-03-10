#' launches the shinyAppDemo app
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function(model.dir = NULL, results.dir = NULL, option.map = NULL, option.var.name = NULL) {
  
  # Search for App Directory
  appDir <- system.file("shiny", package = "HYPEtools")
  if (appDir == "") {
    stop("Could not find Shiny App directory. Try re-installing `HYPEtools`.", call. = FALSE)
  }
  
  # Pass options to Shiny
  shinyOptions(model.dir = model.dir,
               results.dir = results.dir,
               option.map = option.map,
               option.var.name = option.var.name)
  
  # Run Shiny App
  runApp(appDir)
}
