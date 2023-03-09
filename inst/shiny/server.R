#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Get values passed to app
option.x <- getShinyOption("option.x", default = NULL)
option.map <- getShinyOption("option.map", default = NULL)
option.var.name <- getShinyOption("option.var.name", default = NULL)

# Define server logic
shinyAppServer <- function(input, output, session) {
  
  # Get Available File Volumes
  volumes = getVolumes()
  volumes <- c("HYPEtools Demo Model" = system.file("demo_model", package = "HYPEtools"), Home = fs::path_home(), getVolumes()())
  
  # Get Paths to Model Files
  model_files <- reactive({
    shinyFileChoose(input, "button_model", roots = volumes, session = session)

    if (is.null(input$button_model)){
      files <- data.frame("Files" = NA)
    } else{
      files <- data.frame("Files" = parseFilePaths(volumes, input$button_model)$datapath)
    }
  })
  
  output$path_mf <- DT::renderDataTable(model_files())
  
  # Get Paths to Results Files
  results_files <- reactive({
    shinyFileChoose(input, "button_result", roots = volumes, session = session)

    if (is.null(input$button_result)){
      files <- data.frame("Files" = NA)
    } else{
      files <- data.frame("Files" = parseFilePaths(volumes, input$button_result)$datapath)
    }
  })
  
  output$path_results <- DT::renderDataTable(results_files())
  
  # Get Data
  data <- reactive({
    req(!all(is.na(results_files()$Files)))
    ReadMapOutput(results_files()$Files)[, c(1, input$slider + 1)]
  })

  # Render Data Table
  output$table <- renderDataTable(data())

  # Render Map
  # It's probably better to switch to creating a map item and then using leafletProxy to update the map with the new data instead of regerenating the entire map
  output$map <- renderLeaflet({
    PlotMapOutput(
      x = data(),
      map = option.map,
      var.name = option.var.name,
      map.type = "leaflet",
      map.subid.column = 2,
      legend.pos = "bottomleft", # Specify legend position
      legend.title = option.var.name, # Specify legend title
      col.rev = FALSE, # Set to TRUE if you want to reverse the color palette
      legend.signif = 2, # Specify number of significant digits to include in map legend
      plot.searchbar = TRUE, # Add searchbar to search for and zoom to specific subbasins
      plot.label = TRUE, # Display label with subbasin name and parameter values when hovering over subbasins in map
      plot.scale = TRUE, # Add scalebar to map
      weight = 0.15, # Set line weight of subbasin polygons
      opacity = 0.75, # Set opacity of subbasin polygons boundaries
      fillOpacity = 0.5, # Set opacity of subbasin polygons
      na.color = "#808080"
    ) # Specify color for NA values
  })

  # Render Plot
  output$plot <- renderPlotly(
    ggplotly(
      ggplot(data = data()) +
        geom_boxplot(aes_string(y = colnames(data())[2]))
    )
  )
}
