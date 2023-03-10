#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Get values passed to app
model.dir <- getShinyOption("model.dir", default = NULL)
results.dir <- getShinyOption("results.dir", default = NULL)
option.map <- getShinyOption("option.map", default = NULL)
option.var.name <- getShinyOption("option.var.name", default = NULL)

# Define server logic
shinyAppServer <- function(input, output, session) {
  
  # Get Available File Volumes
  volumes <- c("HYPEtools Demo Model" = system.file("demo_model", package = "HYPEtools"), Home = fs::path_home(), getVolumes()())
  
  # Add Directories specified with shiny arguments
  if(!is.null(results.dir)){
    volumes <- c("Results Directory" = results.dir, volumes)
  }
  if(!is.null(model.dir)){
    volumes <- c("Model Directory" = model.dir, volumes)
  }
  
  # Get Paths to Model Files
  model_files <- reactive({
    shinyFileChoose(input, "button_model", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_model) == "list"){
      if(is.null(model.dir)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = list.files(model.dir, full.names = T))
      }
    } else{
      files <- data.frame("Files" = parseFilePaths(volumes, input$button_model)$datapath)
    }
  })

  # Get Paths to Results Files
  results_files <- reactive({
    shinyFileChoose(input, "button_results", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_results) == "list"){
      if(is.null(results.dir)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = list.files(results.dir, full.names = T))
      }
    } else{
      files <- data.frame("Files" = parseFilePaths(volumes, input$button_results)$datapath)
    }
  })
  
  # Create outputs
  output$path_mf <- DT::renderDataTable(model_files())
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
