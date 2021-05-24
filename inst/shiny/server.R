#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'

# Get values passed to app
option.x <- getShinyOption("option.x", default = NULL)
option.map <- getShinyOption("option.map", default = NULL)
option.var.name <- getShinyOption("option.var.name", default = NULL)

# Define server logic required to draw a histogram
shinyAppServer <- function(input, output) {

  # Get Data
  data <- reactive({
    option.x[, c(1, input$slider + 1)] # Subset to first column and then column from slider
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
