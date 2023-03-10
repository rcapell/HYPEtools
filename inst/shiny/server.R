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
  
  # _____________________________________________________________________________________________________________________________________
  # File Management #####
  # _____________________________________________________________________________________________________________________________________
  
  
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
  
  # _____________________________________________________________________________________________________________________________________
  # Process MapOutput Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Get Data
  data <- reactive({
    req(!all(is.na(results_files()$Files)))
    ReadMapOutput(results_files()$Files)[, c(1, input$slider + 1)]
  })
  
  # Render Data Table
  output$table <- renderDataTable(data())
  
  # _____________________________________________________________________________________________________________________________________
  # Create Plotly BoxPlot #####
  # _____________________________________________________________________________________________________________________________________
  
  # Render Plot
  output$plot <- renderPlotly(
    ggplotly(
      ggplot(data = data()) +
        geom_boxplot(aes_string(y = colnames(data())[2]))
    )
  )
  
  # _____________________________________________________________________________________________________________________________________
  # Create Leaflet Map #####
  # _____________________________________________________________________________________________________________________________________

  # Create reactive value to store basemap
  leaf <- reactiveVal()

  # Update basemap when button clicked - UPDATE THIS TO BE WHEN SELECTED MAPOUTPUT FILE CHANGES
  observeEvent(input$button_results,{
    leaf(PlotMapOutput(
      x = data(),
      map = option.map,
      var.name = option.var.name,
      map.type = "leaflet",
      map.subid.column = 2,
      basemap.only = TRUE
    ) %>% suppressMessages())
  })
  
  # Render Map
  output$map <- renderLeaflet({leaf()})
  
  # Update map when data changes
  observe({
    
    # Get Data
    data <- PlotMapOutput(
      x = data(),
      map = option.map,
      var.name = option.var.name,
      map.type = "leaflet",
      map.subid.column = 2,
      legend.signif = 2, # Specify number of significant digits to include in map legend
      na.color = "#808080", # Specify color for NA values
      data.only = TRUE
    ) %>%
      suppressMessages()
    
    # Parse Data
    x <- data$x
    lcol <- data$lcol
    l.label <- data$l.label
    
    # Get Bounds of Data
    bounds <- x %>%
      sf::st_bbox() %>%
      as.character()

    # Update Map
    proxy <- leafletProxy("map", data = x) %>%
      clearControls() %>%
      addPolygons(
        group = "Subbasins",
        data = x,
        color = "black",
        weight = 0.15,
        opacity = 0.75,
        fillColor = ~color,
        fillOpacity = 0.5,
        label = ~label
      ) %>%
      
      # Zoom to Layer
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      
      # Add overlay group
      leaflet::addLayersControl(
        baseGroups = c("Map", "Street", "Topo", "Satellite"),
        overlayGroups = c("Subbasins"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoIndex = TRUE)
      ) %>%
      
      # Add search features
      leaflet.extras::addSearchFeatures(
        targetGroups = "Subbasins",
        options = leaflet.extras::searchFeaturesOptions(zoom = 10, hideMarkerOnCollapse = TRUE)
      ) %>%
      
      # Add legend
      leaflet::addLegend(
        group = "Subbasins",
        position = "bottomleft",
        title = option.var.name,
        colors = lcol,
        labels = l.label,
        values = data()[[2]],
        opacity = 1
      )
  })
  
}
