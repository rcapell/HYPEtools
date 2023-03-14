#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

# Get values passed to app
results.dir <- getShinyOption("results.dir", default = NULL)
map <- getShinyOption("map", default = NULL)
map.subid.column <- getShinyOption("map.subid.column", default = NULL)

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
  if(!is.null(map)){
    volumes <- c("GIS Directory" = dirname(map), volumes)
  }
  
  # Get Path to GIS Files
  gis_file <- reactive({
    shinyFileChoose(input, "button_gis", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_gis) == "list"){
      if(is.null(map)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = map)
      }
    } else{
      files <- data.frame("Files" = parseFilePaths(volumes, input$button_gis)$datapath)
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
  
  # Input to select result file
  output$input_result <- renderUI({selectInput("result", "Select Result File To Display", choices = basename(results_files()$Files))})
  
  # Get selected result file
  result_file <- reactive({
    which(basename(results_files()$Files) == input$result)
  })
  
  # Create outputs for selected file
  output$gis_file <- renderText(paste("Selected GIS File:", gis_file()$Files[1]))
  output$result_file <- renderText(paste("Selected Result File:", results_files()$Files[result_file()]))
  
  output$path_mf <- DT::renderDataTable(gis(), options = list(scrollX = TRUE))
  
  # _____________________________________________________________________________________________________________________________________
  # Process GIS Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Read GIS Data
  gis <- reactive({
    req(!all(is.na(gis_file()$Files)))
    sf::st_read(gis_file()$Files[1])
  })
  
  # Input to select SUBID column in GIS file
  output$input_column <- renderUI({selectInput("column", "Select SUBID Column", choices = colnames(gis())[which(!colnames(gis()) %in% attr(gis(), "sf_column"))], selected = colnames(gis())[map.subid.column])})
  
  # Get column index of SUBID column in GIS file
  gis.subid <- reactive({which(colnames(gis()) == input$column)})
  
  # _____________________________________________________________________________________________________________________________________
  # Process MapOutput Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Read Data
  data_in <- reactive({
    req(!all(is.na(results_files()$Files)), result_file())
    
    # Safely read file and return NA if any error
    read_data <- possibly(~ReadMapOutput(results_files()$Files[result_file()]), otherwise = NA)
    read_data()
    
  })
  
  # Update time period slider based on input data
  observe({
    req(data_in())
    updateSliderInput(session, "slider", max = ncol(data_in()))
  })
  
  # Data used for app
  data <- reactive({
    req(!is.na(data_in()),input$slider)
    data_in()[, c(1, input$slider)]
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

  # Check that data can be joined
  leaf_check <- reactive({
    
    # Requirements
    req(gis(), gis.subid(), data())

    # Test join data
    check <- right_join(gis()[, gis.subid()]%>%mutate(across(1,~as.character(.x))), data()%>%mutate(across(1,~as.character(.x))), by = setNames(nm = colnames(gis())[gis.subid()], colnames(data())[1]))
    
    return(!all(sf::st_is_empty(check[[attr(check, "sf_column")]])))
  })

  # Create basemap
  leaf <- eventReactive(c(gis(), gis.subid(), result_file()),{

    # Require valid data
    req(leaf_check() == TRUE)
    
    # Parse full mapoutput file
    mapdata <- data_in() %>%
      pivot_longer(cols = 2:ncol(.)) %>%
      select(1, "value")
    
    # Create basemap and get data
    data <- PlotMapOutput(
      x = mapdata,
      map = gis(),
      map.type = "leaflet",
      map.subid.column = gis.subid(),
      plot.searchbar = TRUE,
      legend.pos = "bottomleft",
      legend.title = tools::file_path_sans_ext(input$result),
      legend.signif = 2, # Specify number of significant digits to include in map legend
      na.color = "#808080", # Specify color for NA values
      shiny.data = TRUE
    ) %>% suppressMessages()
    
    # Parse Data
    leaf <- data$basemap
    
    return(leaf)
  })
  
  # Render Map
  output$map <- renderLeaflet({leaf()})
  
  # Update map when data changes
  observe({
    
    # Require valid data
    req(leaf_check() == TRUE)
    
    # Get Data
    data <- PlotMapOutput(
      x = data(),
      map = gis(),
      map.type = "leaflet",
      map.subid.column = gis.subid(),
      shiny.data = TRUE
    ) %>%
      suppressMessages()

    # Parse Data
    x <- data$x

    # Update Map
    proxy <- leafletProxy("map", data = x) %>%
      addPolygons(
        group = "Subbasins",
        data = x,
        color = "black",
        weight = 0.15,
        opacity = 0.75,
        fillColor = ~color,
        fillOpacity = 0.5,
        label = ~label
      )
  })
  
}
