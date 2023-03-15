#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

# Get values passed to app
results.dir <- getShinyOption("results.dir", default = NULL)
map <- getShinyOption("map", default = NULL)
map.subid.column <- getShinyOption("map.subid.column", default = NULL)
output.dir <- getShinyOption("output.dir", default = NULL)

# Define server logic
shinyAppServer <- function(input, output, session) {
  
  # _____________________________________________________________________________________________________________________________________
  # Help Buttons #####
  # _____________________________________________________________________________________________________________________________________
  
  # Help message for selecting GIS File
  observeEvent(input$help_gis, {
    shinyalert(
      title = "Select GIS File:",
      type = "info",
      text = 'Use the button to select a GIS file (.shp or .gpkg) containing the polygon geometry of the model subbasins. Then, use the dropdown menu to select the name of the column containing the subbasin SUBIDs.
      
      If "Join Status: FAIL" is displayed, then the MapOutput file could not be joined to the GIS data using the selected SUBID column, and a different column should be selected.'
    )
  })
  
  # Help message for selecting mapoutput files
  observeEvent(input$help_result, {
    shinyalert(
      title = "Select MapOutput Files:",
      type = "info",
      text = 'Use the button to select the HYPE MapOutput files (.txt or .csv) that should be imported. Multiple files may be selected at one time. Use the dropdown menu to select the name of file that should be visualized.'
    )
  })
  
  # Help message for selecting time period
  observeEvent(input$help_slider, {
    shinyalert(
      title = "Select Time Period:",
      type = "info",
      text = 'Use the slider to select the time period in the MapOutput file that should be visualized. The "play" button can be used to animate the visualizations by stepping through the time periods automatically.'
    )
  })
  
  # Help message for MapOutput data table
  observeEvent(input$help_options, {
    shinyalert(
      title = "Options:",
      type = "info",
      text = 'Use the button to select the output directory for saved map images.'
    )
  })
  
  # Help message for GIS data table
  observeEvent(input$help_gis_df, {
    shinyalert(
      title = "GIS Data:",
      type = "info",
      text = 'This table displays the attribute table for the selected GIS file. Columns can be sorted and filtered.'
    )
  })
  
  # Help message for MapOutput data table
  observeEvent(input$help_data_df, {
    shinyalert(
      title = "MapOutput Data:",
      type = "info",
      text = 'This table displays the data for the selected MapOutput file. Columns can be sorted and filtered.'
    )
  })
  
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
        files <- data.frame("Files" = list.files(results.dir, full.names = T, pattern = ".*\\.(txt|csv)$"))
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
  output$gis_file <- renderText(gis_file()$Files[1])
  output$result_file <- renderText(dirname(results_files()$Files[result_file()]))
  output$gis <- DT::renderDataTable(gis() %>% st_drop_geometry(), rownames = F, filter = "top", options = list(scrollX = TRUE))
  # output$gis <- DT::renderDataTable(datatable(gis() %>% st_drop_geometry(), rownames = F, options = list(scrollX = TRUE)) %>% formatRound(unlist(lapply(gis() %>% st_drop_geometry, is.numeric), use.names = FALSE), 3)) # Use this to round numeric columns, but then this affects columns like SUBID 
  
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
    
    # Safely read file and return NA if any error - leave col.prefix because without the prefix the SliderText Input changes the choices for e.g. "1988.10" to "1988.1" and then it doesn't work
    read_data <- possibly(~ReadMapOutput(results_files()$Files[result_file()], col.prefix = "X"), otherwise = NA)
    read_data()
  })
  
  # Update time period slider based on input data
  observe({
    req(data_in())
    if(ncol(data_in()) > 2){ # If more than 1 time period
      updateSliderTextInput(session, "slider", choices = colnames(data_in()[2:ncol(data_in())]))
    } else{ # If only one time period
      updateSliderTextInput(session, "slider", choices = rep(colnames(data_in()[2:ncol(data_in())]), 2))
    }
  })
  
  # Check if time period slider has loaded
  slider_loaded <- reactiveVal(FALSE)
  
  observe({
    if(!input$slider == "NA"){
      slider_loaded(TRUE)
    }
  })

  # Data used for app
  data <- reactive({
    req(!is.na(data_in()), !input$slider == "NA", input$slider %in% colnames(data_in()))
    data_in()[, c(1, which(colnames(data_in()) == input$slider))]
  })
  
  # Render Data Table
  output$table <- renderDataTable(data() %>% rename_with(~gsub("^X", "", .), .cols = 2), rownames = F, filter = "top", options = list(scrollX = TRUE))
  
  # _____________________________________________________________________________________________________________________________________
  # Create Plotly BoxPlot #####
  # _____________________________________________________________________________________________________________________________________
  
  # Reactive value to generate boxplot
  boxplot_load <- reactiveVal(0)
  
  # Update reactive value when new data is available
  observeEvent(c(data_in(), slider_loaded()),{
    req(slider_loaded() == T)
    i = boxplot_load() + 1
    boxplot_load(i)
  })
  
  # Generate Boxplot
  boxplot <- eventReactive(boxplot_load(),{
    
    req(boxplot_load() > 0)
    
    # Create plot first with ggplot
    ggplotly(
      ggplot(data = data()) +
        geom_boxplot(aes_(y = as.name(input$slider)))
    ) %>%

      # Update plot with plotly
      add_trace(y = data()[[input$slider]], type = "box", name = "log", visible = F, marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y") %>% # Trace for log y-axis
      layout(
        xaxis = list(autorange = TRUE, ticks = "", title = list(text = paste0("<b>", gsub("^X", "", colnames(data())[2]), "</b>"), font = list(size = 14)), showticklabels = FALSE),
        yaxis = list(autorange = TRUE, tickmode = "auto", title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "linear"),
        updatemenus = list(list(
          active = 0,
          buttons = list(
            list(
              label = "linear",
              method = "update",
              args = list(list(visible = c(T, F)), list(yaxis = list(title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "linear")))
            ),
            list(
              label = "log",
              method = "update",
              args = list(list(visible = c(F, T)), list(yaxis = list(title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "log")))
            )
          )
        ))
      )
  })
  
  # Update Boxplot
  observe({
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke("deleteTraces", list(as.integer(0), as.integer(1))) %>%
      plotlyProxyInvoke("relayout", list(xaxis = list(autorange = TRUE, ticks = "", title = list(text = paste0("<b>", gsub("^X", "", colnames(data())[2]), "</b>"), font = list(size = 14)), showticklabels = FALSE))) %>%
      plotlyProxyInvoke("addTraces", list(x = 0, y = data()[[input$slider]], type = "box", name = "linear", marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y")) %>%
      plotlyProxyInvoke("addTraces", list(x = 0, y = data()[[input$slider]], type = "box", name = "log", visible = F, marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y"))
  })
  
  # Render Plot
  output$plot <- renderPlotly({boxplot()})
  
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
  
  # Output for leaf_check
  output$join_status <- renderUI({
    
    if(leaf_check() == TRUE){
      div(style = "display: inline-block; font-weight: bold; color: limegreen", "PASS")
    } else{
      div(style = "display: inline-block; font-weight: bold; color: red","FAIL")
    }
  })

  # Create basemap
  leaf <- eventReactive(c(gis(), gis.subid(), result_file(), slider_loaded()),{

    # Require valid data
    req(leaf_check() == TRUE, slider_loaded() == TRUE)
    
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
      var.name = gsub("map", "", tools::file_path_sans_ext(input$result)),
      legend.signif = 2, # Specify number of significant digits to include in map legend
      na.color = "#808080", # Specify color for NA values
      shiny.data = TRUE
    ) %>% 
      suppressMessages() %>%
      suppressWarnings()
    
    # Parse Data and add button to save map
    leaf <- data$basemap %>%
      addEasyButton(easyButton(states = list(
        easyButtonState(
          stateName = "onestate",
          icon = "fa-camera", title = "Save Map",
          onClick = JS(" function(btn, map) {Shiny.onInputChange('leaf_save_button', 'save'); Shiny.onInputChange('leaf_save_button', 'reset')}") # The "reset" state is so that the input resets after it's clicked so you can click the button again
        )
      )))
    
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
      var.name = gsub("map", "", tools::file_path_sans_ext(input$result)),
      shiny.data = TRUE
    ) %>%
      suppressMessages()

    # Parse Data
    x <- data$x

    # Update Map
    proxy <- leafletProxy("map", data = x) %>%
      clearGroup("Subbasins") %>%
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

  # Emulated map for downloading
  leaf_save <- reactive({
    
    # Get Data
    data <- PlotMapOutput(
      x = data(),
      map = gis(),
      map.type = "leaflet",
      map.subid.column = gis.subid(),
      var.name = gsub("map", "", tools::file_path_sans_ext(input$result)),
      shiny.data = TRUE
    ) %>%
      suppressMessages()
    
    # Parse Data
    x <- data$x
    
    # Recreate map
    map <- leaf() %>%
      clearGroup("Subbasins") %>%
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
      setView(lng = input$map_center$lng, lat = input$map_center$lat, zoom = input$map_zoom)
  })
  
  # Get Paths to output directory
  output_dir <- reactive({
  
    shinyDirChoose(input, "button_save", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_save) == "list"){
      if(is.null(output.dir)){
        dir <- NULL
      } else{
        dir <- output.dir
      }
    } else{
      dir <- parseDirPath(volumes, input$button_save)
    }
  })
  
  # Text output for output directory
  output$output_dir <- renderText(output_dir())
  
  # Save map when button clicked
  observeEvent(input$leaf_save_button,{
    
    # Send warning if no output directory selected
    if(is.null(output_dir())){
      shinyalert(
        title = "Save Map:",
        type = "error",
        text = 'No output directory specified. Please click the "Select Output Directory" button and specify a directory.'
      )
    
    # Save map
    } else{
      # Get filename
      file <- file.path(output_dir(), paste0(tools::file_path_sans_ext(input$result), "_", gsub("^X", "", input$slider), ".png"))
      
      # Save Image
      mapview::mapshot(leaf_save(), file = file, remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"), selfcontained = FALSE)
      
      # Confirm success
      if(file.exists(file)){
        shinyalert(
          title = "Save Map:",
          type = "success",
          text = paste0('File saved successfully as: \n', file),
          time = 5000
        )
      } else{
        shinyalert(
          title = "Save Map:",
          type = "error",
          text = paste0('File not saved to: \n', file)
        )
      }
    }
  })
  
}
