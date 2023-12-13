#' Shiny app server function
#' 
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny

# Import required dependencies
require(dplyr)

# Get values passed to app
results.dir <- shiny::getShinyOption("results.dir")
file.pattern <- shiny::getShinyOption("file.pattern")
sites <- shiny::getShinyOption("sites")
sites.subid.column <- shiny::getShinyOption("sites.subid.column")
bg <- shiny::getShinyOption("bg")
output.dir <- shiny::getShinyOption("output.dir")

# Define server logic
shinyAppServer <- function(input, output, session) {
  
  # _____________________________________________________________________________________________________________________________________
  # Help Buttons #####
  # _____________________________________________________________________________________________________________________________________
  
  # Help message for selecting GIS File
  shiny::observeEvent(input$help_gis, {
    shinyalert::shinyalert(
      title = "Select GIS Files:",
      type = "info",
      text = 'Use the "Select GIS File" button to select a GIS file (.shp or .gpkg) containing the point geometry of the model subbasin outlets/points of interest. Then, use the dropdown menu to select the name of the column containing the subbasin SUBIDs.
      
      If "Join Status: CHECK" is displayed, then the MapOutput file was joined to the GIS data, but there may be duplicate SUBIDS in the selected SUBID column or other potential problems that require checking. Ensure that the correct SUBID column is selected.
      
      If Join Status: FAIL" is displayed, then the MapOutput file could not be joined to the GIS data using the selected SUBID column, and a different column should be selected.
      
      Use the "Select Background File" button to select a GIS file (.shp or .gpkg) containing the subbasin polygons to use as a background layer.'
    )
  })
  
  # Help message for selecting mapoutput files
  shiny::observeEvent(input$help_result, {
    shinyalert::shinyalert(
      title = "Select Results Files:",
      type = "info",
      text = 'Use the button to select the HYPE MapOutput or Subass files (.txt or .csv) that should be imported. Multiple files may be selected at one time. Use the dropdown menu to select the name of file that should be visualized.'
    )
  })
  
  # Help message for selecting time period
  shiny::observeEvent(input$help_slider, {
    shinyalert::shinyalert(
      title = "Select Time Period/Statistic:",
      type = "info",
      text = 'Use the slider to select the time period (MapOutput files) or statistic (subass files) that should be visualized. The "play" button can be used to animate the visualizations by stepping through the periods/statistics automatically.'
    )
  })
  
  # Help message for output directory
  shiny::observeEvent(input$help_options, {
    shinyalert::shinyalert(
      title = "Options:",
      type = "info",
      text = 'Use the button to select the output directory for saved map images.'
    )
  })
  
  # Help message for GIS data table
  shiny::observeEvent(input$help_gis_df, {
    shinyalert::shinyalert(
      title = "GIS Data:",
      type = "info",
      text = 'This table displays the attribute table for the selected GIS file. Columns can be sorted and filtered. If the GIS data has been successfully joined to the MapOutput data (Join Status: CHECK or PASS), then filters applied to this table will also filter the data displayed in the "MapOutput Data" table and the boxplot.'
    )
  })
  
  # Help message for Results data table
  shiny::observeEvent(input$help_data_df, {
    shinyalert::shinyalert(
      title = "Results Data:",
      type = "info",
      text = 'This table displays the data for the selected MapOutput or Subass file. Columns can be sorted and filtered. Filters applied to this table do not affect the other outputs. However, if the results data has been successfully joined to the GIS data (Join Status: CHECK or PASS), then filters applied to the "GIS Data" table will also filter the data displayed in this table. If the GIS Data table filters are set such that all SUBIDs are excluded, then the table will reset to show all available result data.'
    )
  })
  
  # _____________________________________________________________________________________________________________________________________
  # File Management #####
  # _____________________________________________________________________________________________________________________________________
  
  # Get Available File Volumes
  volumes <- c("HYPEtools Demo Model" = system.file("demo_model", package = "HYPEtools"), Home = fs::path_home(), shinyFiles::getVolumes()())
  
  # Add Directories specified with shiny arguments
  if(!is.null(results.dir)){
    volumes <- c("Results Directory" = results.dir, volumes)
  }
  if(!is.null(sites)){
    volumes <- c("GIS Directory" = dirname(sites), volumes)
  }
  if(!is.null(bg)){
    volumes <- c("GIS Background Directory" = dirname(bg), volumes)
  }
  
  # Get Path to GIS Files
  gis_file <- shiny::reactive({
    shinyFiles::shinyFileChoose(input, "button_gis", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_gis) == "list"){
      if(is.null(sites)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = sites)
      }
    } else{
      files <- data.frame("Files" = shinyFiles::parseFilePaths(volumes, input$button_gis)$datapath)
    }
  })
  
  # Get Path to Background Files
  bg_file <- shiny::reactive({
    shinyFiles::shinyFileChoose(input, "button_bg", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_bg) == "list"){
      if(is.null(bg)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = bg)
      }
    } else{
      files <- data.frame("Files" = shinyFiles::parseFilePaths(volumes, input$button_bg)$datapath)
    }
  })

  # Get Paths to Results Files
  results_files <- shiny::reactive({
    shinyFiles::shinyFileChoose(input, "button_results", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_results) == "list"){
      if(is.null(results.dir)){
        files <- data.frame("Files" = character())
      } else{
        files <- data.frame("Files" = list.files(results.dir, full.names = TRUE, pattern = file.pattern))
      }
    } else{
      files <- data.frame("Files" = shinyFiles::parseFilePaths(volumes, input$button_results)$datapath)
    }
  })
  
  # Input to select result file
  output$input_result <- shiny::renderUI({shiny::selectInput("result", "Select Result File To Display", choices = basename(results_files()$Files))})
  
  # Get selected result file
  result_file <- shiny::reactive({
    which(basename(results_files()$Files) == input$result)
  })
  
  # Create outputs for selected file
  output$gis_file <- shiny::renderText(gis_file()$Files[1])
  output$bg_file <- shiny::renderText(bg_file()$Files[1])
  output$result_file <- shiny::renderText(dirname(results_files()$Files[result_file()]))
  
  # _____________________________________________________________________________________________________________________________________
  # Process GIS Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Read GIS Data
  gis <- shiny::reactive({
    shiny::req(!all(is.na(gis_file()$Files)))
    gis <- sf::st_read(gis_file()$Files[1])
    geo_type <- sf::st_geometry_type(gis, by_geometry = FALSE) # Get geometry type
    
    # Send warning if GIS file is not point type
    if(!geo_type %in% c("POINT", "MULTIPOINT")){
      shinyalert::shinyalert(title = "Select GIS File", text = "Selected GIS file does not have POINT geometry.", type = "error")
    }
    req(geo_type %in% c("POINT", "MULTIPOINT"))
    return(gis)
  })
  
  # Get column index of SUBID column in GIS file
  gis.subid <- shiny::reactive({which(colnames(gis()) == input$column)})
  
  # Output table for GIS
  output$gis <- DT::renderDataTable(
    gis() %>%
      sf::st_drop_geometry() %>%
      # Convert strings columns to factors if there are any duplicated values
      mutate(across(where(is.character), function(X) {
        if (any(duplicated(X))) {
          as.factor(X)
        }
      })),
    rownames = FALSE, filter = "top", options = list(scrollX = TRUE, lengthMenu = c(5, 10, 25, 50, 100))
  )
  # output$gis <- DT::renderDataTable(datatable(gis() %>% sf::st_drop_geometry(), rownames = FALSE, options = list(scrollX = TRUE)) %>% formatRound(unlist(lapply(gis() %>% sf::st_drop_geometry, is.numeric), use.names = FALSE), 3)) # Use this to round numeric columns, but then this affects columns like SUBID
  
  # GIS Data filtered by data table
  gis_filtered <- shiny::reactive({
    gis()[input$gis_rows_all,]
  })
  
  # Get filtered GIS subids
  gis_filtered_subids <- shiny::reactive({
    get_subids <- purrr::possibly(~{
      gis()[input$gis_rows_all,gis.subid()] %>%
        sf::st_drop_geometry() %>%
        unlist()
    }, otherwise = c())
    get_subids()
  })
  
  # Input to select SUBID column in GIS file
  output$input_column <- shiny::renderUI({shiny::selectInput("column", "Select SUBID Column", choices = colnames(gis())[which(!colnames(gis()) %in% attr(gis(), "sf_column"))], selected = colnames(gis())[sites.subid.column])})
  
  # Download Data
  output$download_gis <- shiny::downloadHandler(
    filename = "gis_data.csv",
    content = function(file){
      write.csv(gis_filtered() %>% sf::st_drop_geometry(), file, row.names = FALSE)
    }
  )
  
  # _____________________________________________________________________________________________________________________________________
  # Process GIS Background Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Read GIS Data
  gis_bg <- shiny::reactive({
    
    if(!all(is.na(bg_file()$Files))){
      shiny::req(!all(is.na(bg_file()$Files)))
      gis <- sf::st_read(bg_file()$Files[1])
      geo_type <- sf::st_geometry_type(gis, by_geometry = FALSE) # Get geometry type
      
      # Send warning if GIS file is not point type
      if(!geo_type %in% c("POLYGON", "MULTIPOLYGON")){
        shinyalert::shinyalert(title = "Select GIS File", text = "Selected GIS file does not have POLYGON geometry.", type = "error")
      }
      req(geo_type %in% c("POLYGON", "MULTIPOLYGON"))
    } else{
      gis <- NULL
    }
    
    return(gis)
  })
  
  # _____________________________________________________________________________________________________________________________________
  # Process MapOutput Data #####
  # _____________________________________________________________________________________________________________________________________
  
  # Save what kind of input data
  result_type <- reactiveVal()
  
  # Read Data
  data_in <- shiny::reactive({
    shiny::req(!all(is.na(results_files()$Files)), result_file())
    
    # Safely read file and return NA if any error - leave col.prefix because without the prefix the SliderText Input changes the choices for e.g. "1988.10" to "1988.1" and then it doesn't work
    read_data_map <- purrr::possibly(~ReadMapOutput(results_files()$Files[result_file()], col.prefix = "X"), otherwise = NA)
    read_data_subass <- purrr::possibly(~ReadSubass(results_files()$Files[result_file()], check.names = TRUE, na.strings = c("****************", -9999)), otherwise = NA)
    
    # Try reading as MapOutput first
    data_in <- read_data_map()
    
    # If reading doesn't work, then try reading as Subass
    if(all(is.na(data_in))){
      data_in <- read_data_subass()
      if(!all(is.na(data_in))){
        result_type("Subass")
      }
    } else{
      result_type("MapOutput")
    }
    
    return(data_in)
  })
  
  # Update time period slider based on input data
  shiny::observe({
    shiny::req(data_in())
    if(ncol(data_in()) > 2){ # If more than 1 time period
      shinyWidgets::updateSliderTextInput(session, "slider", choices = colnames(data_in()[2:ncol(data_in())]))
    } else{ # If only one time period
      shinyWidgets::updateSliderTextInput(session, "slider", choices = rep(colnames(data_in()[2:ncol(data_in())]), 2))
    }
  })
  
  # Check if slider has loaded
  slider_loaded <- shiny::reactiveVal(FALSE)
  
  # Set to TRUE if slider has initialized
  shiny::observe({
    if(!input$slider == "NA"){
      slider_loaded(TRUE)
    }
  })
  
  # Reset if the result type changes and the slider values also change
  shiny::observeEvent(result_type(),{
    slider_loaded(FALSE)
  })

  # Data used for app
  data <- shiny::reactive({
    shiny::req(!is.na(data_in()), slider_loaded() == TRUE, input$slider %in% colnames(data_in()))
    filtered_data <- data_in()[, c(1, which(colnames(data_in()) == input$slider))]
  })
  
  # Data displayed in table
  data_out <- shiny::reactive({
    
    # Get data
    df <- data()

    # Check if GIS data available
    check <- purrr::possibly(~leaf_check(), otherwise = FALSE)
    subids <- purrr::possibly(~gis_filtered_subids(), otherwise = c())
    
    # Filter data to GIS
    if(check() == TRUE & length(subids()) > 0){
      df <- df[which(df[,1] %in% subids()),]
    }
    
    # Format table
    df %>%
      arrange(desc(.[[2]])) # Arrange column
  })
  
  # Render Data Table
  output$table <- DT::renderDataTable(data_out() %>% rename_with(~gsub("^X", "", .), .cols = 2), rownames = FALSE, filter = "top", options = list(scrollX = TRUE, lengthMenu = c(5, 10, 25, 50, 100)))
  
  # Download Data
  output$download_data <- shiny::downloadHandler(
    filename = "result_data.csv",
    content = function(file){
      write.csv(data_out(), file, row.names = FALSE)
    }
  )
  
  # _____________________________________________________________________________________________________________________________________
  # Create Plotly BoxPlot #####
  # _____________________________________________________________________________________________________________________________________
  
  # Reactive value to generate boxplot
  boxplot_load <- shiny::reactiveVal(0)
  
  # Update reactive value when new data is available
  shiny::observeEvent(c(data_in(), slider_loaded(), gis_filtered_subids()),{
    shiny::req(slider_loaded() == TRUE)
    i = boxplot_load() + 1
    boxplot_load(i)
  })
  
  # Generate Boxplot
  boxplot <- shiny::eventReactive(boxplot_load(),{
    
    shiny::req(boxplot_load() > 0)
    
    # Get plot data
    plot_data <- data_out() %>% na.omit()
    
    # Create template plot if all data is NA
    if(nrow(plot_data) == 0){
      plot <- plotly::ggplotly(
        ggplot2::ggplot() +
          ggplot2::geom_boxplot(ggplot2::aes(y = NA))
      )
    # Create plot with available data
    } else{
      plot <- plotly::ggplotly(
        ggplot2::ggplot(data = plot_data) +
          ggplot2::geom_boxplot(ggplot2::aes(y = .data[[input$slider]]))
      )
    }

    # Update plot with plotly
    plot <- plot %>%
      plotly::add_trace(y = plot_data[[input$slider]], type = "box", name = "log", visible = FALSE, marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y") %>% # Trace for log y-axis
      plotly::layout(
        xaxis = list(autorange = TRUE, ticks = "", title = list(text = paste0("<b>", gsub("^X", "", colnames(plot_data)[2]), "</b>"), font = list(size = 14)), showticklabels = FALSE),
        yaxis = list(autorange = TRUE, tickmode = "auto", title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "linear", showticklabels = ifelse(nrow(plot_data) == 0, FALSE, TRUE)), # Show tick labels only if data isn't all NA
        updatemenus = list(list(
          active = 0,
          buttons = list(
            list(
              label = "Linear",
              method = "update",
              args = list(list(visible = c(TRUE, FALSE)), list(yaxis = list(title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "linear", showticklabels = ifelse(nrow(plot_data) == 0, FALSE, TRUE))))
            ),
            list(
              label = "Log",
              method = "update",
              args = list(list(visible = c(FALSE, TRUE)), list(yaxis = list(title = list(text = paste0("<b>", gsub("map", "", tools::file_path_sans_ext(input$result)), "</b>"), font = list(size = 16)), type = "log", showticklabels = ifelse(nrow(plot_data) == 0, FALSE, TRUE))))
            )
          )
        ))
      )

    # Return plot
    return(plot)
  })
  
  # Update Boxplot
  shiny::observe({

    # Get Data
    data <- data_out()

    # Duplicate data if there is only data for one point so that the boxplot can get generated
    if (nrow(data) == 1) {data <- rbind(data, data)}

    plotly::plotlyProxy("plot", session) %>%
      plotly::plotlyProxyInvoke("deleteTraces", list(as.integer(0), as.integer(1))) %>%
      plotly::plotlyProxyInvoke("relayout", list(xaxis = list(autorange = TRUE, ticks = "", title = list(text = paste0("<b>", gsub("^X", "", colnames(data)[2]), "</b>"), font = list(size = 14)), showticklabels = FALSE))) %>%
      plotly::plotlyProxyInvoke("addTraces", list(x = 0, y = data[[input$slider]], type = "box", name = "linear", marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y")) %>%
      plotly::plotlyProxyInvoke("addTraces", list(x = 0, y = data[[input$slider]], type = "box", name = "log", visible = FALSE, marker = list(color = "black"), line = list(color = "black"), fillcolor = "white", hoverinfo = "y"))
  })
  
  # Render Plot
  output$plot <- plotly::renderPlotly({boxplot()})
  
  # _____________________________________________________________________________________________________________________________________
  # Create Leaflet Map #####
  # _____________________________________________________________________________________________________________________________________

  # Check that data can be joined
  leaf_check <- shiny::reactive({
    
    # Requirements
    shiny::req(gis_filtered(), gis.subid(), data())

    # Test join data
    check <- right_join(gis_filtered()[, gis.subid()]%>%mutate(across(1,~as.character(.x))), data()%>%mutate(across(1,~as.character(.x))), by = setNames(nm = colnames(gis_filtered())[gis.subid()], colnames(data())[1]))
    
    return(!all(sf::st_is_empty(check[[attr(check, "sf_column")]])))
  })
  
  # Output for leaf_check
  output$join_status <- shiny::renderUI({
    
    if(leaf_check() == TRUE){
      if(nrow(gis_filtered()) == nrow(data_out())){
        shiny::div(style = "display: inline-block; font-weight: bold; color: limegreen", "PASS")
      } else{
        shiny::div(style = "display: inline-block; font-weight: bold; color: orange", "CHECK")
      }
    } else{
      shiny::div(style = "display: inline-block; font-weight: bold; color: red","FAIL")
    }
  })
  
  # Reactive values to store stuff for legend
  lcol <- reactiveVal()
  cbrks <- reactiveVal()
  
  # Update legend if slider changes for subass file
  slider_legend_update <- reactiveVal(0)
  observeEvent(input$slider,{
    req(result_type())
    val = slider_legend_update()
    if(result_type() == "Subass"){
      slider_legend_update(val + 1)
    }
  })

  # Create basemap
  leaf <- shiny::eventReactive(c(gis_filtered(), gis_bg(), gis.subid(), result_file(), slider_loaded(), result_type(), slider_legend_update()),{

    # Require valid data
    shiny::req(leaf_check() == TRUE, slider_loaded() == TRUE)
    
    # Parse full mapoutput file
    mapdata <- data_in() %>%
      tidyr::pivot_longer(cols = 2:ncol(.)) %>%
      select(1, "value")
    
    # Require data
    shiny::req(!all(is.na(mapdata$value)))
    
    # Get variable name
    if(result_type() == "Subass"){
      var.name <- input$slider
    } else{
      var.name <- gsub("map", "", tools::file_path_sans_ext(input$result))
    }

    # Create basemap and get data
    data <- PlotMapPoints(
      x = mapdata,
      sites = gis_filtered(),
      map.type = "leaflet",
      sites.subid.column = gis.subid(),
      bg = gis_bg(),
      plot.label = TRUE,
      legend.pos = "bottomleft",
      var.name = var.name,
      legend.signif = 2, # Specify number of significant digits to include in map legend
      na.color = "#808080", # Specify color for NA values
      shiny.data = TRUE
    ) %>% 
      suppressMessages() %>%
      suppressWarnings()
    
    # Save function and breaks used to create legend
    lcol(data$lcol)
    cbrks(data$cbrks)
    
    # Parse Data and add button to save map
    leaf <- data$basemap %>%
      leaflet::addEasyButton(leaflet::easyButton(states = list(
        leaflet::easyButtonState(
          stateName = "onestate",
          icon = "fa-camera", title = "Save Map",
          onClick = leaflet::JS(" function(btn, map) {Shiny.onInputChange('leaf_save_button', 'save'); Shiny.onInputChange('leaf_save_button', 'reset')}") # The "reset" state is so that the input resets after it's clicked so you can click the button again
        )
      )))

    return(leaf)
  })
  
  # Render Map
  output$map <- leaflet::renderLeaflet({leaf()})
  
  # Update map when data changes
  shiny::observe({

    # Call background to trigger update if new basemap created
    gis_bg()
    slider_legend_update()

    # Require valid data
    shiny::req(leaf_check() == TRUE, lcol(), cbrks())
    
    # Get variable name
    if(result_type() == "Subass"){
      var.name <- input$slider
    } else{
      var.name <- gsub("map", "", tools::file_path_sans_ext(input$result))
    }
    
    # Require at least some data
    req(!all(is.na(data()[2])))

    # Get Data
    data <- PlotMapPoints(
      x = data(),
      sites = gis_filtered(),
      map.type = "leaflet",
      sites.subid.column = gis.subid(),
      plot.label = TRUE,
      var.name = var.name,
      legend.pos = "bottomleft",
      col = if(length(lcol())==length(cbrks())){lcol()[1:length(lcol())-1]}else{lcol()}, # Remove NA color since this should get added back in
      col.breaks = cbrks(),
      shiny.data = TRUE
    ) %>%
      suppressMessages() %>%
      suppressWarnings()

    # Parse Data
    x <- data$x

    # Update Map
    proxy <- leaflet::leafletProxy("map", data = x) %>%
      leaflet::clearGroup("Points") %>%
      leaflet::addCircleMarkers(
        group = "Points",
        data = x,
        color = "black",
        radius = 5,
        weight = 0.15,
        opacity = 0.75,
        fillColor = ~color,
        fillOpacity = 0.5,
        label = ~label,
        labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", textOnly = FALSE, style = list("font-size" = paste0(10, "px")))
      )
  })

  # Emulated map for downloading
  leaf_save <- shiny::reactive({
    
    # Get variable name
    if(result_type() == "Subass"){
      var.name <- input$slider
    } else{
      var.name <- gsub("map", "", tools::file_path_sans_ext(input$result))
    }
    
    # Get Data
    data <- PlotMapPoints(
      x = data(),
      sites = gis_filtered(),
      map.type = "leaflet",
      sites.subid.column = gis.subid(),
      plot.label = TRUE,
      var.name = var.name,
      legend.pos = "bottomleft",
      col = if(length(lcol())==length(cbrks())){lcol()[1:length(lcol())-1]}else{lcol()}, # Remove NA color since this should get added back in
      col.breaks = cbrks(),
      shiny.data = TRUE
    ) %>%
      suppressMessages() %>%
      suppressWarnings()
    
    # Parse Data
    x <- data$x
    
    # Recreate map
    map <- leaf() %>%
      leaflet::clearGroup("Points") %>%
      leaflet::addCircleMarkers(
        group = "Points",
        data = x,
        color = "black",
        radius = 5,
        weight = 0.15,
        opacity = 0.75,
        fillColor = ~color,
        fillOpacity = 0.5,
        label = ~label,
        labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", textOnly = FALSE, style = list("font-size" = paste0(10, "px")))
      ) %>%
      leaflet::setView(lng = input$map_center$lng, lat = input$map_center$lat, zoom = input$map_zoom)
  })
  
  # Get Paths to output directory
  output_dir <- shiny::reactive({
  
    shinyFiles::shinyDirChoose(input, "button_save", roots = volumes, session = session)

    # If button hasn't been used to select files, then return default value/provided with shiny arguments
    if (!typeof(input$button_save) == "list"){
      if(is.null(output.dir)){
        dir <- NULL
      } else{
        dir <- output.dir
      }
    } else{
      dir <- shinyFiles::parseDirPath(volumes, input$button_save)
    }
  })
  
  # Text output for output directory
  output$output_dir <- shiny::renderText(output_dir())
  
  # Save map when button clicked
  shiny::observeEvent(input$leaf_save_button,{
    
    # Send warning if no output directory selected
    if(is.null(output_dir())){
      shinyalert::shinyalert(
        title = "Save Map:",
        type = "error",
        text = 'No output directory specified. Please click the "Select Output Directory" button and specify a directory.'
      )
    
    # Save map
    } else{
      # Get filename
      file <- file.path(output_dir(), paste0(tools::file_path_sans_ext(input$result), "_", gsub("^X", "", input$slider), ".png"))
      
      # Save Image
      shiny::withProgress(value = 0, message = "Saving Map",{
        mapview::mapshot(leaf_save(), file = file, remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"), selfcontained = FALSE)
        shiny::incProgress(1)
      })
      
      
      # Confirm success
      if(file.exists(file)){
        shinyalert::shinyalert(
          title = "Save Map:",
          type = "success",
          text = paste0('File saved successfully as: \n', file),
          time = 5000
        )
      } else{
        shinyalert::shinyalert(
          title = "Save Map:",
          type = "error",
          text = paste0('File not saved to: \n', file)
        )
      }
    }
  })
  
}
