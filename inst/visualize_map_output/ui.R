#' Shiny app server object

# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Head
  tags$head(
    tags$title("VisualizeMapOutput"),
    
    tags$style(HTML("h5 {font-weight: bold; margin-bottom:0px}", # Set style for H5
                    "hr {margin-bottom:0px}", # Set style for hr
                    ".selectize-control {margin-bottom: 0px}",
                    ".form-group {margin-bottom: 0px}",
                    ".help-button {color:black;}", # Text Color for help buttons on tabs
                    ".help-button:hover {color:gray;}", #Hover Text Color for help buttons on tabs
                    ".help-button:focus {color:black;outline:0;}", #Changes help buttons back to black after clicking on them
                    ".leaflet-control-search .search-button {border: 2px solid rgba(0,0,0,0.2) !important; width:34px !important; height:34px !important; background-size: 20px !important; background-position-x: 5px !important; background-position-y: 6px !important;}",
                    ".leaflet-container .leaflet-control-search {box-shadow: none !important}"
    ))
  ),
  
  # Create button to select directories for model files and results files
  div(
    div(style="display:table-cell; vertical-align:top; width:23em; padding-right: 20px",
        h2(tags$b("VisualizeMapOutput")),
        hr(),
        div(div(style="display:inline-block",h4(tags$b("Select GIS File:"))),div(style="display:inline-block",actionButton("help_gis",label="",icon=icon("circle-question"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
        shinyFiles::shinyFilesButton("button_gis", "Select GIS File" , title = "Please Select File:", buttonType = "default", class = NULL, multiple = FALSE),
        h5("Selected GIS File:"),
        div(style = "max-width: 23em; padding-bottom: 10px; overflow-wrap: break-word; font-size: 0.8em", textOutput("gis_file")),
        uiOutput("input_column"),
        div(style = "display:inline-block",
            div(style = "display: inline-block", h5("Join Status:")),
            div(style = "display:inline-block", uiOutput("join_status"))
            ),
        hr(),
        div(div(style="display:inline-block",h4(tags$b("Select MapOutput Files:"))),div(style="display:inline-block",actionButton("help_result",label="",icon=icon("circle-question"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
        shinyFiles::shinyFilesButton("button_results", "Select MapOutput Files" , title = "Please Select Files:", buttonType = "default", class = NULL, multiple = TRUE),
        h5("Selected Result Directory:"),
        div(style = "max-width: 23em; padding-bottom: 10px; overflow-wrap: break-word; font-size: 0.8em", textOutput("result_file")),
        uiOutput("input_result"),
        hr(),
        div(div(style="display:inline-block",h4(tags$b("Select Time Period:"))),div(style="display:inline-block",actionButton("help_slider",label="",icon=icon("circle-question"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
        shinyWidgets::sliderTextInput("slider", label = NULL, choices = "NA", animate = animationOptions(interval = 750, loop = TRUE)),
        hr(),
        div(div(style="display:inline-block",h4(tags$b("Options:"))),div(style="display:inline-block",actionButton("help_options",label="",icon=icon("circle-question"),class="help-button",style="height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none"))),
        shinyFiles::shinyDirButton("button_save", "Select Output Directory", title = "Save"),
        h5("Selected Output Directory:"),
        div(style = "max-width: 23em; padding-bottom: 10px; overflow-wrap: break-word; font-size: 0.8em", textOutput("output_dir"))
    ),
    div(style="display:table-cell; width: calc(100vw - 27em); min-width:400px;", # Set Width to 100% of View Width -27em
        div(style = "display:flex; margin-top: 20px;align-items:stretch",
            div(style = "width:70%; min-width:300px", leaflet::leafletOutput("map", height = "800px")),
            div(style = "width: 30%; min-width:100px", plotly::plotlyOutput("plot", height = "100%"))
        )
    )
  ),
  hr(),
  div(style = "display:flex",
    div(style = "width: 75%; padding:10px",
        div(
          div(style = "display:inline-block", h4(tags$b("GIS Data:"))),
          div(style = "display:inline-block", actionButton("help_gis_df", label = "", icon = icon("circle-question"), class = "help-button", style = "height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none")),
          div(style = "display:inline-block;margin-left:30px;margin-bottom:10px", downloadButton("download_gis", label = "Download GIS Data"))
        ),
        DT::dataTableOutput("gis")
    ),
    div(style = "width: 25%; padding:10px",
        div(
          div(style = "display:inline-block", h4(tags$b("MapOutput Data:"))),
          div(style = "display:inline-block", actionButton("help_data_df", label = "", icon = icon("circle-question"), class = "help-button", style = "height:0px;width:0px;padding:0px;padding-bottom:25px;border:none;background:none")),
          div(style = "display:inline-block;margin-left:30px;margin-bottom:10px", downloadButton("download_data", label = "Download Result Data"))
        ),
        DT::dataTableOutput("table")
    )
  )
)
