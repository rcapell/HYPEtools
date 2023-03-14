#' Shiny app server object

# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Application title
  titlePanel("Ugly Shiny App - VisualizeMapOutput"),
  hr(),
  
  # Create button to select directories for model files and results files
  div(
    div(style = "display:-webkit-flex; display:-ms-flexbox; display:flex;",
      div(style = "width: 50%;",
          shinyFilesButton("button_gis", "Select GIS File" , title = "Please select files:", buttonType = "default", class = NULL, multiple = F, filetype = c(".shp", ".gpkg")),
          textOutput("gis_file"),
          uiOutput("input_column"),
          dataTableOutput("path_mf")
      ),
      div(style = "width: 50%; padding-left: 25px",
          shinyFilesButton("button_results", "Select Result Files" , title = "Please select files:", buttonType = "default", class = NULL, multiple = T),
          textOutput("result_file"),
          uiOutput("input_result")
      )
    )
  ),
  hr(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Column:", min = 2, max = 2, step = 1, value = 2, animate = TRUE),
      hr(),
      dataTableOutput("table")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("plot")
    )
  )
)
