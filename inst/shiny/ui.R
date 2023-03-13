#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import shinyFiles

# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Application title
  titlePanel("Ugly Shiny App"),
  textOutput("test"),
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
          dataTableOutput("path_results")
      )
    )
  ),
  hr(),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider",
                  "Period:",
                  min = 1,
                  max = 312,
                  value = 1),
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
