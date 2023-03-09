#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import shinyFiles

# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Application title
  titlePanel("Ugly Shiny App"),
  
  # Create button to select directories for model files and results files
  div(
    div(style = "display: inline-block; width = 50%; padding-right: 50px",
      shinyFilesButton("button_model", "Select Model Files" , title = "Please select files:", buttonType = "default", class = NULL, multiple = T),
      dataTableOutput("path_mf")
    ),
    div(style = "display: inline-block; width = 50%",
      shinyFilesButton("button_result", "Select Result Files" , title = "Please select files:", buttonType = "default", class = NULL, multiple = T),
      dataTableOutput("path_results")
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
