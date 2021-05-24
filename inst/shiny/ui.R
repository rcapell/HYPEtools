#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny

# create the shiny application user interface
shinyAppUI <- fluidPage(

  # Application title
  titlePanel("PlotMapOutput Demo"),

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
