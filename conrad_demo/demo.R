library(HYPEtools)
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyFiles)
library(DT)
library(purrr)

# Build HYPEtools!

PlotMapOutput()

# Launch Shiny App with no default paths- for now, need to click on result file button in order to get map to load
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments - for now, need to click on result file button in order to get map to load
HYPEtools::launchApp(results.dir = "C:/Users/a002416/Desktop",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

# fix map reset button
# set legend title based on filename instead of var.name
# handle legend breaks to be based on entire file contents instead of a new legend each time?



# Perform input argument checks? do in launchApp() section not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work (e.g. var.name)


# OTHER FEATURES:
# button to export mapoutput image file

