library(HYPEtools)
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyFiles)
library(DT)

# Build HYPEtools!

PlotMapOutput()

# Launch Shiny App with no default paths- for now, need to click on result file button in order to get map to load
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments - for now, need to click on result file button in order to get map to load
HYPEtools::launchApp(results.dir = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/ref_runs/2023-02-09_v1.0.6_GFD2b",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

# fix map reset button
# set title based on filename instead of var.name



# Perform input argument checks? do in launchApp() section not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work


# OTHER FEATURES:
# button to export mapoutput image file

