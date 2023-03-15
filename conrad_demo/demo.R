library(HYPEtools)
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyFiles)
library(DT)
library(purrr)
library(tidyr)
library(dplyr)
library(sf)
library(shinyalert)
library(shinyWidgets)

# Build HYPEtools!

# Launch Shiny App with no default paths - select demo model files
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments for GuM-HYPE
HYPEtools::launchApp(results.dir = "C:/Users/a002416/Desktop/",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

### Extra Features:
# get plot to autoscale
# fix plotly creating double plot - is it problem with x-axis or with creating visible/nonvisible traces?

# Use data table filters to select subbasins that are shown in map??
# button to export mapoutput image file

### Final Steps:
# Add input argument checks to launchApp() script... not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work (e.g. var.name) <-- maybe remove the ...'s
# Merge my fork into main hypetools
