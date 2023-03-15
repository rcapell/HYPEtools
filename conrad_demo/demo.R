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
                     map.subid.column = 2,
                     output.dir = "C:/Users/a002416/Desktop/"
                     )

### Bugs:
# Add progress popup while saving -- get from puffin
# adjust height of leaflet
# set searchbox border color to match

### Extra Features:
# Use data table filters to select subbasins that are shown in map??

### Final Steps:
# Add input argument checks to launchApp() script... not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Merge my fork into main hypetools
