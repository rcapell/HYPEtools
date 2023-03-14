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

# Build HYPEtools!

# Launch Shiny App with no default paths - select demo model files
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments for GuM-HYPE
HYPEtools::launchApp(results.dir = "C:/Users/a002416/Desktop/map",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

### To Do:
# Add check test under select SUBID column that says e.g. "Join: Success" in green text or "Join: Fail" in red text depending on the status of leaf_check()
# Update select file buttons to only show files that have accepted file extensions
# Fix "X" at beginning of mapoutput column names
# Add actions when clicking help buttons
# Get slider to show dates instead of numbers

### Bugs:
# Fix warning for duplicate subids exist in mapoutput for when I create the legend

### Extra Features:
# Use data table filters to select subbasins that are shown in map??
# button to export mapoutput image file

### Final Steps:
# Add input argument checks to launchApp() script... not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work (e.g. var.name)
