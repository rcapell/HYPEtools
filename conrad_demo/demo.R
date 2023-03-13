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

# Launch Shiny App with no default paths- for now - select demo model files
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments for GuM-HYPE
HYPEtools::launchApp(results.dir = "C:/Users/a002416/Desktop",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

# Add check test under select SUBID column that says e.g. "Join: Success" in green text or "Join: Fail" in red text depending on the status of leaf_check()
# fix map reset button - add it to the part when it gets updated with the data instead of on the basemap
# set legend title based on filename instead of var.name
# handle legend breaks to be based on entire file contents instead of a new legend each time?


# Add input argument checks to launchApp() script... not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work (e.g. var.name)


# OTHER FEATURES:
# button to export mapoutput image file

