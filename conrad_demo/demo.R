library(HYPEtools)
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyFiles)
library(DT)
library(purrr)
library(tidyr)

# Build HYPEtools!

PlotMapOutput()

# Launch Shiny App with no default paths- for now - select demo model files
HYPEtools::launchApp()

# Launch Shiny App with default paths provided in arguments for GuM-HYPE
HYPEtools::launchApp(results.dir = "C:/Users/a002416/Desktop",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2)

# Add check test under select SUBID column that says e.g. "Join: Success" in green text or "Join: Fail" in red text depending on the status of leaf_check()

# Hide geometry column from GIS table
# make UI pretty!
# Update axis labels for boxplot
# button to export mapoutput image file

# Add input argument checks to launchApp() script... not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work (e.g. var.name)
