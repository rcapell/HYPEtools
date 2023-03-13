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
HYPEtools::launchApp(option.var.name = "COUT")

# Launch Shiny App with default paths provided in arguments - for now, need to click on result file button in order to get map to load
HYPEtools::launchApp(#results.dir = "C:/GIT_SVN/4_HYPEtools_Shiny_App/conrad_demo/demo_result",
                     map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
                     map.subid.column = 2,
                     var.name = "COUT")

# Require that map and data can be joined before allowing app to generate graphs


# button to export mapoutput image file

# Perform input argument checks? do in launchApp() section not in shiny section!!
# Setup HYPEtools to do "suggests" for the dependencies and setup input check/warning for packages
# Test if passing extra arguments to function will work

