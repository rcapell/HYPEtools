library(HYPEtools)
library(leaflet)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyFiles)
library(DT)

# Read HYPE Subbasin GIS Data
hype_gis <- sf::st_read("//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp")

# Read MapOutput - Not needed anymore (built into shiny app)
# mapCOUT <- ReadMapOutput("mapCOUT.txt")

# Build HYPEtools!

# Launch Shiny App with no default paths- for now, need to click on result file button in order to get map to load
HYPEtools::launchApp(option.map = hype_gis, option.var.name = "COUT")

# Launch Shiny App with default paths provided in arguments - for now, need to click on result file button in order to get map to load
HYPEtools::launchApp(model.dir = "C:/GIT_SVN/1_Models/GuM-HYPE",
                     results.dir = "C:/GIT_SVN/4_HYPEtools_Shiny_App/conrad_demo/demo_result",
                     option.map = hype_gis, option.var.name = "COUT")

