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

# Launch Shiny App - Need to build HYPEtools first!
# HYPEtools::launchApp(option.x = mapCOUT, option.map = hype_gis, option.var.name = "COUT") # original demo where mapoutput read separately
HYPEtools::launchApp(option.map = hype_gis, option.var.name = "COUT")
