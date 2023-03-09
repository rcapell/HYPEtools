library(HYPEtools)

library(leaflet)
library(ggplot2)
library(plotly)

# Read HYPE Subbasin GIS Data
hype_gis <- sf::st_read("//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp")

# Read MapOutput
mapCOUT <- ReadMapOutput("mapCOUT.txt")

### I can edit/run app from the server.R and ui.R files in ./conrad_demo/ folder!! Maybe need to set path to conrad_demo folder?

# Launch Shiny App - Need to build HYPEtools first!
HYPEtools::launchApp(option.x = mapCOUT, option.map = hype_gis, option.var.name = "COUT")
