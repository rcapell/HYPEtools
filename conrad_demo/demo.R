library(HYPEtools)
library(leaflet)

# Read HYPE Subbasin GIS Data
hype_gis <- sf::st_read("//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp")

# Read MapOutput
mapCOUT <- ReadMapOutput("mapCOUT.txt")

# Launch Shiny App
HYPEtools::launchApp(option.x = mapCOUT, option.map = hype_gis, option.var.name = "COUT")
