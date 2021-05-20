library(HYPEtools)
<<<<<<< HEAD

library(leaflet)
library(ggplot2)
library(plotly)
=======
library(leaflet)
>>>>>>> 699fcd9 (Add demo for launching app)

# Read HYPE Subbasin GIS Data
hype_gis <- sf::st_read("//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp")

# Read MapOutput
mapCOUT <- ReadMapOutput("mapCOUT.txt")

<<<<<<< HEAD
### I can edit/run app from the server.R and ui.R files in ./conrad_demo/ folder!! Maybe need to set path to conrad_demo folder?

# Launch Shiny App - Need to build HYPEtools first!
=======
# Launch Shiny App
>>>>>>> 699fcd9 (Add demo for launching app)
HYPEtools::launchApp(option.x = mapCOUT, option.map = hype_gis, option.var.name = "COUT")
