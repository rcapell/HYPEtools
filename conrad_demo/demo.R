library(HYPEtools)
# Build HYPEtools!

# Launch Shiny App with no arguments
VisualizeMapOutput()

# Launch Shiny App for demo model
VisualizeMapOutput(
  results.dir = system.file("demo_model", "results", package = "HYPEtools"),
  map = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools"),
  map.subid.column = 25
)

# Launch Shiny App with default paths provided in arguments for GuM-HYPE
VisualizeMapOutput(
  results.dir = "C:/Users/a002416/Desktop/",
  map = "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/Model/GIS/gumhype_subbasins.shp",
  map.subid.column = 2,
  output.dir = "C:/Users/a002416/Desktop/"
)

### Final Steps:
# Setup HYPEtools to do "suggests" for the dependencies and update warning text
# Merge my fork into main hypetools
