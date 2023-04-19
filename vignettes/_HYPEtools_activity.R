# _____________________________________________________________________________________________________________________________________
# HYPEtools Activity #####
# _____________________________________________________________________________________________________________________________________

# Author: Conrad Brendel
# Email: conrad.brendel@smhi.se
# Created: 2023-04-14

# _____________________________________________________________________________________________________________________________________
# Resources #####
# _____________________________________________________________________________________________________________________________________

# HYPEtools Reference Manual: https://cran.r-project.org/web/packages/HYPEtools/HYPEtools.pdf
# Browse Vignettes Online: https://cran.r-project.org/web/packages/HYPEtools/vignettes/

# _____________________________________________________________________________________________________________________________________
# Basic Exercise: Browse Vignettes in R #####
# _____________________________________________________________________________________________________________________________________

# Install Package (Latest version is v1.3.0)
install.packages("HYPEtools")

# Browse Vignettes
browseVignettes("HYPEtools")

# Recommended order:
# - Import HYPE Files
# - Analyze HYPE Time Series Outputs
# - Analyze Spatial Patterns
# - Analyze Catchment Area Characteristics
# - Basin Networks
# - Modify a HYPE Parameter File

# _____________________________________________________________________________________________________________________________________
# Advanced Exercises: #####
# _____________________________________________________________________________________________________________________________________

# Some additional R packages are needed for the interactive visualization features:
install.packages(c("DT", "htmlwidgets", "leaflet", "leaflet.extras", "mapview", "plotly", "sf", "shiny", "shinyalert", "shinyFiles", "shinyWidgets", "randomcoloR", "webshot"))

# Load Package
library(HYPEtools)

# Visualize Model Outputs with Shiny app --------------------------------------------------------------------------------------------------------------------

# Start App - Symbolize with polygons
VisualizeMapOutput(
  results.dir = system.file("demo_model", "results", package = "HYPEtools"),
  map = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools"),
  map.subid.column = 25
)

# Start App - Symbolize with points
VisualizeMapPoints(
  results.dir = system.file("demo_model", "results", package = "HYPEtools"),
  sites = system.file("demo_model", "gis", "Nytorp_centroids.gpkg", package = "HYPEtools"),
  sites.subid.column = 25,
  bg = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools")
)

# Run Vignette Code Yourself  --------------------------------------------------------------------------------------------------------------------

# Open the vignette ".Rmd" files in e.g. RStudio and run the code for yourself!
# You can also modify the code to create different outputs etc.

# Recommended order:
# - import_files.Rmd
# - analyze_hype_ts.Rmd
# - plot_map_statistics.Rmd
# - basin_characteristics.Rmd
# - basin_network.Rmd
# - modify_par.Rmd
