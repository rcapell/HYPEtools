# _____________________________________________________________________________________________________________________________________
# HYPEtools Activity #####
# _____________________________________________________________________________________________________________________________________

# Author: Conrad Brendel
# Email: conrad.brendel@smhi.se
# Created: 2023-04-14

# _____________________________________________________________________________________________________________________________________
# Setup #####
# _____________________________________________________________________________________________________________________________________

# Install Package (Latest version is v1.3.0)
install.packages("HYPEtools")

# Import Package
library(HYPEtools)

# _____________________________________________________________________________________________________________________________________
# Resources #####
# _____________________________________________________________________________________________________________________________________

# HYPEtools Reference Manual: https://cran.r-project.org/web/packages/HYPEtools/HYPEtools.pdf
# Vignettes Online: https://cran.r-project.org/web/packages/HYPEtools/vignettes/

# _____________________________________________________________________________________________________________________________________
# Basic Exercise: Browse Vignettes #####
# _____________________________________________________________________________________________________________________________________

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
# Advanced Exercise: Run Vignette Code Yourself #####
# _____________________________________________________________________________________________________________________________________

# Open the vignette ".Rmd" files in RStudio and run the code for yourself!
# You can also modify the code to create different outputs etc.

# Some additional R packages are needed for the interactive visualization features:
install.packages(c("DT", "htmlwidgets", "leaflet", "leaflet.extras", "mapview", "plotly", "sf", "shiny", "shinyalert", "shinyFiles", "shinyWidgets", "randomcoloR", "webshot"))

# Recommended order:
# - import_files.Rmd
# - analyze_hype_ts.Rmd
# - plot_map_statistics.Rmd
# - basin_characteristics.Rmd
# - basin_network.Rmd
# - modify_par.Rmd
