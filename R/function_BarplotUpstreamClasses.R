
#' Bar plots of upstream-averaged classes of HYPE sub-basins
#' 
#' Function to plot upstream-averaged landscape property classes of one or several sub-basins, e.g. land use or soils, 
#' as bar plots.
#' 
#' @param x Data frame, containing column-wise class fractions with SUBIDs in first column. Typically a result from 
#' \code{\link{UpstreamGroupSLCClasses}}
#' @param type Character keyword.
#' @param desc List, with class description labels imported from a 'description.txt' file . See \code{\link{ReadDescription}} for 
#' formatting details.
#' 
#' @details 
#' \code{BarplotUpstreamClasses} is a wrapper for \code{\link{barplot}}, with labeling
#' 
#' @seealso 
#' \code{\link{UpstreamGroupSLCClasses}}
#' \code{\link{barplot}}

BarplotUpstreamClasses <- function (x, type = "landuse") {
  
  barplot(as.matrix(x[, -1]), beside = T)
}

# # DEBUG
# gd <- ReadGeoData("../PlotBasinSummary/GeoData.txt")
# gcl <- ReadGeoClass("../PlotBasinSummary/GeoClass.txt", headrow = 4)
# type <- "landuse"
# x <- UpstreamGroupSLCClasses(subid = 8000152, gd = gd, gc = gcl, type = type)
# x <- UpstreamGroupSLCClasses(subid = c(8000152, 8127943), gd = gd, gc = gcl, type = type)
