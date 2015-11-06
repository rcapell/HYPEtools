#' @export
#' @import pbapply
#' 
#' @title
#' Sort a GeoData dataframe in downstream order
#'
#' @description
#' Function to sort an imported GeoData.txt file in downstream order, so that all upstream sub-basins are listed in rows above downstream sub-basins.
#' 
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file.
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param progbar Logical, display a progress bar while calculating SUBID sorting. 
#' 
#' @details
#' GeoData.txt files need to be sorted in downstream order for HYPE to run without errors. \code{SortGeoData} considers bifurcation connections, but not 
#' irrigation or groundwater flow links.
#' 
#' @return
#' \code{SortGeoData} returns a GeoData dataframe.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}
#' \code{\link{OutletSubids}}
#' 
#' @examples
#' \dontrun{SortGeoData(gd = mygeodata, bd = mybranchdata)}


SortGeoData <- function(gd = gd, bd = NULL, progbar = TRUE) {
  
  # column position of subids in gd
  geocol.sbd <- which(tolower(colnames(gd)) == "subid")
  
  # vector of outlet subids in gd: for these, upstream chains will be calculated below
  osbd <- OutletSubids(gd = gd)
  
  # calculate subid sorting using function AllUpstreamSubids()
  if (progbar) {
    cat("Calculating SUBID order.")
    ssbd <- unlist(pblapply(osbd, function(x, gd, bd) {rev(AllUpstreamSubids(subid = x, gd, bd, sort = FALSE, write.arcgis = FALSE))}, gd = gd, bd = bd))
  } else {
    ssbd <- unlist(lapply(osbd, function(x, gd, bd) {rev(AllUpstreamSubids(subid = x, gd, bd, sort = FALSE, write.arcgis = FALSE))}, gd = gd, bd = bd))
  }
  
  # sort gd
  gd <- gd[match(ssbd, gd[, geocol.sbd]), ]
  
  return(gd)
  
}
