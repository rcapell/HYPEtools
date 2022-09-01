
#' Find all Outlet SUBIDs of a model domain
#'
#' Function to find all outlet SUBIDs of a HYPE model domain.
#'
#' @param gd A data frame, with two columns \code{subid} and \code{maindown}, (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' 
#' @details
#' \code{OutletSubids} finds all outlet SUBIDs of a model domain as provided in a 'GeoData.txt' file, i.e. all SUBIDs from which 
#' stream water leaves the model domain.
#' 
#' @return
#' \code{OutletSubids} returns a vector of outlet SUBIDs.
#' 
#' @seealso
#' \code{\link{AllDownstreamSubids}}, \code{\link{OutletIds}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' OutletSubids(gd = te)
#' 
#' @export


OutletSubids <- function(gd) {
  
  # identify relevant columns
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sub <- which(tolower(colnames(gd)) == "subid")
  
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sub) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  # get outlet id(s)
  oid <- OutletIds(gd)
  
  # select all subids which have outlet ids downstream
  res <- gd[gd[, geocol.md] %in% oid, geocol.sub]
  
  return(res)
}
