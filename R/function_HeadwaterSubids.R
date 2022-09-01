#' Find all headwater SUBIDs of a model domain
#'
#' Function to find all headwater SUBIDs of a HYPE model domain.
#'
#' @param gd A data frame, containing among others two columns \code{subid} and \code{maindown}. Column names are not case-sensitive 
#' and column positions in the data frame are irrelevant. 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' 
#' @details
#' \code{HeadwaterSubids} finds all headwater SUBIDs of a model domain as provided in a 'GeoData.txt' file, i.e. all subcatchments 
#' which do not have any upstream subcatchments.
#' 
#' @return
#' \code{HeadwaterSubids} returns a vector of outlet SUBIDs.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' HeadwaterSubids(gd = te)
#' 
#' @export


HeadwaterSubids <- function(gd) {
  
  # identify relevant columns
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sub <- which(tolower(colnames(gd)) == "subid")
  
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sub) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  # select all subids which do not exist in maindown column, these are headwaters
  res <- gd[!(gd[, geocol.sub] %in% gd[, geocol.md]), geocol.sub]
  
  return(res)
}
