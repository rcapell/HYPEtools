#' @export

#' @title
#' Find Outlet SUBIDs
#'
#' @description
#' Function to find all outlet SUBIDs in a GeoData file. This is typically just one number
#'
#' @param gd A data frame, an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' 
#' @details
#' \code{OutletSubids} finds all SUBIDs of a given SUBID (including itself but not 
#' including potential irrigation links or groundwater flows).
#' 
#' @return
#' \code{AllDownstreamSubids} returns a vector of outlet SUBIDs.
#' 
#' 
#' @examples
#' /dontrun{OutletSubids(gd = mygeodata)}


OutletSubids <- function(gd) {
  
  # identify relevant columns
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sub <- which(tolower(colnames(gd)) == "subid")
  
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sub) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  ## test which ids in maindown DO NOT exist in subid, these would be the ones of interest
  # find unique ids in maindown
  te <- unique(gd[, geocol.md])
  # select the non-existing
  res <- te[!(te %in% gd[, geocol.sub])]
  
  return(res)
}
