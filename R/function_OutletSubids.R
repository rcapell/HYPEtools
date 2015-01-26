#' @export

#' @title
#' Find all Outlet SUBIDs
#'
#' @description
#' Function to find all outlet SUBIDs in a GeoData file, i.e. all SUBIDs from which stream water leaves the model domain.
#'
#' @param gd A data frame, with two columns \code{subid} and \code{maindown}, (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' 
#' @details
#' \code{OutletSubids} finds all SUBIDs of one or several given SUBID (including itself but not 
#' including potential irrigation or groundwater flow links, or secondary bifurcation outlets).
#' 
#' @return
#' \code{OutletSubids} returns a vector of outlet SUBIDs.
#' 
#' @seealso
#' \code{\link{AllDownstreamSubids}}, \code{\link{OutletIds}}
#' 
#' @examples
#' \dontrun{OutletSubids(gd = mygeodata)}


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
  
  ## test which ids in maindown DO NOT exist in subid, these would be the ones of interest
  # find unique ids in maindown
  te <- unique(gd[, geocol.md])
  # select subids which have a maindown id that does not exist in subid column (that is the outlet id)
  res <- te[!(te %in% gd[, geocol.sub])]
  
  # conditional: vector of subids provided or not. get vector of subid for which to find outlets
  if (is.null(subid)) {
    
    
    
  } else {
    
    sAllDownstreamSubids()
  }
  
  return(res)
}
