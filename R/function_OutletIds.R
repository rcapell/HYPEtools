
#' Find Outlet IDs
#'
#' @description
#' Function to find the identifier(s) used to signify model domain outlets, i.e. the "downstream" ID of outlet catchments, in a GeoData file. 
#' This is typically just one number, often e.g. '0' or '-9999', but can be one or several IDs if the GeoData file originates from a HYPE sub-model 
#' set-up, e.g. created with the 'SelectAro' program. Use \code{\link{OutletSubids}} to find the actual SUBID values of the outlet catchments.
#'
#' @param gd Data frame with two columns \code{subid} and \code{maindown} (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' 
#' @details
#' \code{OutletIds} finds the unique outlet IDs of a GeoData file. The outlet ID of a typical model 
#' is a single placeholder number, often e.g. '0' or '-9999', but there can be several outlet IDs, e.g. one or 
#' several SUBIDs if the GeoData file originates from a HYPE sub-model set-up, created 
#' with the 'SelectAro' tool.
#' 
#' @return
#' \code{OutletIds} returns a vector of outlet IDs.
#' 
#' @seealso
#' \code{\link{AllDownstreamSubids}}, \code{\link{OutletSubids}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' OutletIds(gd = te)
#' 
#' @export


OutletIds <- function(gd) {
  
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
