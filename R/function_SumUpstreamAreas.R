#' @export
#'
#' @import pbapply
#' 
#' @title
#' Calculate upstream area sums
#'
#' @description
#' Function to calculate upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table.
#'
#' @param subid Integer vector of SUBIDs to calculate upstream areas for (must exist in \code{gd}). If \code{NULL}, upstream areas for all 
#' SUBIDs will be calculated.
#' 
#' @param gd A data frame containing a column with SUBIDs and a column with areas, an imported 'GeoData.txt' file.
#' 
#' @param bd A data frame, an imported 'BranchData.txt' file. Optional argument.
#' 
#' @param progbar Logical, display a progress bar while calculating upstream areas. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{SumUpstreamAreas} sums upstream areas of all connected upstream SUBIDs, including branch connections in case of stream bifurcations 
#' but not including potential irrigation links or groundwater flows.
#' 
#' @return
#' \code{SumUpstreamAreas} returns a data frame with two columns containing SUBIDs and total upstream areas (in area units as provided in \code{gd}). 
#' Upstream areas include areas of outlet SUBIDs.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' \dontrun{SumUpstreamAreas(gd = mygeodata, progbar = T)}

SumUpstreamArea <- function(subid = NULL, gd, bd = NULL, progbar = F) {
  
  # extract column positions of subid and area in gd
  pos.sbd <- which(toupper(names(gd)) == "SUBID")
  pos.area <- which(toupper(names(gd)) == "AREA")
  
  # force type numeric on area to avoid integer overflow, just a safety measure
  gd[, pos.area] <- as.numeric(gd[, pos.area])
  
  # check if gd contains necessary information
  if (length(pos.sbd) == 0) {
    stop("No SUBID column found in 'gd'. Exiting.")
  }
  if (length(pos.area) == 0) {
    stop("No AREA column found in 'gd'. Exiting.")
  }
  
  # conditional: fill subid vector if not user-provided
  if (is.null(subid)) {
    subid <- gd[, pos.sbd]
  }
  
  
  # conditional: use the progress bar version of sapply if user-requested (imported from package pbapply)
  if (progbar) {
    up.area <- pbsapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
  } else {
    up.area <- sapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
  }
  
  # build and return result dataframe
  res <- data.frame(SUBID = subid, UPSTREAMAREA = up.area)
  return(res)
}

# # DEBUG
# gd <- ReadGeoData("D:/s-hype/2014_highresworkshop/run_sourceapp_HAROs/GeoData.txt")
# bd <- ReadBranchData("D:/s-hype/2014_highresworkshop/run_sourceapp_HAROs/BranchData.txt")
# subid <- NULL
# subid <- 29
# subid <- c(29, 99, 118)
