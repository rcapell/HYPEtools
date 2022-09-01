
#' Calculate SLC class fractions of all upstream areas
#'
#' Function to calculate SLC class fractions over all upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table.
#'
#' @param subid Integer vector of SUBIDs to calculate upstream SUBID fractions for (must exist in \code{gd}). 
#' If \code{NULL}, upstream areas for all SUBIDs will be calculated.
#' 
#' @param gd A data frame containing columns 'SUBID' with SUBIDs, 'MAINDOWN' with downstream SUBIDs, and 'AREA' with sub-basin areas, 
#' e.g. an imported 'GeoData.txt' file.
#' 
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' 
# @param df.up.area A data frame as returned from \code{\link{SumUpstreamArea}}. Two columns with SUBIDs and upstream areas. Can be provided to reduce 
# overall computation time.
#' 
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamSLCClasses} sums upstream areas of all connected upstream SUBIDs, including branch connections in case of stream bifurcations 
#' but not including potential irrigation links or groundwater flows.
#' 
#' @return
#' \code{UpstreamSLCClasses} returns a data frame with columns containing SUBIDs, total upstream areas (in area unit as provided in \code{gd}), and SLC 
#' class fractions for upstream areas.
#' 
#' @note
#' This function is now superseded by \code{\link{UpstreamGeoData}}, which returns more upstream variables.
#' 
#' @seealso
#' \code{\link{SumUpstreamArea}}, \code{\link{UpstreamGeoData}}, \code{\link{UpstreamGroupSLCClasses}}
#' 
#' @examples
#' # Import source data
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' # Upstream SLCs for single SUBID
#' UpstreamSLCClasses(subid = 3361, gd = te1, progbar = FALSE)
#' 
#' @importFrom pbapply pblapply pbsapply
#' @export


UpstreamSLCClasses <- function(subid = NULL, gd, bd = NULL, signif.digits = 3, progbar = TRUE) {
  
  # extract column positions of subid and area in gd
  pos.sbd <- which(toupper(names(gd)) == "SUBID")
  pos.area <- which(toupper(names(gd)) == "AREA")
  # extract SLC columns in gd
  pos.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  # check if gd contains necessary information
  if (length(pos.sbd) == 0) {
    stop("No SUBID column found in 'gd'. Exiting.")
  }
  if (length(pos.area) == 0) {
    stop("No AREA column found in 'gd'. Exiting.")
  }
  if (length(pos.slc) == 0) {
    stop("No SLC columns found in 'gd'. Exiting.")
  }
  
  # conditional: fill subid vector if not user-provided
  if (is.null(subid)) {
    subid <- gd[, pos.sbd]
  }
  
  # safety measure: force type of area to numeric to prevent integer overflow when summing below
  gd[, pos.area] <- as.numeric(gd[, pos.area])
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if subid is long
  cat("\nFinding upstream SUBIDs.\n")
  if (progbar) {
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
  # internal function to calculate area-weighted SLC fraction for one group of upstream SLC classes (one element in up.sbd)
  # used with sapply below
  WeightedSLC <- function(x, g, p.sbd, p.slc, p.area) {
    
    # extract dataframe with areas and slc classes of SUBIDs in x 
    df.slc <- g[g[, p.sbd] %in% x, c(p.area, p.slc)]
    
    
    # area-weighted mean of all SLC columns
    res <- c(UPSTREAMAREA = sum(df.slc[, 1]), apply(df.slc[, -1], 2, weighted.mean, w = df.slc[, 1]))
    
    return(res)
  }
  
  # apply area-weighted SLC mean function to all SUBIDs in subid
  # conditional: use the progress bar version of sapply if subid is long
  cat("\nCalculating upstream SLCs.\n")
  if (progbar) {
    up.slc <- cbind(SUBID = subid, as.data.frame(t(pbsapply(up.sbd, WeightedSLC, g = gd, p.sbd = pos.sbd, p.slc = pos.slc, p.area = pos.area))))
  } else {
    up.slc <- cbind(SUBID = subid, as.data.frame(t(sapply(up.sbd, WeightedSLC, g = gd, p.sbd = pos.sbd, p.slc = pos.slc, p.area = pos.area))))
  }
  
  
  # round to requested number of digits
  if (!is.null(signif.digits)) {
    up.slc[, -c(1:2)] <- apply(up.slc[, -c(1:2)], 2, signif, digits = signif.digits)
  }
  
  # rename SLCs to clarify they are upstream fractions
  names(up.slc)[-c(1:2)] <- paste("UP", names(up.slc)[-c(1:2)], sep = "")
  
  # return result
  return(up.slc)
}

