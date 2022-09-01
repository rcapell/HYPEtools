#' 
#' Calculate area-weighted upstream averages of grouped SLC class fractions.
#'
#' Function to calculate averages of grouped SLC class fractions calculated from imported GeoData.txt and GeoClass.txt or any other user-defined grouping. 
#'
#' @param subid Integer vector of SUBIDs for which to calculate upstream properties (must exist in \code{gd}). 
#' If \code{NULL} (default), upstream areas for all SUBIDs will be calculated.
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file imported with \code{\link{ReadGeoData}}.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param gcl Data frame containing columns with SLCs and corresponding land use and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}. Must be provided if no \code{group} argument is given.
#' @param type Keyword character string for use with \code{gcl}. Type of grouping index, either \code{"landuse"}, \code{"soil"}, or \code{"crop"}, 
#' can be abbreviated.
#' @param group Integer vector, of same length as number of SLC classes in \code{gd}. Alternative grouping index specification to \code{gcl} + \code{type}.
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamGroupSLCClasses} calculates area-weighted upstream averages of CropID fractions from SLC class fractions in a GeoData table and corresponding 
#' grouping columns in a GeoClass table or a user-provided vector. Upstream calculations include branch connections in case of stream bifurcations but not 
#' potential irrigation links or groundwater flows. Averages are weighted by sub-catchment area.
#' 
#' The function builds on \code{\link{GroupSLCClasses}}, which provides grouped sums of SLC classes for several or all sub-basins in a GeoData dataframe.
#' 
#' @note
#' \code{UpstreamGroupSLCClasses} expects SLC class columns in argument \code{gd} to be ordered in ascending order.
#' 
#' @return
#' \code{UpstreamGroupSLCClasses} returns a data frame with SUBIDs in the first column, and upstream group fractions in the following columns.
#' 
#' @seealso
#' \code{\link{GroupSLCClasses}}
#' \code{\link{UpstreamSLCClasses}}
#' \code{\link{UpstreamGeoData}}
#' \code{\link{SumUpstreamArea}}
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' # Import source data
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te2 <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' # Upstream land use fractions for single SUBID
#' UpstreamGroupSLCClasses(subid = 63794, gd = te1, gcl = te2, type = "landuse", progbar = FALSE)
#' # Upstream soil fraction for all SUBIDs in GeoData
#' UpstreamGroupSLCClasses(gd = te1, gcl = te2, type = "soil")
#' 
#' @export
#' @importFrom pbapply pblapply pbsapply


UpstreamGroupSLCClasses <- function(subid = NULL, gd, bd = NULL, gcl = NULL, type = c("landuse", "soil", "crop"), 
                                    group = NULL, signif.digits = 3, progbar = TRUE) {
  
  # input argument checks
  type <- match.arg(type)
  
  # extract column positions of subid and area in gd
  pos.sbd <- which(toupper(names(gd)) == "SUBID")
  pos.area <- which(toupper(names(gd)) == "AREA")
  pos.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
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
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if requested by user
  if (progbar) {
    cat("\nFinding upstream SUBIDs.\n")
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
  # create grouped slc classes using existing function, just for upstream subids
  gd.sel <- gd[gd[, pos.sbd] %in% unlist(up.sbd), ]
  grclass <- GroupSLCClasses(gd = gd.sel, gcl = gcl, type = type, group = group, abs.area = FALSE, verbose = progbar)
  
  
  ## calculate upstream average groups
  
  ########################
  ### internal function applied below
  
  # internal function to calculate area-weighted means for one group of upstream areas (one element in up.sbd)
  # x: vector of upstream subids
  WeightedMean <- function(x, g, p.sbd, p.wmean, p.area) {
    
    # extract dataframe with areas and variables in x, for which to calculate weighted means
    df.wmean <- g[g[, p.sbd] %in% x, c(p.area, p.wmean)]
    
    # averaging only necessary if more than one subid, also avoids NaN result if stddev is 0
    if (nrow(df.wmean) > 1) {
      
      # area-weighted mean of all columns
      res <- apply(as.data.frame(df.wmean[, -1]), 2, weighted.mean, w = df.wmean[, 1])
      
    } else {
      res <- as.numeric(df.wmean[, -1])
      names(res) <- names(df.wmean)[-1]
    }
    
    return(res)
  }
  ########################
  
  # apply area-weighted mean function to all SUBIDs in variable 'subid', for all relevant variables
  # conditional: use the progress bar version of sapply if set by function argument
  if (progbar) {
    cat("\nCalculating upstream area-weighted means.\n")
    te <- pbsapply(up.sbd, WeightedMean, g = grclass, p.sbd = 1, p.wmean = 3:ncol(grclass), p.area = 2)
  } else {
    te <- sapply(up.sbd, WeightedMean, g = grclass, p.sbd = 1, p.wmean = 3:ncol(grclass), p.area = 2)
  }
  # create result dataframe, conditional on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
  if(length(3:ncol(grclass)) > 1) {
    up.grclass <- data.frame(SUBID = subid, t(te))
  } else {
    up.grclass <- data.frame(SUBID = subid, te)
    names(up.grclass)[2] <- names(grclass)[3]
  }
  rm(te)
  
  # round to requested number of digits, conditional on existing results for lake_depth and stddev variables
  if (!is.null(signif.digits)) {
    up.grclass[, -1] <- apply(data.frame(up.grclass[, -1]), 2, signif, digits = signif.digits)
  }
  
  # return upstream groups
  return(up.grclass)
}
