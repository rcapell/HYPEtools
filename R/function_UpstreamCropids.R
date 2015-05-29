
#' @export
#'
#' @import pbapply
#' 
#' @title
#' Calculate area-weighted upstream averages of CropID fractions
#'
#' @description
#' Function to calculate averages of CropID fractions calculated from imported GeoData.txt and GeoClass.txt files. 
#'
#' @param subid Integer vector of SUBIDs for which to calculate upstream properties (must exist in \code{gd}). 
#' If \code{NULL} (default), upstream areas for all SUBIDs will be calculated.
#' 
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file.
#' 
#' @param gc A data frame containing an imported 'GeoClass.txt' file.
#' 
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' 
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#'
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamCropids} calculates area-weighted upstream averages of CropID fractions from SLC class fractions in a GeoData table and corresponding 
#' CropIDs in a GeoClass table, including branch connections in case of stream bifurcations but not including potential irrigation links or 
#' groundwater flows. Averages are weighted by sub-catchment area.
#' 
#' @note
#' \code{UpstreamCropids} expects SLC class columns in argument \code{gd} to be ordered in ascending order.
#' 
#' @return
#' \code{UpstreamCropids} returns a data frame with SUBIDs in the first column, and upstream CropID fractions in the following columns.
#' 
#' @seealso
#' \code{\link{UpstreamSLCClasses}}
#' \code{\link{UpstreamGeoData}}
#' \code{\link{SumUpstreamArea}}
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' \dontrun{UpstreamCropids(subid = 21, gd = mygeodata, gc = mygeoclass, bd = mybranchdata)}


UpstreamCropids <- function(subid = NULL, gd, gc, bd = NULL, signif.digits = 3, progbar = T) {
  
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
  
  # check if numbers of slc classes in gc and gd are consistent
  if (nrow(gc) != length(pos.slc)) {
    stop("Number of SLC classes in 'gd' and 'gc' are not identical. Exiting.")
  }
  
  # conditional: fill subid vector if not user-provided
  if (is.null(subid)) {
    subid <- gd[, pos.sbd]
  }
  
  ## merge SLC classes by crop id and create cropid fractions for each subid in gd
  # extract slc columns from geodata
  slc <- gd[, pos.slc]
  # calculate cropid fraction, conditionally using progress bar
  cat("\nCalculating CROPID fractions.\n")
  if (progbar) {
    te <- pbapply(slc, 1, function(x, y) {tapply(x, y, sum)}, y = paste("UP_CROPID_", gc[, 4], sep = ""))
  } else {
    te <- apply(slc, 1, function(x, y) {tapply(x, y, sum)}, y = paste("UP_CROPID_", gc[, 4], sep = ""))
  }
  # create geodata-like dataframe with subids and cropid fractions
  gcrop <- data.frame(SUBID = gd[, pos.sbd], AREA = gd[, pos.area], 
                      t(te))
  rm(te)
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if requested by user
  cat("\nFinding upstream SUBIDs.\n")
  if (progbar) {
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
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
  cat("\nCalculating upstream area-weighted means.\n")
  if (progbar) {
    te <- pbsapply(up.sbd, WeightedMean, g = gcrop, p.sbd = 1, p.wmean = 3:ncol(gcrop), p.area = 2)
  } else {
    te <- sapply(up.sbd, WeightedMean, g = gcrop, p.sbd = 1, p.wmean = 3:ncol(gcrop), p.area = 2)
  }
  # create result dataframe, conditional on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
  if(length(3:ncol(gcrop)) > 1) {
    up.gcrop <- data.frame(SUBID = subid, t(te))
  } else {
    up.gcrop <- data.frame(SUBID = subid, te)
    names(up.gcrop)[2] <- names(gcrop)[3]
  }
  rm(te)
  
  # round to requested number of digits, conditional on existing results for lake_depth and stddev variables
  if (!is.null(signif.digits)) {
    up.gcrop[, -1] <- apply(data.frame(up.gcrop[, -1]), 2, signif, digits = signif.digits)
  }
  
  # return upstream cropids
  return(up.gcrop)
  
}
