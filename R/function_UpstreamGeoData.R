#' @export
#'
#' @import pbapply
#' 
#' @title
#' Calculate upstream sums and averages of selected GeoData contents
#'
#' @description
#' Function to calculate upstream sums and averages for selected variables of imported GeoData.txt files. 
#'
#' @param subid Integer vector of SUBIDs for which to calculate upstream properties (must exist in \code{gd}). 
#' If \code{NULL} (default), upstream areas for all SUBIDs will be calculated.
#' 
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file.
#' 
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' 
#' @param col.olake.slc Integer, column number with SLC class fraction of outlet lake. For weighted averaging of outlet lake depths. 
#' NOT YET IMPLEMENTED.
#' 
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamGeoData} calculated upstream averages or sums of selected variables in a GeoData data frame, including branch connections 
#' in case of stream bifurcations but not including potential irrigation links or groundwater flows.
#' 
#' Variables
#' 
#' Area-weighted average: elev_mean, slope_mean, buffer, close_w, latitude, longitude, all SLC classes, elev_std, slope_std
#' Sum: area, rivlen
#' 
#' @return
#' \code{UpstreamSLCClasses} returns a data frame of the same dimension as argument \code{gd}, with updated upstream columns marked with a leading 
#' 'UP' in the column names.
#' 
#' @seealso
#' \code{\link{UpstreamSLCClasses}}
#' \code{\link{SumUpstreamArea}}
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' \dontrun{UpstreamGeoData(subid = 21, gd = mygeodata, bd = mybranchdata)}

UpstreamGeoData <- function(subid = NULL, gd, bd = NULL, col.olake.slc = NULL, signif.digits = 3, progbar = T) {
  
  # extract column positions of subid and area in gd
  pos.sbd <- which(toupper(names(gd)) == "SUBID")
  pos.area <- which(toupper(names(gd)) == "AREA")
  
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
  
  # safety measure: force type of area to numeric to prevent integer overflow when summing below
  gd[, pos.area] <- as.numeric(gd[, pos.area])
  
  # create vectors of treated geodata column positions for weighted means, sums, weighted standard deviations
  # unknown/untreated columns will be returned unchanged
  pos.wmean <- c(which(tolower(names(gd)) %in% c("elev_mean", "slope_mean", "buffer", "close_w", "latitude", "longitude")), 
                 which(toupper(substr(names(gd), 1, 3)) == "SLC"))
  pos.sum <- which(tolower(names(gd)) %in% c("area", "rivlen"))
  pos.wsd.elev <- which(tolower(names(gd)) %in% c("elev_std", "elev_mean"))
  pos.wsd.slope <- which(tolower(names(gd)) %in% c("slope_std", "slope_mean"))
  # lake depths are special, because they will be weighted by lake area, if olake slc is provided by user
  pos.ldepth <- which(tolower(names(gd)) %in% c("lake_depth"))
  
    
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if requested by user
  cat("\nFinding upstream SUBIDs.\n")
  if (progbar) {
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
  
  ### internal functions used below
  
  # internal function to calculate area-weighted means for one group of upstream areas (one element in up.sbd)
  # used with sapply below
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
  
  # internal function to calculate area-weighted standard deviations, different to the above because there are just 2 weighted sd
  # variables, slope_sd and elev_sd, and the column positions are variable-specific combinations of mean and sd (see e.g. pos.wsd.slope)
  # used with sapply below
  WeightedSd   <- function(x, g, p.sbd, p.wsd, p.area) {
    
    # extract dataframe with areas and variables in x, for which to calculate weighted stdevs
    df.wsd <- g[g[, p.sbd] %in% x, c(p.area, p.wsd)]
    
    # averaging only necessary if more than one subid, also avoids NaN result if stddev is 0
    if (nrow(df.wsd) > 1) {
      
      # area-weighted std.dev. of variable, see also http://stackoverflow.com/questions/9222056/existing-function-to-combine-standard-deviations-in-r
      res <- sqrt(weighted.mean(df.wsd[, 3]^2 + df.wsd[, 2]^2, df.wsd[, 1]) - weighted.mean(df.wsd[, 2], df.wsd[, 1])^2)
      
    } else {
      res <- df.wsd[, 3]
    }
    
    return(res)
  }
  
  # internal function to calculate sums
  Sum <- function(x, g, p.sum, p.sbd) {
    # extract dataframe with areas and variables in x, for which to calculate weighted stdevs
    df.sum <- data.frame(g[g[, p.sbd] %in% x, p.sum])
    # columnwise sum
    res <- colSums(df.sum)
    return(res)
  }
  

  # apply area-weighted mean function to all SUBIDs in variable 'subid', for all relevant variables
  # conditional: use the progress bar version of sapply if set by function argument
  cat("\nCalculating upstream area-weighted means.\n")
  if (progbar) {
    te <- pbsapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean, p.area = pos.area)
  } else {
    te <- sapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean, p.area = pos.area)
  }
  # create result dataframe, conditiononal on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
  if(length(pos.wmean) > 1) {
    up.wmean <- data.frame(SUBID = subid, t(te))
  } else {
    up.wmean <- data.frame(SUBID = subid, te)
    names(up.wmean)[2] <- names(gd)[pos.wmean]
  }
  rm(te)
  
  # apply area-weighted sd function to all SUBIDs in variable 'subid'
  # do separately for slope and elev, if they exist in gd
  if (length(pos.wsd.elev) == 2) {
    cat("\nCalculating upstream area-weighted elevation standard deviations.\n")
    if (progbar) {
      up.wsd.elev <- data.frame(SUBID = subid, ELEV_STD = pbsapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.elev, p.area = pos.area))
    } else {
      up.wsd.elev <- data.frame(SUBID = subid, ELEV_STD = sapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.elev, p.area = pos.area))
    }
  } else {
    up.wsd.elev <- NULL
  }
  if (length(pos.wsd.slope) == 2) {
    cat("\nCalculating upstream area-weighted slope standard deviations.\n")
    if (progbar) {
      up.wsd.slope <- data.frame(SUBID = subid, SLOPE_STD = pbsapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.slope, p.area = pos.area))
    } else {
      up.wsd.slope <- data.frame(SUBID = subid, SLOPE_STD = sapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.slope, p.area = pos.area))
    }
  } else {
    up.wsd.slope <- NULL
  }
  
  # apply sum function to all SUBIDs in variable 'subid', for all relevant variables
  cat("\nCalculating upstream sums.\n")
  if (progbar) {
    te <- pbsapply(up.sbd, Sum, g = gd, p.sum = pos.sum, p.sbd = pos.sbd)
  } else {
    te <- sapply(up.sbd, Sum, g = gd, p.sum = pos.sum, p.sbd = pos.sbd)
  }
  # create result dataframe, conditiononal on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
  if(length(pos.sum) > 1) {
    up.sum <- data.frame(SUBID = subid, t(te))
  } else {
    up.sum <- data.frame(SUBID = subid, te)
    names(up.sum)[2] <- names(gd)[pos.sum]
  }
  rm(te)
  
  ## post-processing
  cat("\nPost-processing.")
  
  # round to requested number of digits, conditional on existing results for the stddev variables
  if (!is.null(signif.digits)) {
    up.wmean[, -1] <- apply(data.frame(up.wmean[, -1]), 2, signif, digits = signif.digits)
    if (!is.null(up.wsd.elev)) {
      up.wsd.elev[, -1] <- signif(up.wsd.elev[, -1], digits = signif.digits)
    }
    if (!is.null(up.wsd.slope)) {
      up.wsd.slope[, -1] <- signif(up.wsd.slope[, -1], digits = signif.digits)
    }
    up.sum[, -1] <- apply(data.frame(up.sum[, -1]), 2, signif, digits = signif.digits)
  }
  
  # copy all upstream calculations to result GeoData, replacing the originals
  gd[, pos.wmean] <- up.wmean[, -1]
  if (!is.null(up.wsd.elev)) {
    gd[, pos.wsd.elev[2]] <- up.wsd.elev[, -1]
  }
  if (!is.null(up.wsd.slope)) {
    gd[, pos.wsd.slope[2]] <- up.wsd.slope[, -1]
  }
  gd[, pos.sum] <- up.sum[, -1]
  
  
  # rename variables to clarify they are upstream values
  pos.up <- c(pos.wmean, pos.sum, if (length(pos.wsd.elev) == 2) pos.wsd.elev[2] else NULL, if (length(pos.wsd.slope) == 2) pos.wsd.slope[2] else NULL)
  names(gd)[pos.up] <- paste("UP", names(gd)[pos.up], sep = "")
  
  # return result
  return(gd)
}


