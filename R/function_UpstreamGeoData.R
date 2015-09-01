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
#' @param col.olake.slc Integer, column number with SLC class fraction of outlet lake. Mandatory for weighted averaging of outlet lake depths. 
#' 
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#'
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamGeoData} calculates upstream averages or sums of selected variables in a GeoData data frame, including branch connections 
#' in case of stream bifurcations but not including potential irrigation links or groundwater flows. Averages are weighted by sub-catchment area, with 
#' the exception of outlet lake depths provided in GeoData variable 'lake_depth'. These are weighted by outlet lake area and the GeoData column with 
#' SLC class fractions for outlet lakes must be provided in function argument \code{col.olake.slc}. Elevation and slope standard deviations are 
#' averaged if the corresponding mean values exist (sample means are required to calculate overall means of std. devs.).
#' 
#' Currently, the following variables are considered:
#' \describe{
#'   \item{Area-weighted average}{elev_mean, slope_mean, buffer, close_w, latitude, longitude, all SLC classes, elev_std, slope_std}
#'   \item{Sum}{area, rivlen}
#' }
#' 
#' @return
#' \code{UpstreamGeoData} returns a data frame with the same number of columns as argument \code{gd} and number of rows corresponding to number of 
#' SUBIDs in argument \code{subid}, with updated upstream columns marked with a leading 'UP_' in the column names.
#' 
#' @seealso
#' \code{\link{UpstreamSLCClasses}}
#' \code{\link{SumUpstreamArea}}
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' \dontrun{UpstreamGeoData(subid = 21, gd = mygeodata, bd = mybranchdata, col.olake.slc = 12)}

UpstreamGeoData <- function(subid = NULL, gd, bd = NULL, col.olake.slc = NULL, signif.digits = 3, progbar = TRUE) {
  
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
  
  # conditional: fill subid vector if not user-provided, otherwise check that all subids are in gd and get row numbers
  if (is.null(subid)) {
    subid <- gd[, pos.sbd]
    row.sbd <- NULL
  } else {
    row.sbd <- match(x = c(subid), table = gd[, pos.sbd])
    if (any(is.na(row.sbd))) {
      stop("Atleast one SUBID in 'subid' not found in 'gd'. Exiting.")
    }
  }
  
  # safety measure: force type of area to numeric to prevent integer overflow when summing below
  gd[, pos.area] <- as.numeric(gd[, pos.area])
  
  # create vectors of treated geodata column positions for weighted means, sums, weighted standard deviations
  # unknown/untreated columns will be returned unchanged
  pos.wmean <- c(which(tolower(names(gd)) %in% c("elev_mean", "slope_mean", "buffer", "close_w", "latitude", "longitude")), 
                 which(toupper(substr(names(gd), 1, 3)) == "SLC"))
  # lake depths are special, because they will be weighted by lake area, if olake slc is provided by user
  pos.wmean.ldepth <- which(tolower(names(gd)) == "lake_depth")
  pos.sum <- which(tolower(names(gd)) %in% c("area", "rivlen"))
  pos.wsd.elev <- which(tolower(names(gd)) %in% c("elev_std", "elev_mean"))
  pos.wsd.slope <- which(tolower(names(gd)) %in% c("slope_std", "slope_mean"))
  
  # warn user if lake_depth variable found but no lake area slc column provided
  if (length(pos.wmean.ldepth) == 1 && is.null(col.olake.slc)) {
    warning("'lake_depth' found in GeoData, but no lake area column provided in argument 'col.olake.slc'. Skipping upstream 'lake_depth'.")
  }
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if requested by user
  cat("\nFinding upstream SUBIDs.\n")
  if (progbar) {
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
  ########################
  ### internal functions applied below
  
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
  
  # internal function to calculate area-weighted standard deviations, different to the above because there are just 2 weighted sd
  # variables, slope_sd and elev_sd, and the column positions are variable-specific combinations of mean and sd (see e.g. pos.wsd.slope)
  # used with sapply below
  WeightedSd   <- function(x, g, p.sbd, p.wsd, p.area) {
    
    # extract dataframe with areas and variables in x for which to calculate weighted stdevs
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
  
  ##############################################
  
  

  # apply area-weighted mean function to all SUBIDs in variable 'subid', for all relevant variables
  # conditional: use the progress bar version of sapply if set by function argument
  cat("\nCalculating upstream area-weighted means.\n")
  if (progbar) {
    te <- pbsapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean, p.area = pos.area)
  } else {
    te <- sapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean, p.area = pos.area)
  }
  # create result dataframe, conditional on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
  if(length(pos.wmean) > 1) {
    up.wmean <- data.frame(SUBID = subid, t(te))
  } else {
    up.wmean <- data.frame(SUBID = subid, te)
    names(up.wmean)[2] <- names(gd)[pos.wmean]
  }
  rm(te)
  
  # olake-area weighted mean for lake depths
  if (length(pos.wmean.ldepth) == 1 && !is.null(col.olake.slc)) {
    cat("\nCalculating upstream lake-area-weighted lake depths.\n")
    if (progbar) {
      up.wmean.ldepth <- data.frame(SUBID = subid, LAKE_DEPTH = pbsapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.ldepth, p.area = col.olake.slc))
    } else {
      up.wmean.ldepth <- data.frame(SUBID = subid, LAKE_DEPTH = sapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.ldepth, p.area = col.olake.slc))
    }
    # if no lakes exist, all weights are 0 and NaN is computed. Replace those with zeros
    up.wmean.ldepth[is.nan(up.wmean.ldepth[, 2]), 2] <- 0
  } else {
    up.wmean.ldepth <- NULL
  }
  
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
  
  
  #######################################################
  
  ## post-processing
  cat("\nPost-processing.")
  
  # round to requested number of digits, conditional on existing results for lake_depth and stddev variables
  if (!is.null(signif.digits)) {
    up.wmean[, -1] <- apply(data.frame(up.wmean[, -1]), 2, signif, digits = signif.digits)
    if (!is.null(up.wmean.ldepth)) {
      up.wmean.ldepth[, -1] <- signif(up.wmean.ldepth[, -1], digits = signif.digits)
    }
    if (!is.null(up.wsd.elev)) {
      up.wsd.elev[, -1] <- signif(up.wsd.elev[, -1], digits = signif.digits)
    }
    if (!is.null(up.wsd.slope)) {
      up.wsd.slope[, -1] <- signif(up.wsd.slope[, -1], digits = signif.digits)
    }
    up.sum[, -1] <- apply(data.frame(up.sum[, -1]), 2, signif, digits = signif.digits)
  }
  
  ## copy all upstream calculations to result GeoData, replacing the originals, conditional on presence of argument subid
  # create result dataframe
  if (is.null(row.sbd)) {
    res <- gd
  } else {
    res <- gd[row.sbd, ]
  }
  # update result dataframe with upstream variables
  res[, pos.wmean] <- up.wmean[, -1]
  if (!is.null(up.wmean.ldepth)) {
    res[, col.olake.slc] <- up.wmean.ldepth[2]
  }
  if (!is.null(up.wsd.elev)) {
    res[, pos.wsd.elev[2]] <- up.wsd.elev[, -1]
  }
  if (!is.null(up.wsd.slope)) {
    res[, pos.wsd.slope[2]] <- up.wsd.slope[, -1]
  }
  res[, pos.sum] <- up.sum[, -1]
  
  
  # rename upstream variables to clarify they are upstream values
  pos.up <- c(pos.wmean, pos.sum, col.olake.slc, if (length(pos.wsd.elev) == 2) pos.wsd.elev[2] else NULL, if (length(pos.wsd.slope) == 2) pos.wsd.slope[2] else NULL)
  names(res)[pos.up] <- paste("UP_", names(res)[pos.up], sep = "")
  
  # return result
  return(res)
}


