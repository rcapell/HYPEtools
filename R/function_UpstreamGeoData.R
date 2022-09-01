#' 
#' Calculate upstream sums and averages of selected GeoData contents
#'
#' Function to calculate upstream sums and averages for selected variables of imported GeoData.txt files. 
#'
#' @param subid Integer vector of SUBIDs for which to calculate upstream properties (must exist in \code{gd}). 
#' If \code{NULL} (default), upstream areas for all SUBIDs will be calculated.
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file.
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param olake.slc Integer,SLC class number which represents outlet lake fractions. Mandatory for weighted averaging of outlet lake depths. 
#' @param bd.weight Logical, if set to \code{TRUE}, flow weights will be applied for areas upstream of stream bifurcations. See 
#' \code{\link{AllUpstreamSubids}} for further details on flow fraction computation.
#' @param signif.digits Integer, number of significant digits to round upstream variables to. See also \code{\link{signif}}. 
#' Set to \code{NULL} to prevent rounding. 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful 
#' when \code{subid} is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamGeoData} calculates upstream averages or sums of selected variables in a GeoData data frame, including branch connections 
#' in case of stream bifurcations but not including potential irrigation links or groundwater flows. Averages are weighted by sub-catchment area, with 
#' the exception of outlet lake depths and rural household emission concentrations provided in GeoData variables 'lake_depth', 'loc_tn', 
#' and 'loc_tp'. Outlet lake depths are weighted by outlet lake area and the GeoData column with 
#' SLC class fractions for outlet lakes must be provided in function argument \code{col.olake.slc}. Rural household emissions are weighted by 
#' emission volume as provided in column 'loc_vol'. Elevation and slope standard deviations are 
#' averaged if the corresponding mean values exist (sample means are required to calculate overall means of standard deviations).
#' 
#' Currently, the following variables are considered:
#' \describe{
#'   \item{Area-weighted average}{elev_mean, slope_mean, buffer, close_w, latitude, longitude, all SLC classes, lake depths, elev_std, slope_std}
#'   \item{Volume-weighted average}{loc_tn, loc_tp}
#'   \item{Sum}{area, rivlen, loc_vol}
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
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' # Upstream stats for domain outlet
#' UpstreamGeoData(subid = OutletSubids(te), gd = te, olake.slc = 1, progbar = FALSE)
#' 
#' @importFrom pbapply pblapply pbsapply
#' @importFrom stats weighted.mean
#' @export

UpstreamGeoData <- function(subid = NULL, gd, bd = NULL, olake.slc = NULL, bd.weight = FALSE, signif.digits = 5, progbar = TRUE) {
  
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
  
  # warn if argument specification indicates user confusion
  if (is.null(bd) && bd.weight) {
    warning("Bifurcation weights requested with 'bd.weight' but 'bd' not specified. Ignoring request.")
    bd.weight <- FALSE
  }
  
  # conditional: fill subid vector if not user-provided, otherwise check that all subids are in gd and get row numbers
  if (is.null(subid)) {
    subid <- gd[, pos.sbd]
    row.sbd <- NULL
  } else {
    row.sbd <- match(x = c(subid), table = gd[, pos.sbd])
    if (any(is.na(row.sbd))) {
      stop("At least one SUBID in 'subid' not found in 'gd'. Exiting.")
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
  pos.wmean.slc.olake <- which(toupper(names(gd)) == paste0("SLC_", olake.slc))
  pos.wmean.lconc <- which(tolower(names(gd))  %in% c("loc_tn", "loc_tp"))
  # loc_vol is extracted two times, first for using it as weight for loc_tn/p, then for summing
  pos.wmean.lvol <- which(tolower(names(gd))  %in% c("loc_vol"))
  pos.sum <- which(tolower(names(gd)) %in% c("area", "rivlen", "loc_vol"))
  pos.wsd.elev <- which(tolower(names(gd)) %in% c("elev_std", "elev_mean"))
  pos.wsd.slope <- which(tolower(names(gd)) %in% c("slope_std", "slope_mean"))
  
  # warn user if upstream lake_depth cannot be calculated
  if (length(pos.wmean.ldepth) == 1 && is.null(olake.slc)) {
    warning("'lake_depth' found in GeoData, but no outlet lake SLC class provided in argument 'olake.slc'. Skipping upstream 'lake_depth'.")
  }
  if (length(pos.wmean.ldepth) == 0 && !is.null(olake.slc)) {
    warning("Outlet lake SLC class provided in argument 'olake.slc', but no 'lake_depth' found in GeoData. Skipping upstream 'lake_depth'.")
  }
  if (length(pos.wmean.ldepth) == 1 && !is.null(olake.slc) && length(pos.wmean.slc.olake) == 0) {
    warning("'lake_depth' found in GeoData, but outlet lake SLC class provided in argument 'olake.slc' does not exist. Skipping upstream 'lake_depth'.")
  }
  
  # warn user if rural household releases cannot be calculated
  if (length(pos.wmean.lconc) >= 1 && is.null(pos.wmean.lvol)) {
    warning("'loc_tn' and/or 'loc_tp' found in GeoData, but no corresponding 'loc_vol'. Skipping upstream rural household releases.")
  }
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if requested by user
  if (progbar) {
    cat("\nFinding upstream SUBIDs.\n")
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b, get.weights = bd.weight)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b, get.weights = bd.weight)}, g = gd, b = bd)
  }
  
  ########################
  ### internal functions applied below
  
  # internal function to calculate area-weighted means for one group of upstream areas (one element in up.sbd)
  # x: vector of upstream subids
  WeightedMean <- function(x, g, p.sbd, p.wmean, p.area) {
    
    # extract dataframe with areas and variables in x for which to calculate weighted means
    if (bd.weight) {
      df.wmean <- g[g[, p.sbd] %in% x[, 1], c(p.area, p.wmean)]
      df.wmean[, 1] <- df.wmean[, 1] * x[, 2]
    } else {
      df.wmean <- g[g[, p.sbd] %in% x, c(p.area, p.wmean)]
    }
    
    # averaging only necessary if more than one subid, also avoids NaN result if stddev is 0
    if (nrow(df.wmean) > 1) {
      
      # area-weighted mean of all columns, with exception handling where all weights are 0
      res <- apply(as.data.frame(df.wmean[, -1]), 2, weighted.mean, w = df.wmean[, 1])
      if (all(df.wmean[, 1] == 0)) {
        res[] <- 0
      }
      
      # catch
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
    if (bd.weight) {
      df.wsd <- g[g[, p.sbd] %in% x[, 1], c(p.area, p.wsd)]
      df.wsd[, 1] <- df.wsd[, 1] * x[, 2]
    } else {
      df.wsd <- g[g[, p.sbd] %in% x, c(p.area, p.wsd)]
    }
    
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
    if (bd.weight) {
      df.sum <- data.frame(g[g[, p.sbd] %in% x[, 1], p.sum])
      for (i in 1:ncol(df.sum)) {
        df.sum[, i] <- df.sum[, i] * x[, 2]
      }
    } else {
      df.sum <- data.frame(g[g[, p.sbd] %in% x, p.sum])
    }
    # columnwise sum
    res <- colSums(df.sum)
    return(res)
  }
  
  ##############################################
  
  

  # apply area-weighted mean function to all SUBIDs in variable 'subid', for all relevant variables
  # conditional: use the progress bar version of sapply if set by function argument
  if (length(pos.wmean > 0)) {
    if (progbar) {
      cat("\nCalculating upstream area-weighted means.\n")
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
  } else {
    up.wmean <- NULL
  }
  
  # olake-area weighted mean for lake depths
  if (length(pos.wmean.ldepth) == 1 && length(pos.wmean.slc.olake) == 1) {
    if (progbar) {
      cat("\nCalculating upstream lake-area-weighted lake depths.\n")
      up.wmean.ldepth <- data.frame(SUBID = subid, LAKE_DEPTH = pbsapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.ldepth, p.area = pos.wmean.slc.olake))
    } else {
      up.wmean.ldepth <- data.frame(SUBID = subid, LAKE_DEPTH = sapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.ldepth, p.area = pos.wmean.slc.olake))
    }
    # if no lakes exist, all weights are 0 and NaN is computed. Replace those with zeros
    up.wmean.ldepth[is.nan(up.wmean.ldepth[, 2]), 2] <- 0
  } else {
    up.wmean.ldepth <- NULL
  }
  
  # volume-weighted rural household concentrations
  if (length(pos.wmean.lconc) >= 1 && !is.null(pos.wmean.lvol)) {
    if (progbar) {
      cat("\nCalculating upstream volume-weighted rural household releases.\n")
      te <- pbsapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.lconc, p.area = pos.wmean.lvol)
    } else {
      te <- sapply(up.sbd, WeightedMean, g = gd, p.sbd = pos.sbd, p.wmean = pos.wmean.lconc, p.area = pos.wmean.lvol)
    }
    # create result dataframe, conditional on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
    if(length(pos.wmean.lconc) > 1) {
      up.wmean.lconc <- data.frame(SUBID = subid, t(te))
    } else {
      up.wmean.lconc <- data.frame(SUBID = subid, te)
      names(up.wmean.lconc)[2] <- names(gd)[pos.wmean.lconc]
    }
    rm(te)
  } else {
    up.wmean.lconc <- NULL
  }
  
  # apply area-weighted sd function to all SUBIDs in variable 'subid'
  # do separately for slope and elev, if they exist in gd
  if (length(pos.wsd.elev) == 2) {
    if (progbar) {
      cat("\nCalculating upstream area-weighted elevation standard deviations.\n")
      up.wsd.elev <- data.frame(SUBID = subid, ELEV_STD = pbsapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.elev, p.area = pos.area))
    } else {
      up.wsd.elev <- data.frame(SUBID = subid, ELEV_STD = sapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.elev, p.area = pos.area))
    }
  } else {
    up.wsd.elev <- NULL
  }
  
  if (length(pos.wsd.slope) == 2) {
    if (progbar) {
      cat("\nCalculating upstream area-weighted slope standard deviations.\n")
      up.wsd.slope <- data.frame(SUBID = subid, SLOPE_STD = pbsapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.slope, p.area = pos.area))
    } else {
      up.wsd.slope <- data.frame(SUBID = subid, SLOPE_STD = sapply(up.sbd, WeightedSd, g = gd, p.sbd = pos.sbd, p.wsd = pos.wsd.slope, p.area = pos.area))
    }
  } else {
    up.wsd.slope <- NULL
  }
  
  # apply sum function to all SUBIDs in variable 'subid', for all relevant variables
  if (length(pos.sum) > 0) {
    if (progbar) {
      cat("\nCalculating upstream sums.\n")
      te <- pbsapply(up.sbd, Sum, g = gd, p.sum = pos.sum, p.sbd = pos.sbd)
    } else {
      te <- sapply(up.sbd, Sum, g = gd, p.sum = pos.sum, p.sbd = pos.sbd)
    }
    # create result dataframe, conditional on if the was just one variable to be summed, because the apply result is a vector then, not a dataframe..
    if(length(pos.sum) > 1) {
      up.sum <- data.frame(SUBID = subid, t(te))
    } else {
      up.sum <- data.frame(SUBID = subid, te)
      names(up.sum)[2] <- names(gd)[pos.sum]
    }
  } else {
    up.sum <- NULL
  }
  
  
  #######################################################
  
  ## post-processing
  if (progbar) {
    cat("\nPost-processing.\n")
    }
  
  # round to requested number of digits, conditional on existing results for lake_depth and stddev variables
  # the data frame dummy column adding and removing is a workaround for single-column cases (names get messed up because 
  # apply returns a names vector then..)
  if (!is.null(signif.digits)) {
    if (!is.null(up.wmean)) {
      te <- apply(data.frame(1, up.wmean[, -1]), 2, signif, digits = signif.digits)
      # te is a vector for single-subid cases
      if (length(subid) == 1) {
        up.wmean[, -1] <- te[-1]
      } else {
        up.wmean[, -1] <- te[, -1]
      }
    }
    if (!is.null(up.wmean.ldepth)) {
      up.wmean.ldepth[, -1] <- signif(up.wmean.ldepth[, -1], digits = signif.digits)
    }
    if (!is.null(up.wmean.lconc)) {
      te <- apply(data.frame(1, up.wmean.lconc[, -1]), 2, signif, digits = signif.digits)
      # te is a vector for single-subid cases
      if (length(subid) == 1) {
        up.wmean.lconc[, -1] <- te[-1]
      } else {
        up.wmean.lconc[, -1] <- te[, -1]
      }
    }
    if (!is.null(up.wsd.elev)) {
      up.wsd.elev[, -1] <- signif(up.wsd.elev[, -1], digits = signif.digits)
    }
    if (!is.null(up.wsd.slope)) {
      up.wsd.slope[, -1] <- signif(up.wsd.slope[, -1], digits = signif.digits)
    }
    if (!is.null(up.sum)) {
      te <- apply(data.frame(1, up.sum[, -1]), 2, signif, digits = signif.digits)
      if (length(subid) == 1) {
        up.sum[, -1] <- te[-1]
      } else {
        up.sum[, -1] <- te[, -1]
      }
    }
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
    res[, pos.wmean.ldepth] <- up.wmean.ldepth[2]
    }
  if (!is.null(up.wmean.lconc)) {
    res[, pos.wmean.lconc] <- up.wmean.lconc[, -1]
    }
  if (!is.null(up.wsd.elev)) {
    res[, which(tolower(names(gd)) == "elev_std")] <- up.wsd.elev[, -1]
    }
  if (!is.null(up.wsd.slope)) {
    res[, which(tolower(names(gd)) == "slope_std")] <- up.wsd.slope[, -1]
    }
  res[, pos.sum] <- up.sum[, -1]
  
  
  # rename upstream variables to clarify they are upstream values
  pos.up <- c(pos.wmean, pos.sum, pos.wmean.ldepth, pos.wmean.lconc, 
              if (length(pos.wsd.elev) == 2) which(tolower(names(gd)) == "elev_std") else NULL, 
              if (length(pos.wsd.slope) == 2) which(tolower(names(gd)) == "slope_std") else NULL)
  names(res)[pos.up] <- paste0("UP_", names(res)[pos.up])
  
  # return result
  return(res)
}


