#' 
#' Summarize point source emissions of all upstream areas
#'
#' Function to calculate point source emissions over all upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table. 
#'
#' @param subid Integer vector of SUBIDs to calculate upstream point sources for (must exist in \code{gd}). 
#' If \code{NULL}, upstream point sources for all SUBIDs in 'gd' will be calculated.
#' @param gd A data frame containing columns 'SUBID' with SUBIDs and 'MAINDOWN' with downstream SUBIDs, 
#' e.g. an imported 'GeoData.txt' file.
#' @param psd A data frame with HYPE point source specifications, typically a 'PointSourceData.txt' file imported with \code{\link{ReadPointSourceData}}.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamPointSources} calculates summarised upstream point source emissions. For each sub-basin with at least one upstream 
#' point source (including the sub-basin itself), summed emission volumes and volume weighted emission concentrations are calculated. 
#' HYPE point source types ('ps_type') are returned in separate rows. \code{UpstreamPointSources} requires point source types to be one of \code{-1, 0, 1, 2, 3}, 
#' corresponding to water abstractions, no differentiation/tracer, and type 1 to 3 (e.g. wastewater treatment plants, industries, and urban stormwater). 
#' For water abstraction point sources, only summed upstream volumes are returned, i.e., concentrations are simply set to zero in results.
#' 
#' @return
#' \code{UpstreamPointSources} returns a data frame with columns containing SUBIDs, point source types, volumes, and concentrations found in \code{psd}: total nitrogen, 
#' total phosphorus, total suspended sediment, tracer, and temperature.
#' 
#' @examples
#' te1 <- ReadPointSourceData(filename = system.file("demo_model",
#' "PointSourceData.txt", package = "HYPEtools"))
#' te2 <- ReadGeoData(filename = system.file("demo_model",
#' "GeoData.txt", package = "HYPEtools"))
#' UpstreamPointSources(subid = OutletSubids(te2), gd = te2,
#' psd = te1, progbar = FALSE)
#' 
#' @export
#' @importFrom pbapply pblapply


UpstreamPointSources <- function(subid = NULL, gd, psd, bd = NULL, signif.digits = 4, progbar = TRUE) {
  
  # extract relevant column positions in gd and psd
  gd.sbd <- which(toupper(names(gd)) == "SUBID")
  psd.sbd <- which(toupper(names(psd)) == "SUBID")
  psd.type <- which(toupper(names(psd)) == "PS_TYPE")
  psd.vol <- which(toupper(names(psd)) == "PS_VOL")
  psd.tp <- which(toupper(names(psd)) == "PS_TPCONC")
  psd.tn <- which(toupper(names(psd)) == "PS_TNCONC")
  psd.ts <- which(toupper(names(psd)) == "PS_TSCONC")
  psd.t1 <- which(toupper(names(psd)) == "PS_T1")
  psd.t2 <- which(toupper(names(psd)) == "PS_T2")
  
  # number of existing concentration columns
  n.conc <- length(psd.tp) + length(psd.tn) + length(psd.ts) + length(psd.t1) + length(psd.t2)
  
  # check if there are any concentrations in psd, stop otherwise
  if (n.conc == 0) {
    stop("No concentration column(s) found in 'psd'.")
  }
  
  # check if gd and psd contain necessary columns
  if (length(gd.sbd) == 0) {
    stop("No SUBID column found in 'gd'.")
  }
  if (length(psd.sbd) == 0) {
    stop("No SUBID column found in 'psd'.")
  }
  if (length(psd.vol) == 0) {
    stop("No PS_VOL column found in 'psd'.")
  }
  
  # check if there is a ps_type, and create surrogate if not
  if(length(psd.type) == 0) {
    
    # add a dummy column to psd
    psd <- data.frame(psd, PS_TYPE = 0)
    
    # update column position
    psd.type <- ncol(psd)
    
  }
  
  # check that ps_type contains only expected values, and complain otherwise
  if (!all(unique(psd[, psd.type]) %in% -1:3)) {
    stop("Values in PS_TYPE must be in {-1, 0, 1, 2, 3}. See '?UpstreamPointSources' for details.")
  }
  
  # check if user-provided subid(s) exist in gd
  if (!is.null(subid) && any(!(subid %in% gd[, gd.sbd]))) {
    stop("'subid' not found in 'gd'.")
  }
  
  # conditional: fill subid vector with all subids in gd if not user-provided
  if (is.null(subid)) {
    subid <- gd[, gd.sbd]
  }
  
  # get a list of upstream SUBIDs for all SUBIDs in subid
  # conditional: use the progress bar version of lapply if subid is long
  if (progbar) {
    cat("\nFinding upstream SUBIDs.\n")
    up.sbd <- pblapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  } else {
    up.sbd <- lapply(subid, function(x, g, b) {AllUpstreamSubids(subid = x, g, b)}, g = gd, b = bd)
  }
  
  ## calculate upstream point source emissions, grouped by type, for now just TN and TP
  
  # remove water abstractions, if any
  row.abstr <- which(psd[, psd.type] == -1)
  if (length(row.abstr) > 0) {
    psd <- psd[-row.abstr, ]
  }
  # check that there are no abstractions left, remove any found and throw warning
  if (any(psd[, psd.vol] < 0)) {
    psd <- psd[-which(psd[, psd.vol] < 0), ]
    warning("Negative volumes in non-abstraction point sources (PS_TYPE != -1). Removed before 
            upstream average calculation. Check columns PS_TYPE and PS_VOL.")
  }
  
  # internal function used with lapply below
  # calculates upstream sums of point source volumes and volume-weighted upstream concentration means for TP and TN
  # x: vector of upstream subids
  UpPs <- function(x, psd, psd.type, psd.vol, psd.tn, psd.tp, psd.ts, psd.t1, psd.t2, n) {
    
    # result dataframe
    res <- as.data.frame(matrix(nrow = 0, ncol = 3 + n))
    
    # find current upstream point sources
    te <- psd[psd[, psd.sbd] %in% x, c(psd.type, psd.vol, psd.tn, psd.tp, psd.ts, psd.t1, psd.t2)]
    
    # create result names
    nm.res <- c("SUBID", "PS_TYPE", paste0("UP_", names(te)[-1]))
    
    # split upstream point sources by type
    te <- split(te, te[, 1])
    
    # go through ps types and calculate weighted mean concentrations for all variables (creates a bit overhead because there will be 0-only 
    # concentration columns with multi-type tables, but seems ok because psd files are not huge)
    if (!is.null(te$"-1")) {
      # abstractions, no concentrations
      res <- rbind(res, c(x[1], -1, sum(te$"-1"[, 2]), rep(0, n)))
    }
    if (!is.null(te$"0")) {
      res <- rbind(res, c(x[1], 0, sum(te$"0"[, 2]), apply(te$"0"[, -c(1:2)], 2, weighted.mean, w = te$"0"[, 2])))
    }
    if (!is.null(te$"1")) {
      res <- rbind(res, c(x[1], 1, sum(te$"1"[, 2]), apply(te$"1"[, -c(1:2)], 2, weighted.mean, w = te$"1"[, 2])))
    }
    if (!is.null(te$"2")) {
      res <- rbind(res, c(x[1], 2, sum(te$"2"[, 2]), apply(te$"2"[, -c(1:2)], 2, weighted.mean, w = te$"2"[, 2])))
    }
    if (!is.null(te$"3")) {
      res <- rbind(res, c(x[1], 3, sum(te$"3"[, 2]), apply(te$"3"[, -c(1:2)], 2, weighted.mean, w = te$"3"[, 2])))
    }
    
    # assign result names
    names(res) <- nm.res
    
    return(res)
  }
  
  # apply function to list of upstream subid vectors
  if (progbar) {
    cat("\nCalculating upstream point sources.\n")
    res <- pblapply(up.sbd, UpPs, psd = psd, psd.type = psd.type, psd.vol = psd.vol, psd.tn = psd.tn, psd.tp = psd.tp, psd.ts, psd.t1, psd.t2, n = n.conc)
  } else {
    res <- lapply(up.sbd, UpPs, psd = psd, psd.type = psd.type, psd.vol = psd.vol, psd.tn = psd.tn, psd.tp = psd.tp, psd.ts, psd.t1, psd.t2, n = n.conc)
  }
  
  # convert list of dataframes to single dataframe
  res <- do.call("rbind", res)
  
  # replace NaN results in concentrations if they exist
  # these occur if there are zero-volumes in psd (which is nonsense but not formally forbidden by HYPE formatting requirements)
  if(any(is.na(res[, 4]))) {
    res[is.na(res[, 4]), 4] <- 0
    res[is.na(res[, 5]), 5] <- 0
    warning("NAs removed from results. Check for zero-volume point sources in 'psd'!")
  }
  
  # round to requested number of digits, if there are any upstream point sources
  if (!is.null(signif.digits) && nrow(res) > 0) {
    res[, -c(1:2)] <- apply(res[, -c(1:2)], 2, signif, digits = signif.digits)
  }
  
  return(res)
}
