#' @export
#' @importFrom pbapply pblapply
#' 
#' @title
#' Summarize point source emissions of all upstream areas
#'
#' @description
#' Function to calculate point source emissions over all upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table. 
#' Only total nitrogen and total phosphorus emissions are summarised.
#'
#' @param subid Integer vector of SUBIDs to calculate upstream SUBID fractions for (must exist in \code{gd}). 
#' If \code{NULL}, upstream areas for all SUBIDs will be calculated.
#' @param gd A data frame containing columns 'SUBID' with SUBIDs and 'MAINDOWN' with downstream SUBIDs, 
#' e.g. an imported 'GeoData.txt' file.
#' @param psd A data frame with HYPE point source specifications, e.g. and imported 'PointSourceData.txt' file.
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param signif.digits Integer, number of significant digits to round upstream SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{UpstreamPointSources} calculates summarised upstream point source emissions. For each sub-basin with at least one upstream 
#' point source (including the sub-basin itself), summed emission volumes and volume weighted emission concentrations are calculated. 
#' HYPE point source types ('ps_type') are returned in separate rows. Water abstraction point sources are ignored
#' 
#' @return
#' \code{UpstreamPointSources} returns a data frame with columns containing SUBIDs, point source types, volumes, total nitrogen 
#' concentrations, and total phosphorus concentrations.
#' 
#' @examples
#' \dontrun{UpstreamPointSources(subid = 21, gd = mygeodata, psd = mypsdata, bd = mybranchdata)}


UpstreamPointSources <- function(subid = NULL, gd, psd, bd = NULL, signif.digits = 4, progbar = T) {
  
  # extract relevant column positions in gd and psd
  gd.sbd <- which(toupper(names(gd)) == "SUBID")
  psd.sbd <- which(toupper(names(psd)) == "SUBID")
  psd.type <- which(toupper(names(psd)) == "PS_TYPE")
  psd.vol <- which(toupper(names(psd)) == "PS_VOL")
  psd.tp <- which(toupper(names(psd)) == "PS_TPCONC")
  psd.tn <- which(toupper(names(psd)) == "PS_TNCONC")
  
  
  # check if gd and psd contain subids
  if (length(gd.sbd) == 0) {
    stop("No SUBID column found in 'gd'. Exiting.")
  }
  if (length(psd.sbd) == 0) {
    stop("No SUBID column found in 'psd'. Exiting.")
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
  row.abstr <- which(psd[, psd.vol] < 0)
  if (length(row.abstr) > 0) {
    psd <- psd[-row.abstr, ]
  }
  
  # internal function used with lapply below
  # calculates upstream sums of point source volumes and volume-weighted upstream concentration means for TP and TN
  # x: vector of upstream subids
  UpPs <- function(x, psd, psd.type, psd.vol, psd.tn, psd.tp) {
    # result dataframe
    res <- data.frame(integer(), integer(), numeric(), numeric(), numeric())
    
    # find current upstream point sources
    te <- psd[psd[, psd.sbd] %in% x, c(psd.type, psd.vol, psd.tn, psd.tp)]
    te <- split(te, te[, 1])
    if (!is.null(te$"1")) {
      res <- rbind(res, c(x[1], 1, sum(te$"1"[, 2]), weighted.mean(te$"1"[, 3], w = te$"1"[, 2]), weighted.mean(te$"1"[, 4], w = te$"1"[, 2])))
    }
    if (!is.null(te$"2")) {
      res <- rbind(res, c(x[1], 2, sum(te$"2"[, 2]), weighted.mean(te$"2"[, 3], w = te$"2"[, 2]), weighted.mean(te$"2"[, 4], w = te$"2"[, 2])))
    }
    if (!is.null(te$"3")) {
      res <- rbind(res, c(x[1], 3, sum(te$"3"[, 2]), weighted.mean(te$"3"[, 3], w = te$"3"[, 2]), weighted.mean(te$"3"[, 4], w = te$"3"[, 2])))
    }
    names(res) <- c("subid", "ps_type", "up_ps_vol", "up_ps_tnconc", "up_ps_tpconc")
    
    return(res)
  }
  
  # apply function to list of upstream subid vectors
  if (progbar) {
    cat("\nCalculating upstream point sources.\n")
    res <- pblapply(up.sbd, UpPs, psd = psd, psd.type = psd.type, psd.vol = psd.vol, psd.tn = psd.tn, psd.tp = psd.tp)
  } else {
    res <- lapply(up.sbd, UpPs, psd = psd, psd.type = psd.type, psd.vol = psd.vol, psd.tn = psd.tn, psd.tp = psd.tp)
  }
  
  # convert list of dataframes to single dataframe
  res <- do.call("rbind", res)
  
  # replace NaN results in concentrations if they exist
  # these occur if there are zero-volumes in psd (which is nonsense but not formally forbidden by HYPE formatting requirements)
  if(any(is.na(res[, 4]))) {
    res[is.na(res[, 4]), 4] <- 0
    res[is.na(res[, 5]), 5] <- 0
  }
  
  # round to requested number of digits, if there are any upstream point sources
  if (!is.null(signif.digits) && nrow(res) > 0) {
    res[, -c(1:2)] <- apply(res[, -c(1:2)], 2, signif, digits = signif.digits)
  }
  
  return(res)
}
