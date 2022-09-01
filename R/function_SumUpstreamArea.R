#' 
#' Calculate upstream area sums
#'
#' Function to calculate upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table.
#'
#' @param subid Integer vector of SUBIDs to calculate upstream areas for (must exist in \code{gd}). If \code{NULL}, upstream areas for all 
#' SUBIDs will be calculated.
#' @param gd A data frame, containing 'SUBID', 'MAINDOWN', and 'AREA' columns, e.g. an imported 'GeoData.txt' file.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param cl Integer, number of processes to use for parallel computation. Set to `1` for serial computation. See [parallel::detectCores()].
#' @param progbar Logical, display a progress bar while calculating upstream areas. Adds overhead to calculation time but useful if you want 
#' HYPEtools to decide how long your coffee break should take.
#' 
#' @details
#' \code{SumUpstreamArea} sums upstream areas of all connected upstream SUBIDs, including branch connections in case of stream bifurcations 
#' but not including potential irrigation links or groundwater flows.
#' 
#' @return
#' \code{SumUpstreamArea} returns a data frame with two columns containing SUBIDs and total upstream areas (in area units as provided in \code{gd}). 
#' Upstream areas include areas of outlet SUBIDs.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' SumUpstreamArea(subid = c(3361, 63794), gd = te, progbar = FALSE)
#' 
#' @importFrom pbapply pbsapply
#' @importFrom parallel makePSOCKcluster stopCluster mclapply parLapply
#' @export

SumUpstreamArea <- function(subid = NULL, gd, bd = NULL, cl = 2, progbar = FALSE) {
  
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
    if (cl > 1) {
      # parallel computation
      if (.Platform$OS.type == "unix") {
        up.area <- pbsapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area, cl = cl)
      } else if (.Platform$OS.type == "windows") {
        cl <- makePSOCKcluster(cl)
        up.area <- pbsapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% HYPEtools::AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area, cl = cl)
        stopCluster(cl)
      } else {
        warning("Unknown OS type, reverting to serial computation.")
        up.area <- pbsapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
      }
      
    } else {
      # serial computation
      up.area <- pbsapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
    }
  } else {
    if (cl > 1) {
      # parallel computation
      if (.Platform$OS.type == "unix") {
        up.area <- mclapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area, mc.cores = cl)
        up.area <- unlist(up.area)
      } else if (.Platform$OS.type == "windows") {
        cl <- makePSOCKcluster(cl)
        up.area <- parLapply(cl = cl, X = subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% HYPEtools::AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
        stopCluster(cl)
        up.area <- unlist(up.area)
      } else {
        warning("Unknown OS type, reverting to serial computation.")
        up.area <- sapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
      }
    } else {
      # serial computation
      up.area <- sapply(subid, function(x, g, b, ps, pa) {sum(g[g[, ps] %in% AllUpstreamSubids(subid = x, g, b), pa])}, g = gd, b = bd, ps = pos.sbd, pa = pos.area)
    }
  }
  
  # build and return result dataframe
  res <- data.frame(SUBID = subid, UPSTREAMAREA = up.area)
  return(res)
}
