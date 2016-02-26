#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# Method definitions for generic functions from the 'hydroGOF' package.
#
# For the following S3 classes, defined in HYPETOOLS:
#
#     - HypeSingleVar
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @import hydroGOF
#'
#' @title Nash-Sutcliffe Efficiency
#'  
#' @description
#' Nash-sutcliffe Efficiency calculation for imported HYPE outputs with single variables for several catchments, i.e. time and 
#' map files, optionally multiple model runs combined.
#' 
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable.
#' @param obs \code{\link{HypeSingleVar}} array with observed variable.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
# @inheritParams hydroGOF::NSE
# @aliases NSE
# @name NSE
#' 
#' @return 
#' A two-dimensional array with NSE for all time series pairs provided in \code{sim} and \code{obs}, with values in the same order 
#' as the second and third dimension as the input, i.e. \code{[subid, iteration]}.


#NSE.HypeSingleVar <- function (sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
NSEHypeSingleVar <- function (sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop(paste0("Invalid argument: dim(sim) != dim(obs) ( [", paste(dim(sim), collapse=", "), 
                 "] != [", paste(dim(obs), collapse=", "), "] )"))
  
  ## internal variables used in (pb)l/sapply below
  # dimensions of HypeSingleVar array
  dm <- dim(sim)
  # sequence along number of time series in array, to apply over
  dim.seq <- seq(dm[2] * dm[3])
  # 2nd dim indices in correct order, corresponding to time series sequence above
  dim.y <- rep(1:dm[2], times = dm[3])
  # 3rd dim indices in correct order, corresponding to time series sequence above
  dim.z <- rep(1:dm[3], each = dm[2])
  
  # internal function to split HypeSingleVar array into a list of time series, used in (pb)lapply below
  array2list <- function(x, y, z, a) {as.numeric(a[, y[x], z[x]])}
  
  # calculate NSEs, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.\n")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.\n")
    o <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating NSE.\n")
    nse <- array(pbsapply(dim.seq, 
                        FUN = function(x, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[x]], nr = na.rm)}, 
                        s = s, 
                        o = o, 
                        nr = na.rm), 
                 dim = dm[2:3])  
  } else {
    nse <- array(sapply(dim.seq, 
                        FUN = function(x, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[x]], nr = na.rm)}, 
                        s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                        o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                        nr = na.rm), 
                 dim = dm[2:3])  
  }
  
  # return NSEs, array with 2nd and 3rd dimension extent of input array
  return(nse)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @import hydroGOF
#'
#' @title Percent bias
#'  
#' @description
#' Percent bias (PBIAS) calculation for imported HYPE outputs with single variables for several catchments, i.e. time and 
#' map files, optionally multiple model runs combined.
#' 
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable.
#' @param obs \code{\link{HypeSingleVar}} array with observed variable.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
# @inheritParams hydroGOF::pbias
# #' @aliases pbias
#' 
#' @return 
#' A two-dimensional array with PBIAS for all time series pairs provided in \code{sim} and \code{obs}, with values in the same order 
#' as the second and third dimension as the input, i.e. \code{[subid, iteration]}.


#pbias.HypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
pbiasHypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop(paste0("Invalid argument: dim(sim) != dim(obs) ( [", paste(dim(sim), collapse=", "), 
                "] != [", paste(dim(obs), collapse=", "), "] )"))
  
  ## internal variables used in (pb)l/sapply below
  # dimensions of HypeSingleVar array
  dm <- dim(sim)
  # sequence along number of time series in array, to apply over
  dim.seq <- seq(dm[2] * dm[3])
  # 2nd dim indices in correct order, corresponding to time series sequence above
  dim.y <- rep(1:dm[2], times = dm[3])
  # 3rd dim indices in correct order, corresponding to time series sequence above
  dim.z <- rep(1:dm[3], each = dm[2])
  
  # internal function to split HypeSingleVar array into a list of time series, used in (pb)lapply below
  array2list <- function(x, y, z, a) {as.numeric(a[, y[x], z[x]])}
  
  # calculate PBIAS, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.\n")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.\n")
    o <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating PBIAS.\n")
    pb <- array(pbsapply(dim.seq, 
                          FUN = function(x, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[x]], nr = na.rm)}, 
                          s = s, 
                          o = o, 
                          nr = na.rm), 
                 dim = dm[2:3])  
  } else {
    pb <- array(sapply(dim.seq, 
                        FUN = function(x, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[x]], nr = na.rm)}, 
                        s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                        o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                        nr = na.rm), 
                 dim = dm[2:3])  
  }
  
  # return PBIASs, array with 2nd and 3rd dimension extent of input array
  return(pb)
}
