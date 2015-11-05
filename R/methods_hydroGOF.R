#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# Method definitions for generic functions from the 'hydroGOF' package.
#
# For these classes, defined in HYPETOOLS:
#
#     - HypeSingleVar
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' @import hydroGOF
#'
#' @title Nash-Sutcliffe Efficiency
#'  
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable.
#' @param obs \code{\link{HypeSingleVar}} array with observed variable.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
#' @export
#' @aliases NSE
#' @name NSE


NSE.HypeSingleVar <- function (sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
  
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
  array2list <- function(x, y, z, a) {a[, y[x], z[x]]}
  
  # calculate NSEs, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.")
    o <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating NSEs.")
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
