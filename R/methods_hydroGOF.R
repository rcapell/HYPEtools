
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Methods extending generics in package hydroGOF
#
#     - NSE method
#     - pbias method
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#' Nash-Sutcliffe Efficiency
#'
#' Nash-Sutcliffe Efficiency calculation for imported HYPE outputs with single variables for several catchments, i.e. time and
#' map files, optionally multiple model run iterations combined.
#'
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param na.rm Logical. If \code{TRUE}, incomplete sim-obs pairs will be removed prior to NSE computation.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{NSE.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @examples 
#' require(hydroGOF)
#' # Create dummy data, discharge observations with added white noise as model simulations
#' te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + 
#'                                       runif(n = nrow(te1), min = -.5, max = .5), 
#'                                dim = c(nrow(te1), ncol(te1) - 1, 1), 
#'                                dimnames = list(rownames(te1), colnames(te1)[-1])), 
#'                      datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
#' te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
#'                                dim = c(nrow(te2), ncol(te2) - 1, 1), 
#'                                dimnames = list(rownames(te2), colnames(te2)[-1])), 
#'                      datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
#' # Nash-Sutcliffe Efficiency
#' NSE(sim = te1, obs = te2, progbar = FALSE)
#' 
#' 
#' @name NSE
#' 
#' @seealso \code{\link[hydroGOF]{NSE}}, the S3 generic function defined in package 'hydroGOF'.
#' 
#' @importFrom pbapply pblapply
#' @importFrom hydroGOF NSE NSE.default
#' @export

NSE.HypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...) { 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim)[1:2], dim(obs)[1:2]) != TRUE)
    stop(paste0("Invalid argument: dim(sim)[1:2] != dim(obs)[1:2] ( [", paste(dim(sim)[1:2], collapse=", "), 
                "] != [", paste(dim(obs)[1:2], collapse=", "), "] )"))
  
  ## internal variables used in (pb)l/sapply below
  # dimensions of HypeSingleVar array
  dm <- dim(sim)
  # sequence along number of time series in simulation array, to apply over
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
    o <- pblapply(dim.seq[1:dm[2]], array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating NSE.\n")
    nse <- array(pbsapply(dim.seq, 
                          FUN = function(x, y, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                          y = dim.y,
                          s = s, 
                          o = o, 
                          nr = na.rm), 
                 dim = dm[2:3])  
  } else {
    nse <- array(sapply(dim.seq, 
                        FUN = function(x, y, s, o, nr) {NSE.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                        y = dim.y,
                        s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                        o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                        nr = na.rm), 
                 dim = dm[2:3])  
  }
  
  # return NSEs, array with 2nd and 3rd dimension extent of input array
  return(nse)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Percent bias
#' 
#' Percent bias (PBIAS) calculation for imported HYPE outputs with single variables for several catchments, i.e. time and 
#' map files, optionally multiple model runs combined.
#' 
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param na.rm Logical. If \code{TRUE}, incomplete sim-obs pairs will be removed prior to PBIAS computation.
#' @param progbar Logical. If \code{TRUE}, progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{pbias.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @examples 
#' require(hydroGOF)
#' # Create dummy data, discharge observations with added white noise as model simulations
#' te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + 
#'                                runif(n = nrow(te1), min = -.5, max = .5), 
#'                                dim = c(nrow(te1), ncol(te1) - 1, 1), 
#'                                dimnames = list(rownames(te1), colnames(te1)[-1])), 
#'                      datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
#' te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
#'                                dim = c(nrow(te2), ncol(te2) - 1, 1), 
#'                                dimnames = list(rownames(te2), colnames(te2)[-1])), 
#'                      datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
#' # Percentage bias
#' pbias(sim = te1, obs = te2, progbar = FALSE)
#' 
#' @name pbias
#' 
#' @seealso \code{\link[hydroGOF]{pbias}}, the S3 generic function defined in package 'hydroGOF'.
#' 
#' @importFrom pbapply pblapply
#' @importFrom hydroGOF pbias pbias.default
#' @export


pbias.HypeSingleVar <- function(sim, obs, na.rm = TRUE, progbar = TRUE, ...){ 
  
  # Check that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim)[1:2], dim(obs)[1:2]) != TRUE)
    stop(paste0("Invalid argument: dim(sim)[1:2] != dim(obs)[1:2] ( [", paste(dim(sim)[1:2], collapse=", "), 
                "] != [", paste(dim(obs)[1:2], collapse=", "), "] )"))
  
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
    o <- pblapply(dim.seq[1:dm[2]], array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating PBIAS.\n")
    pb <- array(pbsapply(dim.seq, 
                         FUN = function(x, y, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                         y = dim.y,
                         s = s, 
                         o = o, 
                         nr = na.rm), 
                dim = dm[2:3])  
  } else {
    pb <- array(sapply(dim.seq, 
                       FUN = function(x, y, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                       y = dim.y,
                       s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                       o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                       nr = na.rm), 
                dim = dm[2:3])  
  }
  
  # return PBIASs, array with 2nd and 3rd dimension extent of input array
  return(pb)
}
