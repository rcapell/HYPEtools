#'
#' Pearson product-moment correlation coefficient r
#' 
#' Pearson product-moment correlation coefficient calculation, a specific case of function \code{\link{cor}}.
#' 
#' @param sim \code{\link[=HypeSingleVar]{HypeSingleVar array}} with simulated variable (one or several iterations).
#' @param obs \code{\link[=HypeSingleVar]{HypeSingleVar array}} with observed variable, (one iteration). If several iterations 
#' are present in the array, only the first will be used.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... Ignored.
#' 
#' @details 
#' This function wraps a call to \code{cor(x = obs, y = sim, use = "na.or.complete", method = "pearson")}. 
#' 
#' Method \code{r.HypeSingleVar} calculates Pearson's r for imported HYPE outputs with single variables for several 
#' catchments, i.e. time and map files, optionally multiple model runs combined, typically results from calibration runs.
#' 
#' @return 
#' \code{r.HypeSingleVar} returns a 2-dimensional array of Pearson correlation coefficients for all SUBIDs and model 
#' iterations provided in argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @examples 
#' # Create dummy data, discharge observations with added white noise as model simulations
#' te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + runif(n = nrow(te1), 
#'                                                                 min = -.5, max = .5), 
#'                                dim = c(nrow(te1), ncol(te1) - 1, 1), 
#'                                dimnames = list(rownames(te1), colnames(te1)[-1])), 
#'                      datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
#' te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
#' te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
#'                                dim = c(nrow(te2), ncol(te2) - 1, 1), 
#'                                dimnames = list(rownames(te2), colnames(te2)[-1])), 
#'                      datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
#' # Pearson correlation
#' r(sim = te1, obs = te2, progbar = FALSE)
#' 
#' @seealso \code{\link{cor}}, on which the function is based. \code{\link{ReadWsOutput}} for importing HYPE calibration results.
#' 
#' @export

r <- function(sim, obs, ...) {
  UseMethod("r")
  }


#' @rdname r
#' @importFrom pbapply pblapply pbsapply
#' @importFrom stats cor
#' @export


r.HypeSingleVar <- function(sim, obs, progbar = TRUE, ...){ 
  
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
  
  # calculate correlation coefs, with conditional verbosity
  if (progbar) {
    cat("Preparing 'sim'.\n")
    s <- pblapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim)
    cat("Preparing 'obs'.\n")
    o <- pblapply(dim.seq[1:dm[2]], array2list, y = dim.y, z = dim.z, a = obs)
    cat("Calculating CC.\n")
    pc <- array(pbsapply(dim.seq, 
                         FUN = function(x, y, s, o) {cor(x = s[[x]], y = o[[y[x]]], use = "na.or.complete", method = "pearson")}, 
                         y = dim.y,
                         s = s, 
                         o = o), 
                dim = dm[2:3])  
  } else {
    pc <- array(sapply(dim.seq, 
                       FUN = function(x, y, s, o) {cor(x = s[[x]], y = o[[y[x]]], use = "na.or.complete", method = "pearson")}, 
                       y = dim.y,
                       s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                       o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs)), 
                dim = dm[2:3])  
  }
  
  # return pearson correlation, array with 2nd and 3rd dimension extent of input array
  return(pc)
}
