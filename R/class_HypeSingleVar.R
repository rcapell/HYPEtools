
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Class HypeSingleVar, herein:
#
#     - HypeSingleVar (constructor function)
#     - [.HypeSingleVar (indexing method)
#     - summary method
#     - print method for summary list
#     - NSE method
#     - pbias method
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' HypeSingleVar arrays
#' 
#' Constructor function for arrays which can hold time series of a single HYPE variable for multiple sub-basins 
#' and multiple model runs, typically imported time and map output results.
#' 
#' @param x numeric \code{\link{array}} with three dimensions, which holds HYPE results for one variable as (in order)
#' \code{[time, subid, iteration]}.
#' @param date \code{\link{POSIXct}} date-time vector of the same length as \code{time} dimension of \code{x} 
#' with time steps (starting day for time steps from weekly to annual), or character string for full model period averages, 
#' e.g. \code{"2000-2010"}.
#' @param subid Integer vector with HYPE sub-basin IDs, of the same length as \code{subid} dimension of \code{x}.
#' @param hype.var Character string, four-letter keyword to specify HYPE variable ID, see 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variable}. 
#' Not case-sensitive.
#' 
#' @details
#' S3 class constructor function for array objects which can hold (multiple) HYPE time or map output results. 
#' 
#' @return
#' Returns a 3-dimensional array with 
#' \code{[time, subid, iteration]} dimensions and additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{subid}}{A vector of SUBIDs. Corresponds to 2nd array dimension.}
#' \item{\strong{variable}}{HYPE output variable ID.}
#' }
#' 
#' @examples
#' \dontrun{HypeSingleVar(mytimeoutput, date = mydates, subid = c(23, 45, 56), hype.var = "cctn")}
#' 
#' @export

HypeSingleVar <- function(x, date, subid, hype.var) {
  
  # check if data is conform to requirements
  if (is.array(x)) {
    
    # force three dimensions in the resulting array
    dx <- dim(x)
    if (length(dx) < 3) {
      dim(x) <- c(dx, rep(1, 3 - length(dx)))
    }
    
    # check attribute length conformity
    if (length(date) != dim(x)[1]) {
      stop("Different lengths of argument 'date' and corresponding dimension of 'x'.")
    }
    if (length(subid) != dim(x)[2]) {
      stop("Different lengths of argument 'subid' and corresponding dimension of 'x'.")
    }
    class(x) <- c("HypeSingleVar", "array")
    attr(x, "date") <- date
    attr(x, "subid") <- subid
    attr(x, "variable") <- toupper(hype.var)
    return(x)
  } else {
    stop("Non-array input.")
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# indexing method
#' @export
`[.HypeSingleVar` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], ...) {
  y <- NextMethod("[", drop = F)
  attr(y, "variable") <- attr(x, "variable")
  attr(y, "date") <- attr(x, "date")[i]
  attr(y, "subid") <- attr(x, "subid")[j]
  class(y) <- c("HypeSingleVar", "array")
  return(y)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# summary method
#' @export
summary.HypeSingleVar <- function(x, ...) {
  vari <- attr(x, "variable")
  dat <- attr(x, "date")
  ldat <- length(dat)
  ans <- list(hypevar = vari, tslen = ldat)
  if (ldat > 1) {
    ans$sdate <- dat[1]
    ans$edate <- dat[ldat]
    ans$period <- NA
  } else {
    ans$sdate <- NA
    ans$edate <- NA
    ans$period <- dat
  }
  ans$nsbd <- length(attr(x, "subid"))
  ans$niter <- dim(x)[3]
  class(ans) <- "summaryHypeSingleVar"
  ans
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# print method for summary list object
#' @export
print.summaryHypeSingleVar <- function(x, ...) {
  nm <- c("HYPE variable:\t\t\t\t", "Simulation time series length:\t\t", "Simulation start date:\t\t\t", 
          "Simulation end date:\t\t\t", "Simulation time period:\t\t\t", "Number of SUBIDs in simulation:\t\t", 
          "Number of iterations in simulation:\t")
  cat("\n")
  for (i in 1:length(x)) {
    if (!is.na(x[[i]])) {
      cat(paste(nm[i], x[[i]], "\n"))
    }
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# NSE method, generic function in hydroGOF package
#' @export
#' @importFrom hydroGOF NSE NSE.default
#' @importFrom pbapply pblapply
#' @title Nash-Sutcliffe Efficiency
#'
#' @description
#' Nash-sutcliffe Efficiency calculation for imported HYPE outputs with single variables for several catchments, i.e. time and
#' map files, optionally multiple model run iterations combined.
#'
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{NSE.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @name NSE
#' 
#' @seealso \code{\link[hydroGOF]{NSE}}, the S3 generic function defined in package 'hydroGOF'.

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

#' @export
#' @importFrom hydroGOF pbias pbias.default
#' @importFrom pbapply pblapply
#'
#' @title Percent bias
#'  
#' @description
#' Percent bias (PBIAS) calculation for imported HYPE outputs with single variables for several catchments, i.e. time and 
#' map files, optionally multiple model runs combined.
#' 
#' @param sim \code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).
#' @param obs \code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
#' in the array, only the first will be used.
#' @param progbar Logical, if \code{TRUE} progress bars will be printed for main computational steps.
#' @param ... ignored
#' 
#' @return 
#' \code{pbias.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in 
#' argument \code{sim}, with values in the same order 
#' as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
#' 
#' @name pbias
#' 
#' @seealso \code{\link[hydroGOF]{pbias}}, the S3 generic function defined in package 'hydroGOF'.


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
                       FUN = function(x, s, o, nr) {pbias.default(sim = s[[x]], obs = o[[y[x]]], nr = na.rm)}, 
                       y = dim.y,
                       s = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = sim), 
                       o = lapply(dim.seq, array2list, y = dim.y, z = dim.z, a = obs), 
                       nr = na.rm), 
                dim = dm[2:3])  
  }
  
  # return PBIASs, array with 2nd and 3rd dimension extent of input array
  return(pb)
}
