
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   S3 class HypeSingleVar, herein:
#
#     - HypeSingleVar (constructor function)
#     - [.HypeSingleVar (indexing method)
#     - summary method
#     - print method for summary list
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' HypeSingleVar arrays
#' 
#' Constructor function for arrays which hold equi-distant time series of a single HYPE variable for multiple sub-basins 
#' and multiple model runs, typically imported time and map output results.
#' 
#' @param x numeric \code{\link{array}} with three dimensions, which holds HYPE results for one variable as (in order)
#' \code{[date, subid/outregid, iteration]}.
#' @param date \code{\link{POSIXct}} date-time vector of the same length as \code{time} dimension of \code{x} 
#' with equidistant time steps (starting day for time steps from weekly to annual), or character string for full model 
#' period averages, e.g. \code{"2000-2010"}.
#' @param subid Integer vector with HYPE sub-basin IDs, of the same length as \code{subid} dimension of \code{x}. Either this 
#' or \code{outregid} needs to be supplied.
#' @param outregid Integer vector with HYPE output region IDs, alternative to \code{subid}.
#' @param hype.var Character string, four-letter keyword to specify HYPE variable ID, see 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variable}. 
#' Not case-sensitive.
# #' @param ... Ignored.
#' 
#' @details
#' S3 class constructor function for array objects which can hold (multiple) HYPE time or map output results. 
#' 
#' @return
#' Returns a 3-dimensional array with 
#' \code{[time, subid, iteration]} dimensions and additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{subid}}{A vector of SUBIDs. Corresponds to 2nd array dimension (\code{NA}, if it does not apply to data contents).}
#' \item{\strong{outregid}}{A vector of OUTREGIDs. Corresponds to 2nd array dimension (\code{NA}, if it does not apply to data contents).}
#' \item{\strong{variable}}{HYPE output variable ID.}
#' }
#' 
#' @examples
#' \dontrun{HypeSingleVar(mytimeoutput, date = mydates, subid = c(23, 45, 56), hype.var = "cctn")}
#' 
#' @export

HypeSingleVar <- function(x, date, subid = NULL, outregid = NULL, hype.var) {
  
  # ID argument checks
  if ((!is.null(subid) && !is.numeric(subid)) || (!is.null(outregid) && !is.numeric(outregid))) {
    stop("'subid'/'outregid' must be an integer ID.")
  }
  if (is.null(subid) && is.null(outregid)) {
    stop("One of 'subid' or 'outregid' must be provided in function arguments.")
  }
  if (!is.null(subid) && !is.null(outregid)) {
    outregid <- NULL
    warning("Function arguments 'subid' and 'outregid' supplied. 'subid' takes precedence.")
  }
  
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
    if (!is.null(subid) && length(subid) != dim(x)[2]) {
      stop("Different lengths of argument 'subid' and corresponding dimension of 'x'.")
    }
    if (!is.null(outregid) && length(outregid) != dim(x)[2]) {
      stop("Different lengths of argument 'outregid' and corresponding dimension of 'x'.")
    }
    class(x) <- c("HypeSingleVar", "array")
    attr(x, "date") <- date
    attr(x, "subid") <- if (is.null(subid)) NA else subid
    attr(x, "outregid") <- if (is.null(outregid)) NA else outregid
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
# #' @rdname HypeSingleVar
summary.HypeSingleVar <- function(object, ...) {
  vari <- attr(object, "variable")
  dat <- attr(object, "date")
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
  ans$nsbd <- length(attr(object, "subid"))
  ans$niter <- dim(object)[3]
  class(ans) <- "summaryHypeSingleVar"
  print(ans)
  invisible(ans)
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



