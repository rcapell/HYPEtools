#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   S3 class HypeMultiVar, herein:
#
#     - HypeMultiVar (constructor function)
#     - [.HypeMultiVar (indexing method)
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' HypeMultiVar arrays
#' 
#' Constructor function for arrays which hold equidistant time series of multiple HYPE variables for a single sub-basin and 
#' multiple model runs, typically imported HYPE basin output results.
#' 
#' @param x numeric \code{\link{array}} with three dimensions, which holds HYPE results for one sub-basin as (in order)
#' \code{[date, variable, iteration]}.
#' @param date \code{\link{POSIXct}} date-time vector of the same length as \code{time} dimension of \code{x} 
#' with equidistant time steps (starting day for time steps from weekly to annual), or character string for full model 
#' period averages, e.g. \code{"2000-2010"}.
#' @param hype.var Character vector of keywords to specify HYPE variable IDs, corresponding to second dimension 
#' (columns) in \code{x}. See 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variables}
#' @param subid Integer, HYPE sub-basin ID. Either this or \code{outregid} needs to be supplied.
#' @param outregid Integer, HYPE output region ID, alternative to \code{subid}.
#' 
#' @details
#' S3 class constructor function for array objects which can hold (multiple) HYPE basin output results. 
#' 
#' @return
#' Returns a 3-dimensional array with 
#' \code{[time, variable, iteration]} dimensions and additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{variable}}{A character vector of HYPE output variable IDs.}
#' \item{\strong{subid}}{A single SUBID.}
#' \item{\strong{outregid}}{A single OUTREGID.}
#' \item{\strong{timestep}}{A character keyword for the time step.}
#' }
#' 
#' @examples
#' \dontrun{HypeMultiVar(mybasinoutput, date = mydates, hype.var = c("cctn", "ccin", "ccon"), , subid = 23, tstep = "day"}
#' 
#' @export

HypeMultiVar <- function(x, date, hype.var, subid = NULL, outregid = NULL) {
  
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
    if (length(hype.var) != dim(x)[2]) {
      stop("Different lengths of argument 'hype.var' and corresponding dimension of 'x'.")
    }
    
    # conditional: timestep attribute identified by difference between first two entries in date
    tdff <- as.numeric(difftime(date[2], date[1], units = "hours"))
    if (!is.na(tdff)) {
      if (tdff == 24) {
        tstep <- "day"
      } else if (tdff == 168) {
        tstep <- "week"
      } else if (tdff %in% c(744, 720, 696, 672)) {
        tstep <- "month"
      } else if (tdff %in% c(8760, 8784)) {
        tstep <- "year"
      } else {
        tstep <- paste(tdff, "hour", sep = "")
      }
    } else {
      # add timestep attribute with placeholder value
      tstep <- "none"
    }
    
    class(x) <- c("HypeMultiVar", "array")
    attr(x, "date") <- date
    attr(x, "variable") <- toupper(hype.var)
    attr(x, "subid") <- if (is.null(subid)) NA else subid
    attr(x, "outregid") <- if (is.null(outregid)) NA else outregid
    attr(x, "timestep") <- tstep
    return(x)
    
  } else {
    stop("Non-array input.")
  }
    
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# indexing method
#' @export
`[.HypeMultiVar` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], ...) {
  y <- NextMethod("[", drop = F)
  attr(y, "date") <- attr(x, "date")[i]
  attr(y, "variable") <- attr(x, "variable")[j]
  attr(y, "subid") <- attr(x, "subid")
  attr(y, "outregid") <- attr(x, "outregid")
  attr(y, "timestep") <- attr(x, "timestep")
  class(y) <- c("HypeMultiVar", "array")
  return(y)
}

