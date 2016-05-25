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
#' @param hype.var Character vector of four-letter keywords to specify HYPE variable IDs, corresponding to second dimension 
#' (columns) in \code{x}. See 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variables}
#' @param subid Integer with HYPE sub-basin ID. Not case-sensitive.
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
#' }
#' 
#' @examples
#' \dontrun{HypeMultiVar(mybasinoutput, date = mydates, hype.var = c("cctn", "ccin", ccon), , subid = 23}
#' 
#' @export

HypeMultiVar <- function(x, date, hype.var, subid) {
  
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
    class(x) <- c("HypeMultiVar", "array")
    attr(x, "date") <- date
    attr(x, "variable") <- toupper(hype.var)
    attr(x, "subid") <- subid
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
  attr(y, "subid") <- attr(x, "subid")
  attr(y, "date") <- attr(x, "date")[i]
  attr(y, "variable") <- attr(x, "variable")[j]
  class(y) <- c("HypeMultiVar", "array")
  return(y)
}

