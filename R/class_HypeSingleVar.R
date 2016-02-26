
#' @export
#' @title
#' HypeSingleVar arrays
#' 
#' @description
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
#' Returns a 3-dimensional array with additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{subid}}{A vector of SUBIDs. Corresponds to 2nd array dimension.}
#' \item{\strong{variable}}{HYPE output variable ID.}
#' }
#' 
#' @examples
#' \dontrun{HypeSingleVar(mytimeoutput, date = mydates, subid = c(23, 45, 56), hype.var = "cctn")}

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
