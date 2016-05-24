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
#' Constructor function for data frames which hold equidistant time series of multiple HYPE variables for a single sub-basin, 
#' typically imported HYPE basin output results.
#' 
#' @param x numeric \code{\link{data.frame}} with \code{\link{POSIXct}} formatted time steps in the first, and \code{\link{numeric}} 
#' variables in the remaining columns.
#' @param variable Character vector of four-letter keywords to specify HYPE variable IDs, corresponding to second to 
#' last column in \code{x}. Defaults to column names for second to last column.
#' @param subid Integer with HYPE sub-basin ID, corresponding to second to last column in \code{x}.
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

HypeMultiVar <- function(x, variable = NULL, subid, hype.var) {
  
  # check if data is conform to requirements
  if (is.data.frame(x)) {
    
    # check if first column is POSIXct
    if (!inherits(x[, 1], "POSIXct")) {
      stop("Column 1 in 'x' is not of type 'POSIXct'.")
    }
    # check if first column contains NAs
    if (any(is.na(x[, 1]))) {
      stop("Empty values in column 1.")
    }
    # check if time steps are equidistant
    tstep <- diff(x[, 1])
    if (min(tstep) != max(tstep)) {
      stop("Non-equidistant time steps in 'x'.")
    }
    # check if time steps are at least daily
    tunits <- attr(tstep, "units")
    if (tunits == "days" && tstep[1] > 1) {
      stop("Longer-than-daily time steps not allowed in HypeXobs objects.")
    }
    
    # check attribute length conformities
    if (length(comment) != 1) {
      comment <- comment[1]
      warning("Length of argument 'comment' > 1. Only first element used.")
    }
    if (length(variable) != (ncol(x) - 1)) {
      stop("Lengths of argument 'variable' and number of data columns in 'x' differ.")
    }
    if (length(subid) != (ncol(x) - 1)) {
      stop("Lengths of argument 'subid' and number of data columns in 'x' differ.")
    }
    class(x) <- c("HypeXobs", "data.frame")
    attr(x, "comment") <- comment
    attr(x, "variable") <- toupper(variable)
    attr(x, "subid") <- subid
    if (tunits == "days") {
      attr(x, "timestep") <- "day"
    } else {
      attr(x, "timestep") <- paste(tstep[1], tunits)
    }
    
    # update header, composite of variable and subid
    names(x) <- c("date", paste(attr(x, "variable"), attr(x, "subid"), sep = "_"))
    
    return(x)
  } else {
    stop("Non-data frame input.")
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

