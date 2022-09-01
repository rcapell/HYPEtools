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
#' \code{[datetime, variable, iteration]}.
#' @param datetime \code{\link{POSIXct}} date-time vector of the same length as \code{time} dimension of \code{x} 
#' with equidistant time steps (starting day for time steps from weekly to annual), or character string for full model 
#' period averages, e.g. \code{"2000-2010"}.
#' @param hype.var,hype.unit Character vectors of keywords to specify HYPE variable IDs, corresponding to second dimension 
#' (columns) in \code{x}. See 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variables}
#' @param subid Integer, HYPE sub-basin ID. Either this or \code{outregid} needs to be supplied.
#' @param outregid Integer, HYPE output region ID, alternative to \code{subid}.
#' @param hype.comment Character, first-row optional comment string of basin output file. Empty string, if non-existing.
#' 
#' @details
#' S3 class constructor function for array objects which can hold (multiple) HYPE basin output results. 
#' 
#' @return
#' Returns a 3-dimensional array with 
#' \code{[time, variable, iteration]} dimensions and additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{datetime}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{variable}}{A character vector of HYPE output variable IDs.}
#' \item{\strong{hypeunit}}{A character vector of HYPE output variable units.}
#' \item{\strong{subid}}{A single SUBID.}
#' \item{\strong{outregid}}{A single OUTREGID.}
#' \item{\strong{timestep}}{A character keyword for the time step.}
#' \item{\strong{comment}}{A comment string, currently used for class group outputs.}
#' }
#' 
#' @examples
#' # import a basin output file
#' te1 <- ReadBasinOutput(filename = system.file("demo_model",
#' "results", "0003587.txt", package = "HYPEtools"))
#' # create a dummy array with two iterations from imported basin file
#' te2 <- array(data = c(unlist(te1[, -1]),  unlist(te1[, -1])), 
#'              dim = c(nrow(te1), ncol(te1) - 1, 2), 
#'              dimnames = list(rownames(te1), colnames(te1)[-1]))
#' # Construct HypeMultiVar array
#' HypeMultiVar(te2, datetime = te1$DATE, hype.var = variable(te1),
#' hype.unit = hypeunit(te1), subid = 3587)
#' 
#' @export

HypeMultiVar <- function(x, datetime, hype.var, hype.unit, subid = NULL, outregid = NULL, hype.comment = "") {
  
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
    if (length(datetime) != dim(x)[1]) {
      stop("Different lengths of argument 'datetime' and corresponding dimension of 'x'.")
    }
    if (length(hype.var) != dim(x)[2]) {
      stop("Different lengths of argument 'hype.var' and corresponding dimension of 'x'.")
    }
    
    # conditional: timestep attribute identified by difference between first two entries in date
    tdff <- as.numeric(difftime(datetime[2], datetime[1], units = "hours"))
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
    attr(x, "datetime") <- datetime
    attr(x, "variable") <- toupper(hype.var)
    attr(x, "hypeunit") <- toupper(hype.unit)
    attr(x, "subid") <- if (is.null(subid)) NA else subid
    attr(x, "outregid") <- if (is.null(outregid)) NA else outregid
    attr(x, "timestep") <- tstep
    comment(x) <- hype.comment
    return(x)
    
  } else {
    stop("Non-array input.")
  }
    
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# indexing method
#' @export
`[.HypeMultiVar` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], ...) {
  y <- NextMethod("[", drop = FALSE)
  attr(y, "date") <- attr(x, "date")[i]
  attr(y, "variable") <- attr(x, "variable")[j]
  attr(y, "hypeunit") <- attr(x, "hypeunit")[j]
  attr(y, "subid") <- attr(x, "subid")
  attr(y, "outregid") <- attr(x, "outregid")
  attr(y, "timestep") <- attr(x, "timestep")
  class(y) <- c("HypeMultiVar", "array")
  return(y)
}

