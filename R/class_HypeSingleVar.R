

#--------------------------------------------------------------------------------------------------------------------------------------
#   S3 class HypeSingleVar, herein:
#--------------------------------------------------------------------------------------------------------------------------------------
#
#     - HypeSingleVar (constructor function)
#     - [.HypeSingleVar (indexing method)
#     - summaryHypeSingleVar (constructor function for internal class used for summary print method)
#     - summary method
#     - print method for summary list
#     - 
#
#--------------------------------------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------------------------------------
# Constructor function
#--------------------------------------------------------------------------------------------------------------------------------------

#' HypeSingleVar arrays
#' 
#' Constructor function for arrays which hold equidistant time series of a single HYPE variable for multiple sub-basins 
#' and multiple model runs, typically imported time and map output results.
#' 
#' @param x numeric \code{\link{array}} with three dimensions, which holds HYPE results for one variable as (in order)
#' \code{[datetime, subid/outregid, iteration]}.
#' @param datetime \code{\link{POSIXct}} date-time vector of the same length as \code{time} dimension of \code{x} 
#' with equidistant time steps (starting day for time steps from weekly to annual), or character string for full model 
#' period averages, e.g. \code{"2000-2010"}.
#' @param subid Integer vector with HYPE sub-basin IDs, of the same length as \code{subid} dimension of \code{x}. Either this 
#' or \code{outregid} must be supplied.
#' @param outregid Integer vector with HYPE output region IDs, alternative to \code{subid}.
#' @param hype.var Character string, keyword to specify HYPE variable ID, see 
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
#' \item{\strong{datetime}}{A vector of date-times. Corresponds to 1st array dimension.}
#' \item{\strong{subid}}{A vector of SUBIDs. Corresponds to 2nd array dimension (\code{NA}, if it does not apply to data contents).}
#' \item{\strong{outregid}}{A vector of OUTREGIDs. Corresponds to 2nd array dimension (\code{NA}, if it does not apply to data contents).}
#' \item{\strong{variable}}{HYPE output variable ID.}
#' \item{\strong{timestep}}{A character keyword for the time step.}
#' }
#' 
#' @examples
#' # Import a time output file
#' te1 <- ReadTimeOutput(filename = system.file("demo_model", "results",
#' "timeCOUT.txt", package = "HYPEtools"), dt.format = "%Y-%m")
#' # Create a dummy array with two iterations from imported time file
#' te2 <- array(data = c(unlist(te1[, -1]),  unlist(te1[, -1])), 
#'              dim = c(nrow(te1), ncol(te1) - 1, 2), 
#'              dimnames = list(rownames(te1), colnames(te1)[-1]))
#' # Construct HypeSingleVar array
#' HypeSingleVar(x = te2, datetime = te1$DATE,
#' subid = subid(te1), hype.var = variable(te1))
#' 
#' @export

HypeSingleVar <- function(x, datetime, subid = NULL, outregid = NULL, hype.var) {
  
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
    if (!is.null(subid) && length(subid) != dim(x)[2]) {
      stop("Different lengths of argument 'subid' and corresponding dimension of 'x'.")
    }
    if (!is.null(outregid) && length(outregid) != dim(x)[2]) {
      stop("Different lengths of argument 'outregid' and corresponding dimension of 'x'.")
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
    
    # add class names and other attributes
    class(x) <- c("HypeSingleVar", "array")
    attr(x, "datetime") <- datetime
    subid(x) <- if (length(subid) == 0) NA else subid
    outregid(x) <- if (length(outregid) == 0) NA else outregid
    variable(x) <- toupper(hype.var)
    timestep(x) <- tstep
    
    # return S3 object 'HypeSingleVar'
    return(x)
    
  } else {
    stop("Non-array input.")
  }
}




#--------------------------------------------------------------------------------------------------------------------------------------
# Indexing method
#--------------------------------------------------------------------------------------------------------------------------------------

#' @export

`[.HypeSingleVar` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], ...) {
  y <- NextMethod("[", drop = FALSE)
  print(names(attributes(y)))
  # add class names and other attributes, sub-set them where needed
  attributes(y) <- c(attributes(y), 
                     class = c("HypeSingleVar", "array"), 
                     datetime = attr(x, "datetime")[i], 
                     subid = if (length(subid(x)) == 1 && is.na(subid(x))) NA else subid(x)[j], 
                     outregid = if (length(outregid(x)) == 1 && is.na(outregid(x))) NA else outregid(x)[j], 
                     variable = variable(x), 
                     timestep = timestep(x))
  
  y
  
}



#--------------------------------------------------------------------------------------------------------------------------------------
# summaryHypeSingleVar constructor (internal access only), used for printing summaries
#--------------------------------------------------------------------------------------------------------------------------------------

summaryHypeSingleVar <- function(x) {
  
  # expects a list with elements sdate, edate, period, nsbd, and niter
  class(x) <- "summaryHypeSingleVar"
  
  x
}


#--------------------------------------------------------------------------------------------------------------------------------------
# Summary method
#--------------------------------------------------------------------------------------------------------------------------------------

#' @method summary HypeSingleVar
#' @export

summary.HypeSingleVar <- function(object, ...) {
  vari <- attr(object, "variable")
  dat <- attr(object, "datetime")
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
  ans$nsbd <- if (length(subid(object) == 1) && is.na(subid(object))) length(attr(object, "outregid")) else length(subid(object))
  ans$niter <- dim(object)[3]
  ans <- summaryHypeSingleVar(x = ans)
  print(ans)
  invisible(ans)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# print method for summary list object
#' @export
print.summaryHypeSingleVar <- function(x, ...) {
  nm <- c("HYPE variable:\t\t\t\t", "Simulation time series length:\t\t", "Simulation start:\t\t\t", 
          "Simulation end:\t\t\t\t", "Simulation time period:\t\t\t", "Number of SUBIDs in simulation:\t\t", 
          "Number of iterations in simulation:\t")
  cat("\n")
  for (i in 1:length(x)) {
    if (!is.na(x[[i]])) {
      cat(paste(nm[i], x[[i]], "\n"))
    }
  }
}



