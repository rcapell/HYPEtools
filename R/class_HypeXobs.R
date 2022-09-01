#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# S3 class HypeXobs, herein:
#
#     - HypeXobs (constructor function)
#     - [.HypeXobs (indexing method)
#     - 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Constructor function

#' HypeXobs data frames
#' 
#' Constructor function for data frames which hold HYPE Xobs.txt file contents, i.e. time series of a multiple observation 
#' variables for multiple sub-basins and equidistant time steps in POSIXct format in the first column.
#' 
#' @param x \code{\link{data.frame}} with \code{\link{POSIXct}} formatted time steps in the first, and \code{\link{numeric}} 
#' variables in the remaining columns.
#' @param comment Character string, metadata or other information, first line of a HYPE Xobs.txt file.
#' @param variable Character vector of four-letter keywords to specify HYPE variable IDs, corresponding to second to 
#' last column in \code{x}.
#' @param subid Integer vector with HYPE sub-basin IDs, corresponding to second to last column in \code{x}.
#' @param verbose Logical, throw warning if attribute \code{timestep} cannot be computed.
#'  
#' Not case-sensitive.
#' 
#' @details
#' S3 class constructor function for \code{HypeXobs} data frame objects which hold HYPE Xobs.txt file contents. Xobs.txt 
#' files contain three header rows, see the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt description in the HYPE documentation}. 
#'  These headers are stored as additional attributes in  objects.
#' 
#' @return
#' Returns a data frame of class \code{HypeXobs} with additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{comment}}{A character vector.}
#' \item{\strong{variable}}{A character vector of HYPE variable IDs.}
#' \item{\strong{subid}}{A vector of SUBIDs.}
#' \item{\strong{timestep}}{Time step keyword, \code{"day"}, or \code{"n hour"} (n = number of hours). \code{NULL}, if \code{x} 
#' contains just one row.}
#' }
#' 
#' @examples
#' # Use the Xobs file import function instead of the class constructor for standard work flows
#' te <- ReadXobs(file = system.file("demo_model", "Xobs.txt", package = "HYPEtools"))
#' summary(te)
#' # Class constructor
#' HypeXobs(x = as.data.frame(te), comment = comment(te), variable = variable(te), subid = subid(te))
#' 
#' @export

HypeXobs <- function(x, comment, variable, subid, verbose = TRUE) {
  
  # check if data is conform to requirements
  if (is.data.frame(x)) {
    
    # check if first column is POSIXct
    if (!inherits(x[, 1], "POSIXct")) {
      stop("Column 1 in 'x' is not of type 'POSIXct'.")
    }
    # check if first column contains NAs
    if (any(is.na(x[, 1]))) {
      stop("Empty values in column 1 (time steps).")
    }
    # time step checks, skip and throw warning if just one row in dataframe
    if (nrow(x) > 1) {
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
    } else {
      tunits <- NULL
      if (verbose) {
        warning("Less than 2 rows in 'x'. Attribute 'timestep set to 'NULL'.")
        }
    }
    
    # check attribute length conformities
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
    if (!is.null(tunits)) {
      if (tunits == "days") {
        attr(x, "timestep") <- "day"
      } else {
        attr(x, "timestep") <- paste(tstep[1], tunits)
      }
    } else {
      attr(x, "timestep") <- NULL
    }
    
    # update header, composite of variable and subid
    names(x) <- c("date", paste(attr(x, "variable"), attr(x, "subid"), sep = "_"))
    
    return(x)
  } else {
    stop("Non-data frame input.")
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Indexing method, for integer and logical subsetting
#' @export
`[.HypeXobs` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], drop) {
  y <- NextMethod("[")
  
  attr(y, "comment") <- attr(x, "comment")
  attr(y, "timestep") <- attr(x, "timestep")
  
  # attribute indexing, conditional on indexing specification
  
  if (is.numeric(j)){
    
    # only active if more than one column in index, or if single index is negative
    if (length(j) > 1 || j < 0) {
      # column index adapted because attributes are one element shorter than the data frame (no datetime), 
      # conditional on negative or positive indexes
      # (negative and positive indices cannot be combined, so that the two choices are safe), but 0s are allowed and removed
      # prior to attribute indexing
      j.check <- if (length(which(j == 0)) == 0) {j} else {j[-which(j == 0)]}
      
      
      # HypeXobs objects require POSIX dates in first column, check that this requirement is met after indexing
      # Conditions: a 1 required as first value in a positive indexing vector OR no -1 allowed in a negative indexing vector
      if ((!(j.check[1] == 1) && any(j.check > 0))) {
        warning("Date column removed or moved from first column, class 'HypeXobs' dropped, other attributes preserved.")
        class(y) <- class(y)[-1]
      } else if (any(j.check[1] == -1)) {
        warning("Date column removed or moved from first column, class 'HypeXobs' dropped, other attributes preserved.")
        class(y) <- class(y)[-1]
      } else {
        if (all(j >= 0)) {
          j.attr <- (j.check - 1)[-1]
        }
        if (all(j <= 0)) {
          j.attr <- (j.check + 1)
        }
        attr(y, "variable") <- attr(x, "variable")[j.attr]
        attr(y, "subid") <- attr(x, "subid")[j.attr]
        
      }
    }
    
    
  } else if(is.logical(j)) {
    attr(y, "variable") <- attr(x, "variable")[(j)[-1]]
    attr(y, "subid") <- attr(x, "subid")[(j)[-1]]
    if (!j[1]) {
      warning("Date column removed, class 'HypeXobs' dropped, other attributes preserved.")
      class(y) <- class(y)[-1]
    }
  } else {
    warning("Indexing of attributes 'subid' and 'variable' only defined for integer and logical element indices. 
            Attributes and class 'HypeXobs' dropped.")
    class(y) <- class(y)[-1]
  }
  return(y)
}

