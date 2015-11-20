#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# S3 class for imported Xobs files

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Constructor function

#' @export
#' @title
#' HypeXobs data frames
#' 
#' @description
#' Constructor function for data frames which hold HYPE time series of a multiple observation variables for multiple
#' sub-basins and POSIXct time steps in the first column, i.e. Xobs.txt files.
#' 
#' @param x \code{\link{data.frame}} with \code{\link{POSIXct}} formatted time steps in the first, and \code{\link{numeric}} 
#' variables in the remaining columns.
#' @param comment Character string, metadata or other information, first line of a HYPE Xobs.txt file.
#' @param variable Character vector of four-letter keywords to specify HYPE variable IDs, corresponding to second to 
#' last column in \code{x}.
#' @param date \code{\link{POSIXct}} date-time vector of the same length as number of rows in \code{x} 
#' with time steps (starting day for time steps from weekly to annual).
#' @param subid Integer vector with HYPE sub-basin IDs, corresponding to second to last column in \code{x}.
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
#' \item{\strong{comment}}{A character string.}
#' \item{\strong{variable}}{A character vector of HYPE variable IDs.}
#' \item{\strong{subid}}{A vector of SUBIDs.}
#' }
#' 
#' @examples
#' \dontrun{HypeXobs(mydata, comment = "Water quality data", variable = c("cctn", "cctp", "cctp"), subid = c(23, 45, 56))}

HypeXobs <- function(x, comment, variable, subid) {
  
  # check if data is conform to requirements
  if (data.frame(x)) {
    
    # check if first column is POSIXct
    if (!inherits(x[, 1], "POSIXct")) {
      stop("Column 1 in 'x' is not of type 'POSIXct'.")
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
    attr(x, "variable") <- toupper(hype.var)
    attr(x, "subid") <- subid
    
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
  # attribute indexing, conditional on indexing specification
  if (is.numeric(j)){
    attr(y, "variable") <- attr(x, "variable")[(j - 1)[-1]]
    attr(y, "subid") <- attr(x, "subid")[(j - 1)[-1]]
    if (j[1] != 1) {
      warning("Date column removed or moved from first column, class 'HypeXobs' lost, other attributes preserved.")
      class(y) <- class(y)[-1]
    }
  } else if(is.logical(j)) {
    attr(y, "variable") <- attr(x, "variable")[(j)[-1]]
    attr(y, "subid") <- attr(x, "subid")[(j)[-1]]
    if (!j[1]) {
      warning("Date column removed, class 'HypeXobs' lost, other attributes preserved.")
      class(y) <- class(y)[-1]
    }
  } else {
    warning("Indexing of attributes 'subid' and 'variable' only defined for integer and logical element indices. 
            Attributes and class 'HypeXobs' lost.")
    class(y) <- class(y)[-1]
  }
  return(y)
}

