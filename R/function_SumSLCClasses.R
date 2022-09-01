#' Calculate sums of SLC classes in a GeoData file
#'
#' \code{SumSLCClasses} sums all SLC classes for each SUBID in a GeoData data frame and optionally plots the results.
#' 
#' @param gd Data frame containing columns with SLC fractions, typically a 'GeoData.txt' file imported with \code{\link{ReadGeoData}}.
#' @param plot.box Logical, if \code{TRUE}, a box plot of SLC area sums is returned.
#' @param silent Logical, if set to \code{TRUE}, the default printing of a result summary is suppressed.
#' @param ... Other arguments to be passed to \code{\link{boxplot}}.
#' 
#' @details
#' \code{SumSLCClasses} is a wrapper for \code{\link{colSums}} with a boxplot output option, and allows to quickly control if SLCs of all SUBIDs in a
#' GeoData data frame sum up to 1.
#' 
#' @return
#' \code{SumSLCClasses} returns a vector of SLC sums, invisibly if \code{plot.box} is \code{TRUE}. 
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' SumSLCClasses(gd = te, plot.box = TRUE)
#' SumSLCClasses(gd = te, plot.box = FALSE)
#' 
#' 
#' @importFrom graphics boxplot
#' @export


SumSLCClasses <- function(gd, plot.box = TRUE, silent = FALSE, ...) {
  
  # identify SLC columns in gd
  gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  # stop if none were found
  if (length(gdcols.slc) == 0) {
    stop("No SLC classes found in 'gd'. Exiting.")
  }
  
  
  # calculate row sums
  res <- rowSums(gd[, gdcols.slc])
  
  
  # print number of SLC classes and range
  if (!silent) {
    cat(paste("\nNumber of SLCs in 'gd':", length(gdcols.slc), "\n"))
    cat(paste("\nSLC sums range from", min(res), "to", max(res), "\n"))
  }
  
  
  # plot results if desired, otherwise return res
  if (plot.box) {
    boxplot(res, ...)
    invisible(res)
  } else {
    return(res)
  }
}
