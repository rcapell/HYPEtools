#' @export
#' @title
#' Calculate sums of SLC classes in a GeoData file
#'
#' @description
#' \code{SumSLClasses} sums all SLC classes for each SUBID in a GeoData data frame and optionally plots the results.
#' 
#' @param gd Data frame containing columns with SLC fractions, typically a 'GeoData.txt' file imported with \code{\link{ReadGeoData}}.
#' 
#' @param plot.box Logical, if \code{TRUE}, a box plot of SLC area sums is returned.
#' 
#' @param ... Other arguments to be passed to \code{\link{boxplot}}.
#' 
#' 
#' 
#' @details
#' \code{SumSLClasses} is a wrapper for \code{\link{colSums}} with a boxplot output option, and allows to quickly control if SLCs of all SUBIDs in a
#' GeoData data frame sum up to 1.
#' 
#' 
#' @return
#' \code{SumSLClasses} a vector of SLC sums, invisibly if \code{plot.box} is \code{TRUE}. 
#' 
#' @examples
#' \dontrun{my.gd <- ReadGeoData("GeoData.txt")
#' SumSLClasses(gd = my.gd, plot.box = F)
#' }
#' 





SumSLClasses <- function(gd, plot.box = T, ...) {
  
  # identify SLC columns in gd
  gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  # stop if none were found
  if (length(gdcols.slc) == 0) {
    stop("No SLC classes found in 'gd'. Exiting.")
  }
  
  # print number of SLC classes
  cat(paste("\nNumber of SLCs in 'gd':", length(gdcols.slc), "\n"))
  
  # calculate row sums
  res <- rowSums(gd[, gdcols.slc])
  
  # print range
  cat(paste("\nSLC sums range from", min(res), "to", max(res), "\n"))
  
  # plot results if desired, otherwise return res
  if (plot.box) {
    boxplot(res, ...)
    invisible(res)
  } else {
    return(res)
  }
}
