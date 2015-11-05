#' @export
#' 
#' @import pbapply
#' 
#' @title
#' Calculate grouped sums for SLC classes in a GeoData file
#'
#' @description
#' \code{GroupSLCClasses} calculates grouped sums for SLC classes (area fractions or absolute areas) based on land use, soil, or crop groups in a GeoClass 
#' table, or any other user-provided grouping index. 
#' 
#' @param gd Data frame containing columns with SUBIDs, SLC fractions, and SUBID areas if \code{abs.area = TRUE}. Typically a 'GeoData.txt' file 
#' imported with \code{\link{ReadGeoData}}.
#' 
#' @param gc Data frame containing columns with SLCs and corresponding landuse and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}. Must be provided if no \code{group} argument is given.
#' 
#' @param type Keyword character string for use with \code{gc}. Type of grouping index, either \code{"landuse"}, \code{"soil"}, or \code{"crop"}, 
#' can be abbreviated.
#' 
#' @param group Integer vector, of same length as number of SLC classes in \code{gd}. Alternative grouping index specification to \code{gc} + \code{type}.
#' 
#' @param abs.area Logical, if \code{TRUE}, absolute areas will be calculated for eatch group, rather than area fractions.
#' 
#' @param verbose Logical, if \code{TRUE} information and progress bar will be printed.
#' 
#' @details
#' If absolute areas are calculated, area units will correspond to areas provided in \code{gd}.
#' 
#' @return
#' \code{GroupSLClasses} returns the data frame with SUBIDs, SUBID areas, and grouped SLC class columns. 
#' 
#' @examples
#' \dontrun{
#' GroupSLCClasses(gd = mygeodata, gc = mygeoclass, type = "s")
#' }


GroupSLCClasses <- function(gd, gc = NULL, type = "landuse", group = NULL, abs.area = FALSE, verbose = T) {
  
  # input checks
  if (is.null(gc) && is.null(group)) {
    stop("Neither GeoClass table nor user-defined grouping index provided.")
  }
  if (!is.null(gc) && !is.null(group)) {
    stop("Both GeoClass table and user-defined grouping index provided. Please provide just one of them.")
  }
  if (!any(type == "landuse", type == "l", type == "soil", type == "s", type == "crop", type == "c")) {
    stop("'type' keyword unknown.")
  }
  
  # local grouping index object, depending on input arguments
  if (!is.null(gc)) {
    if (type == "landuse" || type == "l") {
      lgroup <- gc[, 2]
      grname <- "landuse"
    }
    if (type == "soil" || type == "s") {
      lgroup <- gc[, 3]
      grname <- "soil"
    }
    if (type == "crop" || type == "c") {
      lgroup <- gc[, 4]
      grname <- "crop"
    }
  } else {
    lgroup <- group
    grname <- "group"
  }
  
  # columns with SLCs in GeoData
  gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  
  # extract slc class area fractions or absolute slc areas as working data frame
  if (abs.area) {
    # force conversion of areas in gd to numeric, to prevent integer overflow errors
    area <- as.numeric(gd[, which(toupper(names(gd)) == "AREA")])
    # calculate absolute areas from fractions and area sums provided in gd
    if (verbose) {
      cat("Calculating absolute areas.")
      slc <- pbapply(gd[, gdcols.slc], 2, function(x, y) {x * y}, y = area)
    } else {
      slc <- apply(gd[, gdcols.slc], 2, function(x, y) {x * y}, y = area)
    }
    # convert to matrix if just one row in gd, would be a vector otherwise
    if (nrow(gd) == 1) {
      slc <- t(slc)
    }
  } else {
    slc <- gd[, gdcols.slc]
  }
  
  # number of slc classes in GeoData
  nslc <- ncol(slc)
  
  # error check: number of SLCs in grouping index and gd must be identical
  if (nslc != length(lgroup)) {
    stop("Number of SLCs in 'GeoData'gd' and number of elements in grouping index do not match.")
  }
  
  # print to screen if verbose
  if (verbose) {
    cat(paste("\nNumber of SLC classes in 'gd':", nslc, "\n"))
  }
  
  # extract areas from gd if absolute areas are to be calculated
  if (verbose) {
    cat("\nCalculating grouped SLC sums.\n")
    res <- pbapply(slc, MARGIN = 1, FUN = tapply, INDEX = lgroup, sum)
  } else {
    res <- apply(slc, MARGIN = 1, FUN = tapply, INDEX = lgroup, sum)
  }
  # formatting: transpose result and convert to dataframe
  res <- as.data.frame(t(res))
  res <- cbind(gd[, which(toupper(names(gd)) == "SUBID")], gd[, which(toupper(names(gd)) == "AREA")], res)
  names(res) <- c("SUBID", "AREA", paste(grname, names(res)[-c(1:2)], sep = "_"))
  
  # return results
  return(res)
}
