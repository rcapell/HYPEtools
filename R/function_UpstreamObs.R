
#' @export
#' @useDynLib RHYPE nrows
#' @title
#' Upstream forcing data averages
#'
#' @description
#' Calculate average upstream forcing for a single SUBID
#'
#' @param filename Path to and file name of the forcing data text file to extract from. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param subid
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory argument.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument.
#' 
#' @details
#' \code{MergeXobs} handles time steps of different lengths (e.g. daily, hourly), but requires identical time 
#' step lengths from both input data frames.
#' 
#' @return
#' \code{MergeXobs} returns a data frame with attributes for Xobs data.
#' 
#' 
#' @examples
#' \dontrun{MergeXobs(x = myxobs1, y = myxobs2)}

UpstreamObs <- function(filename, subid, gd, bd) {
  nrows <- 0
  try(.Fortran(nrows, funit = as.integer(10), infile = as.character(filename), n=as.integer(nrows)))
  return(nrows)
}