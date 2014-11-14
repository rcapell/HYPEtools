
# #' @export
#' @useDynLib RHYPE count_rows count_data_cols count_datestring_len wmean
#' @title
#' Upstream forcing data averages
#'
#' @description
#' Calculate average upstream forcing time series for a single SUBID from area-weighted forcing in upstream subcatchments.
#'
#' @param filename Path to and file name of the forcing data text file to extract from. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param subid
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory argument.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument. 
#' If provided, upstream areas will include areas linked through bifurcations.
#' @param nr.obs Integer, number of rows in forcing data file. Optional argument, computed if \code{NULL}, which can be costly 
#' depending on file size. \code{UpstreamObs} prints a message
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

UpstreamObs <- function(filename, subid, gd, bd = NULL, nr.obs = NULL, verbose = T) {
  
  # number of characters in file path, needed for dimensioning of fortran variables in called subroutines
  in.len <- nchar(filename)
  
  # find upstream subids
  sbd <- AllUpstreamSubids(subid = subid, gd = gd, bd = bd)
  
  # identify area column position in geodata
  geocol.are <- which(tolower(colnames(gd)) == "area")
  geocol.sub <- which(tolower(colnames(gd)) == "subid")
  
  # get areas for all
  area <- gd[gd[, geocol.sub] %in% sbd, geocol.are]
  
  # calculate weights from areas
  wght <- area / sum(area)
  
  # conditional on function argument: calculate number of rows in obs file
  if (is.null(nr.obs)) {
    nr <- 1
    te <- tryCatch(
      .Fortran("count_rows", funit = as.integer(10), infile = as.character(filename), infile_len = as.integer(in.len), n = as.integer(nr)), 
      error = function(e) {print("Error when calling Fortran subroutine 'nrows'.")}
    )
    nr.obs <- te$n
  }
  
  message(paste("Number of rows in forcing data file:", nr.obs))
  
  
  # calculate number of columns in obs file
  n <- 1
  nc <- 1
  te <- tryCatch(
    .Fortran("count_data_cols", funit = as.integer(10), infile = as.character(filename), infile_len = as.integer(in.len), ncols = as.integer(nc), n_Result = as.integer(n)), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_data_cols'.")}
  )
  nc.obs <- te$ncols
  
  message(paste("Number of columns in forcing data file:", nc.obs))
  
  
  # calculate datestring format
  ds <- 1
  ts <- 1
  te <- tryCatch(
    .Fortran("count_datestring_len", funit = as.integer(10), infile = as.character(filename), infile_len = as.integer(in.len), dslen = as.integer(ds), tslen = as.integer(ts)), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_datestring_len'.")}
  )
  ds.len <- te$dslen
  ts.len <- te$tslen
  
  message(paste("Date string length:", ds.len))
  message(paste("Time string length:", ts.len))
  
  
  # calculate area-weighted forcing data mean
  nsbd <- length(sbd)
  # initialise result vectors, length does not include header of obs file, therefore "- 1"
  dt <- rep(paste(rep(" ", ds.len), collapse = ""), times = nr.obs - 1)
  tm <- rep(paste(rep(" ", ts.len), collapse = ""), times = nr.obs - 1)
  wm <- rep(0., times = nr.obs - 1)
#   te <- tryCatch(
#     .Fortran("wmean", funit = as.integer(10), infile = as.character(filename), infile_len = as.integer(in.len), subid = as.integer(sbd), weight = as.integer(wght), m = as.integer(nsbd), nc = as.integer(nc.obs), nr = as.integer(nr.obs), dslen = as.integer(ds.len), tslen = as.integer(ts.len), date = as.character(dt), time = as.character(tm), res = as.numeric(res)), 
#     error = function(e) {print("Error when calling Fortran subroutine 'wmean'.")}
#   )
#   te <- tryCatch(
#     .Fortran("wmean", funit = as.integer(10), infile = as.character(filename), infile_len = as.integer(in.len), subid = as.integer(sbd), weight = as.numeric(wght), m = as.integer(nsbd), nc = as.integer(nc.obs), nr = as.integer(nr.obs), dslen = as.integer(ds.len), tslen = as.integer(ts.len), res = as.numeric(wm)), 
#     error = function(e) {print("Error when calling Fortran subroutine 'wmean'.")}
#   )
te <- .Fortran("wmean", 
               funit = as.integer(10), 
               infile = as.character(filename), 
               infile_len = as.integer(in.len), 
               subid = as.integer(sbd), 
               weight = as.numeric(wght), 
               m = as.integer(nsbd), 
               nc = as.integer(nc.obs), 
               nr = as.integer(nr.obs), 
               dslen = as.integer(ds.len), 
               tslen = as.integer(ts.len), 
               #date = as.character(dt), 
               #time = as.character(tm), 
               res = as.numeric(wm)
               )
  return(data.fte$res)
}
