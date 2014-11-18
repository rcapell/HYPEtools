
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
#' \code{UpstreamObs} reads potentially very large text files, function calls can be time-consuming
#' 
#' @return
#' \code{MergeXobs} returns a data frame with attributes for Xobs data.
#' 
#' 
#' @examples
#' \dontrun{MergeXobs(x = myxobs1, y = myxobs2)}

UpstreamObs <- function(filename, subid, gd, bd = NULL, nr.obs = NULL, verbose = T) {
  
  starttime <- Sys.time()
  message(paste("UpstreamObs start at", starttime))
  
  # number of characters in file path, needed for dimensioning of fortran variables in called subroutines
  infile_len <- nchar(filename)
  
  # find upstream subids
  sbd <- AllUpstreamSubids(subid = subid, gd = gd, bd = bd)
  
  # identify area column position in geodata
  geocol.are <- which(tolower(colnames(gd)) == "area")
  geocol.sbd <- which(tolower(colnames(gd)) == "subid")
  
  # get areas for all
  area <- gd[gd[, geocol.sbd] %in% sbd, geocol.are]
  
  # calculate weights from areas
  weight <- area / sum(area)
  
  preptime <- Sys.time()
  message(paste("Time for preparing steps", difftime(preptime, starttime, units = "secs")))
  
  # conditional on function argument: calculate number of rows in obs file
  if (is.null(nr.obs)) {
    nr <- 0
    te <- tryCatch(
      .Fortran("count_rows", 
               infile = as.character(filename), 
               infile_len = as.integer(infile_len), 
               nr = as.integer(nr)
               ), 
      error = function(e) {print("Error when calling Fortran subroutine 'nrows'.")}
    )
    nr <- te$nr
  }
  
  rowtime <- Sys.time()
  message(paste("Time for computing rows", difftime(rowtime, preptime, units = "secs")))
  
  message(paste("Number of rows in forcing data file:", nr.obs))
  
  
  # calculate number of columns in obs file
  ncols <- 1
  te <- tryCatch(
    .Fortran("count_data_cols", 
             infile = as.character(filename), 
             infile_len = as.integer(in.len), 
             ncols = as.integer(ncols)
             ), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_data_cols'.")}
  )
  ncols <- te$ncols
  
  coltime <- Sys.time()
  message(paste("Time for computing columns", difftime(coltime, rowtime, units = "secs")))
  
  message(paste("Number of columns in forcing data file:", nc.obs))
  
  
  # calculate datestring format
  dslen <- 0
  tslen <- 0
  te <- tryCatch(
    .Fortran("count_datestring_len", 
             infile = as.character(filename), 
             infile_len = as.integer(infile_len), 
             dslen = as.integer(dslen), 
             tslen = as.integer(tslen)
             ), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_datestring_len'.")}
  )
  dslen <- te$dslen
  tslen <- te$tslen
  
  dattime <- Sys.time()
  message(paste("Time for computing datetime lengths", difftime(dattime, coltime, units = "secs")))
  
  message(paste("Date string length:", dslen))
  message(paste("Time string length:", tslen))
  
  
  ## calculate area-weighted forcing data mean
  # number of subids of interest
  m <- length(sbd)
  # initialise result vector, length does not include header of obs file, therefore "- 1"
  res <- rep(0, times = nr - 1)
  out <- tryCatch(
    .Fortran("wmean", 
             infile = as.character(filename), 
             infile_len = as.integer(infile_len), 
             sbd = as.integer(sbd), 
             weight = as.numeric(weight), 
             m = as.integer(m), 
             nc = as.integer(ncols), 
             nr = as.integer(nr), 
             tslen = as.integer(tslen), 
             res = as.numeric(res)
             ), 
    error = function(e) {print("Error when calling Fortran subroutine 'wmean'.")}
  )
  
  caltime <- Sys.time()
  message(paste("Time for computing weighted mean", difftime(caltime, dattime, units = "secs")))

  # read dates in first rows and construct date-time vector
  if (dslen == 10 && tslen == 0) {
    dt.fmt <- "%Y-%m-%d"
  } else if (dslen == 10 && tslen == 5) {
    dt.fmt <- "%Y-%m-%d %H:%M"
  } else if(dslen == 8 && tslen == 0) {
    dt.fmt <- "%Y%m%d"
  } else if(dslen == 8 && tslen == 5) {
    dt.fmt <- "%Y%m%d %H:%M"
  }
  
  cn <- file(description = filename, open = "r")
  te1 <- readLines(con=cn, n=1)
  te1 <- readLines(con=cn, n=1)
  te2 <- readLines(con=cn, n=1)
  close(cn)
  te1 <- as.POSIXct(strptime(substr(te1, start = 1, stop = ifelse(tslen > 0, dslen + tslen + 1, dslen)), dt.fmt, tz = "GMT"))
  te2 <- as.POSIXct(strptime(substr(te2, start = 1, stop = ifelse(tslen > 0, dslen + tslen + 1, dslen)), dt.fmt, tz = "GMT"))
  # calculate date vector of length of obs file (excluding the header)
  te <- 0:(nr - 2)
  dt <- difftime(te2, te1)
  date <- te1 + te * dt
  return(data.frame(DATE = date, meanobs = out$res))
}
