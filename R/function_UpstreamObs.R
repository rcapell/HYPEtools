
#' @export
#' @useDynLib HYPEtools count_rows count_data_cols count_datestring_len wmean
#' @title
#' Upstream forcing data averages
#'
#' @description
#' Calculate average upstream forcing time series for a single SUBID from area-weighted upstream forcing data, given as HYPE forcing data 
#' text file.
#'
#' @param filename Path to and file name of the forcing data text file to extract from. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param subid Integer, giving a single SUBID for which upstream forcing data are to be calculated.
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory argument.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument. 
#' If provided, upstream areas will include areas linked through bifurcations.
#' @param nr.obs Integer, number of rows in forcing data file. Optional argument, computed if \code{NULL}, which can be costly 
#' depending on file size. If argument \code{verbose} is \code{TRUE}, \code{UpstreamObs} prints a message containing the number of rows.
#' @param dry.run Logical, if \code{TRUE}, only obs file dimensions will be computed. Set argument \code{verbose} to \code{TRUE} to
#' print them to standard output.
#' @param verbose Logical, if \code{TRUE}, information about obs file dimensions and the current computational status will be 
#' printed to the standard output during runtime.
#' 
#' @details
#' \code{UpstreamObs} reads from HYPE forcing data text files, primarily Pobs.txt and Tobs.txt. Depending on the model domain, these 
#' can be large (several GB). The function therefore does not import the files but uses external read and computation routines and only 
#' imports the averaged upstream results for the target catchment given in argument \code{subid}.
#' 
#' @note
#' \code{UpstreamObs} reads potentially very large text files, function calls can be time-consuming.
#' 
#' @return 
#' \code{UpstreamObs} returns a data frame with two columns, and one additional attribute \code{subid}, suitable for export using 
#' \code{\link{WritePTQobs}}. First column \code{DATE} contains POSIX dates or character strings if date-time conversion failed, 
#' conversion failure will produce a message to the standard output. The second column \code{meanobs} contains area-weighted upstream 
#' average forcing. 
#' 
#' @examples
#' \dontrun{UpstreamObs(filename = "Pobs.txt", subid = 1000, gd = mygeodata)}

UpstreamObs <- function(filename, subid, gd, bd = NULL, nr.obs = NULL, dry.run = FALSE, verbose = FALSE) {
  
  # print information
  if (verbose) {
    starttime <- Sys.time()
    message(paste("UpstreamObs start at", starttime))
  }
  
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
  
  # print information
  if (verbose) {
    preptime <- Sys.time()
    message(paste("Time for preparation steps", difftime(preptime, starttime, units = "secs")))
  }
  
  # conditional on function argument: calculate number of rows in obs file
  if (is.null(nr.obs)) {
    nr <- 0
    te <- tryCatch(
      .Fortran(count_rows, 
               infile = as.character(filename), 
               infile_len = as.integer(infile_len), 
               nr = as.integer(nr)
               ), 
      error = function(e) {print("Error when calling Fortran subroutine 'nrows'.")}
    )
    nr <- te$nr
  } else {
    nr <- nr.obs
  }
  
  # print information
  if (verbose) {
    rowtime <- Sys.time()
    message(paste("Time for computing rows", difftime(rowtime, preptime, units = "secs")))
    message(paste("Number of rows in forcing data file:", nr))
  }
  
  
  # calculate number of columns in obs file
  ncols <- 0
  te <- tryCatch(
    .Fortran(count_data_cols, 
             infile = as.character(filename), 
             infile_len = as.integer(infile_len), 
             ncols = as.integer(ncols)
             ), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_data_cols'.")}
  )
  ncols <- te$ncols
  
  # print information
  if (verbose) {
    coltime <- Sys.time()
    message(paste("Time for computing columns", difftime(coltime, rowtime, units = "secs")))
    message(paste("Number of columns in forcing data file:", ncols))
  }
  
  
  # calculate datestring format
  dslen <- 0
  tslen <- 0
  lclen <- 0
  te <- tryCatch(
    .Fortran(count_datestring_len, 
             infile = as.character(filename), 
             infile_len = as.integer(infile_len), 
             dslen = as.integer(dslen), 
             tslen = as.integer(tslen),
             lclen = as.integer(lclen)
             ), 
    error = function(e) {print("Error when calling Fortran subroutine 'count_datestring_len'.")}
  )
  dslen <- te$dslen
  tslen <- te$tslen
  lclen <- te$lclen
  
  # print information
  if (verbose) {
    dattime <- Sys.time()
    message(paste("Time for computing datetime lengths", difftime(dattime, coltime, units = "secs")))
    
    message(paste("Date string length:", dslen))
    message(paste("Time string length:", tslen))
    message(paste("Character count before date string:", lclen))
  }
  
  
  ## calculate area-weighted forcing data mean and output preparation
  ## conditional on argument dry.run
  if (!dry.run) {
    
    # number of subids of interest
    m <- length(sbd)
    # initialise result vector, length does not include header of obs file, therefore "- 1"
    res <- rep(0, times = nr - 1)
    out <- tryCatch(
      .Fortran(wmean, 
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
    
    # print information
    if (verbose) {
      caltime <- Sys.time()
      message(paste("Time for computing weighted mean", difftime(caltime, dattime, units = "secs")))
    }
    
    
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
    
    ## create date information from date entries in first two obs-file rows (hype input has regular time steps)
    # get information from obs-file
    cn <- file(description = filename, open = "r")
    te1 <- readLines(con=cn, n=1)
    te1 <- readLines(con=cn, n=1)
    te2 <- readLines(con=cn, n=1)
    close(cn)
    te1 <- substr(te1, start = 1 + lclen, stop = ifelse(tslen > 0, dslen + tslen + 1 + lclen, dslen + lclen))
    te2 <- substr(te2, start = 1 + lclen, stop = ifelse(tslen > 0, dslen + tslen + 1 + lclen, dslen + lclen))
    
    # attempt to convert to date-time values
    xd1 <- as.POSIXct(strptime(te1, dt.fmt, tz = "GMT"))
    xd2 <- as.POSIXct(strptime(te2, dt.fmt, tz = "GMT"))
    
    # handle conversion errors
    check <- list(xd1, xd2, TRUE)
    check <- tryCatch(na.fail(check), error = function(e) {
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings in first 
        two rows of DATE column for inspection."); return(list(te1, te2, FALSE))})
    
    # if no errors, calculate date vector of length of obs file (excluding the header), otherwise return character strings
    if(check[[3]]) {
      te <- 0:(nr - 2)
      dt <- difftime(xd2, xd1)
      date <- xd1 + te * dt
    } else {
      date <- c(check[[1]], check[[2]], rep("", nr - 3))
    }
    
    # compose and return output
    out <- data.frame(DATE = date, meanobs = out$res)
    names(out) <- c("DATE", paste("X", subid, sep = ""))
    attr(out, "subid") <- subid
    return(out)
    
  } else {
    
    return(NULL)
    
  }
  
  
}
