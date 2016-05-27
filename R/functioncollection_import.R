#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of HYPE file import functions, herein:
#
#     - ReadGeoClass()
#     - ReadBasinOutput()
#     - ReadXobs()
#     - ReadGeoData()
#     - ReadBranchData()
#     - ReadCropData()
#     - ReadPar()
#     - ReadMapOutput()
#     - ReadTimeOutput()
#     - ReadPTQobs()
#     - ReadLakeData()
#     - ReadPmsf()
#     - ReadMgmtData()
#     - ReadAquiferData()
#     - ReadPointSourceData()
#     - ReadAllsim()
#     - ReadOptpar()
#     - ReadSubass()
#     - ReadDescription()
#     - ReadGlacierData()
#     - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadGeoClass~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'GeoClass.txt' File
#'
#' @description
#' This is a convenience wrapper function to import a GeoClass file as data frame into R. GeoClass files contain definitions
#' of SLC (\bold{S}oil and \bold{L}and use \bold{C}lass) classes.
#' 
#' @param filename Path to and file name of the GeoClass file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param headrow Row number with header information, defaults to 3 (i.e., two comment rows preceding the header).
#' 
#' @details
#' \code{ReadGeoClass} is a convenience wrapper function of \code{\link{read.table}}, with treatment of leading 
#' comment rows and a column header. Comment rows are imported as strings in \code{attribute} 'comment'. HYPE column headers are 
#' converted during import to  eliminate invalid characters (e.g. '-') and saved to \code{attribute} 'header'.
#' 
#' @return
#' \code{ReadGeoClass} returns a data frame with added attribute 'comment'.
#' 
#' @examples
#' \dontrun{ReadGeoClass("Geoclass.txt")}
#' 

ReadGeoClass <- function(filename = "GeoClass.txt", headrow = 3) { 
    
  # read in the data in the file, skipping the comments and header
  x <- read.table(filename, header = T, skip = headrow - 1, comment.char = "", fill = T)
  # clean header from remnant of comment character in txt file
  names(x)[1] <- gsub("X.", "", names(x)[1])
  
  # update with new attributes to hold comment rows
  xattr <- readLines(filename, n = headrow)
  attr(x, which = "comment") <- xattr[1:headrow - 1]
  attr(x, which = "header") <- xattr[headrow]
  
  return(x)
}

## DEBUG
# te <- "//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/S-HYPE2012B/Leverans,2013-09-30/GeoClass.txt"
# te2 <- ReadGeoClass(filename = te)
# str(te2)
# rm(te, te2, ReadGeoClass)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadBasinOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a Basin Output File
#'
#' @description
#' This is a convenience wrapper function to import a basin output file as data frame or matrix into R.
#' 
#' @param filename Path to and file name of the basin output file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'. If set to \code{NULL}, no date-time conversion will be attempted and the column will
#' be imported as \code{character}, applicable e.g. for files containing just one row of summary values over the model period.
#' @param outformat Format of the returned object. Character string, either \code{'dataframe'} (the standard choice) or 
#' \code{'matrix'}, can be abbreviated.
#' 
#' @details
#' \code{ReadBasinOutput} is a convenience wrapper function of \code{\link{read.table}}, with conversion of date-time strings to
#' POSIX time representations. Monthly and annual time steps are returned as first day of the time step period.
#' 
#' @return
#' \code{ReadBasinOutput} returns a data frame or a matrix, see argument 'outformat'. In the matrix case, date-time information
#' is converted to numeric POSIX representations (seconds since 1970-01-01). This will lead to NAs if Date-time conversion failed. 
#' Variable units are imported as string \code{attribute} 'unit' and a time step keyword string in \code{attribute} 'timestep'. 
#' The catchment's SUBID is extracted from the \code{filename} argument if possible and stored in \code{attribute} 'subid'.
#' If a matrix is returned, these attributes will not be preserved.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. Results will
#' then contain values of '****************'. \code{ReadBasinOutput} will convert those cases to 'NaN' entries and throw a warning.
#' 
#' @examples
#' \dontrun{ReadBasinOutput("0000001.txt")}
#' 

ReadBasinOutput <- function(filename, dt.format = "%Y-%m-%d", outformat = "df") {
  
  # check validity of outformat argument
  if (outformat != "df" & outformat != "m" & outformat != "dataframe" & outformat != "matrix") {
    stop("Argument 'outformat' invalid.")
  }
  
  x <- read.table(filename, header = F, na.strings = "-9999", skip = 2, sep = "\t")      
  names(x) <- strsplit(readLines(filename, n = 1),split = "\t")[[1]]
  
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format, on error, strings are returned.
  
  # convert date column to character to avoid problems with factor levels in the date conversion
  x[, 1] <- as.character(x[, 1])
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    # convert date column to posix string if possible, catch failed attempts with error condition and return string unchanged
    # add new attribute with time step information
    if (dt.format == "%Y-%m") {
      xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
      x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      attr(x, which = "timestep") <- "month"
    } else if (dt.format == "%Y%m") {
      xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
      x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      attr(x, which = "timestep") <- "month"
    } else if (dt.format == "%Y") {
      xd <- as.POSIXct(strptime(paste(x[, 1], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
      x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      attr(x, which = "timestep") <- "year"
    } else {
      xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
      x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
    }
    # conditional: timestep attribute identified by difference between first two entries
    tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
    if (is.na(tdff)) {
      attr(x, which = "timestep") <- "unknown"
    } else if (tdff == 24) {
      attr(x, which = "timestep") <- "day"
    } else if (tdff == 168) {
      attr(x, which = "timestep") <- "week"
    } else if (tdff %in% c(744, 720, 696, 672)) {
      attr(x, which = "timestep") <- "month"
    } else {
      attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
    }
  } else {
    # add timestep attribute with placeholder value
    attr(x, which = "timestep") <- "unknown"
  }
  
  
  # update with new attributes to hold measurement units and SUBID
  xattr <- readLines(filename, n = 2)
  attr(x, which = "unit") <- strsplit(xattr[2], split = "\t")[[1]]
  te <- strsplit(filename, "/")[[1]]
  attr(x, which = "subid") <- as.integer(gsub("[[:alpha:][:punct:]]", "", te[length(te)]))
  
  
  # search data rows for occurrences of "****************", which represent values which had too many digits at the requested
  # decimal accuracy during HYPE's Fortran export to text file
  te <- sapply(x[, -1], FUN = is.factor)
  # conditional: walk through columns and if type is factor, convert to numeric and convert NAs and NaNs (for "*" values)
  if (any(te)){
    warning(paste("Column(s)", paste(names(x)[-1][te], collapse =", "), "initially imported as factors. Internally converted to numeric, occurrences of '****************' values converted to 'NaN'."))
    for (i in (2:(length(te)+1))[te]) {
      if(is.factor(x[, i])) {
        x[, i] <- as.character(x[, i])
        if (length(which(x[, i] == "****************")) > 0){
          x[which(x[, i] == "****************"), i] <- "NaN"
        }
        if (length(which(x[, i] == "-9999")) > 0){
          x[which(x[, i] == "-9999"), i] <- "NA"
        }
        x[, i] <- as.numeric(x[, i])
      }
    }
  }
  
  
  # handling of argument 'outformat', 
  if(outformat == "matrix" | outformat == "m") {
    return(as.matrix(cbind(DATE = as.numeric(x[, 1]), x[, -1])))
  } else return(x)
}


## DEBUG
# ReadBasinOutput(filename=te)
# dt.format <- "%Y-%m-%d"
# outformat <- "df"
# filename <- "d:/2014_soils2sea/WP5/2014-09_NorsmindeHYPE/EHYPERes/EHYPEData/res/9001282.txt"
# rm(te, x, xd, dt.format, filename, tdff, xattr, outformat)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadXobs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read an 'Xobs.txt' file
#'
#' @description
#' This is a convenience wrapper function to import an Xobs file into R.
#' 
#' @param filename Path to and file name of the Xobs file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param nrows Integer, number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives the number of rows
#' to import.
#'  
#' @details
#' \code{ReadXobs} is a convenience wrapper function of \code{\link{read.table}}, with conversion of date-time strings to
#' POSIX time representations. Variable names, SUBIDs, comment, and timestep are returned as attributes (see \code{\link{attr}} on 
#' how to access these).
#' 
#' Duplicated variable-SUBID combinations are not allowed, and import will abort if any are found.
#' 
#' @return
#' If datetime import to POSIXct worked, \code{ReadXobs} returns a \code{\link{HydroXobs}} object, a data frame with four 
#' additional attributes \code{variable}, \code{subid}, \code{comment}, and \code{timestep}: \code{variable} 
#' and \code{subid} each contain a vector with column-wise HYPE IDs (first column with date/time information omitted). 
#' \code{comment} contains the content of the Xobs file comment row as single string. \code{timestep} contains a keyword string.
#' Column names of the returned data frame are composed of variable names and SUBIDs, separated by an underscore, 
#' i.e. \code{[variable]_[subid]}. If datetime conversion failed on import, the returned object is a data frame 
#' (i.e. no class \code{HypeXobs}).
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' @examples
#' \dontrun{ReadXobs("Xobs.txt")}
#' 


ReadXobs <- function (filename = "Xobs.txt", dt.format="%Y-%m-%d", nrows = -1) {
  
  # read first line to get number of columns in file, used for colClasses below
  te <- read.table(filename, header = F, skip = 3, na.strings = "-9999", nrows = 1, sep = "\t")
  nc <- ncol(te)
  
  # read the data, skip header and comment rows, force numberic data (automatic column classes can be integer)
  xobs <- read.table(filename, header = F, skip = 3, na.strings = "-9999", nrows = nrows, sep = "\t", colClasses = c(NA, rep("numeric", nc - 1)))
    
  # update with new attributes to hold subids and obs-variables for all columns
  xattr <- readLines(filename,n=3)
  #attr(xobs, which = "comment") <- strsplit(xattr[1], split = "\t")[[1]]
  attr(xobs, which = "comment") <- xattr[1]
  attr(xobs, which = "variable") <- toupper(strsplit(xattr[2], split = "\t")[[1]][-1])
  attr(xobs, which = "subid") <- as.integer(strsplit(xattr[3], split = "\t")[[1]][-1])
  
  # update header, composite of variable and subid
  names(xobs) <- c("date", paste(attr(xobs, "variable"), attr(xobs, "subid"), sep = "_"))
  
  # warn if duplicate columns found, throw useful msg
  if (length(names(xobs)) != length(unique(names(xobs)))) {
    warning(paste0("Duplicated variable-SUBID combination(s) in file: ", paste(names(xobs)[duplicated(names(xobs))], collapse = " ")))
    duplifree <- FALSE
  } else {
    duplifree <- TRUE
  }
  
  # date conversion 
  xd <- as.POSIXct(strptime(xobs[, 1], format = dt.format), tz = "GMT")
  xobs[, 1] <- tryCatch(na.fail(xd), error = function(e) {
    cat("Date/time conversion attempt led to introduction of NAs, date/times returned as strings.\nImported as data frame, not as 'HypeXobs' object."); return(xobs[, 1])})
  
  # if date conversion worked and time steps are HYPE-conform, add class HydroXobs to returned object 
  if(!is.factor(xobs[, 1]) && duplifree) {
    # check if time steps are equidistant
    tstep <- diff(xobs[, 1])
    hypexobsclass <- TRUE
    if (min(tstep) != max(tstep)) {
      hypexobsclass <- FALSE
      warning("Non-equidistant time steps in 'x'.")
    }
    # check if time steps are at least daily
    tunits <- attr(tstep, "units")
    if (tunits == "days" && tstep[1] > 1) {
      hypexobsclass <- FALSE
      warning("Longer-than-daily time steps not allowed in HypeXobs objects.")
    }
    
    # add new class and timestep attribute, conditional on time step conformity
    if (hypexobsclass) {
      class(xobs) <- c("HypeXobs", "data.frame")
      if (tunits == "days") {
        attr(xobs, "timestep") <- "day"
      } else {
        attr(xobs, "timestep") <- paste0(tstep[1], tunits)
      }
    } else {
      warning("Imported as data frame, not as 'HypeXobs' object.")
    }
    
  } else {
    warning("Imported as data frame, not as 'HypeXobs' object.")
  }
  
  return(xobs)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadGeoData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'GeoData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a GeoData file as data frame into R.
#' 
#' @param filename Path to and file name of the GeoData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param sep  character string. Field separator character as described in \code{\link{read.table}}.
#' @details
#' \code{ReadGeoData} runs \code{read.table(file = filename, header = T, sep = sep)} with a forced numeric column type for 
#' columns \code{AREA} and \code{RIVLEN}, and upper-case column names.
#' 
#' @return
#' \code{ReadGeoData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadGeoData("GeoData.txt")}
#' 


ReadGeoData <- function(filename = "GeoData.txt", sep = "\t") {
  res <- read.table(file = filename, header = T, sep = sep)
  names(res) <- toupper(names(res))
  # force type numeric for selected columns if they exist. Otherwise there can be problem with integer calculation in other functions..
  te <- which(names(res) == "AREA")
  if (length(te) == 1) {
    res$AREA <- as.numeric(res$AREA)
  }
  te <- which(names(res) == "RIVLEN")
  if (length(te) == 1) {
    res$RIVLEN <- as.numeric(res$RIVLEN)
  }
  return(res)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadBranchData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'BranchData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a BranchData file as data frame into R.
#' 
#' @param filename Path to and file name of the Branchdata file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadBranchData} is just \code{read.table(file = filename, header = T)}, mainly added to provide a comparable 
#' function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadBranchData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadBranchData("BranchData.txt")}
#' 


ReadBranchData <- function(filename = "BranchData.txt") {
  te <- read.table(file = filename, header = T)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadCropData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'CropData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a CropData file as data frame into R.
#' 
#' @param filename Path to and file name of the CropData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadCropData} is just \code{read.table(file = filename, header = T)}, mainly added to provide a comparable 
#' function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadCropData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadCropData("CropData.txt")}
#' 


ReadCropData <- function(filename = "CropData.txt") {
  te <- read.table(file = filename, header = T)
}







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadPar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'par.txt' file
#'
#' @description
#' Import a HYPE parameter file as list into R.
#' 
#' @param filename Path to and file name of the parameter file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadPar} does not check for comment lines in 'par.txt' files, the file structure is imported unchanged.
#' 
#' @return
#' \code{ReadPar} returns a list with one vector for each row in 'par.txt'. HYPE parameter names (first entries in 'par.txt') 
#' are returned as vector names.
#' 
#' 
#' 
#' @examples
#' \dontrun{ReadPar("par.txt")}
#' 

ReadPar <- function (filename = "par.txt") {
  
  ## builds on suggestion found here: http://stackoverflow.com/questions/6602881/text-file-to-list-in-r
  # read par file into a character vector (one string per row in file)
  x <- scan(filename, what = "", sep = "\n")
  # split string elements along whitespaces, returns list of character vectors
  x <- strsplit(x, split = "[[:space:]]+")
  # assign first vector elements as list element names and convert to lower-case (as standardisation)
  names(x) <- sapply(x, `[[`, 1)
  names(x) <- tolower(names(x))
  # remove first vector elements
  x <- lapply(x, `[`, -1)
  # convert list elements to numeric, if possible, catch conversion errors and return non-numeric strings untouched (typically comment lines)
  lapply(x, function(x) tryCatch(na.fail(as.numeric(x, options("warn" = -1))), error = function(e) return(x)))
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadMapOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a Map Output File
#'
#' @description
#' This is a convenience wrapper function to import a map output file ('map<\emph{HYPE_output_variable}>.txt') as data frame into R.
#' 
#' @param filename Path to and file name of the basin output file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}, for conversion of date-time information in column 
#' headers to POSIX dates, which are returned as attribute. Incomplete format strings for monthly and annual values allowed, e.g. 
#' '\%Y'. Defaults to \code{NULL}, which prevents date-time conversion, applicable e.g. for files containing just one column of 
#' summary values over the model period.
#' 
#' @details
#' \code{ReadMapOutput} is a convenience wrapper function of \code{\link{read.table}}, with treatment of a leading 
#' comment row in HYPE's output file. The comment row is imported as string \code{attribute} 'comment'. If \code{dt.format} is specified, 
#' POSIX dates are returned in \code{attribute} 'date', and a time step keyword in \code{attribute} 'time step'.
#' 
#' @return
#' \code{ReadMapOutput} returns a data frame with additional attributes, see details.
#' 
#' @note
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. Results will
#' then contain values of '****************'. \code{ReadMapOutput} will convert those cases to 'NaN' entries and throw a warning.
#' 
#' @examples
#' \dontrun{ReadMapOutput("mapCOUT.txt")}
#' 

ReadMapOutput <- function(filename, dt.format = NULL) {
    
  x <- read.table(filename, header = T, sep = ",", na.strings = "-9999", skip = 1)      
  
  # update with new attribute to hold comment row
  attr(x, which = "comment") <- readLines(filename, n = 1)
  
  # update with new attribute to hold output variable name
  attr(x, which = "variable") <- substr(strsplit(filename, "map")[[1]][2], start = 1, stop = 4)
  
  ## update with new attributes to hold POSIX dates and timestep keyword, create from column names
  
  # extract date string from colnames
  te <- substr(names(x)[-1], start = 2, stop = nchar(names(x)[2]))
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
    if (dt.format == "%Y-%m") {
      xd <- as.POSIXct(strptime(paste(te, "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
      attr(x, which = "date") <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else if (dt.format == "%Y%m") {
      xd <- as.POSIXct(strptime(paste(te, "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
      attr(x, which = "date") <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else if (dt.format == "%Y") {
      xd <- as.POSIXct(strptime(paste(te, "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
      attr(x, which = "date") <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else {
      xd <- as.POSIXct(strptime(te, format = dt.format), tz = "GMT")
      attr(x, which = "date") <- tryCatch(na.fail(xd), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    }
    # conditional: timestep attribute identified by difference between first two entries
    tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
    if (!is.na(tdff)) {
      if (tdff == 24) {
        attr(x, which = "timestep") <- "day"
      } else if (tdff == 168) {
        attr(x, which = "timestep") <- "week"
      } else if (tdff %in% c(744, 720, 696, 672)) {
        attr(x, which = "timestep") <- "month"
      } else if (tdff %in% c(8760, 8784)) {
        attr(x, which = "timestep") <- "year"
      } else {
        attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
      }
    } else {
      # add timestep attribute with placeholder value
      attr(x, which = "timestep") <- "none"
    }
    
  } else {
    # add timestep attribute with placeholder value
    attr(x, which = "timestep") <- "none"
    attr(x, which = "date") <- te
  }
  
  # search data columns for occurrences of "****************", which represent values which had too many digits at the requested
  # decimal accuracy during HYPE's Fortran export to text file
  te <- sapply(x[, -1], FUN = is.factor)
  # conditional: walk through columns and if type is factor, convert to numeric and convert NAs and NaNs (for "*" values)
  if (any(te)){
    warning(paste("Column(s)", paste(names(x)[-1][te], collapse =", "), "initially imported as factors. Internally converted to numeric, occurrences of '****************' values converted to 'NaN'."))
    for (i in (2:(length(te)+1))[te]) {
      if(is.factor(x[, i])) {
        x[, i] <- as.character(x[, i])
        if (length(which(x[, i] == "****************")) > 0){
          x[which(x[, i] == "****************"), i] <- "NaN"
        }
        if (length(which(x[, i] == "-9999")) > 0){
          x[which(x[, i] == "-9999"), i] <- "NA"
        }
        x[, i] <- as.numeric(x[, i])
      }
    }
  }
  
  return(x)
}

## DEBUG
#filename <- "//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/test/test_basin/mapCCIN_0000001.txt"
#colnames <- NA
# dt.format <- "%Y"
#rm(filename, colnames, te, x)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadTimeOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Read a Time Output File
#'
#' This is a convenience wrapper function to import a time output file as data frame or matrix into R.
#' 
#' @param filename Path to and file name of the time output file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'. If set to \code{NULL}, no date-time conversion will be attempted and the column will
#' be imported as \code{character}, applicable e.g. for files containing just one row of summary values over the model period.
#' @param datatable Logical, if \code{TRUE}, return a \code{\link{data.table}} object, otherwise a data frame.
#' 
#' @details
#' \code{ReadTimeOutput} is a convenience wrapper function of \code{\link{fread}} from the \code{\link{data.table-package}}, 
#' with conversion of date-time strings to POSIX time representations. Monthly and annual time steps are returned as first day 
#' of the time step period.
#' 
#' @return
#' \code{ReadTimeOutput} returns a \code{\link{data.table}} object or a data frame. Information on the output variable is stored 
#' in attribute \code{comment}, a vector of subid integers in attribute \code{subid}.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. Results will
#' then contain values of '****************'. \code{ReadTimeOutput} will convert those cases to 'NA' entries.

#' 
#' @examples
#' \dontrun{ReadTimeOutput("timeCCIN.txt", dt.format = "%Y-%m)}
#' 
#' @importFrom data.table fread is.data.table
#' @export

ReadTimeOutput <- function(filename, dt.format = "%Y-%m-%d", datatable = FALSE) {
    
  # read.table(filename, header = T, na.strings = "-9999", skip = 1)      
  x <- fread(filename,  na.strings = c("-9999", "****************"), skip = 2, sep = "\t", header = F, data.table = datatable)
  
  # update with new attributes to hold comment row and subids (column headers will have a leading X)
  xattr <- readLines(filename, n = 2)
  # the strsplit on comment is only needed if the file to import was saved in excel before, and lots of empty cells added to the comment line
  attr(x, which = "comment") <- strsplit(xattr[1],split = "\t")[[1]][1]
  attr(x, which = "subid") <- as.numeric(strsplit(xattr[2], split = "\t")[[1]][-1])
  
  
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format, on error, strings are returned.
  
  # 
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    # safety measure: override for date column name
    names(x)[1] <- "DATE"
    
    # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
    # conditional on class of imported data (different syntax for data.table)
    if (is.data.table(x)) {
      
      if (dt.format == "%Y-%m") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else if (dt.format == "%Y%m") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else if (dt.format == "%Y") {
        xd <- as.POSIXct(strptime(paste(x[, DATE], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      } else {
        xd <- as.POSIXct(strptime(x[, DATE], format = dt.format), tz = "GMT")
        x[, DATE := tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, DATE])})]
      }
      
    } else {
      
      if (dt.format == "%Y-%m") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else if (dt.format == "%Y%m") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else if (dt.format == "%Y") {
        xd <- as.POSIXct(strptime(paste(x[, 1], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      } else {
        xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
        x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(eval(dcol))})
      }
    }
    
    
    # conditional: timestep attribute identified by difference between first two entries
    tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
    if (!is.na(tdff)) {
      if (tdff == 24) {
        attr(x, which = "timestep") <- "day"
      } else if (tdff == 168) {
        attr(x, which = "timestep") <- "week"
      } else if (tdff %in% c(744, 720, 696, 672)) {
        attr(x, which = "timestep") <- "month"
      } else if (tdff %in% c(8760, 8784)) {
        attr(x, which = "timestep") <- "year"
      } else {
        attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
      }
    } else {
      # add timestep attribute with placeholder value
      attr(x, which = "timestep") <- "none"
    }
    
  } else {
    # add timestep attribute with placeholder value
    attr(x, which = "timestep") <- "none"
  }
  
  
  # OLD, LEFT FOR REFERENCE BUT CAN SOON BE DELETED
  # # search data rows for occurrences of "****************", which represent values which had too many digits at the requested
  # # decimal accuracy during HYPE's Fortran export to text file
  # te <- sapply(x[, -DATE], FUN = is.character)
  # # conditional: walk through columns and if type is factor, convert to numeric and convert NAs and NaNs (for "*" values)
  # if (any(te)){
  #   warning(paste("Column(s)", paste(names(x)[-1][te], collapse =", "), "initially imported as factors. Internally converted to numeric, occurrences of '****************' values converted to 'NaN'."))
  #   for (i in (2:(length(te)+1))[te]) {
  #     if(is.factor(x[, i])) {
  #       x[, i] <- as.character(x[, i])
  #       if (length(which(x[, i] == "****************")) > 0){
  #         x[which(x[, i] == "****************"), i] <- "NaN"
  #       }
  #       if (length(which(x[, i] == "-9999")) > 0){
  #         x[which(x[, i] == "-9999"), i] <- "NA"
  #       }
  #       x[, i] <- as.numeric(x[, i])
  #     }
  #   }
  # }
  
  return(x)
}

## DEBUG
# filename <- "../timeCCTN.txt"
# dt.format <- "%Y"





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadPTQobs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read 'Pobs.txt', 'Tobs.txt', or 'Qobs.txt' files
#'
#' @description
#' Import precipitation, temperature, or discharge observation files as data frame into R.
#' 
#' @param filename Path to and file name of the file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param nrows Number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives the number of rows
#' to import.
#'  
#' @details
#' \code{ReadPTQobs} is a convenience wrapper function of \code{\link{read.table}}, with conversion of date-time strings to
#' POSIX time representations. SUBIDs are returned as integer attribute \code{subid} 
#' (see \code{\link{attr}} on how to access it). 
#' 
#' This function can only be used with reasonably small data files. Attempts to 
#' read large files can lead to exceedance of R's memory space.
#' 
#' @return
#' \code{ReadPTQobs} returns a data frame with an additional attribute \code{subid}.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' @examples
#' \dontrun{ReadPTQobs("Tobs.txt")}
#' 


ReadPTQobs <- function (filename, dt.format = "%Y-%m-%d", nrows = -1) {
  
  # read the data
  x <- read.table(filename, header = T, na.strings = "-9999", nrows = nrows)
  
  # make an object of a new s3 class, KEPT FOR FUTURE REF, ACTIVATE IF METHODS TO BE WRITTEN, E.G. SUMMARY
  # class(te) <- c("xobs", "data.frame")
  
  # update with new attributes to hold subids and obs-variables for all columns
  xattr <- readLines(filename, n = 1)
  attr(x, which = "subid") <- as.integer(strsplit(xattr[1], split = "\t")[[1]][-1])
  
  # date conversion 
  xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
  x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
    print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])
    }
  )
  
  return(x)
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadLakeData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'LakeData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a LakeData file as data frame into R.
#' 
#' @param filename Path to and file name of the LakeData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadLakeData} is a simple \code{\link{read.table}} wrapper, mainly added to provide a comparable 
#' function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadLakeData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadLakeData("LakeData.txt")}
#' 


ReadLakeData <- function(filename = "LakeData.txt") {
  read.table(file = filename, header = T, na.strings = "-9999", sep = "\t", comment.char = "")
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadPmsf~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'pmsf.txt' file
#'
#' @description
#' This is a small convenience function to import a 'partial model setup file' as integer vector into R.
#' 
#' @param filename Path to and file name of the pmsf file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' Pmsf.txt files imported with \code{ReadPmsf} are stripped from the first value containing the total number of subcatchments 
#' in the file. No additional attribute is added to hold this number since it can be easily obtained using \code{\link{length}}.
#' 
#' @return
#' \code{ReadPmsf} returns an integer vector.
#' 
#' @examples
#' \dontrun{ReadLakeData("pmsf.txt")}
#' 

ReadPmsf <- function(filename = "pmsf.txt") {
  x <- read.table(filename, header = T)
  x <- as.integer(x[,1])
  return(x)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadMgmtData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'MgmtData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a HYPE MgmtData file as data frame into R.
#' 
#' @param filename Path to and file name of the MgmtData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadMgmtData} is a simple \code{\link{read.table}} wrapper, mainly added to provide a comparable 
#' function to the other RHYPE imports. Will check for \code{NA} values in imported data and return a warning if any are found. 
#' HYPE requires \code{NA}-free input in required 'AquiferData.txt' columns, but empty values are allowed in comment 
#' columns which are not read.
#' 
#' @return
#' \code{ReadMgmtData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadMgmtData("MgmtData.txt")}
#' 


ReadMgmtData <- function(filename = "MgmtData.txt") {
  # import
  res <- read.table(file = filename, header = T, na.strings = "-9999", sep = "", comment.char = "")
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te)) warning(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadAquiferData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read an 'AquiferData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a HYPE AquiferData file as data frame into R.
#' 
#' @param filename Path to and file name of the AquiferData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param sep Character string, field separator as in \code{\link{read.table}}. 
#'  
#' @details
#' \code{ReadAquiferData} is a simple \code{\link{read.table}} wrapper, mainly added to provide a comparable 
#' function to other RHYPE import functions. Will check for \code{NA} values in imported data and return a warning if any are found. 
#' HYPE requires \code{NA}-free input in required 'AquiferData.txt' columns, but empty values are allowed in comment columns 
#' which are not read.
#' 
#' @return
#' \code{ReadAquiferData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadAquiferData("../myhype/AquiferData.txt")}
#' 


ReadAquiferData <- function(filename = "AquiferData.txt", sep = "\t") {
  # import
  res <- read.table(file = filename, header = T, na.strings = "-9999", sep = sep, comment.char = "")
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te)) warning(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadPointSourceData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read a 'PointSourceData.txt' file
#'
#' @description
#' This is a convenience wrapper function to import a PointSourceData file as data frame into R.
#' 
#' @param filename Path to and file name of the PointSourceData file to import. 
#'  
#' @details
#' \code{ReadPointSourceData} is just \code{read.table(file = filename, header = T)}, mainly added to provide a comparable 
#' function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadPointSourceData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadPointSourceData("PointSourceData.txt")}
#' 


ReadPointSourceData <- function(filename = "PointSourceData.txt") {
  read.table(file = filename, header = T, sep = "\t")
}







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadAllsim~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read an 'allsim.txt' file
#'
#' @description
#' This is a convenience wrapper function to import an allsim.txt optimisation result file as data frame into R.
#' 
#' @param filename Path to and file name of the 'allsim.txt' file to import. 
#'  
#' @details
#' \code{ReadAllsim} is just \code{read.table(file = filename, header = T, sep = ",")}, mainly added to provide a 
#' comparable function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadAllsim} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadAllsim("allsim.txt")}
#' 


ReadAllsim <- function(filename = "allsim.txt") {
  read.table(file = filename, header = T, sep = ",")
}









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadOptpar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Read an 'optpar.txt' file
#' 
#' This function imports an 'optpar.txt' into a list.
#' 
#' @param filename Path to and file name of the 'optpar.txt' file to import. 
#' 
#' @details 
#' \code{ReadOptpar} imports a HYPE 'optpar.txt' file. Optpar files contain instructions for parameter calibration/optimisation 
#' and parameter value ranges, for details on the file format, see the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:optpar.txt}{optpar.txt online documentation}. 
#' 
#' @return 
#' \code{ReadOptpar} returns a \code{\link{list}} object with four elements: \itemize{ 
#' \item \code{comment}, the file's first-row comment string.
#' \item \code{tasks}, a two-column dataframe with row-wise key-value pairs for tasks and settings.
#' \item \code{pars}, a list of dataframes, each containing values for one parameter. Three columns each, holding parameter 
#' range minima, maxima, and intervals. 
#' The number of rows in each dataframe corresponds to the number of soil or land use classes for class-specific parameters. 
#' Parameter names as list element names.
#' \item \code{calib}, a list of vectors with (soil or land use) class numbers of parameters included in calibration (parameters 
#' with identical min and max values are omitted in calibration, but need to be specified in optpar files). Parameter names 
#' as list element names.
#' }
#' 
#' @seealso \code{\link{ReadPar}}

#' @examples
#' \dontrun{ReadOptpar("optpar.txt")}
#' 
#' @export

ReadOptpar <- function(filename) {
  
  # read tasks and settings into a character vector (one string per row in file)
  tasks <- scan(filename, what = "", sep = "\n", nlines = 21)
  # split string elements along whitespaces, returns list of character vectors
  tasks <- strsplit(tasks, split = "[[:space:]]+")
  # re-merge and separate first-row comment string
  comm <- paste(tasks[[1]], collapse = " ")
  #remove comment from tasks
  tasks <- tasks[-1]
  # convert tasks and settings to two-column dataframe (they are always combinations of single key single value)
  tasks <- do.call(rbind.data.frame, tasks)
  names(tasks) <- c("key", "value")
  
  # read parameters
  x <- scan(filename, what = "", sep = "\n", skip = 21)
  x <- strsplit(x, split = "[[:space:]]+")
  # assign first vector elements as list element names and convert to lower-case (as standardisation)
  names(x) <- sapply(x, `[[`, 1)
  names(x) <- tolower(names(x))
  # remove first vector elements
  x <- lapply(x, `[`, -1)
  # convert list elements to numeric, if possible, catch conversion errors and return non-numeric strings untouched (typically comment lines)
  x <- lapply(x, as.numeric)
  # organise into data frames containing boundaries and interval
  pars <- list()
  j <- 0
  for (i in seq(from = 1, to = length(x), by = 3)) {
    j <- j + 1
    pars[[j]] <- data.frame(min = x[[i]], max = x[[i + 1]], ival = x[[i + 2]])
    names(pars)[j] <- names(x)[i]
  }
  # extract calibrated classes (as vector of class number per parameter)
  calib <- list()
  for (i in 1:length(pars)) {
    calib[[i]] <- which(pars[[i]][, 1] - pars[[i]][, 2] != 0)
    names(calib)[i] <- names(pars)[i]
  }
  return(list(comment = comm, tasks, pars, calib))
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadSubass~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Read an 'subassX.txt' file
#'
#' This is a convenience wrapper function to import an subassX.txt sub-basin assessment file as data frame into R.
#' Sub-basins assessment files contain performance criteria results, as defined in 'info.txt', for individual 
#' sub-basins with observations.
#' 
#' @param filename Path to and file name of the 'subassX.txt' file to import. 
#' @param nhour Integer, time step of sub-daily model results in hours. See details. 
#'  
#' @details
#' \code{ReadSubass} imports a sub-basin assessement file into R. Information on model variables evaluated in the 
#' file is imported as additional \code{\link{attribute}} \code{variables}, the evaluation time step in an attribute 
#' \code{timestep}.
#' 
#' Sub-daily time steps are reported with time step code '0' in HYPE result files. In order to preserve the time step 
#' information in the imported R object, users must provide the actual model evaluation time step in hours 
#' in argument \code{nhour} in the sub-daily case.
#' 
#' @return
#' \code{ReadSubass} returns a data frame with two additional attributes: \code{variables} contains a 2-element 
#' character vector with IDs of evaluated observed and simulated HYPE variables, \code{timestep} contains a character 
#' keyword detailing the evaluation time step.
#' 
#' @examples
#' \dontrun{ReadSubass("subass1.txt")}
#'
#' @export 


ReadSubass <- function(filename = "subass1.txt", nhour = NULL) {
  x <- read.table(file = filename, header = F, sep = "\t", skip = 2)
  te <- readLines(filename, n = 2)
  names(x) <- strsplit(te[2], split = "\t")[[1]]
  # extract additional information from first row in file and add as attributes
  attrsrc <- strsplit(te[1], split = "[[:space:][:punct:]]+")[[1]]
  tstep <- c("day", "week", "month", "year", "modelperiod")[attrsrc[9]]
  # treatment for sub-daily time step (code 0)
  if (length(tstep) == 0) {
    if (is.null(nhour)) {
      tstep <- "nhour"
      warning("Sub-daily results in imported file, but argument 'nhour' not provided. Attribute 'timestep' returned as 'nhour'")
    } else {
      tstep <- paste0(nhour, "hour") 
    }
  }
  attr(x, "timestep") <- tstep
  attr(x, "variables") <- attrsrc[11:12]
  return(x)
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadDescription~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Read a 'description.txt' file
#'
#' Read a 'description.txt' file as \code{list} object into R. A 'description.txt' file contains land use, soil, and crop 
#' class names of a HYPE set-up, as well as set-up name and version. 
#' 
#' @param filename Path to and file name of the 'subassX.txt' file to import. 
#' @param gcl dataframe, imported GeoClass table to compare number of class names with. A warning will be thrown if numbers 
#' are not identical. 
#'  
#' @details
#' \code{ReadDescription} imports a 'description.txt' into R. This file is not used by HYPE, but is convenient for 
#' e.g. plotting legends or examining imported GeoClass files. The text file must contain 10 lines as follows: HYPE 
#' set-up name (2nd line), set-up version (4th line), tab-separated land use class names (6th line), tab-separated 
#' soil class names (8th line),  tab-separated crop class names (8th line). Lines in between can contain headers, 
#' but are not read by the function.
#' 
#' @return
#' \code{ReadDescription} returns a list with 5 elements, corresponding to the imported lines.
#' 
#' @examples
#' \dontrun{ReadDescription("description.txt")}
#'
#' @export 

ReadDescription <- function(filename, gcl = NULL) {
  
  ## builds on suggestion found here: http://stackoverflow.com/questions/6602881/text-file-to-list-in-r
  # read description file into a character vector (one string per row in file)
  x <- scan(file = filename, what = "", sep = "\n", quiet = T)
  # split string elements along tabs, returns list of character vectors
  x <- strsplit(x, split = "\t")
  # remove empty strings (excel export artefacts)
  x <- sapply(x, function(x) {te <- nchar(x);te <- ifelse(te == 0, F, T);x[te]})
  # create result list, assign names
  res <- x[c(2, 4, 6, 8, 10)]
  names(res) <- c("Name", "Version", "Landuse", "Soil", "Crop")
  
  # check conformity with geoclass if provided
  if (!is.null(gcl)) {
    lu.g <- length(unique(gcl[, 2]))
    lu.d <- length(res$Landuse)
    if (lu.g != lu.d) {
      warning(paste0("Land use classes: Number in description (", lu.d, ") != number of land use classes in 'gcl' (", lu.g, ")."))
    }
    s.g <- length(unique(gcl[, 3]))
    s.d <- length(res$Soil)
    if (s.g != s.d) {
      warning(paste0("Soil classes: Number in description (", s.d, ") != number in 'gcl' (", s.g, ")."))
    }
    # crop classes in two columns, and can be 0
    te <- unique(c(gcl[, 4], gcl[, 5]))
    c.g <- length(te[te > 0])
    c.d <- length(res$Crop)
    if (c.g != c.d) {
      warning(paste0("Crop classes: Number in description (", c.d, ") != number in 'gcl' (", c.g, ")."))
    }
  }
  return(res)
}

# filename <- "../description.txt"
# rm(filename, x, res, lu.d, lu.g, s.g, s.d, c.d, c.g, te)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadGlacierData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' Read a 'GlacierData.txt' file
#'
#' This is a convenience wrapper function to import a GlacierData file as data frame into R.
#' 
#' @param filename Path to and file name of the GlacierData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param sep  character string. Field separator character as described in \code{\link{read.table}}.

#' @details
#' \code{ReadGlacierData} runs \code{read.table(file = filename, header = T, sep = sep)} and forces upper-case column names.
#' 
#' @return
#' \code{ReadGlacierData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadGlacierData("GlacierData.txt")}
#' 
#' @export


ReadGlacierData <- function(filename = "GlacierData.txt", sep = "\t") {
  res <- read.table(file = filename, header = T, sep = sep)
  names(res) <- toupper(names(res))
  return(res)
}
