#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of import functions, herein:
#
#     - ReadGeoClass()
#     - ReadBasinOutput()
#     - ReadXobs()
#     - ReadGeoData()
#     - ReadBranchData()
#     - ReadCropData()
#     - ReadPar()
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

ReadGeoClass<-function(filename = "GeoClass.txt", headrow = 3) { 
    
  # read in the data in the file, skipping the comments and header
  x <- read.table(filename, header = T, skip = headrow - 1, comment.char = "", fill = T)
  # clean header from remnant of comment caharacter in txt file
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
#' @param dt.format Date-time format string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'.
#' @param outformat Format of the returned object. Character string, either \code{'dataframe'} or \code{'matrix'}, 
#' can be abbreviated.
#' 
#' @details
#' \code{ReadBasinOutput} is a convenience wrapper function of \code{\link{read.table}}, with conversion of date-time strings to
#' POSIX time representations. Monthly and annual time steps are returned as first day of the time step period.
#' 
#' @return
#' \code{ReadBasinOutput} returns a data frame or a matrix, see argument 'outformat'. In the matrix case, date-time information
#' is converted to numeric POSIX representations (seconds since 1970-01-01). This will lead to NAs if Date-time conversion failed.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' @examples
#' \dontrun{ReadBasinOutput("0000001.txt")}
#' 

ReadBasinOutput <- function(filename, dt.format="%Y-%m-%d", outformat="df") {
  
  # check validity of outformat argument
  if (outformat != "df" & outformat != "m" & outformat != "dataframe" & outformat != "matrix") {
    stop("Argument 'outformat' invalid.")
  }
  
  x <- read.table(filename, header = F, na.strings = "-9999", skip = 2)      
  names(x) <- strsplit(readLines(filename, n = 1),split = "\t")[[1]]
  
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format, on error, strings are returned.
  
  # convert date column to character to avoid problems with factor levels in the date conversion
  x[, 1] <- as.character(x[, 1])
  
  # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
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
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
  }
  
  # handling of argument 'outformat'
  if(outformat == "matrix" | outformat == "m") {
    
    return(as.matrix(cbind(DATE = as.numeric(x[, 1]), x[, -1])))
  } else return(x)
}


## DEBUG
# ReadBasinOutput(filename=te)
# dt.format <- "%Y-%m-%d"
# outformat <- "df"
# filename <- "//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-01-10_koppling_SHYPE2012B_HBVsv/res_daily_thomas_hadley/0042041.txt"
# rm(te, x, xd, ReadBasinOutput, dt.format, filename)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadXobs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Read an 'Xobs.txt' file
#'
#' @description
#' This is a convenience wrapper function to import an Xobs file as data frame into R.
#' 
#' @param filename Path to and file name of the Xobs file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time format string as in \code{\link{strptime}}. 
#' @param nrows Number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives the number of rows
#' to import.
#'  
#' @details
#' \code{ReadXobs} is a convenience wrapper function of \code{\link{read.table}}, with conversion of date-time strings to
#' POSIX time representations. Variable names and SUBIDs are returned as attributes (see \code{\link{attr}} on how to access these).
#' 
#' @return
#' \code{ReadXobs} returns a data frame with two additional attributes \code{variable} and \code{subid}, each containing a 
#' vector with column-wise information. The two are merged in the column names. A third attribute \code{comment} contains 
#' the content of the Xobs file comment row as single string.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' @examples
#' \dontrun{ReadXobs("Xobs.txt")}
#' 


ReadXobs <- function (filename = "Xobs.txt", dt.format="%Y-%m-%d", nrows = -1) {
  
  # read the data, skip header and comment rows
  xobs <- read.table(filename, header = F, skip = 3, na.strings = "-9999", nrows = nrows)
  
  # make an object of a new s3 class, KEPT FOR FUTURE REF, ACTIVATE IF METHODS TO BE WRITTEN, E.G. SUMMARY
  # class(te) <- c("xobs", "data.frame")
    
  # update with new attributes to hold subids and obs-variables for all columns
  xattr <- readLines(filename,n=3)
  attr(xobs, which = "comment") <- xattr[1]
  attr(xobs, which = "variable") <- strsplit(xattr[2], split = "\t")[[1]]
  attr(xobs, which = "subid") <- as.integer(strsplit(xattr[3], split = "\t")[[1]])
  
  # update header, composite of variable and subid
  names(xobs) <- paste(attr(xobs, "variable"), attr(xobs, "subid"), sep = "_")
  names(xobs)[1] <- "date"
  
  # date conversion 
  xd <- as.POSIXct(strptime(xobs[, 1], format = dt.format), tz = "GMT")
  xobs[, 1] <- tryCatch(na.fail(xd), error = function(e) {
    print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(xobs[, 1])})
  
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
#'  
#' @details
#' \code{ReadGeoData} is just \code{read.table(file = filename, header = T)}, mainly added to provide a comparable 
#' function to the other RHYPE imports.
#' 
#' @return
#' \code{ReadGeoData} returns a data frame.
#' 
#' @examples
#' \dontrun{ReadGeoData("GeoData.txt")}
#' 


ReadGeoData <- function(filename = "GeoData.txt") {
  read.table(file = filename, header = T)
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


ReadCropData <- function(filename = "BranchData.txt") {
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
  # assign first vector elements as list element names
  names(x) <- sapply(x, `[[`, 1)
  # remove first vector elements
  x <- lapply(x, `[`, -1)
  # convert list elements to numeric, if possible, catch conversion errors and return non-numeric strings untouched (typically comment lines)
  lapply(y, function(x) tryCatch(na.fail(as.numeric(x, options("warn" = -1))), error = function(e) return(x)))
}
