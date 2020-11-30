
#--------------------------------------------------------------------------------------------------------------------------------------
#   Collection of HYPE file import functions, herein:
#
#     - ReadGeoClass()
#     - ReadBasinOutput()
#     - ReadXobs()
#     - ReadGeoData()
#     - ReadPar()
#     - ReadMapOutput()
#     - ReadTimeOutput()
#     - ReadPTQobs()
#     - HypeDataImport: 
#         ReadLakeData(), ReadDamData(), ReadMgmtData(), ReadAquiferData(), ReadPointSourceData(), ReadGlacierData(), 
#         ReadCropData(), ReadBranchData()
#     - ReadPmsf()
#     - ReadAllsim()
#     - ReadOptpar()
#     - ReadSubass()
#     - ReadDescription()
#     - ReadSimass()
#     - 
#--------------------------------------------------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------------------------------------------------
# ReadGeoClass
#--------------------------------------------------------------------------------------------------------------------------------------

#' Read a 'GeoClass.txt' File
#'
#' This is a convenience wrapper function to import a GeoClass file as data frame into R. GeoClass files contain definitions
#' of SLC (\bold{S}oil and \bold{L}and use \bold{C}rop) classes in twelve to 14 predefined columns, see 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:geoclass.txt}{GeoClass.txt documentation}.
#' 
#' @param filename Path to and file name of the GeoClass file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link[data.table]{fread}}.
#' @param verbose Print information on number of data columns in imported file.
#' 
#' @details
#' \code{ReadGeoClass} is a convenience wrapper function of \code{\link[data.table]{fread}}, with treatment of leading 
#' comment rows. Column names are created on import, optional comment rows are imported as strings in \code{attribute} 'comment'. 
#' Optional inline comments (additional non-numeric columns) are automatically identified and imported along with data columns. 
#' 
#' @return
#' \code{ReadGeoClass} returns a data frame with added attribute 'comment'.
#' 
#' @examples
#' \dontrun{ReadGeoClass("Geoclass.txt")}
#' 
#' @export

ReadGeoClass <- function(filename = "GeoClass.txt", encoding = c("unknown", "UTF-8", "Latin-1"), verbose = TRUE) { 
  
  # argument checks
  encoding <- match.arg(encoding)
  
  
  # identify comment rows, lines starting with '!' at the top of the file
  gf <- file(description = filename, open = "r")
  cm <- TRUE
  skip <- 0
  while (cm) {
    te <- substr(readLines(gf, n = 1), 1, 1)
    if (te == "!") {
      skip <- skip + 1
    } else {
      cm <- FALSE
    }
  }
  close(gf)
  
  # read in the data in the file, skipping the comments and header
  x <- fread(filename, header = FALSE, skip = skip, fill = T, data.table = FALSE, encoding = encoding)
  
  ## identify number of soil layers in the file
  
  # number of columns, data and comments
  ncl <- ncol(x)
  
  # conditional on number of columns: check if 13 and 14 (depth2 and depth3) are numeric
  if (ncl <= 12) {
    
    # only depth1, all columns should contain data
    ndcl <- ncl
    
    # something is amiss..
    if (ncl < 12) {
      warning(paste(ncl, "columns in imported file. GeoClass files need at least 12 columns."))
    }
    
  } else {
    # at least 13 columns in file
    if (!is.numeric(x[, 13])) {
      
      # column 13 is a comment column, further columns are also comments
      ndcl <- 12
      
    } else if (ncl >= 14) {
      if (!is.numeric(x[, 14])) {
        
        # column 14 is a comment column, further columns are also comments
        ndcl <- 13
      } else {
        
        # cols 13 and 14 are data columns
        ndcl <- 14
      }
    } else {
      # remaining case, exactly 13 columns, 13th is depth2
      ndcl <- 13
    }
  }
  
  # add data column names
  names(x)[1:ndcl] <- c("slc", "landuse", "soil", "cropid1", "cropid2", "rotation", "vegtype", "special", "tiledepth", 
                      "streamdepth", "nsoils", "depth1", "depth2", "depth3")[1:ndcl]
  # add comment column names, if any
  if (ncl > ndcl) {
    names(x)[(ndcl + 1):ncl] <- paste0("comment", 1:(ncl-ndcl))
  }
  
  # update with new attributes to hold comment rows
  attr(x, which = "comment") <- readLines(filename, n = skip, encoding = ifelse(encoding == "Latin-1", "latin1", encoding))
  
  # check if all data columns are numeric, with a useful warning
  if (!all(apply(x[, 1:ndcl], 2, is.numeric))) {
    warning("Non-numeric contents in data columns of imported file.")
  }
  
  if (verbose) {
    message(paste(ndcl, "data columns in imported file."))
  }
  
  return(x)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadBasinOutput
#--------------------------------------------------------------------------------------------------------------------------------------

#' Read a Basin Output File
#'
#' This is a convenience wrapper function to import a basin output file as data frame or matrix into R.
#' 
#' @param filename Path to and file name of the basin output file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'. If set to \code{NULL}, no date-time conversion will be attempted and the column will
#' be imported as \code{character}, applicable e.g. for files containing just one row of summary values over the model period.
#' @param type Character, keyword for data type to return. \code{"df"} to return a standard data frame, \code{"dt"} to 
#' return a \code{\link[data.table]{data.table}} object, or \code{"hmv"} to return a \code{\link{HypeMultiVar}} array. 
#' @param id Integer, SUBID or OUTREGID of the imported sub-basin or outregion results. If \code{NULL} (default), the function attempts to read this 
#' from the imported file's name, which only works for standard HYPE basin output file names or any where the first 7 digits 
#' give the SUBID or OUTREGID with leading zeros. See details.
#' @param warn.nan Logical, check if imported results contain any \code{NaN} values. If \code{TRUE} and \code{NaN}s are found, 
#' a warning is thrown and affected SUBIDs saved in an attribute \code{subid.nan}. Adds noticeable overhead to import time for large files.
#' 
#' @details
#' \code{ReadBasinOutput} is a convenience wrapper function of \code{\link[data.table]{fread}} from package 
#' \code{\link{data.table}}, with conversion of date-time strings to
#' POSIX time representations. Monthly and annual time steps are returned as first day of the time step period.
#' 
#' HYPE basin output files can contain results for a single sub-basin or for a user-defined output region. \code{ReadBasinOutput} checks HYPE 
#' variable names (column headers in imported file) for an "RG"-prefix. If it is found, the ID read from either file name or argument 
#' \code{id} is saved to attribute \code{outregid}, otherwise to attribute \code{subid}.
#' 
#' @return
#' \code{ReadBasinOutput} returns a \code{data.frame}, \code{\link{data.table}}, or a \code{\link{HypeMultiVar}} array. 
#' Data frames and data tables contain additional \code{\link{attributes}}: \code{unit}, a vector of HYPE variable units, 
#' \code{subid} and \code{outregid}, the HYPE SUBID/OUTREGID to which the time series belong (both attributes always created and assigned \code{NA} 
#' if not applicable to data contents), and \code{timestep} 
#' with a time step keyword attribute. An additional attribute \code{subid.nan} might be returned, see argument \code{warn.nan}.


#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. 
#' Results will then contain values of '****************'. \code{ReadBasinOutput} will convert those cases to 'NA' entries.
#' 
#' Current versions of HYPE allow for defining significant numbers of digits instead of fixed ones, which should prevent this 
#' issue from arising.
#' 
#' @examples
#' \dontrun{ReadBasinOutput("0000001.txt")}
#' 
#' @importFrom data.table fread
#' @export


ReadBasinOutput <- function(filename, dt.format = "%Y-%m-%d", type = c("df", "dt", "hmv"), id = NULL, warn.nan = FALSE) {
  
  # handling output type user choice
  type <- match.arg(type)
  if (type == "df") {
    d.t <- F
  } else {
    d.t <- T
  }
  
  # read header with variable names
  nm <- toupper(strsplit(readLines(filename, n = 1),split = "\t")[[1]])
  
  # import data
  x <- fread(filename, 
             na.strings = c("-9999", "****************", "-1.0E+04", "-1.00E+04", "-9.999E+03", "-9.9990E+03", 
                            "-9.99900E+03", "-9.999000E+03", "-9.9990000E+03", "-9.99900000E+03", "-9.999000000E+03"), 
             skip = 2, sep = "\t", header = F, data.table = d.t, colClasses = c("character", rep("numeric", length(nm) - 1)))      
  
  # check if results are region outputs and update header with HYPE variable names
  if (all(substr(nm[-1], 1, 2) == "RG")) {
    reg.out <- T
    nm[-1] <- substr(nm[-1], 3, 100)
  } else {
    reg.out <- F
  }
  
  names(x) <- nm
  
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format, on error, strings are returned.
  
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    
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
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      }
    }
  } else {
    # dummy date vector as there is always one needed in timestep attribute derivation below
    xd <- NA
  }
  
  ## extract attributes to hold measurement units and SUBID/OUTREGID
  
  munit <- readLines(filename, n = 2)
  munit <- strsplit(munit[2], split = "\t")[[1]][-1]
  
  # subid/outregid conditional on user argument
  if (is.null(id)) {
    sbd <- strsplit(filename, "/")[[1]]
    sbd <- as.integer(substr(sbd[length(sbd)], start = 1, stop = 7))
    #as.integer(gsub("[[:alpha:][:punct:]]", "", sbd[length(sbd)]))
  } else {
    sbd  <- id
  }
  # conditional: timestep attribute identified by difference between first two entries
  tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
  if (!is.na(tdff)) {
    if (tdff == 24) {
      tstep <- "day"
    } else if (tdff == 168) {
      tstep <- "week"
    } else if (tdff %in% c(744, 720, 696, 672)) {
      tstep <- "month"
    } else if (tdff %in% c(8760, 8784)) {
      tstep <- "year"
    } else {
      tstep <- paste(tdff, "hour", sep = "")
    }
  } else {
    # add timestep attribute with placeholder value
    tstep <- "unknown"
  }
  
  # check for existence of NaN values
  if (warn.nan) {
    te <- apply(as.data.frame(x[, -1]), 2, function(x) any(is.nan(x)))
    if (any(te)) {
      warning("'NaN' values found in one or more SUBIDs. SUBIDs saved in attribute 'subid.nan'.")
      attr(x, "subid.nan") <- sbd[te]
    }
  }
  
  # conditional on user choice: output formatting
  if (type %in% c("dt", "df")) {
    
    # update with new attributes
    attr(x, which = "unit") <- munit
    attr(x, which = "timestep") <- tstep
    if (reg.out) {
      attr(x, which = "outregid") <- sbd
      attr(x, which = "subid") <- NA
    } else {
      attr(x, which = "outregid") <- NA
      attr(x, which = "subid") <- sbd
    }
    
  } else {
    ## HypeMultiVar formatting
    hvar <- toupper(names(x)[-1])
    # remove dates
    x <- x[, !"DATE", with = F]
    # convert to array (straigtht conversion to array gives error, therefore intermediate matrix)
    x <- as.array(as.matrix(x))
    # adding 'iteration' dimension
    dim(x) <- c(dim(x), 1)
    # construct HypeMultiVar array, conditional on subid/outregid contents
    if (reg.out) {
      x <- HypeMultiVar(x = x, date = xd, hype.var = hvar, outregid = sbd)
    } else {
      x <- HypeMultiVar(x = x, date = xd, hype.var = hvar, subid = sbd)
    }
    
  }
  
  return(x)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadXobs
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read an 'Xobs.txt' file
#'
#' This is a convenience wrapper function to import an Xobs file into R.
#' 
#' @param filename Path to and file name of the Xobs file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param variable Character vector, HYPE variable ID(s) to select for import. Not case-sensitive. If \code{NULL} (default), all 
#' variables are imported. See \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt documentation} 
#' for a list of variable IDs.
#' @param nrows Integer, number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives 
#' the number of rows to import.
#' @param verbose Logical, throw warning if class \code{HypeXobs}'s attribute \code{timestep} cannot be computed.
#'  
#' @details
#' \code{ReadXobs} is a convenience wrapper function of \code{\link[data.table]{fread}} from package  
#' \code{\link{data.table}}, 
#' with conversion of date-time strings to POSIX time representations. Variable names, SUBIDs, comment, and timestep are returned as 
#' attributes (see \code{\link{attr}} on how to access these).
#' 
#' Duplicated variable-SUBID combinations are not allowed in HYPE Xobs files, and the function will throw a warning if any are found.
#' 
#' @return
#' If datetime import to POSIXct worked, \code{ReadXobs} returns a \code{\link{HypeXobs}} object, a data frame with four 
#' additional attributes \code{variable}, \code{subid}, \code{comment}, and \code{timestep}: \code{variable} 
#' and \code{subid} each contain a vector with column-wise HYPE IDs (first column with date/time information omitted). 
#' \code{comment} contains the content of the Xobs file comment row as single string. \code{timestep} contains a keyword string.
#' Column names of the returned data frame are composed of variable names and SUBIDs, separated by an underscore, 
#' i.e. \code{[variable]_[subid]}. If datetime conversion failed on import, the returned object is a data frame 
#' (i.e. no class \code{HypeXobs}).
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and e.g. converting to string representations during the process).
#' 
#' @examples
#' \dontrun{ReadXobs("Xobs.txt")}
#' 
#' @importFrom data.table fread
#' @export


ReadXobs <- function (filename = "Xobs.txt", dt.format="%Y-%m-%d", variable = NULL, nrows = -1L, verbose = if (nrows %in% 0:2) FALSE else TRUE) {
  
  ## import xobs file header, extract attributes
  # import (3-row header)
  xattr <- readLines(filename,n=3)
  # 1st row, comment
  # split string elements along tabs, returns list of character vectors, enc2utf to catch problems with unknown encoding 
  cmt <- strsplit(enc2utf8(xattr[1]), split = "\t")[[1]]
  # remove empty strings (excel export artefacts)
  cmt <- cmt[!(nchar(cmt) == 0)]
  # cmt <- sapply(cmt, function(x) {te <- nchar(x);te <- ifelse(te == 0, F, T);x[te]})
  # 2nd row, HYPE variable IDs
  hype.var <- toupper(strsplit(xattr[2], split = "\t")[[1]][-1])
  # 3rd row, SUBIDs
  sbd <- as.integer(strsplit(xattr[3], split = "\t")[[1]][-1])
  
  # save number of data columns in file for colClasses formatting below
  ncl <- length(sbd)
  
  # conditional on user choice: identify columns with variables to import
  if (!is.null(variable)) {
    # variables selected
    
    # formatting
    variable <- toupper(variable)
    
    # check if selected variables exist, warn if not
    te <- !(variable %in% hype.var)
    if (any(te)) {
      warning(paste0("User-requested variable(s) ", paste(variable[te], collapse = ", "), " not found in imported file."))
    }
    
    # subset attributes to selection
    selbool <- hype.var %in% variable
    hype.var <- hype.var[selbool]
    sbd <- sbd[selbool]
    
    # create selection vector for import
    sel <- c(1, which(selbool) + 1)
    
  } else {
    # no variables selected
    
    # create selection vector for import (safe version which works with buggy data.table versions where explicit NULL selections don't work)
    sel <- 1:(length(sbd) + 1)
  }
  
  # read the data, skip header and comment rows, force numeric data (automatic column classes can be integer)
  xobs <- fread(filename,  na.strings = "-9999", skip = 3, sep = "\t", header = F, data.table = F, nrows = nrows, 
                colClasses = c("character", rep("numeric", ncl)), select = sel)
  
  # update header, composite of variable and subid
  names(xobs) <- c("DATE", paste(hype.var, sbd, sep = "_"))
  
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
    cat("Date/time conversion attempt led to introduction of NAs, date/times returned as strings.\nImported as data frame, not as 'HypeXobs' object.\n"); return(xobs[, 1])})
  
  
  # if date conversion worked and time steps are HYPE-conform, make returned object class HypeXobs
  if(!is.character(xobs[, 1]) && duplifree) {
    
    # create HypeXobs object, can fail if multi-day time steps in imported table
    xobs <- tryCatch(HypeXobs(x = xobs, comment = cmt, variable = hype.var, subid = sbd, verbose = verbose), 
                     error = function(e) {cat("Longer-than-daily time steps not allowed in HypeXobs objects.\n"); return(xobs)})
    
    # update with additional attributes if HypeXobs class assignment failed
    if (!any(class(xobs) == "HypeXobs")) {
      attr(xobs, which = "comment") <- cmt
      attr(xobs, which = "variable") <- hype.var
      attr(xobs, which = "subid") <- sbd
      warning("Imported as data frame, not as 'HypeXobs' object.")
    }
    
  } else {
    # update with additional attributes if not a HypeXobs object
    attr(xobs, which = "comment") <- cmt
    attr(xobs, which = "variable") <- hype.var
    attr(xobs, which = "subid") <- sbd
    
    warning("Imported as data frame, not as 'HypeXobs' object.")
  }
  
  return(xobs)
}






#--------------------------------------------------------------------------------------------------------------------------------------
# ReadGeoData
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'GeoData.txt' file
#'
#' Import a GeoData file into R.
#' 
#' @param filename Path to and file name of the GeoData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param sep  character string. Field separator character as described in \code{\link{read.table}}.
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link[data.table]{fread}}.

#' @details
#' \code{ReadGeoData} uses \code{\link[data.table]{fread}} from the \code{\link{data.table}} package 
#' with type \code{numeric} type for columns \code{AREA} and \code{RIVLEN} (if they exist), and 
#' upper-case column names.
#' 
#' @return
#' If the imported file is a HYPE-conform GeoData file, \code{ReadGeoData} returns an object of S3 class \code{\link{HypeGeoData}} 
#' (see the class description there), providing its own \code{summary} method. If mandatory GeoData columns are missing, 
#' a standard dataframe is returned along with informative warning messages.
#' 
#' @examples
#' \dontrun{ReadGeoData("GeoData.txt")}
#' 
#' @importFrom data.table fread
#' @export


ReadGeoData <- function(filename = "GeoData.txt", sep = "\t", encoding = c("unknown", "UTF-8", "Latin-1")) {
  
  # argument checks
  encoding <- match.arg(encoding)
  
  # find AREA and RIVLEN columns, to force type numeric (will be autodetected as integer/integer64 otherwise, which leads to 
  # problems with integer calculation in other functions)
  cnames <- strsplit(readLines(con = filename, n = 1), split = sep)[[1]]
  cnumeric <- which(toupper(cnames) %in% c("AREA", "RIVLEN"))
  
  res <- fread(filename, header = T, sep = sep, colClasses = list("numeric" = cnumeric), data.table = F, encoding = encoding)
  names(res) <- toupper(names(res))
  
  # # NOT USED ATM, REPLACED BY colClasses ARGUMENT IN fread. LEFT FOR REFERENCE
  # # force type numeric for selected columns if they exist. Otherwise there can be problem with integer calculation in other functions..
  # te <- which(names(res) == "AREA")
  # if (length(te) == 1) {
  #   res$AREA <- as.numeric(res$AREA)
  # }
  # te <- which(names(res) == "RIVLEN")
  # if (length(te) == 1) {
  #   res$RIVLEN <- as.numeric(res$RIVLEN)
  # }
   
  ## assign HypeGeoData class, check if requirements are met and strip class if not
  
  class(res) <- c("HypeGeoData", "data.frame")
  
  # mandatory columns except SLCs and their positions
  m <- c("AREA", "SUBID", "MAINDOWN", "RIVLEN")
  pos.m <- match(m, names(res))
  
  ## check if SLCs are consecutively numbered and throw warning if not
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(res), 1, 4) == "SLC_")
  # extract numbers
  if (length(pos.s) > 0) {
    suppressWarnings(n.s <- as.numeric(substr(names(res)[pos.s], 5, 99)))
    # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
    pos.s <- pos.s[!is.na(n.s)]
    n.s <- n.s[!is.na(n.s)]
    # sort so that consecutiveness can be tested below
    n.s <- sort(n.s)
    # SLC number increase, tested below, 0 padded to check SLC_1 existence
    dn.s <- diff(c(0, n.s))
  } else {
    n.s <- integer(0)
    dn.s <- integer(0)
  }
  
  if (any(is.na(pos.m))) {
    # warn if mandatory columns are missing
    warning(paste0("Mandatory 'HypeGeoData' column(s) '", paste(m[is.na(pos.m)], collapse = "', '"), "' missing. Imported as 'data.frame'."))
    class(res) <- class(res)[-1]
  }
  
  if (length(pos.s) == 0) {
    # warn if there are no SLC columns
    warning("Mandatory 'HypeGeoData' column(s) 'SLC_n' missing. Imported as 'data.frame'.")
    if (class(res)[1] == "HypeGeoData") {
      class(res) <- class(res)[-1]
    }
  }
  
  if (any(dn.s > 1) || any(dn.s == 0)) {
    # warn if there are SLC classes missing or duplicated
    if (any(dn.s > 1)) {
      te1 <- n.s[dn.s > 1]
      te2 <- sapply(dn.s[dn.s > 1] - 1, function(x) 1:x)
      slc.miss <- sort(unlist(sapply(1:length(te1), function(x, y, z) y[x] - z[[x]], y = te1, z = te2)))
      warning(paste0("SLC class column(s) missing in imported file: ", paste0("SLC_", slc.miss, collapse = ", ")))
    }
    if (any(dn.s == 0)) {
      # warn if there are SLC class duplicates
      warning(paste0("SLC class column duplicate(s) in imported file: ", 
                     paste0("SLC_", n.s[dn.s == 0], " (", dn.s[dn.s == 0], ")", collapse = ", ")))
    }
  }
  
  return(res)
}








#--------------------------------------------------------------------------------------------------------------------------------------
# ReadPar
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'par.txt' file
#'
#' Import a HYPE parameter file as list into R.
#' 
#' @param filename Path to and file name of the parameter file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link{scan}}.
#'  
#' @details
#' \code{ReadPar} checks for inline comments in 'par.txt' files, these are moved to separate "lines" (list elements).
#' 
#' @return
#' \code{ReadPar} returns a list of named vectors. Parameters are returned as numeric vectors with HYPE parameter names as list 
#' element names. Comments are returned in separate list elements as single character strings, former inline comments are moved 
#' to elements preceding the original comment position (i.e. to a line above in the par.txt file structure). Comment elements are 
#' named \code{`!!`}.
#' 
#' @examples
#' \dontrun{ReadPar("par.txt")}
#' 
#' @export

ReadPar <- function (filename = "par.txt", encoding = c("unknown", "UTF-8", "latin1")) {
  
  # argument checks
  encoding <- match.arg(encoding)
  
  ## builds on suggestion found here: http://stackoverflow.com/questions/6602881/text-file-to-list-in-r
  # read par file into a character vector (one string per row in file)
  x <- scan(filename, what = "", sep = "\n", quiet = T, encoding = encoding)
    # insert blank after comment character, to make sure they get split for comment identification below
  x <- gsub(pattern = "!!", replacement = "!!\t", x = x)
  # split string elements along whitespaces, returns list of character vectors
  x <- strsplit(x, split = "[[:space:]]+")
  # assign first vector elements as list element names and convert to lower-case (as standardisation)
  names(x) <- sapply(x, `[[`, 1)
  names(x) <- tolower(names(x))
  # # replace comment row names
  # names(x) <- ifelse(names(x) == "!!", "comment", names(x))
  # remove first vector elements (parameter names)
  x <- lapply(x, `[`, -1)
  
  ## identify inline comments and move to separate list elements (preceding element)
  # list of vector indices in x with comment characters
  te <- sapply(x, function(x){grep(pattern = "!!", x)})
  # initialise result list and result list element counter
  res <- list()
  j <- 1
  for (i in 1:length(te)) {
    # comment characters identification, but omit comment rows
    if (length(te[[i]] > 0) && names(te)[i] != "!!") {
      # copy comment to new result row
      res[[j]] <- x[[i]][(te[[i]][1] + 1):length(x[[i]])]
      names(res)[j] <- "!!"
      j <- j + 1
      # copy parameter value(s) without comments to result list
      res[[j]] <- x[[i]][1:(te[[i]][1] - 1)]
      # update result name
      names(res)[j] <- names(x)[i]
      j <- j + 1
    } else {
      res[[j]] <- x[[i]]
      names(res)[j] <- names(x)[i]
      j <- j + 1
    }
  }
  # convert list elements to numeric, if possible, catch conversion errors and collapse non-numeric vectors to single strings
  res <- lapply(res, function(x) tryCatch(na.fail(as.numeric(x, options("warn" = -1))), error = function(e) paste(x, collapse = " ")))
  
  return(res)
}









#--------------------------------------------------------------------------------------------------------------------------------------
# ReadMapOutput
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a Map Output File
#'
#' This is a convenience wrapper function to import a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:mapxxxx.txt}{map output file} 
#' ('map<\emph{HYPE_output_variable}>.txt') into R.
#' 
#' @param filename Path to and file name of the map output file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}, for conversion of date-time information in column 
#' headers to POSIX dates, which are returned as attribute. Incomplete format strings for monthly and annual values allowed, e.g. 
#' '\%Y'. \strong{Defaults to \code{NULL}, which prevents date-time conversion}, applicable e.g. for files containing just one column of 
#' summary values over the model period.
#' @param hype.var Character string, a four-letter keyword to specify HYPE variable ID of file contents. See 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variables}.
#' If \code{NULL} (default), the variable ID is extracted from the provided file name, which only works for standard HYPE 
#' map output file names.
#' @param type Character, keyword for data type to return. \code{"df"} to return a standard data frame, \code{"dt"} to 
#' return a \code{\link[data.table]{data.table}} object, or \code{"hsv"} to return a \code{\link{HypeSingleVar}} array.
#' @param warn.nan Logical, check if imported results contain any \code{NaN} values. If \code{TRUE} and \code{NaN}s are found, 
#' a warning is thrown and affected SUBIDs saved in an attribute \code{subid.nan}. Adds noticeable overhead to import time for large files.
#' 
#' @details
#' \code{ReadMapOutput} is a convenience wrapper function of \code{\link[data.table]{fread}} from package  
#' \code{\link{data.table}}, 
#' with conversion of date-time strings to POSIX time representations. Monthly and annual time steps are returned as first day 
#' of the time step period.
#' 
#' @return
#' \code{ReadMapOutput} returns a \code{data.frame}, \code{\link{data.table}}, or a \code{\link{HypeSingleVar}} array. 
#' Data frames and data tables contain additional \code{\link{attributes}}: \code{variable}, giving the HYPE variable ID, 
#' \code{date}, a vector of date-times (corresponding to columns from column 2), \code{timestep} with a time step attribute, 
#' and \code{comment} with the first line of the imported file as text string. An additional attribute \code{subid.nan} might be 
#' returned, see argument \code{warn.nan}.
#' 
#' @note
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. 
#' Results will then contain values of '****************'. \code{ReadMapOutput} will convert those cases to 'NA' entries. 
#' 
#' Current versions of HYPE allow for defining significant instead of fixed number of digits, which should prevent this 
#' issue from arising.
#' 
#' @examples
#' \dontrun{ReadMapOutput("mapCOUT.txt", type = "hsv")}
#' 
#' @importFrom data.table fread transpose :=
#' @export

ReadMapOutput <- function(filename, dt.format = NULL, hype.var = NULL, type = "df", warn.nan = FALSE) {
  
  # handling output type user choice
  if (type == "df") {
    d.t <- F
  } else if (type %in% c("dt", "hsv")) {
    d.t <- T
  } else {
    stop(paste("Unknown type", type, "."))
  }
  
  #x <- read.table(filename, header = T, sep = ",", na.strings = "-9999", skip = 1)      
  x <- fread(filename, 
             na.strings = c("-9999", "****************", "-1.0E+04", "-1.00E+04", "-9.999E+03", "-9.9990E+03", "-9.99900E+03", "-9.999000E+03", "-9.9990000E+03", "-9.99900000E+03", "-9.999000000E+03"), 
             skip = 2, sep = ",", header = F, data.table = d.t)
  
  
  # read hype.var from filename, if not provided by user
  if (is.null(hype.var)) {
    hype.var <- strsplit(filename, "map")[[1]]
    hype.var <- substr(hype.var[length(hype.var)], start = 1, stop = 4)
  }
  
  # import comment and dates, prepare date attribute vector
  xattr <- readLines(filename, n = 2)
  xd <- strsplit(xattr[2], split = ",")[[1]][-1]
  
  # create column names
  names(x) <- c("SUBID", paste0("X", gsub(pattern = "-", replacement = ".", x = xd)))
  
  ## update with new attributes to hold POSIX dates and timestep keyword, create from column names
  
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    # temporary copy to fall back to
    te <- xd
    # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
    if (dt.format == "%Y-%m") {
      xd <- tryCatch(na.fail(as.POSIXct(strptime(paste(xd, "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else if (dt.format == "%Y%m") {
      xd <- tryCatch(na.fail(as.POSIXct(strptime(paste(xd, "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else if (dt.format == "%Y") {
      xd <- tryCatch(na.fail(as.POSIXct(strptime(paste(xd, "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    } else {
      xd <- tryCatch(na.fail(as.POSIXct(strptime(xd, format = dt.format, tz = "GMT"))), error = function(e) {
        print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(te)})
    }
  }
  
  # check for existence of NaN values
  if (warn.nan) {
    te <- apply(as.data.frame(x[, -1]), 1, function(x) any(is.nan(x)))
    if (any(te)) {
      warning("'NaN' values found in one or more SUBIDs. SUBIDs saved in attribute 'subid.nan'.")
      attr(x, "subid.nan") <- x[te, 1]
    }
  }
  
  # conditional on user choice: output formatting
  if (type %in% c("dt", "df")) {
    
    attr(x, which = "date") <- xd
    attr(x, "variable") <- toupper(hype.var)
    attr(x, "comment") <- xattr[1]
    
    # conditional: timestep attribute identified by difference between first two entries
    tdff <- tryCatch(as.numeric(difftime(xd[2], xd[1], units = "hours")), error = function(e) {NA})
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
      attr(x, which = "timestep") <- "unknown"
    }
    
  } else {
    ## HypeSingleVar formatting
    # copy and remove subids
    sbd <- x[, SUBID]
    x <- x[, !"SUBID", with = F]
    # transpose and convert to array (straigtht conversion to array gives error, therefore intermediate matrix)
    x <- transpose(x)
    x <- as.array(as.matrix(x))
    # adding 'iteration' dimension
    dim(x) <- c(dim(x), 1)
    x <- HypeSingleVar(x = x, date = xd, subid = sbd, hype.var = toupper(hype.var))
  }
  
  return(x)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadTimeOutput
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a Time Output File
#'
#' Import a time output file 'time<\emph{HYPE_output_variable}>.txt' or a converted time output file in netCDF format 
#' into R.
#' 
#' @param filename Path to and file name of the time output file to import. Acceptable file choices are \code{*.txt} files following 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:timexxxx.txt}{HYPE time output file format} or \code{.nc} 
#' files following the \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_netcdf_standard}{HYPE netCDF formatting standard}. 
#' See also details for netCDF import. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'. If set to \code{NULL}, no date-time conversion will be attempted and the column will
#' be imported as \code{character}, applicable e.g. for files containing just one row of summary values over the model period.
#' @param hype.var Character, HYPE variable ID in \code{x}. See 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{list of HYPE variables}.
#' If \code{NULL} (default), the variable ID is extracted from the provided file name, which only works for standard HYPE 
#' time output file names (incl. regional and class outputs).
#' @param out.reg Logical, specify if file contents are sub-basin or output region results (i.e. SUBIDs or OUTREGIDs as columns). 
#' \code{TRUE} for output regions, \code{FALSE} for sub-basins. \strong{Use only in combination with user-provided \code{hype.var} 
#' argument.} 
#' @param type Character, keyword for data type to return. \code{"df"} to return a standard data frame, \code{"dt"} to 
#' return a \code{\link[data.table]{data.table}} object, or \code{"hsv"} to return a \code{\link{HypeSingleVar}} array.
#' @param select Integer vector, column numbers to import. Note: first column with dates must be imported and will be added if missing.
#' @param id Integer vector, HYPE SUBIDs/OUTREGIDs to import. Alternative to argument \code{select}, takes precedence if both are provided.
#' @param nrows Integer, number of rows to import, see documentation in \code{\link[data.table]{fread}}.
#' @param skip Integer, number of \strong{data} rows to skip on import. Time output header lines are always skipped. 
#' @param warn.nan Logical, check if imported results contain any \code{NaN} values. If \code{TRUE} and \code{NaN}s are found, 
#' a warning is thrown and affected IDs saved in an attribute \code{id.nan}. Adds noticeable overhead to import time for large files.
#' @param verbose Logical, print information during import.
#' 
#' @details
#' \code{ReadTimeOutput} imports from text or netCDF files. Text file import uses \code{\link[data.table]{fread}} from package  
#' \code{\link{data.table}}, netCDF import extracts data and attributes using functions from package \code{\link[ncdf4:nc_open]{ncdf4}}. 
#' Date-time representations in data files are converted to POSIX time representations. Monthly and annual time steps are returned as 
#' first day of the time step period. 
#' 
#' Import from netCDF files requires an \code{id} dimension in the netCDF data. Gridded data with remapped HYPE results in spatial x/y 
#' dimensions as defined in the \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_netcdf_standard}{HYPE netCDF formatting standard} 
#' are currently not supported. 
#' 
#' @return
#' \code{ReadTimeOutput} returns a \code{data.frame}, \code{\link{data.table}}, or a \code{\link{HypeSingleVar}} array. 
#' Data frames and data tables contain additional \code{\link{attributes}}: \code{variable}, giving the HYPE variable ID, 
#' \code{subid} and \code{outregid}, the HYPE SUBIDs/OUTREGIDs (corresponding to columns from column two onwards) to which the time 
#' series belong (both attributes always created and assigned \code{NA} if not applicable to data contents), \code{timestep} with a 
#' time step attribute, and \code{comment} with first row comment of imported file as character string. An additional attribute 
#' \code{id.nan} might be returned, see argument \code{warn.nan}.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and possibly converting to string representations during the process).
#' 
#' HYPE results are printed to files using a user-specified accuracy. This accuracy is specified in 'info.txt' as a number of 
#' decimals to print. If large numbers are printed, this can result in a total number of digits which is too large to print. 
#' Results will then contain values of '****************'. \code{ReadTimeOutput} will convert those cases to 'NA' entries.
#' 
#' Current versions of HYPE allow for defining significant instead of fixed number of digits, which should prevent this 
#' issue from arising.
#' 
#' @examples 
#' \dontrun{ReadTimeOutput("timeCCIN.txt", dt.format = "%Y-%m")}
#' 
#' @importFrom data.table fread is.data.table
#' @importFrom ncdf4 nc_open nc_close
#' @export

ReadTimeOutput <- function(filename, dt.format = "%Y-%m-%d", hype.var = NULL, out.reg = NULL, type = c("df", "dt", "hsv"), 
                           select = NULL, id = NULL, nrows = -1L, skip = 0L, warn.nan = FALSE, verbose = T) {
  
  # check file type to import
  ftype <- tolower(substr(filename, nchar(filename) - 2, nchar(filename)))
  if (ftype == "txt") {
    nc <- F
  } else if (ftype == ".nc") {
    nc <- T
  } else {
    stop("Unknown file extension. Accepted file types are standard HYPE text files (*.txt), or netcdf 
         files following the HYPE standard (*.nc).")
  }
  
  # argument checks
  if (!is.null(select) && !(1 %in% select)) {
    select <- c(1, select)
  }
  
  # handling output type user choice
  type <- match.arg(type)
  if (type == "df") {
    d.t <- F
  } else {
    d.t <- T
  }
  
  
  ## data import, conditional on file type
  
  if (nc) {
    
    ## import from netCDF file
    
    # open netCDF file connection
    ncf <- nc_open(filename = filename)
    
    nm.var <- names(ncf$var)
    nm.dim <- names(ncf$dim)
    
    "id" %in% nm.dim
    
    # close netCDF file connection
    nc_close(ncf)
    
  } else {
    
    # import from text file
    
  }
  
  # import subids/outregids, prepare attribute vector
  xattr <- readLines(filename, n = 2)
  sbd <- as.numeric(strsplit(xattr[2], split = "\t")[[1]][-1])
  
  # create select vector from 'id' argument, overrides 'select' argument
  if (!is.null(id)) {
    if (!is.null(select)) {
      warning("Arguments 'select' and 'id' provided. 'id' takes precedence.")
    }
    te <- match(id, sbd)
    # stop if unknown ids provided by user
    if (any(is.na(te))) {
      stop(paste0("Argument 'id': IDs ", paste(id[is.na(te)], collapse = ", "), " not found in imported file."))
    }
    select <- c(1, te + 1)
    sbd <- id
  } else if (!is.null(select)) {
    # update id attribute to selected ids
    sbd <- sbd[select[-1] - 1]
  }
  
  
  
  # create full select vector for fread, workaround for suspected bug in data.table (reported at https://github.com/Rdatatable/data.table/issues/2007)
  if (is.null(select) && is.null(id)) {
    select <- 1:(length(sbd) + 1)
  }
  
  # read.table(filename, header = T, na.strings = "-9999", skip = 1)      
  x <- fread(filename, 
             na.strings = c("-9999", "****************", "-1.0E+04", "-1.00E+04", "-9.999E+03", "-9.9990E+03", "-9.99900E+03", 
                            "-9.999000E+03", "-9.9990000E+03", "-9.99900000E+03", "-9.999000000E+03"), 
             skip = 2 + skip, sep = "\t", header = F, data.table = d.t, select = select, nrows = nrows)
  
  
  # read hype.var from filename, if not provided by user
  if (is.null(hype.var)) {
    
    # extract filename from path-filename string
    te <-strsplit(filename, "/")[[1]]
    te <- strsplit(te[length(te)], "[.]")[[1]][1]
    
    # check if standard HYPE file name, and extract variable ID
    if (substr(te, 1, 4) == "time") {
      
      ## HYPE variable string should be in the eight characters after "time" prefix (max. 6 char ID + optional "RG" for regional outputs)
      # extract string to extract from, inbuilt lookup vector is uppercase
      te <- toupper(substr(te, 5, 12))
      
      # check against lookup vector
      if (te %in% INTERNAL.hype.vars) {
        
        # exists as is, normal SUBID results
        hype.var <- te
        out.reg <- F
        
      } else if (substr(te, 1, 6) %in% INTERNAL.hype.vars) {
        
        # exists, normal SUBID results
        hype.var <- substr(te, 1, 6)
        out.reg <- F
      }
      else if (substr(te, 1, 4) %in% INTERNAL.hype.vars) {
        
        # exists, normal SUBID results
        hype.var <- substr(te, 1, 4)
        out.reg <- F
      }
      else if (substr(te, 3, 8) %in% INTERNAL.hype.vars) {
        
        # exists, is a regional variable
        hype.var <- substr(te, 3, 8)
        out.reg <- T
      }
      else if (substr(te, 3, 6) %in% INTERNAL.hype.vars) {
        
        # exists, is a regional variable
        hype.var <- substr(te, 3, 6)
        out.reg <- T
      } else {
        
        stop("Could not extract HYPE variable ID from filename, please provide arguments 'hype.var' and 'out.reg'.")
      }
      
    } else {
      
      stop("Could not extract HYPE variable ID from filename, please provide arguments 'hype.var' and 'out.reg'.")
    }
  } else {
    
    # check that there a valid value for out.reg
    if (is.null(out.reg) || !is.logical(out.reg)) {
      stop("Please provide valid value for argument 'out.reg'.")
    }
    
    # convert user-provided string to uppercase and warn if not in lookup vector
    hype.var <- toupper(hype.var)
    if (!(hype.var %in% INTERNAL.hype.vars)) {
      warning("User-provided HYPE variable ID unknown. Proceeding to import.")
    }
    
  }
  
  # create column names, special treatment if only dates are imported
  if (length(sbd) == 0) {
    names(x) <- c("DATE")
  } else {
    names(x) <- c("DATE", paste0("X", sbd))
  }
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format. On error, strings are returned.
  
  # if user-requested, hop over date-time conversion
  if (!is.null(dt.format)) {
    
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
          print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
      }
    }
  } else {
    # dummy date vector as there is always one needed in timestep attribute derivation below
    xd <- NA
  }
  
  
  # check for existence of NaN values
  if (warn.nan) {
    te <- apply(as.data.frame(x[, -1]), 2, function(x) any(is.nan(x)))
    if (any(te)) {
      warning("'NaN' values found in one or more IDs. IDs saved in attribute 'id.nan'.")
      attr(x, "id.nan") <- sbd[te]
    }
  }
  
  # conditional on user choice: output formatting
  if (type %in% c("dt", "df")) {
    
    if (out.reg) {
      attr(x, which = "subid") <- NA
      attr(x, which = "outregid") <- sbd
    } else {
      attr(x, which = "subid") <- sbd
      attr(x, which = "outregid") <- NA
    }
    attr(x, "variable") <- hype.var
    attr(x, "comment") <- xattr[1]
    
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
      attr(x, which = "timestep") <- "unknown"
    }
    return(x)
    
  } else {
    ## HypeSingleVar formatting
    # remove dates
    x <- x[, !"DATE", with = F]
    # convert to array (straigtht conversion to array gives error, therefore intermediate matrix)
    x <- as.array(as.matrix(x))
    # adding 'iteration' dimension
    dim(x) <- c(dim(x), 1)
    x <- HypeSingleVar(x = x, date = xd, subid = sbd, hype.var = toupper(hype.var))
    # construct HypeSingleVar array, conditional on subid/outregid contents
    if (out.reg) {
      x <- HypeSingleVar(x = x, date = xd, outregid = sbd, hype.var = toupper(hype.var))
    } else {
      x <- HypeSingleVar(x = x, date = xd, subid = sbd, hype.var = toupper(hype.var))
    }
    
  }
  
  return(x)
}

## DEBUG
# filename <- "../timeCCTN.txt"
# dt.format <- "%Y"







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadPTQobs
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read 'Pobs.txt', 'Tobs.txt', 'Qobs.txt', and other observation data files
#'
#' Import forcing data and discharge observation files as data frame into R.
#' 
#' @param filename Path to and file name of the file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param variable Character string, HYPE variable ID of file contents. If \code{""} (default), the ID is extracted 
#' from \code{filename}, which only works with file names as required by HYPE or file names beginning with those names 
#' ('e.g. Pobs_old.txt').
#' @param nrows Number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives the number of rows
#' to import.
#' @param type Character, keyword for data type to return. \code{"df"} to return a standard data frame or \code{"dt"} to 
#' return a \code{\link[data.table]{data.table}} object.
#' @param select Integer vector, column numbers to import. Note: first column with dates must be imported and will be added if missing.
#' @param obsid Integer vector, HYPE OBSIDs to import. Alternative to argument \code{select}, takes precedence if both are provided.
#'  
#' @details
#' \code{ReadPTQobs} is a convenience wrapper function of \code{\link[data.table]{fread}} from package  
#' \code{\link{data.table}}, 
#' with conversion of date-time strings to POSIX time representations. Observation IDs (SUBIDs or IDs connected to SUBIDs with a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:forckey.txt}{ForcKey.txt file}) are returned as integer 
#' attribute \code{obsid} (see \code{\link{attr}} on how to access it). 
#' 
#' @return
#' \code{ReadPTQobs} returns a data frame with additional attributes: \code{obsid} with observation IDs, \code{timestep} with a time 
#' step string, either \code{"day"} or \code{"nhour"} (only daily or n-hourly time steps supported), and \code{variable} wih a HYPE 
#' variable ID string.
#' 
#' @note
#' For the conversion of date/time strings, time zone "GMT" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and e.g. converting to string representations during the process).
#' 
#' @seealso 
#' \code{\link{WritePTQobs}}
#' \code{\link{ReadXobs}}
#' 
#' @examples
#' \dontrun{ReadPTQobs("Tobs.txt")}
#' 
#' @importFrom data.table fread
#' @export


ReadPTQobs <- function(filename, variable = c("", "prec", "temp", "rout"), dt.format = "%Y-%m-%d", nrows = -1, 
                       type = "df", select = NULL, obsid = NULL) {
  
  # extract hype variable
  variable <- match.arg(variable)
  if (variable == "") {
    te <- strsplit(filename, split = c("/"))[[1]]
    te <- tolower(substr(te[length(te)], 1, 4))
    if (te == "qobs") {
      variable <- "rout"
    } else if (te == "pobs") {
      variable <- "prec"
    } else if (te == "tobs") {
      variable <- "temp"
    } else {
      stop("Unable to extract HYPE variable name from file name, please provide argument 'variable'.")
    }
  }
  
  ## import ptqobs file header, extract obsid attribute
  # import
  xattr <- readLines(filename,n = 1)
  # extract obsids
  sbd <- as.integer(strsplit(xattr, split = "\t")[[1]][-1])
  
  # argument checks
  if (!is.null(select) && !(1 %in% select)) {
    select <- c(1, select)
  }
  
  # handling output type user choice
  if (type == "df") {
    d.t <- F
  } else if (type == "dt") {
    d.t <- T
  } else {
    stop(paste("Unknown type", type, "."))
  }
  
  # create select vector from 'obsid' argument, overrides 'select' argument
  if (!is.null(obsid)) {
    if (!is.null(select)) {
      warning("Arguments 'select' and 'obsid' provided. 'obsid' takes precedence.")
    }
    te <- match(obsid, sbd)
    # stop if unknown obsids provided by user
    if (any(is.na(te))) {
      stop(paste0("Argument 'obsid': OBSIDs ", paste(obsid[is.na(te)], collapse = ", "), " not found in imported file."))
    }
    select <- c(1, te + 1)
    sbd <- obsid
  } else if (!is.null(select)) {
    # update obsid attribute to selected obsids
    sbd <- sbd[select[-1] - 1]
  }
  
  
  # create full select vector for fread, workaround for suspected bug in data.table (reported at https://github.com/Rdatatable/data.table/issues/2007)
  if (is.null(select) && is.null(obsid)) {
    select <- 1:(length(sbd) + 1)
  }
  
  # read the data
  x <- fread(filename, 
             na.strings = c("-9999", "****************", "-1.0E+04", "-1.00E+04", "-9.999E+03", "-9.9990E+03", "-9.99900E+03", "-9.999000E+03", "-9.9990000E+03", "-9.99900000E+03", "-9.999000000E+03"), 
             sep = "\t", header = T, data.table = d.t, nrows = nrows, select = select)

  # date conversion 
  xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
  x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
    print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])
    })
  
  
  ## add attributes
  
  attr(x, which = "obsid") <- sbd
  
  # conditional: timestep attribute identified by difference between first two entries
  tdff <- as.numeric(difftime(xd[2], xd[1], units = "hours"))
  if (!is.na(tdff)) {
    if (tdff == 24) {
      attr(x, which = "timestep") <- "day"
    } else {
      attr(x, which = "timestep") <- paste(tdff, "hour", sep = "")
    }
  } else {
    # add timestep attribute with placeholder value
    attr(x, which = "timestep") <- "unknown"
  }
  attr(x, "variable") <- variable
  
  return(x)
}









#--------------------------------------------------------------------------------------------------------------------------------------
# HypeDataImport
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read HYPE data files
#'
#' These are simple convenience wrapper functions to import various HYPE data files as data frame into R.
#' 
#' @param filename Path to and file name of HYPE data file file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param verbose Logical, display message if columns contain \code{NA} values.
#' @param header \code{\link{read.table}} argument, with appropriate default for HYPE data file import.
#' @param na.strings See \code{header}.
#' @param sep See \code{header}.
#' @param stringsAsFactors See \code{header}.
#' @param quote See \code{header}.
#' @param encoding \code{\link{read.table}} argument. Specify character encoding when 
#' importing files created under Windows (default encoding "latin1") in Linux (default encoding "UTF-8") 
#' and vice versa.
#' @param ... Other parameters passed to \code{\link{read.table}}. 
#'  
#' @details
#' Hype data file imports, simple \code{\link{read.table}} wrappers with formatting arguments set to match HYPE file 
#' specifications:
#' 
#' \itemize{
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:lakedata.txt}{LakeData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:damdata.txt}{DamData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:mgmtdata.txt}{MgmtData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:aquiferdata.txt}{AquiferData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:pointsourcedata.txt}{PointSourceData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:glacierdata.txt}{GlacierData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:cropdata.txt}{CropData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:branchdata.txt}{BranchData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:outregions.txt}{Outregions.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:allsim.txt}{allsim.txt}
#' }
#' 
#' In most files, HYPE requires \code{NA}-free input in required columns, but empty values are 
#' allowed in additional comment columns. Informative warnings will be thrown if \code{NA}s are found during import.
#' 
#' @return
#' Imported files are returned as data frames.
#' 
#' @examples
#' \dontrun{ReadLakeData("LakeData.txt")}
#' \dontrun{ReadDamData("DamData.txt")}
#' \dontrun{ReadMgmtData("MgmtData.txt")}
#' 
#' @name HypeDataImport

NULL

#' @rdname HypeDataImport
#' @export
ReadAquiferData <- function(filename = "AquiferData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                            stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  # import
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadOutregions <- function(filename = "Outregions.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                            stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  # import
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadBranchData <- function(filename = "BranchData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                           stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadCropData <- function(filename = "CropData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                         stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadDamData <- function(filename = "DamData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                        quote = "", stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, quote = quote, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadGlacierData <- function(filename = "GlacierData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                            stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadLakeData <- function(filename = "LakeData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                         quote = "", stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, quote = quote, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadMgmtData <- function(filename = "MgmtData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                         stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  # import
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadPointSourceData <- function(filename = "PointSourceData.txt", verbose = T, header = T, na.strings = "-9999", sep = "\t", 
                                stringsAsFactors = F, encoding = c("unknown", "latin1", "UTF-8"), ...) {
  # argument checks
  encoding <- match.arg(encoding)
  
  res <- read.table(file = filename, header = header, na.strings = na.strings, sep = sep, 
                    stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  names(res) <- toupper(names(res))
  # check for NAs
  te <- apply(res, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) message(paste("NA values in imported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  return(res)
}

#' @rdname HypeDataImport
#' @export
ReadAllsim <- function(filename = "allsim.txt") {
  read.table(file = filename, header = T, sep = ",")
}

#' @rdname HypeDataImport
#' @export
ReadForcKey <- function(filename = "ForcKey.txt", sep = "\t", encoding = c("unknown", "latin1", "UTF-8")) {
  
  # argument checks
  encoding <- match.arg(encoding)
  
  read.table(file = filename, header = T, sep = sep, encoding = encoding)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadPmsf
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'pmsf.txt' file
#'
#' @description
#' This is a small convenience function to import a 'partial model setup file' as integer vector into R.
#' 
#' @param filename Path to and file name of the pmsf file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{ReadPmsf} imports 'pmsf.txt' files, which contain SUBIDs and are used to run only parts of a HYPE setup's domain 
#' without having to extract a separate model setup. For details on the file format, see the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:pmsf.txt}{pmsf.txt online documentation}.
#' Pmsf.txt files imported with \code{ReadPmsf} are stripped from the first value containing the total number of subcatchments 
#' in the file. No additional attribute is added to hold this number since it can be easily obtained using \code{\link{length}}.
#' 
#' @return
#' \code{ReadPmsf} returns an integer vector.
#' 
#' @examples
#' \dontrun{ReadPmsf("pmsf.txt")}
#' 
#' @export

ReadPmsf <- function(filename = "pmsf.txt") {
  x <- read.table(filename, header = T)
  x <- as.integer(x[, 1])
  return(x)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadOptpar
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read an 'optpar.txt' file
#' 
#' This function imports an 'optpar.txt' into a list.
#' 
#' @param filename Path to and file name of the 'optpar.txt' file to import. 
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link{scan}}.
#' 
#' @details 
#' \code{ReadOptpar} imports HYPE 'optpar.txt' files. Optpar files contain instructions for parameter calibration/optimisation 
#' and parameter value ranges, for details on the file format, see the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:optpar.txt}{optpar.txt online documentation}. 
#' 
#' @return 
#' \code{ReadOptpar} returns a \code{\link{list}} object with three elements: \itemize{ 
#' \item \code{comment}, the file's first-row comment string.
#' \item \code{tasks}, a two-column dataframe with row-wise key-value pairs for tasks and settings.
#' \item \code{pars}, a list of dataframes, each containing values for one parameter. Three columns each, holding parameter 
#' range minima, maxima, and intervals. 
#' The number of rows in each dataframe corresponds to the number of soil or land use classes for class-specific parameters. 
#' Parameter names as list element names.
#' }
#' 
#' @seealso \code{\link{ReadPar}}

#' @examples
#' \dontrun{ReadOptpar("optpar.txt")}
#' 
#' @export

ReadOptpar <- function(filename = "optpar.txt", encoding = c("unknown", "UTF-8", "latin1")) {
  
  # argument checks
  encoding <- match.arg(encoding)
  
  # read tasks and settings into a character vector (one string per row in file)
  tasks <- scan(filename, what = "", sep = "\n", nlines = 21, quiet = TRUE, encoding = encoding)
  # split string elements along whitespaces, returns list of character vectors
  tasks <- strsplit(tasks, split = "[[:space:]]+")
  # re-merge and separate first-row comment string
  comm <- paste(tasks[[1]], collapse = " ")
  #remove comment from tasks
  tasks <- tasks[-1]
  # convert tasks and settings to two-column dataframe (they are always combinations of single key single value)
  te <- function(x) {rbind.data.frame(x, stringsAsFactors = FALSE)}
  tasks <- do.call(rbind.data.frame, c(tasks, stringsAsFactors = FALSE))
  names(tasks) <- c("key", "value")
  
  # read parameters
  x <- scan(filename, what = "", sep = "\n", skip = 21, quiet = TRUE)
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
  
  return(list(comment = comm, tasks = tasks, pars = pars))
}







#--------------------------------------------------------------------------------------------------------------------------------------
# ReadSubass
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'subassX.txt' file
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
#' file is imported as additional \code{\link{attributes}} \code{variables}, the evaluation time step in an attribute 
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








#--------------------------------------------------------------------------------------------------------------------------------------
# ReadDescription
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'description.txt' file
#'
#' Read a 'description.txt' file as \code{list} object into R. A 'description.txt' file contains land use, soil, and crop 
#' class names of a HYPE set-up, as well as model set-up name and version. 
#' 
#' @param filename Path to and file name of the 'description.txt' file to import. 
#' @param gcl dataframe, GeoClass.txt file imported with \code{\link{ReadGeoClass}} to compare class IDs with. 
#' A warning will be thrown if not all class IDs in \code{gcl} exist in the description file.
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link{scan}}.
#'  
#' @details
#' \code{ReadDescription} imports a 'description.txt' into R. This file is not used by HYPE, but is convenient for 
#' e.g. plotting legend labels or examining imported GeoClass files. E.g., \code{\link{PlotBasinSummary}} requires a list 
#' as returned from \code{ReadDescription} for labeling. 
#' 
#' A 'description.txt' file consists of 22 lines, alternating names and semicolon-separated content. Lines 
#' with names are not read by the import function, they just make it easier to compose and read the actual text file.
#' 
#' File contents read by \code{ReadDescription}: 
#' \itemize{
#'  \item HYPE set-up name (line 2)
#'  \item HYPE set-up version (line 4)
#'  \item Land use class IDs (line 6)
#'  \item Land use class names (line 6)
#'  \item Land use class short names (line 8)
#'  \item Soil class IDs (line 10)
#'  \item Soil class names (line 10)
#'  \item Soil class short names (line 12)
#'  \item Crop class IDs (line 14)
#'  \item Crop class names (line 14)
#'  \item Crop class short names (line 16)
#' }
#' 
#' Note that Crop class IDs start from \code{0}, which means no crop, whereas land use and soil IDs start from \code{1} (or higher).
#' 
#' Formatting example for description.txt files: 
#' 
#' \code{# Name} \cr
#' \code{MyHYPE} \cr
#' \code{# Version} \cr
#' \code{0.1} \cr
#' \code{# Land use class IDs} \cr
#' \code{1;2} \cr
#' \code{# Land use class names} \cr
#' \code{Agriculture;Coniferous forest} \cr
#' \code{# Short land use class names} \cr
#' \code{Agric.;Conif. f.} \cr
#' \code{# Soil class IDs} \cr
#' \code{1;2} \cr
#' \code{# Soil class names} \cr
#' \code{Coarse soils;Medium to fine soils} \cr
#' \code{# Short soil class names} \cr
#' \code{Coarse;Medium} \cr
#' \code{# Crop class IDs} \cr
#' \code{0;1;2} \cr
#' \code{# Crop class names} \cr
#' \code{None;Row crops;Autumn-sown cereal} \cr
#' \code{# Short crop class names} \cr
#' \code{None;Row;Aut.-sown} \cr
#' 
#' @return
#' \code{ReadDescription} returns a named list with 11 named character elements, corresponding to the 
#' imported lines:
#' 
#' \code{Name}, \code{Version}, \code{lu.id}, \code{Landuse}, \code{lu} (short names), \code{so.id}, 
#' \code{Soil}, \code{so} (short names), \code{cr.id}, \code{Crop}, \code{cr} (short names)
#' 
#' @examples
#' \dontrun{ReadDescription("description.txt")}
#'
#' @export 

ReadDescription <- function(filename, gcl = NULL, encoding = c("unknown", "UTF-8", "latin1")) {
  
  # argument checks
  encoding <- match.arg(encoding)
  
  ## builds on suggestion found here: http://stackoverflow.com/questions/6602881/text-file-to-list-in-r
  # read description file into a character vector (one string per row in file)
  x <- scan(file = filename, what = "", sep = "\n", quiet = T, encoding = encoding)
  # split string elements along semicolons, returns list of character vectors
  x <- strsplit(enc2utf8(x), split = ";", useBytes = F)
  # remove empty strings (excel export artefacts)
  x <- sapply(x, function(x) {te <- nchar(x);te <- ifelse(te == 0, F, T);x[te]})
  # create result list, assign names
  res <- x[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)]
  names(res) <- c("Name", "Version", "lu.id", "Landuse", "lu", "so.id", "Soil", "so", "cr.id", "Crop", "cr")
  
  # convert IDs to numeric
  res$lu.id <- as.numeric(res$lu.id)
  res$so.id <- as.numeric(res$so.id)
  res$cr.id <- as.numeric(res$cr.id)
  
  # check conformity of names and short names
  if (length(res$Landuse) != length(res$lu) || length(res$Landuse) != length(res$lu.id)) {
    warning("Diffent numbers of IDs, names, or short names for land uses in imported file.")
  }
  if (length(res$Soil) != length(res$so) || length(res$Soil) != length(res$so.id)) {
    warning("Diffent numbers of IDs, names, or short names for soils in imported file.")
  }
  if (length(res$Crop) != length(res$cr) || length(res$Crop) != length(res$cr.id)) {
    warning("Diffent numbers of IDs, names, or short names for crops in imported file.")
  }
  
  # check conformity with geoclass if provided, missing classes allowed in gcl, but not in description
  if (!is.null(gcl)) {
    te <- unique(gcl[, 2])
    if (any(!(te %in% res$lu.id))) {
      warning("Not all land use classes IDs in 'gcl' present in description file.")
    }
    te <- unique(gcl[, 3])
    if (any(!(te %in% res$so.id))) {
      warning("Not all soil classes IDs in 'gcl' present in description file.")
    }
    # crop classes in two columns
    te <- unique(c(gcl[, 4], gcl[, 5]))
    if (any(!(te %in% res$cr.id))) {
      warning("Not all crop classes IDs in 'gcl' present in description file.")
    }
  }
  return(res)
}






#--------------------------------------------------------------------------------------------------------------------------------------
# ReadSimass
#--------------------------------------------------------------------------------------------------------------------------------------


#' Read a 'simass.txt' file
#'
#' This is a convenience wrapper function to import an simass.txt simulation assessment file as data frame into R.
#' Simulation assessment files contain domain-wide aggregated performance criteria results, as defined in 'info.txt'.
#' 
#' @param filename Path to and file name of the 'simass.txt' file to import. 
#'  
#' @details
#' \code{ReadSimass} imports a simulation assessement file into R. Evaluated variables and other metadata are stored as additional 
#' \code{\link{attributes}}.
#' 
#' The function interpretes character-coded time steps (e.g. \code{"DD"} for daily time steps), as used in some HYPE versions. 
#' Sub-daily time steps are currently not treated and will probably result in a warning during time step evaluation within the 
#' function. Please update issue #106 on \href{https://github.com/rcapell/HYPEtools} if you need support for sub-daily time steps!
#' 
#' @return
#' \code{ReadSubass} returns a data frame with column-wise performance measure values of evaluated variable pairs and performance 
#' measure names in the first column. Additional attributes: 
#' \itemize{
#' \item{\code{n.data.regional}, \code{numeric} vector with number of data points used in regional criteria (optional, depending on 
#' HYPE version)}
#' \item{\code{n.area.mean}, \code{numeric} vector with number of subbasins used in mean and median criteria (optional, depending on 
#' HYPE version)}
#' \item{\code{variable.obs}, \code{character} vector with HYPE variable ID(s) used as observed variable}
#' \item{\code{variable.sim}, \code{character} vector with HYPE variable ID(s) used as simulated variable}
#' \item{\code{agg.period}, \code{numeric} vector with integer code(s) for time period aggregation used in evaluation. Same format 
#' as used in info.txt files, i.e. 1 = daily, 2 = weekly, 3 = monthly, 4 = annual.}
#' \item{\code{n.simulation}, \code{integer}, simulation number (e.g. with Monte Carlo simulations)}
#' \item{\code{crit.total}, \code{numeric}, total criteria value}
#' \item{\code{crit.conditional}, \code{numeric}, conditional criteria value}
#' \item{\code{threshold}, \code{integer}, data limit threshold}
#' }
#' 
#' @examples
#' \dontrun{ReadSimass("simass.txt")}
#'
#' @export 


ReadSimass <- function(filename = "simass.txt") {
  
  # file header with simulation info
  fhead <- readLines(con = filename, n = 5)
  
  # determine number of header rows
  if(nchar(gsub(" ", "", fhead[5])) == 0) {
    n.head <- 5
  } else {
    n.head <- 4
  }
  
  # import data content
  te <- read.table(file = filename, sep = ":", fill = T, skip = n.head, header = F, stringsAsFactors = F, blank.lines.skip = F)


  # calculate number of performance measures and variable pairs for which performance measures are present (different numbers of 
  # perf meas possible, depending on HYPE version)
  # conditional: treat case with only 1 variable pair separately (diff() does not work then)
  te.perf <- which(gsub(pattern = "[[:blank:]]", replacement = "", x = te[, 1]) == "Variables")
  if (length(te.perf) == 1) {
    n.perf <- nrow(te)
  } else {
    n.perf <- diff(te.perf)[1]
  }
  n.var <- nrow(te) / n.perf
  
  # extract performance measures for each variable pair into data frame
  res <- data.frame(matrix(unlist(tapply(te[, 2], rep(1:n.var, each = n.perf), 
                                         function(x) {te <- as.numeric(c(x[3:(n.perf - 1)])); ifelse(te == -9999, NA, te)})), 
                           nrow = n.perf - 3, byrow = F))
  
  
  # add performance measure name as first column, strip from superfluous space characters along the way
  res <- cbind(NAME = gsub(pattern = "^ *|(?<= ) | *$", replacement = "", te[, 1][3:(n.perf - 1)], perl = TRUE), res)
  
  
  # move data numbers for regional and mean/median criteria to attribute vectors, if they exist
  if (res[nrow(res) - 1, 1] == "Number of data for regional criterion") {
    attr(res, "n.data.regional") <- as.numeric(res[nrow(res) - 1, -1])
    res <- res[-(nrow(res) - 1), ]
  }
  
  if (res[nrow(res), 1] == "Number of areas in mean/median criterion") {
    attr(res, "n.area.mean") <- as.numeric(res[nrow(res), -1])
    res <- res[-(nrow(res)), ]
  }
  
  
  # add HYPE variables as attributes and column names
  hvar <- strsplit(gsub(pattern = "^ *", replacement = "", te[, 2][seq(from = 1, length.out = n.var, by = n.perf)]), ", ")
  hvar <- matrix(toupper(unlist(hvar)), ncol = 2, byrow = T)
  attr(res, "variable.obs") <- hvar[, 1]
  attr(res, "variable.sim") <- hvar[, 2]
  names(res)[-1] <- apply(hvar, 1, paste, collapse = ".")
  
  
  ## add aggregation periods as attributes, with conversion of letter code to standard HYPE numeric codes (as used in info.txt)
  
  # extract information
  agg.period <- te[, 2][seq(from = 2, length.out = n.var, by = n.perf)]
  
  # conditional: check if periods are given as numeric, and if not, try to convert from predefined character codes, 
  #              as last option return whatever was found in file and return a warning
  
  if (any(is.na(suppressWarnings(as.numeric(agg.period))))) {
    
    # periods are coded as characters, try to convert to numeric code
    te.agg <- gsub(pattern = "^ *", replacement = "", agg.period)
    if (all(te.agg %in% c("DD", "WW", "MM", "YY"))) {
      te.agg <- factor(te.agg, levels = c("DD", "WW", "MM", "YY"))
      levels(te.agg) <- 1:4
      te.agg <- as.numeric(te.agg)
      attr(res, "agg.period") <- te.agg
    } else {
      attr(res, "agg.period") <- te.agg
      warning("Aggregation period code unknown. Returned as imported in attribute 'agg.period'.")
    }
  } else {
    # periods are coded numerically, just return the numbers
    attr(res, "agg.period") <- as.numeric(agg.period)
  }
  
  
  ## add general simulation attributes from simass.txt file header
  
  # simulation number (in case of monte carlo results)
  attr(res, "n.simulation") <- as.numeric(substr(fhead[2], 20, 100))
  
  # conditional on number of header rows
  if (n.head == 5) {
    te.head <- as.numeric(na.omit(suppressWarnings(as.numeric(strsplit(paste(fhead[3:4], collapse = " "), split = " +|,")[[1]]))))
  } else {
    te.head <- as.numeric(na.omit(suppressWarnings(as.numeric(strsplit(fhead[3], split = " +|,")[[1]]))))
  }
  
  # total criteria value
  attr(res, "crit.total") <- te.head[1]
  # conditional criteria value
  attr(res, "crit.conditional") <- te.head[2]
  # threshold
  attr(res, "threshold") <- te.head[3]
  
  return(res)
  
}
