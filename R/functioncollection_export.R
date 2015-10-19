#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of export functions, herein:
#
#     - WritePar()
#     - WriteGeoData()
#     - WriteGeoClass()
#     - WriteBranchData()
#     - WriteXobs()
#     - WriteBasinOutput()
#     - WriteTimeOutput()
#     - WriteLakeData()
#     - WritePmsf()
#     - WritePTQobs()
#     - WriteMgmtData()
#     - WriteAquiferData()
#     - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WritePar~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'par.txt' File
#'
#' @description
#' \code{WritePar} prints its required argument \code{x} to a file.
#' 
#' @param x The object to be written, a list with named vector elements, as an object returned from \code{\link{ReadPar}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param digits Integer, number of significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#' 
#' @details
#' \code{WritePar} writes a 'par.txt' file, typically originating from an imported and modified 'par.txt'.
#' 
#' @examples
#' \dontrun{WritePar(mypar)}
#' 



WritePar <- function (x, filename = "par.txt", digits = 10, nsmall = 1) {
  
  # format par list contents to avoid scientific format in output
  fx <- sapply(x, format, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T, justify = "none")
  
  # write formatted list elements to file, first converts all list elements (vectors) and their names to concatenated strings.
  write(sapply(seq_along(x), function(x, y) paste(c(names(y)[x], y[[x]]), collapse="\t"), y = fx), filename)
  
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteGeoData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'GeoData.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a 'GeoData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadGeoData}}. 
#' No \code{NA}s in parameter values allowed.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param digits Integer, number of significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#'  
#' @details
#' \code{WriteGeoData} exports a GeoData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE does neither allow empty values in any GeoData column nor any string elements with more than 50 characters, the 
#' function will return with warnings if \code{NA}s or long strings were exported.
#' (these are exported as "NA" to the text file).
#' 
#' @examples
#' \dontrun{WriteGeoData(x = mygeodata)}
#' 


WriteGeoData <- function(x, filename = "GeoData.txt", digits = 10, nsmall = 1) {
  
  # warn if there are NAs, which should not occur in GeoData files for HYPE
  if (!is.null(na.action(na.omit(x)))) {
    warning("NA values in exported object.")
  }
  
  # test length of string columns elements, throws warning if any element longer than 50, since HYPE does not read them
  .CheckCharLengthDf(x, maxChar = 50)
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE)
  
}


## DEBUG
# filename <- "Gd.txt"
# x <- fgd
# rm(x, fgd)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteGeoClass~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'GeoClass.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a 'GeoClass.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadGeoClass}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#'  
#' @details
#' \code{WriteGeoClass} exports a GeoClass dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE accepts comment rows with a leading '!' in the beginning rows of a GeoClass file. The export function looks for those in 
#' \code{attribute} 'comment', where \code{\link{ReadGeoClass}} stores such comments.
#' 
#' @examples
#' \dontrun{WriteGeoClass(x = mygeoclass)}
#' 


WriteGeoClass <- function(x, filename = "GeoClass.txt") {

  # set decimal print options to force decimal output (not scientific 'e'-notation). Prob not necessary, just to be safe
  options(digits = 10, scipen = 5)
  
  # export comment (if it exists) and header attributes
  if (!is.null(attr(x, which = "comment"))) {
    writeLines(c(attr(x, which = "comment"), attr(x, which = "header")), con = filename)
    #writeLines(, con = filename)
  } else {
    writeLines(attr(x, which = "header"), con = filename)
  }
  
  # export data frame, append to existing file if comment attributes were exported
  write.table(x, file = filename, quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE, na = "")
  
  # reset options to defaults
  options(digits = 7, scipen = 0)
}

## DEBUG
# x <- gec
# filename <- "GeoClass.txt"
# rm(x, filename)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteBranchData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'BranchData.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a 'BranchData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadBranchData}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#'  
#' @details
#' \code{WriteBranchData} exports a BranchData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE does neither allow empty values in any BranchData column nor any string elements with more than 50 characters, the 
#' function will return with warnings if \code{NA}s or long strings were exported.
#' 
#' @examples
#' \dontrun{WriteBranchData(x = mybranchdata)}
#' 


WriteBranchData <- function(x, filename = "BranchData.txt") {
  
  # set options for number of digits and scientific notation so that HYPE-compatible decimal strings are returned
  options(digits = 10, scipen = 5)
  #
  if (!is.null(na.action(na.omit(x)))) {
    warning("NA values in exported object.")
  }
  
  # test length of string columns elements, throws warning if any element longer than 50
  .CheckCharLengthDf(x, maxChar = 50)
  
  # export
  write.table(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)
  
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteXobs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write an 'Xobs.txt' File
#'
#' @description
#' \code{WriteXobs} writes or appends an observation data set to an Xobs file.
#' 
#' @param x A data frame, e.g. an object originally imported with \code{\link{ReadXobs}}. Date-time information in the first 
#' column and measured values in the remaining columns. Column names are ignored on export, but if attributes \code{comment}, 
#' \code{variable} , and \code{subid} are available, these can be exported as Xobs headers (see also arguments of the same names 
#' below).
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param append Logical. If \code{TRUE}, \code{x} will be appended to file \code{filename}. File must exist and 
#' have an identical column structure as \code{x}. If \code{FALSE}, existing file in \code{filename} will be overwritten!
#' @param comment A character string to be exported as first row comment in the Xobs file. Will take precedence over a \code{comment}
#' attribute of \code{x}. Comments are only exported if \code{append} is \code{FALSE}.
#' @param variable A character vector to be exported as second row in the Xobs file. Must contain the same number of 
#' variables as \code{x}. If omitted or \code{NA}, an attribute \code{variable} in \code{x} is mandatory.
#' Will take precedence over a \code{variable} attribute of \code{x}. If \code{append} is \code{TRUE} the values are 
#' used to test for consistency between export object and the existing file.
#' @param subid Third row in Xobs, containing SUBIDs (integer). Behaviour otherwise as argument \code{variable}.
#' @param lastDate Date-time of last observation in \code{\link{POSIXct}} format. See details.
#' @param timestep Character string, either "daily" or "hourly", giving the time step between observations. Can be 
#' abbreviated.
#' 
#' @details
#' \code{WriteXobs} writes a 'Xobs.txt' file, typically originating from an imported and modified 'Xobs.txt'.
#' HYPE Xobs files contain a three-row header, with a comment line first, next a line of variables, and then a line of subids. 
#' Objects imported with \code{\link{ReadXobs}} include attributes holding this information, and \code{WriteXobs} will use this
#' information. Otherwise, these attributes can be added to objects prior to calling code{WriteXobs}, or passed as function 
#' arguments.
#'
#' The function currenctly requires daily or hourly time steps as input, when the \code{append} argument is \code{TRUE}. 
#' The date-time column must be of class \code{POSIXct}, see \code{\link{as.POSIXct}}. Objects returned from 
#' \code{\link{ReadXobs}} per default have the correct class for the date-time column. Argument \code{lastDate} gives the 
#' last observation date-time in the existing 'Xobs' file and must be of the same type. Create with e.g. 
#' 'as.POSIXct(strptime("1999-01-01", format = "%Y-%m-%d", tz = "GMT"))'. Missing time steps between existing and new 
#' observations will be added, with '-9999' values in all columns. If time periods overlap, the export will stop with an
#' error message.
#' 
#' 
#' @note
#' Both \code{variable} and \code{subid} do not include elements for the first column in the Xobs file/object, in accordance 
#' with \code{\link{ReadXobs}}. These elements will be added by the function.
#' 
#' There is no automatic checking of last observation dates in existing 'Xobs' files when appending because R internal
#' functions available are time-consuming for large files. Google results for 'R read last line in text file' point to 
#' platform-dependent solutions using e.g. 'tail', 'gawk', or 'wc'.
#' 
#' @examples
#' \dontrun{WriteXobs(myxobs)}
#' 



WriteXobs <- function(x, filename = "Xobs.txt", append = F, comment = NA, variable = NA, subid = NA, 
                      lastDate = NA, timestep = "d") {
  
  if (!append) {
    # no appending, just write to a new file
    
    # Export of comment (if it exists) and header information, uses a cascade of conditional checks
    # which will stop the export with informative error messages if inconsistencies are found
    
    # remove existing export file. this makes it possible to have a consistent 'append' connection open below, instead of
    # overwriting first and then reopening in append mode...
    invisible(suppressWarnings(file.remove(filename)))
    
    # create and open a connection to which the following writeLines() will be written
    fcon <- file(description = filename, open ="at")
    
    ## export comment line
    if (is.na(comment)){
      # comment argument is empty
      if(!is.null(attr(x, which = "comment"))) {
        # comment attribute exists, export
        writeLines(paste(attr(x, which = "comment"), collapse = "\t"), con = fcon)
      } else {
        # comment attribute does not exist, export the following string
        writeLines("Exported from R", con = fcon)
      }
    } else {
      # export comment argument
      writeLines(comment, con = fcon)
    }
    
    ## export variable line
    if (length(variable) == 1 & is.na(variable[1])) {
      # variable argument is empty
      if(!is.null(attr(x, which = "variable"))) {
        # attribute variable exists
        if (length(attr(x, which = "variable")) == ncol(x) - 1) {
          # attribute and export dataframe match in length, export attribute with padded 'name' string and newline
          tmp <- paste(c("x", attr(x, which = "variable")), collapse = "\t")
          writeLines(tmp, con = fcon)
        } else {
          # mismatch in length, stop with error
          close(fcon)
          stop("Length of attribute 'variable' does not match number of variables in export object.\n 
               Check consistency, e.g. with attr(x, 'variable') and names(x)[-1].")
        }
      } else {
        # attribute variable does not exist, stop with error
        close(fcon)
        stop("'variable' argument not given and 'variable' attribute not existing in export object.")
      }
    } else {
      # export the variable argument with padded 'name' string and newline, if length matches no. of observation data cols in x
      if (length(variable) == ncol(x) - 1) {
        tmp <- paste(c("name", variable), collapse ="\t")
        writeLines(tmp, con = fcon)
      } else {
        # mismatch in length, stop with error
        close(fcon)
        stop("Length of argument 'variable' does not match number of variables in export object.")
      }
    }
    
    ## export subid line
    if (length(subid == 1) & is.na(subid[1])) {
      # subid argument is empty
      if(!is.null(attr(x, which = "subid"))) {
        # attribute subid exists
        if (length(attr(x, which = "subid")) == ncol(x) - 1) {
          # attribute and export dataframe match in length, export attribute with padded 0 and newline
          tmp <- paste(as.character(c(0, attr(x, which = "subid"))), collapse = "\t")
          writeLines(tmp, con = fcon)
        } else {
          # mismatch in length, stop with error
          close(fcon)
          stop("Length of attribute 'subid' does not match number of variables in export object.\n 
               Check consistency, e.g. with attr(x, 'subid') and names(x)[-1].")
        }
        } else {
          # attribute subid does not exist, stop with error
          close(fcon)
          stop("'subid' argument not given and 'subid' attribute not existing in export object.")
      }
    } else {
      # export the subid argument with padded 0 and newline, if length matches no. of observation data cols in x
      if (length(subid) == ncol(x) - 1) {
        tmp <- paste(as.character(c(0, subid)), collapse = "\t")
        writeLines(tmp, con = fcon)
      } else {
        # mismatch in length, stop with error
        close(fcon)
        stop("Length of argument 'subid' does not match number of variables in export object.")
      }
    }
    
    close(fcon)
    
    
    
  } else {
    # export will be appended to existing file
    
    # check if file to export to exists, stop otherwise
    stopifnot(file.exists(filename))
    
    # read variable names from existing file into two vectors, to be compared against the export data for consistency
    tmp <- readLines(filename,n=3)
    existingVar <- strsplit(tmp[2], split = "\t")[[1]][-1]
    existingSbd <- as.integer(strsplit(tmp[3], split = "\t")[[1]][-1])
    
    # first consistency check: number of columns identical?
    if (length(existingVar) != ncol(x) - 1) {
      stop("Inconsistent number of data columns between export object and existing file.")
    }
    
    ## second consistency check: variables and SUBIDs identical and in the same order?
    # select export data to compare with, either from function argument or from attribute of x
    if (!is.na(variable[1])) {
      exportVar <- variable
    } else {
      if (!is.null(attr(x, "variable"))) {
        exportVar <- attr(x, "variable")
      } else {
        stop("'variable' argument not given and 'variable' attribute not existing in export object.")
      }
    }
    
    if (!is.na(subid[1])) {
      exportSbd <- subid
    } else {
      if (!is.null(attr(x, "subid"))) {
        exportSbd <- attr(x, "subid")
      } else {
        stop("'subid' argument not given and 'subid' attribute not existing in export object.")
      }
    }
    
    # check consistency, will fail if at least one column is different for either subid or variable
    if (any(!(exportVar == existingVar), !(exportSbd == existingSbd))) {
      stop("Inconsistent variable names or SUBIDs.")
    }
    
    ## third consistency check: is last date in existing Xobs earlier than the first export row?
    ## split into hour and day cases
    if (timestep == "h" | timestep == "hourly") {
      tdiff <- difftime(x[1,1], lastDate, units = "hours")
      if (tdiff < 1) {
        stop("Time series in existing and new Xobs overlap or difference is smaller than one hour.")
      }
      ## export '-9999' lines if gap is larger than one hour
      # create date vector
      dpad <- seq(from = lastDate, to = x[1,1], length = tdiff + 1)
      # cut off end and start dates
      dpad <- dpad[-c(1, length(dpad))]
      # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
      pad <- cbind(format(dpad, format = "%Y-%m-%d %H:%M"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                                 ncol = ncol(x) - 1)))
      write.table(pad, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
    }
    
    if (timestep == "d" | timestep == "daily") {
      tdiff <- difftime(x[1,1], lastDate, units = "days")
      if (tdiff < 1) {
        stop("Time series in existing and new Xobs overlap or difference is smaller than one day.")
      }
      ## export '-9999' lines if gap is larger than one day
      # create date vector
      dpad <- seq(from = lastDate, to = x[1,1], length = tdiff + 1)
      # cut off end and start dates
      dpad <- dpad[-c(1, length(dpad))]
      # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
      pad <- cbind(format(dpad, format = "%Y-%m-%d"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                           ncol = ncol(x) - 1)))
      write.table(pad, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
    }
  }
  
  # Export of the dataframe, format date-times to HYPE requirements first
  if (timestep == "d" | timestep == "daily") {
    x[,1] <- format(x[,1], format = "%Y-%m-%d")
  }
  if (timestep == "h" | timestep == "hourly") {
    x[,1] <- format(x[,1], format = "%Y-%m-%d %H:%M")
  }
  write.table(x, file = filename, col.names = F, sep = "\t", append = T, na = "-9999", row.names = F, quote = F)
  
  
    }




# ## DEBUG
# library(RHYPE)
# filename <- xobsloc
# filename <- "xobs.txt"
# x <- ReadXobs(filename = xobsloc)
# variable <- NA
# comment <- NA
# subid <- NA
# attributes(x)
# attr(x, "variable") <- attr(x, "variable")[-1]
# names(x)
#WriteXobs(x, filename = filename, append = F, subid = c(2689,1432,1022,668,122,40706,40682,40672,4128,40555,2473,40350,1740))
#subid <- c(2689,1432,1022,668,122,40706,40682,40672,4128,40555,2473,40350,1740)
# WriteXobs(x = x, filename = filename, append = T, lastDate = as.POSIXct(strptime("1959-12-11", format = "%Y-%m-%d", tz = "GMT")))
# WriteXobs(x = x, filename = filename, append = T, lastDate = as.POSIXct(strptime("1960-12-11", format = "%Y-%m-%d", tz = "GMT")))
# 
# lastDate <- as.POSIXct(strptime("1959-12-01", format = "%Y-%m-%d", tz = "GMT"))
# 
# 







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteBasinOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a basin output '[SUBID].txt' file
#'
#' @description
#' Function to export a basin output file from R.
#' 
#' @param x The object to be written, a dataframe with \code{unit} attribute, as an object returned from \code{\link{ReadBasinOutput}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'.
#'  
#' @details
#' \code{WriteBasinOutput} exports a dataframe with headers and formatting options adjusted to match HYPE's basin output files.
#  The function attempts to format date-time information to strings and will return a warning if the attempt fails.
#' 
#' @examples
#' \dontrun{WriteBasinOutput(x = mybasin, filename = "000001.txt")}
#' 



WriteBasinOutput <- function(x, filename, dt.format = "%Y-%m-%d") {
  
  # create and open a file connection to write header to
  conn <- file(description = filename, open = "w")
  # write header lines
  writeLines(paste(names(x), collapse = "\t"), con = conn)
  writeLines(paste(attr(x, "unit"), collapse = "\t"), con = conn)
  # close the connection
  close(conn)
  
  # attempt to format the date column if it is POSIX (double number) to the given format string, otherwise return unchanged with a warning
  if (is.double(x[, 1])) {
    x[,1] <- format(x[,1], format = dt.format)
  } else {
    warning("Date column not formatting failed. Exported unchanged.")
  }
  
  # export the object, omitting header
  write.table(x, file = filename, append = T, sep = "\t", row.names = F, col.names = F, na = "-9999", quote = F)
  
}


## DEBUG
# te <- ReadBasinOutput(filename=filename)
# WriteBasinOutput(te, filename = "test3.txt")
# dt.format <- "%Y-%m-%d"
# outformat <- "df"
# filename <- "//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-01-10_koppling_SHYPE2012B_HBVsv/res_daily_thomas_hadley/0042041.txt"
# rm(te, x, xd, ReadBasinOutput, dt.format, filename)










#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteTimeOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'timeXXXX.txt' file
#'
#' @description
#' Function to export a time output file from R.
#' 
#' @param x The object to be written, a dataframe with \code{comment} and \code{subid} attributes, as an object returned from 
#' \code{\link{ReadTimeOutput}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\%Y'.
#'  
#' @details
#' \code{WriteTimeOutput} exports a dataframe with headers and formatting options adjusted to match HYPE's time output files.
#  The function attempts to format date-time information to strings and will return a warning if the attempt fails.
#' 
#' @examples
#' \dontrun{WriteTimeOutput(x = myCCTN, filename = "timeCCTN.txt")}
#' 

WriteTimeOutput <- function(x, filename, dt.format = "%Y-%m-%d") {
  
  # create and open a file connection to write header to
  conn <- file(description = filename, open = "w")
  # write header lines
  writeLines(attr(x, "comment"), con = conn)
  writeLines(paste(c("DATE", attr(x, "subid")), collapse = "\t"), con = conn)
  # close the connection
  close(conn)
  
  # attempt to format the date column if it is POSIX (double number) to the given format string, otherwise return unchanged with a warning
  if (is.double(x[, 1])) {
    x[,1] <- format(x[,1], format = dt.format)
  } else {
    warning("Date column not formatting failed. Exported unchanged.")
  }
  
  # export the object, omitting header
  write.table(x, file = filename, append = T, sep = "\t", row.names = F, col.names = F, na = "-9999", quote = F)
  
}


# te <- ReadTimeOutput("../timeCCTN.txt", dt.format="%Y")
# WriteTimeOutput(te, "test2.txt", dt.format="%Y")
# x[,1]
# dt.format <- "%Y"
# filename <- "test.txt"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteLakeData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'LakeData.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a 'LakeData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadLakeData}}. 
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param digits Integer, number significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#'  
#' @details
#' \code{WriteLakeData} exports a LakeData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE does not allow string elements with more than 50 characters in any LakeData column, the 
#' function will return with a warning if long strings were exported.
#' 
#' @examples
#' \dontrun{WriteLakeData(x = mylakedata)}
#' 


WriteLakeData <- function(x, filename = "LakeData.txt", digits = 10, nsmall = 0L) {
  
  # test length of string columns elements, throws warning if any element longer than 50, since HYPE does not read them
  .CheckCharLengthDf(x, maxChar = 50)
  
  # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  x[is.na(x)] <- -9999
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE)
  
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WritePmsf~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'pmsf.txt' file
#'
#' @description
#' This is a small convenience function to export a 'partial model setup file' from R.
#' 
#' @param x The object to be written, an \code{integer} vector containing SUBIDs.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' Pmsf files are represented as integer vectors in R. The total number of subcatchments in the file are added as first value on export. 
#' pmsf.txt files need to be ordered as downstream sequence. 
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}, which extracts upstream SUBIDs from a GeoData dataframe.
#' 
#' @examples
#' \dontrun{WritePmsf(x = mypmsf)}
#' 

WritePmsf <- function(x, filename = "../pmsf.txt") {
  # concatenate number of subids and vector of subids and export
  write.table(c(length(x), x), filename, col.names = FALSE, row.names = FALSE)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WritePTQobs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write 'Pobs.txt', 'Tobs.txt', or 'Qobs.txt' files
#'
#' @description
#' Export precipitation, temperature, or discharge observation files from R.
#' 
#' @param x The object to be written, a \code{dataframe} containing observation date-times in first and observations in SUBIDs in 
#' remaining columns, with an additional attribute \code{subid} containing SUBIDs in column order.
#' @param filename Path to and file name of the file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param subid Integer vector containing SUBIDs in same order as columns in \code{x}. To be exported as header in the obs file. 
#' Must contain the same number of SUBIDs as \code{x}. If \code{NULL}, an attribute \code{subid} in \code{x} is mandatory.
#' An existing \code{subid} argument takes precedence over a \code{subid} attribute.
#' @param digits Integer, number significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#'  
#' @details
#' \code{WritePTQobs} is a convenience wrapper function of \code{\link{write.table}} to export a HYPE compliant observation file. 
#' headers are generated from attribute \code{subid} on export (see \code{\link{attr}} on how to create and access it). 
#' 
#' 
#' @examples
#' \dontrun{WritePTQobs(mytops, "Tobs.txt")}
#' 


WritePTQobs <- function (x, filename, dt.format = "%Y-%m-%d", digits = 1, nsmall = 1, subid = NULL) {
  
  ## check if consistent header information is available, subid arguments take precedence before attribute
  if(!is.null(subid)) {
    if (length(subid) == ncol(x) - 1) {
      header <- c("DATE", subid)
    } else {
      stop("Length of function argument 'subid' does not match number of subid columns in export object.")
    }
  } else if (!is.null(attr(x, which = "subid"))) {
      if (length(attr(x, which = "subid")) == ncol(x) - 1) {
        header <- c("DATE", attr(x, which = "subid"))
      } else {
        stop("Length of attribute 'subid' does not match number of subid columns in export object.")
      }
  } else {
    stop("No information available from 'subid' argument or 'subid' attribute to construct export header.")
  }
  
  # date conversion, conditional on that the date column is a posix class
  if (any(class(x[, 1]) == "POSIXt")) {
    xd <- format(x[, 1], format = dt.format)
  } else {
    warning("First column in export data frame is not of class 'POSIXt', will be exported unchanged.")
  }
  
  # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  x[is.na(x)] <- -9999
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = header)
  
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteMgmtData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'MgmtData.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a HYPE 'MgmtData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadMgmtData}}. 
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param digits Integer, number significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#'  
#' @details
#' \code{WriteMgmtData} exports a MgmtData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE requires \code{NA}-free data in all mandatory MgmtData column, but \code{NA}s are allowed in optional comment 
#' columns which are not read by HYPE. The function will return with a warning if \code{NA}s were exported.
#' 
#' @examples
#' \dontrun{WriteMgmtData(x = myMgmtdata, file = "../myhype/MgmtData.txt")}
#' 


WriteMgmtData <- function(x, filename = "MgmtData.txt", digits = 10, nsmall = 0L) {
  
  # warn if NAs in data, since HYPE does not allow empty values in 
  # check for NAs
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te)) warning(paste("NA values in exported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  
  # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  x[is.na(x)] <- -9999
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE)
  
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WriteAquiferData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write an 'AquiferData.txt' file
#'
#' @description
#' This is a convenience wrapper function to export a HYPE 'AquiferData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadAquiferData}}. 
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param digits Integer, number significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#'  
#' @details
#' \code{WriteAquiferData} exports a AquiferData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE requires \code{NA}-free data in all mandatory AquiferData column, but \code{NA}s are allowed in optional comment 
#' columns which are not read by HYPE. The function will return with a warning if \code{NA}s were exported.
#' 
#' @examples
#' \dontrun{WriteAquiferData(x = myaquiferdata, file = "../myhype/AquiferData.txt")}
#' 


WriteAquiferData <- function(x, filename = "AquiferData.txt", digits = 10, nsmall = 0L) {
  
  # warn if NAs in data, since HYPE does not allow empty values in 
  # check for NAs
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te)) warning(paste("NA values in exported dataframe in column(s):", paste(names(res)[te], collapse=", ")))
  
  # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  x[is.na(x)] <- -9999
  
  # export
  write.table(format(x, digits = digits, nsmall = nsmall, scientific = F, drop0trailing = T, trim = T), file = filename, 
              quote = FALSE, sep = "\t", row.names = FALSE)
  
}
