
#--------------------------------------------------------------------------------------------------------------------------------------
#   Collection of export functions, herein:
#
#     - WritePar()
#     - WriteGeoData()
#     - WriteGeoClass()
#     - WriteXobs()
#     - WriteBasinOutput()
#     - WriteTimeOutput()
#     - WriteMapOutput()
#     - WritePmsf()
#     - WriteObs()
#     - HypeDataExport:
#         WriteAquiferData(), WriteBranchData(), WriteCropData(), WriteDamData(), WriteLakeData(), WriteMgmtData(), 
#         WritePointSourceData()
#     - WriteOptpar()
#     - 
#--------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------
# WritePar
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'par.txt' File
#'
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
#' @return 
#' No return value, called for export to text files.
#' 
#' @seealso \code{\link{ReadOptpar}} with a description of the expected content of \code{x}.
#' 
#' @examples
#' te <- ReadPar(filename = system.file("demo_model", "par.txt", package = "HYPEtools"))
#' # Note that par files loose all comment rows on import
#' WritePar(x = te, filename = tempfile())
#' 
#' 
#' @export


WritePar <- function (x, filename, digits = 10, nsmall = 1) {
  
  # format par list contents to avoid scientific format in output
  fx <- lapply(x, format, digits = digits, nsmall = nsmall, scientific = FALSE, drop0trailing = TRUE, trim = TRUE, justify = "none")
  
  # write formatted list elements to file, first converts all list elements (vectors) and their names to concatenated strings.
  write(sapply(seq_along(x), function(x, y) paste(c(names(y)[x], y[[x]]), collapse="\t"), y = fx), filename)
  
}






#--------------------------------------------------------------------------------------------------------------------------------------
# WriteGeoData
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'GeoData.txt' file
#'
#' This is a convenience wrapper function to export a 'GeoData.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadGeoData}}. 
#' \code{NA}s in any column will result in a warning (no \code{NA}s allowed in GeoData data columns).
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param digits Integer, number of significant digits \strong{in SLC class columns} to export. See \code{\link{signif}}.
#' @param scipen Integer, scientific notification bias, see documentation in \code{\link[data.table]{fwrite}}.
#'  
#' @details
#' \code{WriteGeoData} exports a GeoData dataframe using \code{\link[data.table]{fwrite}}. \code{SUBID} and \code{MAINDOWN} 
#' columns are forced to non-scientific notation by conversion to text strings prior to exporting. For all other numeric columns, 
#' use \code{\link[data.table]{fwrite}} argument \code{scipen}.
#' HYPE does neither allow empty values in any GeoData column nor any string elements with more than 50 characters. The 
#' function will return with warnings if \code{NA}s or long strings were exported.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' summary(te)
#' WriteGeoData(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @importFrom stats na.action na.omit
#' @export


WriteGeoData <- function(x, filename, digits = 6, scipen = getOption('scipen', 0L)) {
  
  # warn if there are NAs, which should not occur in GeoData files for HYPE
  if (!is.null(na.action(na.omit(x)))) {
    warning("NA values in exported object.")
  }
  
  # test length of string columns elements, throws warning if any element longer than 50, since HYPE does not read them
  .CheckCharLengthDf(x, maxChar = 50)
  
  # round slc class columns to requested number of significant digits
  x[, substr(names(x), 1, 4) == "SLC_"] <- signif(x[, substr(names(x), 1, 4) == "SLC_"], digits = digits)
  
  # convert SUBID and MAINDOWN columns to character to suppress scientific notation
  x[, tolower(names(x)) == "subid"] <- format(x[, tolower(names(x)) == "subid"], scientific = FALSE, trim = TRUE)
  x[, tolower(names(x)) == "maindown"] <- format(x[, tolower(names(x)) == "maindown"], scientific = FALSE, trim = TRUE)
  
  
  # ## export, with handling of custom fwrite arguments
  # 
  # # arguments provided by user in call
  # call <- as.list(match.call())[-1]
  # 
  # # fwrite arguments with fixed choices for GeoData export
  # custom.args <- list(file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
  # 
  # # handle overlap, give priority to fixed choices (ignore user input)
  # overlap.args <-  names(call) %in% names(custom.args)
  # if (!any(overlap.args)) call <- c(call, custom_args)
  # 
  # do.call(table, call) # exectue table() with the custom settings
  
  fwrite(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE, scipen = scipen)
}






#--------------------------------------------------------------------------------------------------------------------------------------
# WriteGeoClass
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'GeoClass.txt' file
#'
#' This is a convenience wrapper function to export a 'GeoClass.txt' file from R.
#' 
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadGeoClass}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param use.comment Logical, set to \code{TRUE} to export comment lines saved in \code{attribute} 'comment'. Per
#' default, column names are exported as header. See details.
#'  
#' @details
#' \code{WriteGeoClass} exports a GeoClass dataframe. HYPE accepts comment rows with a leading '!' in the beginning rows of a 
#' GeoClass file. Comment rows typically contain some class descriptions in a non-structured way. With argument 
#' \code{use.comment = TRUE}, the export function looks for those in \code{attribute} 'comment', 
#' where \code{\link{ReadGeoClass}} stores such comments. Description files (see \code{\link{ReadDescription}}) offer a more structured 
#' way of storing that information.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' WriteGeoClass(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @export


WriteGeoClass <- function(x, filename, use.comment = FALSE) {

  # conditional: export with comment attribute or with column names
  if (use.comment) {
    
    if (!is.null(attr(x, "comment"))) {
      
      writeLines(attr(x, which = "comment"), con = filename)
      fwrite(file = filename, x = x, append = TRUE, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      
    } else {
      
      warning("Attribute 'comment' not found in 'x'. Column names exported instead.")
      writeLines(paste0("!", paste(names(x), collapse = "\t")), con = filename)
      fwrite(file = filename, x = x, append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
      
    }
    
  } else {
    
    writeLines(paste0("!", paste(names(x), collapse = "\t")), con = filename)
    fwrite(file = filename, x = x, append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
    
  }
}





#--------------------------------------------------------------------------------------------------------------------------------------
# WriteXobs
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write an 'Xobs.txt' File
#'
#' \code{WriteXobs} writes or appends an observation data set to an Xobs file.
#' 
#' @param x A data frame, e.g. an object originally imported with \code{\link{ReadXobs}}. Date-time information in the first 
#' column and measured values in the remaining columns. Column names are ignored on export, but if attributes \code{comment}, 
#' \code{variable}, and \code{subid} are available, these can be exported as Xobs headers (see also arguments of the same names 
#' below).
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param append Logical. If \code{TRUE}, \code{x} will be appended to file \code{filename}. File must exist and 
#' have an identical column structure as \code{x}. If \code{FALSE}, existing file in \code{filename} will be overwritten!
#' @param comment A character string to be exported as first row comment in the Xobs file. If provided, it takes precedence over 
#' a \code{comment} attribute of \code{x}. Comments are only exported if \code{append} is \code{FALSE}.
#' @param variable A character vector to be exported as second row in the Xobs file. Must contain the same number of 
#' variables as \code{x}. If omitted or \code{NULL}, an attribute \code{variable} in \code{x} is mandatory.
#' Will take precedence over a \code{variable} attribute of \code{x}. If \code{append} is \code{TRUE} the values are 
#' used to test for consistency between export object and the existing file.
#' @param subid Third row in Xobs, containing SUBIDs (integer). Behaviour otherwise as argument \code{variable}.
#' @param last.date Optional date-time of last observation in existing Xobs file as text string. Only relevant with \code{append = TRUE}. 
#' Formatting depending on time step, e.g. \code{'2000-01-01'} (day) or \code{'2000-01-01 00:00'} (hour). Will be automatically read 
#' from file per default, but can be provided to reduce execution time when appending to large files.
#' @param timestep Character string, either "day" or "hour", giving the time step between observations. Can be 
#' abbreviated.
#' 
#' @details
#' \code{WriteXobs} writes a 'Xobs.txt' file, typically originating from an imported and modified 'Xobs.txt'.
#' HYPE Xobs files contain a three-row header, with a comment line first, next a line of variables, and then a line of subids. 
#' Objects imported with \code{\link{ReadXobs}} include attributes holding this information, and \code{WriteXobs} will use this
#' information. Otherwise, these attributes can be added to objects prior to calling \code{WriteXobs}, or passed as function 
#' arguments.
#'
#' If argument \code{append} is \code{TRUE}, the function requires daily or hourly time steps as input. 
#' The date-time column must be of class \code{POSIXct}, see \code{\link{as.POSIXct}}. Objects returned from 
#' \code{\link{ReadXobs}} per default have the correct class for the date-time column. When appending to existing file, the 
#' function adds new rows with '-9999' values in all data columns to fill any time gaps between existing and new data. If time 
#' periods overlap, the export will stop with an error message. Argument \code{last.date} can be provided to speed up appending exports, 
#' but per default, \code{WriteXobs} extracts the last observation in the existing file automatically. 
#' 
#' 
#' @note
#' Both \code{variable} and \code{subid} do not include elements for the first column in the Xobs file/object, in accordance 
#' with \code{\link{ReadXobs}}. These elements will be added by the function.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadXobs(filename = system.file("demo_model", "Xobs.txt", package = "HYPEtools"))
#' WriteXobs(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @export


WriteXobs <- function(x, filename, append = FALSE, comment = NULL, variable = NULL, subid = NULL, 
                      last.date = NULL, timestep = "d") {
  
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
    if (is.null(comment)) {
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
      writeLines(paste(comment, collapse = "\t"), con = fcon)
    }
    
    ## export variable line
    if (is.null(variable)) {
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
          stop("Length of attribute 'variable' does not match number of variables in export object.\nCheck consistency, e.g. with attr(x, 'variable') and names(x)[-1].")
        }
      } else {
        # attribute variable does not exist, stop with error
        close(fcon)
        stop("'variable' argument not given and 'variable' attribute not existing in export object.")
      }
    } else {
      # export the variable argument with padded 'name' string and newline, if length matches no. of observation data cols in x
      if (length(variable) == ncol(x) - 1) {
        tmp <- paste(c("x", variable), collapse ="\t")
        writeLines(tmp, con = fcon)
      } else {
        # mismatch in length, stop with error
        close(fcon)
        stop("Length of argument 'variable' does not match number of variables in export object.")
      }
    }
    
    ## export subid line
    if (is.null(subid)) {
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
    if (!is.null(variable)) {
      exportVar <- variable
    } else {
      if (!is.null(attr(x, "variable"))) {
        exportVar <- attr(x, "variable")
      } else {
        stop("'variable' argument not given and 'variable' attribute not existing in export object.")
      }
    }
    
    if (!is.null(subid[1])) {
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
    if (timestep == "h" | timestep == "hour") {
      # extract last date from existing file
      if (is.null(last.date)) {
        te <- fread(filename,  na.strings = "-9999", skip = 3, sep = "\t", header = FALSE, data.table = FALSE, 
                    colClasses = "NA", select = 1)
        last.date <- as.POSIXct(strptime(te[nrow(te), ], format = "%Y-%m-%d %H:%M", tz = "UTC"))
      } else {
        last.date <- as.POSIXct(strptime(last.date, format = "%Y-%m-%d %H:%M", tz = "UTC"))
        stop(paste0("Conversion of user-provided argument 'last.date' to POSIXct failed. Check formatting requirements."))
      }
      # calculate gap between existing and new file
      tdiff <- difftime(x[1,1], last.date, units = "hours")
      if (tdiff < 1) {
        stop("Time series in existing and new Xobs overlap or difference is smaller than one hour.")
      }
      ## export '-9999' lines if gap is larger than one hour
      # create date vector
      dpad <- seq(from = last.date, to = x[1,1], length = tdiff + 1)
      # cut off end and start dates
      dpad <- dpad[-c(1, length(dpad))]
      # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
      pad <- cbind(format(dpad, format = "%Y-%m-%d %H:%M"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                                 ncol = ncol(x) - 1)))
      # export
      fwrite(pad, file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, na = "-9999")
    }
    
    if (timestep == "d" | timestep == "day") {
      # extract last date from existing file
      if (is.null(last.date)) {
        te <- fread(filename,  na.strings = "-9999", skip = 3, sep = "\t", header = FALSE, data.table = FALSE, 
                    colClasses = "NA", select = 1)
        last.date <- as.POSIXct(strptime(te[nrow(te), ], format = "%Y-%m-%d", tz = "UTC"))
      } else {
        last.date <- as.POSIXct(strptime(last.date, format = "%Y-%m-%d %H:%M", tz = "UTC"))
        stop(paste0("Conversion of user-provided argument 'last.date' to POSIXct failed. Check formatting requirements."))
      }
      # calculate gap between existing and new file
      tdiff <- difftime(x[1,1], last.date, units = "days")
      if (tdiff < 1) {
        stop("Time series in existing and new Xobs overlap or difference is smaller than one day.")
      }
      ## export '-9999' lines if gap is larger than one day
      # create date vector
      dpad <- seq(from = last.date, to = x[1,1], length = tdiff + 1)
      # cut off end and start dates
      dpad <- dpad[-c(1, length(dpad))]
      # create data frame from a matrix (it is more straightforward to span up an empty matrix and then convert to df)
      pad <- cbind(format(dpad, format = "%Y-%m-%d"), as.data.frame(matrix(data = -9999, nrow = length(dpad), 
                                                                           ncol = ncol(x) - 1)))
      # export
      fwrite(pad, file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, na = "-9999")
    }
  }
  
  # Export of new xobs, format date-times to HYPE requirements first
  if (timestep == "d" | timestep == "day") {
    x[,1] <- format(x[,1], format = "%Y-%m-%d")
  }
  if (timestep == "h" | timestep == "hour") {
    x[,1] <- format(x[,1], format = "%Y-%m-%d %H:%M")
  }
  # export
  fwrite(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, na = "-9999")
  # old version, delete after while
  #write.table(x, file = filename, col.names = FALSE, sep = "\t", append = TRUE, na = "-9999", row.names = FALSE, quote = FALSE)

}









#--------------------------------------------------------------------------------------------------------------------------------------
# WriteBasinOutput
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a basin output '\[SUBID\].txt' file
#'
#' Function to export a basin output file from R.
#' 
#' @param x The object to be written, a dataframe with \code{hypeunit} attribute, as an object returned from \code{\link{ReadBasinOutput}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\\%Y'.
#'  
#' @details
#' \code{WriteBasinOutput} exports a dataframe with headers and formatting options adjusted to match HYPE's basin output files.
#  The function attempts to format date-time information to strings and will return a warning if the attempt fails.
#' 
#' @return 
#' No return value, called for file export.
#' 
#' @examples
#' te <- ReadBasinOutput(filename = system.file("demo_model", "results", "0003587.txt", 
#'                       package = "HYPEtools"))
#' WriteBasinOutput(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @export


WriteBasinOutput <- function(x, filename, dt.format = "%Y-%m-%d") {
  
  # create and open a file connection to write header to
  conn <- file(description = filename, open = "w")
  # write header lines
  writeLines(paste(names(x), collapse = "\t"), con = conn)
  writeLines(paste(c("UNITS", attr(x, "hypeunit")), collapse = "\t"), con = conn)
  # close the connection
  close(conn)
  
  # attempt to format the date column if it is POSIX (double number) to the given format string, otherwise return unchanged with a warning
  if (is.double(x[, 1])) {
    x[,1] <- format(x[,1], format = dt.format)
  } else {
    warning("Date column formatting failed. Exported unchanged.")
  }
  
  # export object, omitting header
  fwrite(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE, append = TRUE, na = "-9999")
  # old version, delete after while
  #write.table(x, file = filename, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, na = "-9999", quote = FALSE)
  
}


## DEBUG
# te <- ReadBasinOutput(filename=filename)
# WriteBasinOutput(te, filename = "test3.txt")
# dt.format <- "%Y-%m-%d"
# outformat <- "df"
# filename <- "//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-01-10_koppling_SHYPE2012B_HBVsv/res_daily_thomas_hadley/0042041.txt"
# rm(te, x, xd, ReadBasinOutput, dt.format, filename)










#--------------------------------------------------------------------------------------------------------------------------------------
# WriteTimeOutput
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'timeXXXX.txt' file
#'
#' Function to export a time output file from R.
#' 
#' @param x The object to be written, a dataframe with \code{comment} and \code{subid} attributes, as an object returned from 
#' \code{\link{ReadTimeOutput}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Incomplete format strings for monthly 
#' and annual values allowed, e.g. '\\%Y'.
#'  
#' @details
#' \code{WriteTimeOutput} exports a data frame with headers and formatting options adjusted to match HYPE's time output files.
#  The function attempts to format date-time information to strings and will return a warning if the attempt fails.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadTimeOutput(filename = system.file("demo_model", "results", "timeCOUT.txt", 
#'                      package = "HYPEtools"), dt.format = "%Y-%m")
#' WriteTimeOutput(x = te, filename = tempfile(), dt.format = "%Y-%m")
#' 
#' @importFrom data.table fwrite
#' @export


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
    x[, 1] <- format(x[, 1], format = dt.format)
  } else {
    warning("Date column formatting failed. Exported unchanged.")
  }
  
  # export the object, omitting header
  fwrite(x, file = filename, append = TRUE, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE)
  # old version, cann be deleted after a while
  # write.table(x, file = filename, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE, na = "-9999", quote = FALSE)
  
}







#--------------------------------------------------------------------------------------------------------------------------------------
# WriteMapOutput
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'mapXXXX.txt' file
#'
#' Function to export a map output file from R.
#' 
#' @param x The object to be written, a dataframe with \code{comment}, \code{date}, and \code{timestep} 
#' attributes, as an object returned from \code{\link{ReadMapOutput}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. Date format for export of column headers.
#' Incomplete format strings for monthly and annual values allowed, e.g. '\%Y', if columns contain annual aggregates. 
#' Use \code{NULL} for single-column dataframes, i.e. long-term average map files.
#'  
#' @details
#' \code{WriteMapOutput} exports a dataframe with headers and formatting options adjusted to match HYPE's map output files.
#'  The function attempts to format date-time information to strings and will return a warning if the attempt fails.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadMapOutput(filename = system.file("demo_model", "results", "mapEVAP.txt", 
#'                     package = "HYPEtools"), dt.format = NULL)
#' WriteMapOutput(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @export

WriteMapOutput <- function(x, filename, dt.format = "%Y-%m-%d") {
  
  # convert date comment to text, if requested
  if (!is.null(dt.format)) {
    if (!(inherits(attr(x, "datetime"), "POSIXt") | inherits(attr(x, "datetime"), "Date"))) {
      warning("Date formatting requested through argument 'dt.format' but date attribute of 'x' contains only strings. Continuing without formatting.")
      dt <- attr(x, "datetime")
    } else {
      dt <- format(attr(x, "datetime"), format = dt.format)
    }
  } else {
    dt <- attr(x, "datetime")
  }
  
  # create and open a file connection to write header to
  conn <- file(description = filename, open = "w")
  # write header lines
  writeLines(attr(x, "comment"), con = conn)
  writeLines(paste(c("SUBID", dt), collapse = ","), con = conn)
  # close the connection
  close(conn)
  
  # export the object, omitting header
  fwrite(x, file = filename, append = TRUE, sep = ",", quote = FALSE, na = "-9999", row.names = FALSE, col.names = FALSE)

}







#--------------------------------------------------------------------------------------------------------------------------------------
# WritePmsf
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a 'pmsf.txt' file
#'
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
#' @return 
#' No return value, called for export to text files.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}, which extracts upstream SUBIDs from a GeoData dataframe.
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' WritePmsf(x = te$SUBID[te$SUBID %in% AllUpstreamSubids(3564, te)], filename = tempfile())
#' 
#' @importFrom data.table fwrite
#' @export

WritePmsf <- function(x, filename) {
  # concatenate number of subids and vector of subids and export
  fwrite(data.frame(c(length(x), x)), file = filename, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  # old version, can be deleted after a while
  # write.table(c(length(x), x), filename, col.names = FALSE, row.names = FALSE)
}





#--------------------------------------------------------------------------------------------------------------------------------------
# WriteObs
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write 'Pobs.txt', 'Tobs.txt', 'Qobs.txt', and other observation data files
#'
#' Export forcing data and discharge observation files from R.
#' 
#' @param x The object to be written, a \code{dataframe} containing observation date-times in first and observations in SUBIDs or OBSIDs in 
#' remaining columns. If argument \code{obsid} is not provided, \code{x} must have an additional attribute \code{obsid} containing 
#' observation IDs/SUBIDs in column order.
#' @param filename Path to and file name of the file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param obsid Integer vector containing observation IDs/SUBIDs in same order as columns in \code{x}. To be exported as header 
#' in the obs file. Must contain the same number of IDs as observation series in \code{x}. If \code{NULL}, an attribute \code{obsid} 
#' in \code{x} is mandatory. An existing \code{obsid} argument takes precedence over a \code{obsid} attribute.
#' @param round,signif Integer, number of decimal places and number of significant digits to export. See \code{\link{round}}. Applied in 
#' sequence. If \code{NULL} (default), the data to export is not touched.
#'  
#' @details
#' \code{WriteObs} is a convenience wrapper function of \code{\link[data.table]{fwrite}} to export a HYPE-compliant observation file. 
#' headers are generated from attribute \code{obsid} on export (see \code{\link{attr}} on how to create and access it). 
#' 
#' Observation IDs are SUBIDs or IDs connected to SUBIDs with a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:forckey.txt}{ForcKey.txt file}.
#' 
# #' The exported dataframe is formatted using \code{\link{format}} prior to exporting. This because HYPE does not accept 
# #' scientific numbers in '1e+1' notation and because it allows to fine-tune the number of digits to export. Besides user-changeable 
# #' arguments \code{digits} and \code{nsmall}, \code{format} arguments \code{scientific = FALSE, drop0trailing = TRUE, trim = TRUE} are 
# #' hard-coded into \code{WriteObs}.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @seealso 
#' \code{\link{ReadObs}}
#' \code{\link{WriteXobs}}
#' 
#' @examples
#' te <- ReadObs(filename = system.file("demo_model", "Tobs.txt", package = "HYPEtools"))
#' WriteObs(x = te, filename = tempfile())
#' 
#' @importFrom data.table fwrite .SD
#' @export


WriteObs <- function (x, filename, dt.format = "%Y-%m-%d", round = NULL, signif = NULL, obsid = NULL) {
  
  ## check if consistent header information is available, obsid arguments take precedence before attribute
  ## construct HYPE-conform header for export (violates R header rules)
  if(!is.null(obsid)) {
    if (length(obsid) == ncol(x) - 1) {
      names(x) <- c("DATE", obsid)
    } else {
      stop("Length of function argument 'obsid' does not match number of obsid columns in export object.")
    }
  } else if (!is.null(attr(x, which = "obsid"))) {
      if (length(obsid(x)) == ncol(x) - 1) {
        names(x) <- c("DATE", attr(x, which = "obsid"))
      } else {
        stop("Length of attribute 'obsid' does not match number of obsid columns in export object.")
      }
  } else {
    stop("No information available from 'obsid' argument or 'obsid' attribute to construct export header.")
  }
  
  # date conversion, conditional on that the date column is a posix class
  if (any(class(x$DATE) == "POSIXt")) {
    x$DATE <- format(x$DATE, format = dt.format)
  } else {
    warning("First column in export data frame is not of class 'POSIXt', will be exported unchanged.")
  }
  
  # round to user-specified number of decimals and significant digits
  if (!is.null(round)) {
    if ("data.table" %in% class(x)) {
      x[, 2:ncol(x)] <- x[, round(.SD, round), .SDcols = 2:ncol(x)]
    } else {
      x[, -1] <- round(x[, -1], digits = round)
    }
  }
  if (!is.null(signif)) {
    if ("data.table" %in% class(x)) {
      x[, 2:ncol(x)] <- x[, signif(.SD, signif), .SDcols=2:ncol(x)]
    } else {
      x[, -1] <- signif(x[, -1], digits = signif)
    }
  }
  
  # export
 fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)

}

# alias, for backwards compatibility
#' @rdname WriteObs
#' @importFrom stats na.fail
#' @export
WritePTQobs <- WriteObs






#--------------------------------------------------------------------------------------------------------------------------------------
# HypeDataExport
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write HYPE data files
#'
#' These are simple convenience wrapper functions to export various HYPE data files from R.
#' 
#' @param x The object to be written, a dataframe as returned from the \code{\link{HypeDataImport}} functions. 
#' @param filename A character string naming a path and file name to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
# #' @param digits Integer, number of significant digits to export. See \code{\link{format}}.
# #' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#' @param verbose Logical, display informative warning messages if columns contain \code{NA} values or if character strings are
#' too long. See Details.
#'  
#' @details
#' Hype data file exports, simple \code{\link[data.table]{fwrite}} wrappers with formatting options adjusted to match HYPE file 
#' specifications:
#' 
#' \itemize{
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:lakedata.txt}{LakeData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:damdata.txt}{DamData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:mgmtdata.txt}{MgmtData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:aquiferdata.txt}{AquiferData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:pointsourcedata.txt}{PointSourceData.txt}
# NOT YET IMPLEMENTED #'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:glacierdata.txt}{GlacierData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:cropdata.txt}{CropData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:branchdata.txt}{BranchData.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:forckey.txt}{forckey.txt}
#'   \item \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:outregions.txt}{Outregions.txt}
#' }
#' 
#' In most files, HYPE requires \code{NA}-free input in required columns, but empty values are 
#' allowed in additional comment columns which are not read by HYPE. Informative warnings will be thrown if \code{NA}s are 
#' found during export. Character string lengths in comment columns of HYPE data files are restricted to 100 characters, 
#' the functions will return with a warning if longer strings were exported.
#' 
# #' Exported dataframes are formatted using \code{\link{format}} prior to exporting. This because HYPE does not accept 
# #' scientific numbers in '1e+1' notation and because it allows to fine-tune the number of digits to export. Besides user-changeable 
# #' arguments \code{digits} and \code{nsmall}, \code{format} arguments \code{scientific = FALSE, drop0trailing = TRUE, trim = TRUE} are 
# #' hard-coded into the export functions.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @examples
#' te <- ReadForcKey(filename = system.file("demo_model", "ForcKey.txt", package = "HYPEtools"))
#' WriteForcKey(x = te, filename = tempfile())
#' 
#' @name HypeDataExport

NULL

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteAquiferData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) {
    warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
  # old version
  # # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  # x[is.na(x)] <- -9999
  # write.table(format(x, digits = digits, nsmall = nsmall, scientific = FALSE, drop0trailing = TRUE, trim = TRUE), file = filename, 
  #             quote = FALSE, sep = "\t", row.names = FALSE, na = "")
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteOutregions <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) {
    warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
  # old version
  # # convert NAs to -9999, needed because format() below does not allow for automatic replacement of NA strings 
  # x[is.na(x)] <- -9999
  # write.table(format(x, digits = digits, nsmall = nsmall, scientific = FALSE, drop0trailing = TRUE, trim = TRUE), file = filename, 
  #             quote = FALSE, sep = "\t", row.names = FALSE, na = "")
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteBranchData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  #
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) {
    warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteCropData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) {
    warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteDamData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) {
    warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteLakeData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
    }
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteMgmtData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WritePointSourceData <- function(x, filename, verbose = TRUE) {
  # test length of string columns elements, throws warning if any element longer than 100, since HYPE does not read them
  if (verbose) {
    .CheckCharLengthDf(x, maxChar = 100)
  }
  # warn if NAs in data, since HYPE does not allow empty values in 
  te <- apply(x, 2, function(x) {any(is.na(x))})
  if (any(te) && verbose) warning(paste("NA values in exported dataframe in column(s):", paste(names(x)[te], collapse=", ")))
  # export
  fwrite(x, file = filename, sep = "\t", quote = FALSE, na = "-9999", row.names = FALSE, col.names = TRUE)
}

#' @rdname HypeDataExport
#' @importFrom data.table fwrite
#' @export
WriteForcKey <- function(x, filename) {
  fwrite(x, file = filename, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
}







#--------------------------------------------------------------------------------------------------------------------------------------
# WriteOptpar
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write an 'optpar.txt' File
#'
#' \code{WriteOptpar} prints a HYPE parameter optimisation list to a file.
#' 
#' @param x The object to be written, a list with named elements, as an object returned from \code{\link{ReadOptpar}}.
#' @param filename A character string naming a file to write to. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param digits Integer, number of significant digits to export. See \code{\link{format}}.
#' @param nsmall Integer, number of significant decimals to export. See \code{\link{format}}.
#' 
#' @return 
#' No return value, called for export to text files.
#' 
#' @seealso \code{\link{ReadOptpar}} with a description of the expected content of \code{x}.
#' 
#' @examples
#' te <- ReadOptpar(filename = system.file("demo_model", "optpar.txt", package = "HYPEtools"))
#' WriteOptpar(x = te, filename = tempfile())
#' 
#' @export


WriteOptpar <- function (x, filename, digits = 10, nsmall = 1) {
  
  # convert all data frames in x$pars to lists of vectors and then flatten list of lists to list with vector elements
  px <- unlist(lapply(x$pars, as.list), recursive = FALSE)
  # format list contents to avoid scientific format in output
  px <- lapply(px, format, digits = digits, nsmall = nsmall, scientific = FALSE, drop0trailing = TRUE, trim = TRUE, justify = "none")
  # add parameter names in first position of vector elements
  px <- lapply(1:length(px), function(x, y, z) {c(y[x], z[[x]])}, y = rep(names(x$pars), each = 3), z = px)
  
  # create and open file device
  out <- file(description = filename, open = "w")
  
  # write comment row
  writeLines(text = x$comment, con = out)
  
  # write optimisation settings
  write(apply(x$tasks, 1, paste, collapse = "\t"), file = out)
  
  # write empty lines to fill optimisation settings section (static section in optpar file, line 2 to 21)
  if (nrow(x$tasks) < 20) {
    empty <- rep("", 20 - nrow(x$tasks))
    writeLines(empty, con = out)
  }
  
  # write parameter ranges
  write(sapply(px, paste, collapse = "\t"), file = out)
  close(out)
  
}

