#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of export functions, herein:
#
#     - WritePar()
#     - WriteGeoData()
#     - WriteGeoClass()
#     - WriteBranchData()
#     - 
#
#
#   Internal helper functions:
#
#     - .CheckCharLengthDf()
#
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
#' 
#' @details
#' \code{WritePar} writes a 'par.txt' file, typically originating from an imported and modified 'par.txt'.
#' 
#' @examples
#' \dontrun{WritePar(mypar)}
#' 



WritePar <- function (x, filename = "par.txt") {
  # set options for number of digits and scientific notation so that HYPE-compatible decimal strings are returned
  options(digits = 10, scipen = 10)
  # write list elements to file, first converts all list elements (vectors), together with their names, to strings.
  write(sapply(names(y), function(x) paste(c(x, y[[x]]), collapse="\t")), filename)
  # reset options to defaults
  options(digits = 7, scipen = 0)
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


WriteGeoData <- function(x, filename = "GeoData.txt") {
  # set options for number of digits and scientific notation so that HYPE-compatible decimal strings are returned
  options(digits = 10, scipen = 0)
  #
  if (!is.null(na.action(na.omit(x)))) {
    warning("NA values in exported object.")
  }
  
  # test length of string columns elements, throws warning if any element longer than 50
  .CheckCharLengthDf(x, maxChar = 50)
  
  # export
  write.table(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)
  # reset options to defaults
  options(digits = 7, scipen = 0)
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
  # reset options to defaults
  options(digits = 7, scipen = 0)
  
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.CheckCharLengthDf~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## internal function to test if string columns elements are longer than a number of characters
# x: a dataframe to be tested
# maxChar: maximum number of characters
.CheckCharLengthDf <- function (x, maxChar) {
  ## test if string columns elements are longer than 50 characters, which is the maximum accepted for BranchData by HYPE
  
  # which columns are of type factor or character (strings)
  facts <- sapply(x, function(z) {is.factor(z)})
  chars <- sapply(x, function(z) {is.character(z)})
  lf <- length(which(facts))
  lc <- length(which(chars))
  
  # select and convert factor columns to a character matrix, if factors exist
  if (lf > 0) {
    facmat <- apply(as.matrix(x[, facts]), 2, as.character)
    # test if the longest string in any column is longer than 50 characters, return with warning
    if (max(apply(facmat, 2, function (z) max(nchar(z)))) > maxChar) {
      warning("String with more than 50 characters in exported data detected. This will lead to an error in HYPE.")
    }
  }
  # select and convert character columns to a character matrix, if characters exist
  if (lc > 0) {
    chamat <- as.matrix(x[, chars])
    # test if the longest string in any column is longer than 50 characters, return with warning
    if (max(apply(chamat, 2, function (z) max(nchar(z)))) > maxChar) {
      warning("String with more than 50 characters in exported data detected. This will lead to an error in HYPE.")
    }  
  }
}