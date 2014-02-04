#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of export functions, herein:
#
#     - WritePar()
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
#' @param x The object to be written, a dataframe, as an object returned from \code{\link{ReadGeoData}}. No \code{NA}s allowed.
#' @param filename Path to and file name of the GeoData file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#'  
#' @details
#' \code{WriteGeoData} exports a GeoData dataframe with formatting options adjusted for the output to be read by HYPE.
#' HYPE does not allow empty values in any GeoData column, the function will return with a warning if \code{NA}s were exported
#' (these are exported as "NA" to the text file).
#' 
#' @examples
#' \dontrun{WriteGeoData(x = mygeodata)}
#' 


WriteGeoData <- function(x, filename = "GeoData.txt") {
  # set options for number of digits and scientific notation so that HYPE-compatible decimal strings are returned
  options(digits = 10, scipen = 0)
  #
  if (!is.null(na.action(na.omit(te)))) {
    warning("NA values in exported object.")
  }
  # export
  write.table(x, file = filename, quote = FALSE, sep = "\t", row.names = FALSE)
  # reset options to defaults
  options(digits = 7, scipen = 0)
}

