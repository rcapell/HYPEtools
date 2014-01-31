#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of export functions, herein:
#
#     - WritePar()
#     - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ReadGeoClass~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#' @export
#' @title
#' Write a 'par.txt' File
#'
#' @description
#' \code{WritePar} prints its required argument \code{x} to a file.
#' 
#' @param The object to be written, a list with named vector elements, as an object returned from \code{\link{ReadPar}}.
#' @param A character string naming a file to write to. Windows users: Note that 
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
  # write file, first converts all list elements (vectors), together with their names, to strings.
  write(sapply(names(y), function(x) paste(c(x, y[[x]]), collapse="\t")), filename)
  # reset options to defaults
  options(digits = 7, scipen = 0)
}



