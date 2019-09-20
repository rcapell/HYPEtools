
#' Find outlet-near observations in HYPE observation data files
#' 
#' @param qobs,xobs Character string, file location of HYPE observation data file, either an
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt} or a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:qobs.txt}{Qobs.txt} file.
#' @param gd Data frame with two columns \code{subid} and \code{maindown} (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' @param variable Character string, HYPE variable to use. If \code{NULL} (default), a vector of available variables 
#' in \code{obsfile} is returned
#' 
#' @details 
#' 
#' @return 
#' 
#' @examples 
#' \dontrun{}
#' 
#' @export

OutletNearObs <- function(qobs = NULL, xobs = NULL, gd, variable = NULL) {
  
  
}