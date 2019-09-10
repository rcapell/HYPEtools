
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   S3 class HypeGeoData (simple data.frame, class added on import with ReadGeoData()), herein:
#
#     - HypeGeoData (constructor function)
#     - [.HypeGeoData (indexing method)
#     - summary method
#     - print method for summary list
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#--------------------------------------------------------------------------------------------------------------------------------------
# Constructor function

#' HypeGeoData data frames
#' 
#' Constructor function for data frames which hold HYPE GeoData tables with information on sub-basins.
#' 
#' @param x Data frame with at least five mandatory columns, see details.
#' 
#' @details 
#' S3 constructor function for data frames which hold HYPE GeoData tables. These are normal data frames with at least five mandatory 
#' columns, all \code{numeric}: \emph{AREA}, \emph{SUBID}, \emph{MAINDOWN}, \emph{RIVLEN}, and \emph{SLC_n}, where \emph{n} are 
#' consecutive SLC class numbers (up to 999).See also the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE file description} 
#'  for GeoData.txt files for reference. 
#' 
#' Usually, this class will be assigned to GeoData tables on import with \code{\link{ReadGeoData}}. A \code{summary} method exists for 
#' \code{HypeGeoData} data frames.
#' 
#' @return 
#' Returns a data frame with added \code{\link{class}} attribute \code{HypeGeoData}.
#' 
#' @seealso 
#' \code{\link{ReadGeoData}}, \code{\link{summary.HypeGeoData}}
#' 
#' @examples
#' \dontrun{HypeGeoData(x = )}
#' @export

HypeGeoData <- function(x) {
  
  ## check for mandatory column names and corresponding column data types
  
  # mandatory columns except SLCs and their positions
  m <- c("AREA", "SUBID", "MAINDOWN", "RIVLEN")
  pos.m <- match(m, names(x))
  
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(x), 1, 4) == "SLC_")
  # extract numbers
  if (length(pos.s) > 0) {
    suppressWarnings(n.s <- as.numeric(substr(names(x)[pos.s], 5, 99)))
    # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
    pos.s <- pos.s[!is.na(n.s)]
    n.s <- n.s[!is.na(n.s)]
    # sort so that consecutiveness can be tested below
    n.s <- sort(n.s)
  }
    
  if (any(is.na(pos.m))) {
    
    stop(paste0("Mandatory column(s) '", paste(m[is.na(pos.m)], collapse = "', '"), "' missing."))
  } else if (any(!apply(x[, pos.m], 2, is.numeric))) {
    
    stop(paste0("Mandatory column(s) '", paste(m[!apply(x[, pos.m], 2, is.numeric)], collapse = "', '"), "' non-numeric."))
  } 
}

#--------------------------------------------------------------------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# indexing method
#' @export
`[.HypeGeoData` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], ...) {
  y <- NextMethod("[", drop = F)
  class(y) <- c("HypeGeoData", "data.frame")
  return(y)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
