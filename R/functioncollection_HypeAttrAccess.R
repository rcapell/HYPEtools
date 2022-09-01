
#--------------------------------------------------------------------------------------------------------------------------------------
# 
# Collection of attribute access functions
# 
#--------------------------------------------------------------------------------------------------------------------------------------

#' Quickly query and set HYPE-specific attributes
#' 
#' These are simple convenience wrapper functions to quickly query and assign values of attributes which are added to HYPE data  
#' on import. 
#' 
#' @param x Object whose attribute is to be accessed
#' @param value Value to be assigned
#' 
#' @details 
#' These functions are just shortcuts for \code{\link{attr}}.
#' 
#' @return 
#' The extractor functions return the value of the respective attribute or \code{NULL} if no matching attribute is found.
#' 
#' @examples
#' te <- ReadBasinOutput(filename = system.file("demo_model", "results",
#' "0003587.txt", package = "HYPEtools"))
#' hypeunit(te)
#' timestep(te)
#' subid(te)
#' 
#' @name HypeAttrAccess
#' 

#' @rdname HypeAttrAccess
#' @export
datetime <- function(x) attr(x, "datetime")

#' @rdname HypeAttrAccess
#' @export
`datetime<-` <- function(x, value) {
  attr(x, "datetime") <- value
  x
}

#' @rdname HypeAttrAccess
#' @export
hypeunit <- function(x) attr(x, "hypeunit")

#' @rdname HypeAttrAccess
#' @export
`hypeunit<-` <- function(x, value) {
  attr(x, "hypeunit") <- value
  x
}

#' @rdname HypeAttrAccess
#' @export
obsid <- function(x) attr(x, "obsid")

#' @rdname HypeAttrAccess
#' @export
`obsid<-` <- function(x, value) {
  attr(x, "obsid") <- value
  x
}

#' @rdname HypeAttrAccess
#' @export
outregid <- function(x) attr(x, "outregid")

#' @rdname HypeAttrAccess
#' @export
`outregid<-` <- function(x, value) {
  attr(x, "outregid") <- value
  x
}

#' @rdname HypeAttrAccess
#' @export
subid <- function(x) attr(x, "subid")

#' @rdname HypeAttrAccess
#' @export
`subid<-` <- function(x, value) {
  attr(x, "subid") <- value
  x
  }

#' @rdname HypeAttrAccess
#' @export
timestep <- function(x) attr(x, "timestep")

#' @rdname HypeAttrAccess
#' @export
`timestep<-` <- function(x, value) {
  attr(x, "timestep") <- value
  x
}

#' @rdname HypeAttrAccess
#' @export
variable <- function(x) attr(x, "variable")

#' @rdname HypeAttrAccess
#' @export
`variable<-` <- function(x, value) {
  attr(x, "variable") <- value
  x
}

