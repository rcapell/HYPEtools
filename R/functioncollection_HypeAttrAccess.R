
#--------------------------------------------------------------------------------------------------------------------------------------
# 
# Collection of attribute assessment functions
# 
#--------------------------------------------------------------------------------------------------------------------------------------

#' Quickly query HYPE-specific attributes
#' 
#' These are simple convenience wrapper functions to quickly query attributes which are added to HYPE files on import.
#' 
#' @param x an object whose attribute is to be accessed
#' 
#' @details 
#' These functions are just shortcuts for \code{\link{attr}}.
#' 
#' @name HypeAttrAccess
#' 

#' @rdname HypeAttrAccess
#' @export
datetime <- function(x) attr(x, "datetime")

#' @rdname HypeAttrAccess
#' @export
obsid <- function(x) attr(x, "obsid")

#' @rdname HypeAttrAccess
#' @export
outregid <- function(x) attr(x, "outregid")

#' @rdname HypeAttrAccess
#' @export
subid <- function(x) attr(x, "subid")

#' @rdname HypeAttrAccess
#' @export
timestep <- function(x) attr(x, "timestep")

#' @rdname HypeAttrAccess
#' @export
variable <- function(x) attr(x, "variable")

