
#' Get optimised classes from an imported optpar.txt file
#' 
#' \code{OptimisedClasses} checks which classes (land use or soil) of parameters in an imported optpar list are actually 
#' optimised, i.e. have a min/max range larger than zero.
#' 
#' @param x list with named elements, as an object returned from \code{\link{ReadOptpar}}. 
#' 
#' @details 
#' \code{OptimisedClasses} allows to quickly check which classes of parameters in an optpar.txt file are actually optimised 
#' during a HYPE optimisation run. The function compares min and max values in the \code{pars} element of an imported HYPE 
#' optpar.txt file to identify those.
#' 
#' @return 
#' \code{OptimisedClasses} returns a named list with one vector element for each parameter found in \code{x}. List element 
#' names are HYPE parameter names. Each vector contains the optimised class numbers for the respective parameter.
#' 
#' @examples 
#' te <- ReadOptpar(filename = system.file("demo_model", "optpar.txt", package = "HYPEtools"))
#' OptimisedClasses(te)
#' 
#' @aliases OptimizedClasses
#' @export

OptimisedClasses <- function(x) {
  
  # extract calibrated classes (as vector of class number per parameter)
  calib <- list()
  for (i in 1:length(x$pars)) {
    calib[[i]] <- which(x$pars[[i]][, 1] - x$pars[[i]][, 2] != 0)
    names(calib)[i] <- names(x$pars)[i]
  }
  
  return(calib)
}

#' @export

OptimizedClasses <- OptimisedClasses

