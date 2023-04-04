#' Scale 'par.txt' files to different model time step
#'
#' `ScalePar` scales time step-dependent parameters in an imported 
#' [HYPE 'par.txt'](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:par.txt) parameter file to a 
#' new target time step. 
#' 
#' @param x List containing HYPE parameters. Typically imported with [ReadPar()].
#' @param sfac Numeric, scale factor. Defaults to scaling from daily to hourly time steps.
#' @param digits Integer, number of significant digits *in SLC class columns* to export. See [signif()].
#' @param verbose Logical, if \code{TRUE}, then information will be printed.
#' @param print.par Logical, print known time-scale dependent parameters instead of scaling a par list.
#' 
#' @details
#' `ScalePar` simply applies a user-chosen scaling factor `sfac` to all time scale-dependent parameters
#' in a HYPE parameter list. Parameters are matched against an inbuilt set of parameter names. 
#' *[Please notify us](https://github.com/rcapell/HYPEtools/issues) if you find parameters missing.*
#' 
#' @return
#' A [list()] object as supplied in `x`, with parameters re-scaled parameters, or nothing if `print.par = TRUE`.
#' 
#' @examples
#' # Import daily HYPE parameter file
#' hpar <- ReadPar(filename = system.file("demo_model", "par.txt", package = "HYPEtools"))
#' # Scale to hourly time steps
#' ScalePar(x = hpar)
#' # Print all time scale-dependent parameters known to the function
#' ScalePar(print.par = TRUE)
#' 
#' @export

ScalePar <- function(x, sfac = 1/24, digits = 3, verbose = TRUE, print.par = FALSE) {
  
  # look-up vector with scale-dependent parameters
  luv <- c("cevp", "cmlt", "drydeppp", "eroddecay", "glaccmlt", "irrdemand", 
           "licesndens", "mactrinf", "mperc1", "mperc2", "ocfldelx", "ppenrflow", 
           "pprelmax", "ricesndens", "rrcs1", "rrcs2", "rrcs3", "sdnsradd", 
           "sdnsrate", "sedae", "sedoc", "sedon", "sedpp", "sedsi", "sedss", 
           "snalbkexp", "snowdensdt", "srrcs", "trrcs", "wetdepspl")
  
  
  if (print.par) {
    
    # just print known parameters
    cat(paste0("Known time scale-dependent parameters:\n", paste(luv, collapse = ", "), "."), "\n")
    
  } else {
    
    ## scale
    
    # print information
    if (verbose) {
      cat(paste0("Scaled parameters:\n", paste(names(x)[names(x) %in% luv], collapse = ", "), "."), "\n")
    }
    
    # scale
    x[names(x) %in% luv] <- lapply(x[names(x) %in% luv], function(x, sfac, digits) {x <- signif(x * sfac, digits = digits)}, 
                                   sfac = sfac, digits = digits)
    
    x
  }
}
