#' Scale 'par.txt' files to different model time step
#'
#' `ScalePar` scales time step-dependent parameters in an imported 
#' [HYPE 'par.txt'](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:par.txt) parameter file to a 
#' new target time step. See HYPE wiki tutorial on [sub-daily time steps](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps).
#' 
#' @param x List containing HYPE parameters. Typically imported with [ReadPar()].
#' @param timestep.ratio Numeric, time step scaling factor. Defaults to (1/24) to scale from daily to hourly time steps. To scale from hourly to daily time steps, then use 24.
#' @param digits Integer, number of significant digits in scaled parameter values to export. See [signif()].
#' @param verbose Logical, if \code{TRUE}, then information will be printed.
#' @param print.par Logical, print known time-scale dependent parameters instead of scaling a parameter list.
#' 
#' @details
#' `ScalePar` simply applies a user-chosen scaling factor `timestep.ratio` to all time scale-dependent parameters
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

ScalePar <- function(x, timestep.ratio = 1 / 24, digits = 3, verbose = TRUE, print.par = FALSE) {
  
  # Timestep Dependent Parameters --------------------------------------------------------------------------------------------------------------------
  ts_dependent <- c(
    "cevp",
    "cmlt",
    "drydeppp",
    "eroddecay",
    "glaccmlt",
    "irrdemand",
    "licesndens",
    "mactrinf",
    "mperc1",
    "mperc2",
    "ocfldelx",
    "ppenrflow",
    "pprelmax",
    "rcgrwst", # Lotta checked and this parameter is time-step dependent, but it isn't in the table the HYPE wiki: http://www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
    "ricesndens",
    "rrcs1",
    "rrcs2",
    "rrcs3",
    "sdnsradd",
    "sdnsrate",
    "sedae",
    "sedoc",
    "sedon",
    "sedpp",
    "sedsi",
    "sedss",
    "snalbkexp",
    # "snowdensdt", # Lotta said that in the current HYPE version (5.25.0), this is actually hard-coded as daily and should not be scaled
    "srrcs",
    "trrcs",
    "wetdepspl"
  )
  
  # Reservoir Constant Parameters --------------------------------------------------------------------------------------------------------------------
  rc_dependent <- c(
    "rcgrwst", # Lotta checked and this parameter is time-step dependent, but it isn't in the table the HYPE wiki: http://www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
    "rrcs1",
    "rrcs2",
    "rrcs3",
    "srrcs",
    "trrcs"
  )
  
  # Conversion Function --------------------------------------------------------------------------------------------------------------------
  
  # To go from daily to hourly time steps use a ratio of 1/24, vice versa use 24/1
  conversion <- function(value, conversion_factor, reservoir_constant, digits){
    
    # If the parameter is a time constant in a linear reservoir
    if(reservoir_constant == TRUE){
      new_value <- 1 - ((1 - value) ^ conversion_factor)
    } else{
      new_value <-  value * conversion_factor
    }
    
    # Adjust significant digits
    new_value <- signif(new_value, digits = digits)
    
    # Return value
    return(new_value)
  }
  
  # Perform scaling --------------------------------------------------------------------------------------------------------------------
  
  # Just print known parameters
  if (print.par) {
    cat(paste0("Known time-step dependent parameters:\n", paste(ts_dependent, collapse = ", "), "."), "\n")
    cat(paste0("Known reservoir constant time-step dependent parameters:\n", paste(rc_dependent, collapse = ", "), "."), "\n")
    
  # Perform Scaling
  } else {
    
    # Get parameters to scale
    scale_parameters <- sort(names(x)[names(x) %in% ts_dependent])
    
    # Print information
    if (verbose) {
      cat(paste0("Scaled parameters:\n", paste(scale_parameters, collapse = ", "), "."), "\n")
    }
    
    # Scale parameters while accounting for reservoir constant dependent parameters
    for(parameter in scale_parameters){
      x[[parameter]] <- sapply(x[[parameter]], conversion, conversion_factor = timestep.ratio, reservoir_constant = parameter %in% rc_dependent, digits = digits)
    }
    
  }
  
  # Return scaled list
  return(x)
}
