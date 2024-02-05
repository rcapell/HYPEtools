#' Scale 'par.txt' files to different model time step
#'
#' `ScalePar` scales time step-dependent parameters and recession coefficients in an imported 
#' [HYPE 'par.txt'](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:par.txt) parameter file to a 
#' new target time step. See HYPE wiki tutorial on [sub-daily time steps](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps).
#' 
#' @param x List containing HYPE parameters. Typically imported with [ReadPar()].
#' @param timestep.ratio Numeric, time step scaling factor. Defaults to (1/24) to scale from daily to hourly time steps. To scale from hourly to daily time steps use 24.
#' @param digits Integer, number of significant digits in scaled parameter values to export. See [signif()].
#' @param verbose Logical, if \code{TRUE}, then information will be printed.
#' @param print.par Logical, print known time-scale dependent parameters and recession coefficients instead of scaling a parameter list.
#' 
#' @details
#' `ScalePar` applies a user-specified scaling factor, `timestep.ratio`, to all time scale-dependent parameters and recession coefficients
#' in a HYPE parameter list. Parameters are matched against an inbuilt set of parameter names. To see these parameters, call \code{ScalePar(print.par = TRUE)}. 
#' *[Please notify us](https://github.com/rcapell/HYPEtools/issues) if you find any missing parameters.*
#'
#' If parameters are not timestep-dependent recession coefficients, then scaling is performed using the ratio between the two time step lengths (e.g. 1/24 when scaling from daily to hourly time steps).
#' If parameters are timestep-dependent recession coefficients, then scaling is performed using the relationship described in: 
#' Nalbantis, Ioannis (1995). “Use of multiple-time-step information in rainfall-runoff modelling”, Journal of Hydrology 165, 1-4, pp. 135–159.
#'
#' \code{new_parameter_value = 1 - (1 - old_parameter_value)^time_step_ratio}
#' 
#' @return
#' A [list()] object as supplied in `x`, with re-scaled parameters and recession coefficients, or nothing if `print.par = TRUE`.
#' 
#' @seealso
#' \code{\link{ScaleAquiferData}}
#' \code{\link{ScaleFloodData}}
#' 
#' @examples
#' # Import daily HYPE parameter file
#' par <- ReadPar(filename = system.file("demo_model", "par.txt", package = "HYPEtools"))
#' # Scale to hourly time steps
#' ScalePar(x = par)
#' # Print all time scale-dependent parameters known to the function
#' ScalePar(print.par = TRUE)
#' 
#' @export

ScalePar <- function(x = NULL, timestep.ratio = 1 / 24, digits = 3, verbose = TRUE, print.par = FALSE) {
  
  # Timestep Dependent Parameters --------------------------------------------------------------------------------------------------------------------
  # www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
  
  # Parameters that are timestep-dependent
  ts_dependent <- c(
    "cevp",
    "cmlt",
    "drydeppp",
    "eroddecay",
    "glaccmlt",
    "irrdemand",
    # "licesndens", # Lotta said that in the current HYPE version (5.25.0), this is actually hard-coded as daily and should not be scaled
    "mactrinf",
    "mperc1",
    "mperc2",
    "ocfldelx",
    "opt5", # Replaces FloodData recession coefficients if optonoff == 1 or optonoff == 3 - The floodplain routine hasn't been checked to see if it works with other timesteps
    "opt8", # Replaces FloodData recession coefficients if optonoff == 1 or optonoff == 3 - The floodplain routine hasn't been checked to see if it works with other timesteps
    "ppenrflow",
    "pprelmax",
    "rcgrw",
    "rcgrwst",
    # "ricesndens",  # Lotta said that in the current HYPE version (5.25.0), this is actually hard-coded as daily and should not be scaled
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
  
  # Recession Coefficients --------------------------------------------------------------------------------------------------------------------
  # www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
  
  # Recession coefficients that are timestep-dependent
  rc_dependent <- c(
    "opt5", # Replaces FloodData recession coefficients if optonoff == 1 or optonoff == 3
    "opt8", # Replaces FloodData recession coefficients if optonoff == 1 or optonoff == 3
    "rcgrw",
    "rcgrwst",
    "rrcs1",
    "rrcs2",
    "rrcs3",
    "srrcs",
    "trrcs"
  )
  
  # Conversion Function --------------------------------------------------------------------------------------------------------------------
  
  # To go from daily to hourly time steps use a ratio of 1/24, vice versa use 24/1
  conversion <- function(value, conversion_factor, recession_coefficient, digits){
    
    # If the parameter is a recession coefficient - Relationship from: Nalbantis, Ioannis (1995). “Use of multiple-time-step information in rainfall-runoff modelling”, Journal of Hydrology 165, 1-4, pp. 135–159.
    if(recession_coefficient == TRUE){
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
    cat(paste0("Known time-step dependent parameters:\n", paste(ts_dependent, collapse = ", ")), "\n")
    cat(paste0("Known time-step dependent rececession coefficients:\n", paste(rc_dependent, collapse = ", ")), "\n")
    
  # Perform Scaling
  } else {
    
    # Get parameters to scale
    scale_parameters <- sort(names(x)[names(x) %in% ts_dependent])
    
    # Print information
    if (verbose) {
      cat(paste0("Scaled parameters:\n", paste(scale_parameters, collapse = ", ")), "\n")
      if(any(c("opt5", "opt8") %in% scale_parameters)){
        warning("The floodplain routine has not been tested with different time steps as of 2024-02-05. Use caution when scaling opt5 and opt8!", call. = FALSE)
      }
    }
    
    # Scale parameters while accounting for recession coefficient dependent parameters
    for(parameter in scale_parameters){
      x[[parameter]] <- sapply(x[[parameter]], conversion, conversion_factor = timestep.ratio, recession_coefficient = parameter %in% rc_dependent, digits = digits)
    }
    
  }
  
  # Return scaled list
  return(x)
}
