#' Scale 'AquiferData.txt' files to different model time steps
#'
#' `ScaleAquiferData` scales the \code{RETRATE} time step-dependent recession coefficient in an imported 
#' [HYPE 'AquiferData.txt'](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:aquiferdata.txt) file to a 
#' new target time step. See HYPE wiki tutorial on [sub-daily time steps](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps).
#' 
#' @param x Data frame containing HYPE AquiferData contents. Typically imported with [ReadAquiferData()].
#' @param timestep.ratio Numeric, time step scaling factor. Defaults to (1/24) to scale from daily to hourly time steps. To scale from hourly to daily time steps use 24.
#' @param digits Integer, number of significant digits in scaled parameter values to export. See [signif()].
#' @param verbose Logical, if \code{TRUE}, then information will be printed.
#' @param print.par Logical, print known time-scale dependent recession coefficients instead of scaling an AquiferData data frame.
#' 
#' @details
#' `ScaleAquiferData` applies a user-specified scaling factor `timestep.ratio` to the \code{RETRATE} time step-dependent recession coefficient
#' in a HYPE AquiferData data frame. All \code{RETRATE} values that are not equal to zero are assumed to be aquifer rows and will be converted to the new time step.
#' Recession coefficients are matched against an inbuilt set of column names. To see these names, call \code{ScaleAquiferData(print.par = TRUE)}. 
#' *[Please notify us](https://github.com/rcapell/HYPEtools/issues) if you find any missing coefficients.*
#'
#' Timestep-dependent recession coefficients are scaled using the relationship described in: 
#' Nalbantis, Ioannis (1995). “Use of multiple-time-step information in rainfall-runoff modelling”, Journal of Hydrology 165, 1-4, pp. 135–159.
#'
#' \code{new_coefficient_value = 1 - (1 - old_coefficient_value)^time_step_ratio}
#' 
#' @return
#' A [data.frame()] object as supplied in `x`, with re-scaled recession coefficients, or nothing if `print.par = TRUE`.
#' 
#' @seealso
#' \code{\link{ScalePar}}
#' 
#' @examples
#' # Import daily HYPE AquiferData file
#' ad <- ReadAquiferData(filename = system.file("demo_model",
#' "AquiferData_Example.txt", package = "HYPEtools"))
#' # Scale to hourly time steps
#' ScaleAquiferData(x = ad)
#' # Print all time scale-dependent coefficients known to the function
#' ScaleAquiferData(print.par = TRUE)
#' 
#' @export

ScaleAquiferData <- function(x = NULL, timestep.ratio = 1 / 24, digits = 3, verbose = TRUE, print.par = FALSE) {
  
  # Timestep Dependent Parameters --------------------------------------------------------------------------------------------------------------------
  # www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
  
  # Parameters that are timestep-dependent
  ts_dependent <- c(
    "RETRATE"
  )
  
  # Recession Coefficients --------------------------------------------------------------------------------------------------------------------
  # www.smhi.net/hype/wiki/doku.php?id=start:hype_tutorials:subdaily_timesteps
  
  # Recession coefficients that are timestep-dependent
  rc_dependent <- c(
    "RETRATE"
  )
  
  # Conversion Function --------------------------------------------------------------------------------------------------------------------
  
  # To go from daily to hourly time steps use a ratio of 1/24, vice versa use 24/1
  conversion <- function(value, conversion_factor, recession_coefficient, digits){
    
    # Do not scale values that are zero (i.e. non-aquifer rows)
    if(value == 0){
      new_value <- value
      
    # Scale values that are non-zero (i.e. aquifer rows)
    } else{
      # If the parameter is a recession coefficient - Relationship from: Nalbantis, Ioannis (1995). “Use of multiple-time-step information in rainfall-runoff modelling”, Journal of Hydrology 165, 1-4, pp. 135–159.
      if(recession_coefficient == TRUE){
        new_value <- 1 - ((1 - value) ^ conversion_factor)
      } else{
        new_value <-  value * conversion_factor
      }
      
      # Adjust significant digits
      new_value <- signif(new_value, digits = digits)
    }
    
    # Return value
    return(new_value)
  }
  
  # Perform scaling --------------------------------------------------------------------------------------------------------------------
  
  # Just print known parameters
  if (print.par) {
    cat(paste0("Known time-step dependent parameters:\n", paste(ts_dependent, collapse = ", "), "."), "\n")
    cat(paste0("Known time-step dependent rececession coefficients:\n", paste(rc_dependent, collapse = ", "), "."), "\n")
    
  # Perform Scaling
  } else {
    
    # Get columns to scale
    scale_columns <- sort(colnames(x)[colnames(x) %in% ts_dependent])
    
    # Print information
    if (verbose) {
      cat(paste0("Scaled columns:\n", paste(scale_columns, collapse = ", "), "."), "\n")
    }
    
    # Scale columns while accounting for recession coefficient dependent parameters
    for(column in scale_columns){
      x[[column]] <- sapply(x[[column]], conversion, conversion_factor = timestep.ratio, recession_coefficient = column %in% rc_dependent, digits = digits)
    }
    
    # Return scaled list
    return(x)
  }
}
