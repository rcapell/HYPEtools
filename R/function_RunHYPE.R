#' Run HYPE model
#'
#' `RunHYPE` is a wrapper to run a HYPE model using a specified HYPE executable.
#' 
#' @param hype.path Path to HYPE \code{.exe} executable (Windows) or HYPE file on Linux
#' @param info.dir Optional path to a separate directory containing the HYPE model setup (info.txt, GeoData.txt, etc.). If not specified, then the directory of \code{hype.path} will be used.
#' @param sequence Optional integer between 0 and 999 specifying which forcing files to use.
#' @param p Optional logical. If \code{TRUE}, then the \code{sequence} number will also be applied to the parameter file. Ignored if \code{FALSE} (default).
#' @param ... Additional arguments passed on to \code{\link{system}}.
#' 
#' @details
#' `RunHYPE` is a \code{\link{system}} wrapper to run a HYPE model via the system console. HYPE executables can be downloaded from the [official code repository](https://sourceforge.net/projects/hype/files/).
#' The path to the HYPE executable must be specified with \code{hype.path}. If your HYPE model is saved to a different directory than the HYPE executable (\code{hype.path}), then you can
#' specify the model directory with \code{info.dir}. The \code{sequence} argument can be used to specify which forcing files should be used for the simulation,
#' and \code{p} can be used to specify if the \code{sequence} number should also be applied to the parameter file.
#' Read more about how to run HYPE on the [HYPE wiki](http://hype.smhi.net/wiki/doku.php?id=start#how_to_run_hype).
#' 
#' @return
#' Output messagess from the HYPE executable are printed to the console.
#' 
#' @examples
#' \donttest{
#' RunHYPE()
#' }
#' 
#' @importFrom tools file_ext
#' @export

RunHYPE <- function(hype.path = NULL, info.dir = NULL, sequence = 0, p = FALSE, ...) {

  # Checks for hype.path --------------------------------------------------------------------------------------------------------------------
  
  # Stop if hype.path not specified
  if (is.null(hype.path)) {
    stop('"hype.path" must be specified')
  }
  
  # Check that HYPE executable exists
  if (!file.exists(hype.path)) {
    stop('HYPE executable not found at "hype.path"')
  }
  
  # Check that HYPE executable is correct file type
  if(Sys.info()["sysname"] == "Windows" & !file_ext(hype.path) == "exe"){
    stop('File at "hype.path" does not have extension ".exe"')
  } else if(Sys.info()["sysname"] == "Linux" & !file_ext(hype.path) == ""){
    stop('File at "hype.path" does not have extension "".')
  }
  
  # Checks for info.dir --------------------------------------------------------------------------------------------------------------------
  
  # Default model path as directory containing HYPE executable
  if (is.null(info.dir)) {
    info.dir <- dirname(hype.path)
  }

  # Check that model path exists
  if (!dir.exists(info.dir)) {
    stop('"info.dir" directory does not exist')
  }
  
  # Checks for sequence --------------------------------------------------------------------------------------------------------------------
  if(!is.numeric(sequence)){
    stop('"sequence" must be an integer between 0 and 999')
  } else if (!sequence%%1 == 0 | !(sequence >= 0) | !(sequence <= 999)) {
    stop('"sequence" must be an integer between 0 and 999')
  }
  
  # Checks for p --------------------------------------------------------------------------------------------------------------------
  if (!p %in% c(TRUE, FALSE)) {
    stop('"p" must be either "TRUE" or "FALSE"')
  }
  
  # Create Command --------------------------------------------------------------------------------------------------------------------
  
  # Create base command with HYPE executable, infodir, and sequence number
  command <- c(hype.path, "-i", paste0(info.dir, "/"), "-s", sequence)

  # Add p
  if (p == TRUE) {
    command <- c(command, "-p")
  }
  
  # Run HYPE --------------------------------------------------------------------------------------------------------------------
  
  # Run HYPE using system command
  system(paste(command, collapse = " "), ...)
  
}
