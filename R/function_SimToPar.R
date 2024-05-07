#'
#' HYPE Calibration Outputs to par.txt
#'
#' Update par.txt with values from an allsim.txt or bestsims.txt file
#'
#' @param simfile Imported allsim.txt or bestsims.txt file imported as data frame.
#' @param row Integer, row number indicating row containing the parameter values that should be replaced/added to \code{par}.
#' @param par Imported par.txt file that should be updated using parameter values from \code{simfile}. Typically imported using \code{\link{ReadPar}}.
#' 
#' @details
#' \code{AllSimToPar} and \code{BestSimsToPar} can be used to update an existing par.txt file with the parameter values from a HYPE allsim.txt or bestsims.txt file.
#' If a parameter in the allsim or bestsims file already exists in \code{par}, then the parameter values will be overwritten in \code{par}. If the parameter does not exist,
#' then the parameter will be added to the bottom of the output.
#'
#' @return
#' \code{AllSimToPar} and \code{BestSimsToPar} return a list of named vectors in the format used by \code{\link{ReadPar}}.
#'
#' @seealso
#' \code{\link{ReadPar}} for HYPE par.txt import; \code{\link{WritePar}} to export HYPE par.txt files

#' @examples
#' simfile <- read.table(file = system.file("demo_model", "results",
#'   "bestsims.txt",
#'   package = "HYPEtools"
#' ), header = TRUE, sep = ",")
#' par <- ReadPar(filename = system.file("demo_model", "par.txt", package = "HYPEtools"))
#' BestSimsToPar(simfile, 1, par)
#' 
#' @name SimToPar
NULL

#' @export AllSimToPar
#' @export BestSimsToPar
#' @importFrom dplyr filter group_by summarize
#' @importFrom data.table transpose
#' @importFrom rlang .data

#' @rdname SimToPar
AllSimToPar <- function(simfile, row, par){
  
  # Allsim column names from: http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:allsim.txt&s[]=jpop
  # Bestsim column names from: http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:bestsims.txt
  stat_cols  <- c("NO", "CRIT", "rr2", "sr2", "mr2", "rmae", "sre", "rre", "mre", "rra", "sra", "mra", "tau", "md2", "mda", "mrs", "mcc", "mdkg", "akg", "asckg", "mar", "mdnr", "mnw", "snr", "smb", "numrc", "nummc", "jpop", "igen", "iacc")
  
  # Get parameters from allsims or bestsims file
  pars_df <- data.table::transpose(simfile[row,], keep.names = "par") %>% # Get row from simfile
    filter(!par %in% stat_cols) %>% # Remove statistics columns
    group_by(par) %>% # Group by parameter name
    summarize(value = paste(.data$V1, collapse = " ")) # Concatenate values into one string for each parameter
  
  # Message stating all identified parameters
  message(paste0('Parameters identified in input "simfile": ', paste(pars_df$par, collapse = ", ")))
  
  # Update par.txt
  for(i in 1:nrow(pars_df)){
    
    # Identify rows which match parameter name
    rows <- which(names(par) == pars_df$par[i])
    
    # Update existing parameter values
    if(length(rows) > 0){
      par[rows] <- pars_df$value[i]
      
      # Add new parameter
    } else{
      par[pars_df$par[i]] <- pars_df$value[i]
      message(paste0('Parameter "', pars_df$par[i], '" not found in input "par". This parameter will be added to the bottom of the output.'))
    }
  }
  
  # Return par
  return(par)
}

# Alias
#' @rdname SimToPar
#' @export
BestSimsToPar <- AllSimToPar
