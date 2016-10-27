
#' Create an optpar list
#' 
#' \code{CreateOptpar} creates a list representing a HYPE optpar.txt file from an imported par.txt file 
#' and a selection of parameters.
#' 
#' @param x a list with named vector elements, as an object returned from \code{\link{ReadPar}}.
#' 

CreateOptpar <- function(x, pars, tasks, comment = "") {
  
  # check existense of pars in x
  te <- pars %in% names(x)
  if (!all(te)) {
    stop(paste0("Parameter(s) '", paste(pars[!te], collapse = "', '"), "' missing in 'x'."))
  }
  
  # check tasks argument
  if (!is.data.frame(tasks) || ncol(tasks) != 2) {
    stop("Argument 'tasks' is not a two-column data frame.")
  }
  if (nrow(tasks) > 20) {
    stop("Too many elements in argument 'tasks'. Maximum number of tasks and settings = 20.")
  }
  
  # fill tasks with empty rows
  
  # extract parameter from x
  pr <- x[names(x) %in% pars]
  
  # create par list with data frame elements (as in Optpar lists)
  prs <- lapply(pr, function(x) {data.frame(min = x, max = x, ival = 10^floor(log10(x/1000)))})
  
}