
#' Create an optpar list
#' 
#' \code{CreateOptpar} creates a list representing a HYPE optpar.txt file from an imported par.txt file 
#' and a selection of parameters.
#' 
#' @param x a list with named vector elements, as an object returned from \code{\link{ReadPar}}. 
#' @param pars Character vector with HYPE parameter names to be included in optpar list. Parameters must 
#' exist in \code{x}. Not case-sensitive. For a complete list of HYPE parameters, see the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:par.txt}{par.txt online documentation}. 
#' @param tasks Data frame with two columns providing optimisation tasks and settings (key-value pairs) as 
#' described in the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:optpar.txt}{optpar.txt online documentation}. 
#' Defaults to an empty task section.
#' @param comment Character string, comment (first row in optpar.txt file). 
#' @param fun.ival Either \code{NULL} (default), or a function with a single argument. See Details.
#' 
#' @details 
#' \code{CreateOptpar} makes it a bit more convenient to compose a HYPE optimisation file. The function creates a template 
#' with all parameters to be included in an optimisation run.
#' 
#' Parameter boundaries for individual classes have to be adapted after creation of the template, the function takes the 
#' existing parameter value(s) in \code{x} as upper and lower boundaries. 
#' 
#' Parameter step width intervals (third parameter rows in optpar.txt files) are calculated with an internal function 
#' which per default returns the nearest single 1/1000th of the parameter value, with conditional replacement of '0' intervals: 
#' 
#' \preformatted{function(x) {
#'   res <- 10^floor(log10(x/1000))
#'   ifelse(res == 0, .1, res)
#' }}
#' 
#' Alternative functions can be passed to \code{CreateOptpar} using argument \code{fun.ival}. Such functions must have a 
#' single argument \code{x}, which represents the parameter value taken from argument \code{x}. The function is applied to 
#' all parameters in the resulting optpar list.
#' 
#' @return 
#' The function returns a list with elements as described in \code{\link{ReadOptpar}}.
#' 
#' @seealso 
#' \code{\link{ReadOptpar}} \code{\link{WriteOptpar}} \code{\link{OptimisedClasses}} 
#' 
#' @examples
#' # Import a HYPE parameter file
#' te1 <- ReadPar(filename = system.file("demo_model", "par.txt", package = "HYPEtools"))
#' # Create optimisation parameters for a Monte Carlo run with 1000 iterations
#' te2 <- data.frame(key = c("task", "num_mc", "task"), value = c("MC", 1000, "WS"))
#' # Create an optpar file structure for HYPE recession coefficients
#' te3 <- CreateOptpar(x = te1, pars = c("rrcs1", "rrcs2"), tasks = te2)
#' te3
#' 
#' @export

CreateOptpar <- function(x, pars, tasks = data.frame(character(), character()), comment = "", fun.ival = NULL) {
  
  # convert parameter names check existense of pars in x
  te <- tolower(pars) %in% tolower(names(x))
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
  
  # fill tasks with empty rows and add header
  nrt <- nrow(tasks)
  if (nrt < 20) {
    # create and name empty task rows (identical names required for rbind)
    etr <- data.frame(rep("", 20 - nrt), rep("", 20 - nrt))
    names(etr) <- names(tasks)
    # combine with user-provided task rows
    tasks <- rbind(tasks, etr)
  }
  
  # extract parameter from x
  pr <- x[tolower(names(x)) %in% tolower(pars)]
  
  # function to calculate ival with
  if (!is.null(fun.ival)) {
    if (!is.function(fun.ival)) {
      stop("'fun.ival' is not a function.")
    }
  } else {
    fun.ival <- function(x) {
      res <- 10^floor(log10(x/1000))
      ifelse(res == 0, .1, res)
    }
  }
  
  # create par list with data frame elements (as in Optpar lists)
  prs <- lapply(pr, function(x) {data.frame(min = x, max = x, ival = fun.ival(x))})
  
  return(list(comment = comment, tasks = tasks, pars = prs))
}
