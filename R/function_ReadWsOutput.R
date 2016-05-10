#' @export
#' @importFrom pbapply pblapply
#' 
#' @title
#' Read optimisation simulation results
#'
#' @description
#' Read and combine HYPE optimisation simulation output files, generated with 'task WS' during HYPE optimisation runs. Outputs can 
#' consist of basin, time, or map output files.
#' 
#' @param path Character string, path to the directory holding simulation output files to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param type Character string, keyword for HYPE output file type to import. One of \code{"time"}, \code{"map"}, or 
#' \code{"basin"}. Can be abbreviated. The first two require specification of argument \code{hype.var}, the latter of argument 
#' \code{subid}. Format of return value depends on output type, see details.
#' @param hype.var Character string, keyword to specify HYPE output variable to import. Not case-sensitive. Required in combination 
#' with \code{type} \code{"time"} or \code{"map"}.
#' @param subid Integer, giving a single SUBID for which to import basin output files. Required in combination with \code{type} 
#' \code{"basin"}.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}, for conversion of date-time information in imported 
#' result files to POSIX dates, which are returned as attribute. Incomplete format strings for monthly and annual values allowed, e.g. 
#' '\%Y'. Defaults to \code{NULL}, which prevents date-time conversion, applicable e.g. for files containing just one column of 
#' summary values over the model period.
#' @param subid.select Integer, vector of HYPE SUBIDs to select from imported time and map output files.
#' @param from Integer. For partial imports, number of simulation iteration to start from.
#' @param to Integer. For partial imports, number of simulation iteration to end with.
#' @param progbar Logical, display a progress bar while importing HYPE output files. Adds overhead to calculation time but useful 
#' when many files are imported.
#' 
#' @details
#' HYPE optimisation routines optionally allow for generation of simulation output files for each iteration in the optimisation routine. 
#' For further details see decumentation on 'task WS' in the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:optpar.txt}{optpar.txt online documentation}. 
#' 
#' \code{ReadWsOutput} imports and combines all simulation iterations in an \code{\link{array}}, which can then be easily used in 
#' further analysis, most likely in combination with performance and parameter values from an imported corresponding 'allsim.txt' file. 
#' The result folder containing HYPE WS results, argument \code{path}, can contain other files as well, \code{ReadWsOutput} searches for 
#' file name pattern to filter targeted result files.
#' 
#' For large numbers of result files, simulations can be partially imported using arguments \code{from} and \code{to}, in order to avoid 
#' memory exceedance problems.
#' 
#' @return
#' \code{ReadWsOutput} returns a 3-dimensional array with additional attributes. The array content depends on the HYPE output file type 
#' specified in argument \code{type}. Time and map output file imports return an array of class \code{\link{HypeSingleVar}} with 
#' \code{[time, subid, iteration]} dimensions, basin output file imports return a numeric array with \code{[time, subid, iteration]} 
#' dimensions.
#' 
#' Returned arrays contain additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times, \code{POSIX} if argument \code{dt.format} is non-\code{NULL}. Corresponds to 1st array 
#' dimension.}
#' \item{\strong{subid}}{A vector of SUBIDs. Corresponds to 2nd array dimension for time and map output files.}
#' \item{\strong{variable}}{A vector of HYPE output variables. Corresponds to 2nd array dimension for basin output files.}
#' }
#' 
#' @examples
#' \dontrun{ReadWsOutput(path = "../my_optim_results/", hype.var = "cout", dt.format = "%Y-%m")}



ReadWsOutput <- function(path, type = c("time", "map", "basin"), hype.var = NULL, subid = NULL, dt.format = NULL, 
                         subid.select = NULL, from = NULL, to = NULL, progbar = T) {
  
  type <- match.arg(type)
  
  # create vector of time or map file locations
  if (type %in% c("map", "time")) {
    # check if required argument hype.var exists
    if (is.null(hype.var)) {
      stop("Argument 'hype.var' required with time and map output files.")
    }
    # import
    locs <- list.files(path = path, pattern = glob2rx(paste0(type, toupper(hype.var), "*.txt")), full.names = TRUE)
    # break if no files found
    if (length(locs) == 0) {
      stop(paste("No", type, "output files for variable", toupper(hype.var), "found in directory specified in argument 'path'."))
    }
    # select iteration sub-set
    if (!is.null(from) || !is.null(to)) {
      if (is.null(from)) {
        from <- 1
      }
      if (is.null(to)) {
        to <- length(locs)
      }
      locs <- locs[from:to]
    }
  }
  
  # create vector of basin file locations
  if (type == "basin") {
    # check if required argument subid exists
    if (is.null(subid)) {
      stop("Argument 'subid' required with time and map output files.")
    }
    # import
    locs <- list.files(path = path, pattern = glob2rx(paste0("*", subid, "_", "???????.txt")), full.names = TRUE)
    # break if no files found
    if (length(locs) == 0) {
      stop(paste("No", type, "output files for subid", subid, "found in directory specified in argument 'path'."))
    }
  }
  
  # import routines, conditional on file type
  if (type == "time") {
    
    # dummy file to extract attributes from
    te <- ReadTimeOutput(filename = locs[1], dt.format = dt.format)
    
    sbd <- attr(te, "subid")
    
    if (!is.null(subid.select)) {
      if (!all(subid.select %in% sbd)) {
        stop("Not all SUBIDs in 'subid.select' found in time output files.")
      }
      sbd <- subid.select
    }
    
    # columns to select from each imported time output file
    ind <- c(FALSE, attr(te, "subid") %in% sbd)
    
    if (progbar) {
      res <- pblapply(locs, function(x, df, i) {as.matrix(ReadTimeOutput(filename = x, dt.format = df)[, i])}, df = dt.format, i = ind)
      res <- simplify2array(res)
    } else {
      res <- lapply(locs, function(x, df, i) {as.matrix(ReadTimeOutput(filename = x, dt.format = df)[, i])}, df = dt.format, i = ind)
      res <- simplify2array(res)
    }
    # add attributes with information
    attr(res, "variable") <- toupper(hype.var)
    attr(res, "date") <- te[, 1]
    attr(res, "subid") <- attr(te, "subid")[attr(te, "subid") %in% sbd]
    class(res) <- c("HypeSingleVar", "array")
    
  } else if (type == "map") {
    
    # dummy file to extract attributes from
    te <- ReadMapOutput(filename = locs[1], dt.format = dt.format)
    
    sbd <- te[, 1]
    
    if (!is.null(subid.select)) {
      if (!all(subid.select %in% sbd)) {
        stop("Not all SUBIDs in 'subid.select' found in map output files.")
      }
      sbd <- subid.select
    }
    
    # rows to select from each imported map output file
    ind <- te[, 1] %in% sbd
    
    if (progbar) {
      res <- pblapply(locs, function(x, df, i) {t(as.matrix(ReadMapOutput(filename = x, dt.format = df)[i, -1]))}, df = dt.format, i = ind)
      res <- simplify2array(res)
    } else {
      res <- lapply(locs, function(x, df, i) {t(as.matrix(ReadMapOutput(filename = x, dt.format = df)[i, -1]))}, df = dt.format, i = ind)
      res <- simplify2array(res)
    }
    # add attributes with information
    attr(res, "variable") <- toupper(hype.var)
    attr(res, "date") <- attr(te, "date")
    attr(res, "subid") <- te[, 1][te[, 1] %in% sbd]
    class(res) <- c("HypeSingleVar", "array")
    
  } else {
    # type == "basin"
    
    # dummy file to extract attributes from
    te <-ReadBasinOutput(locs[1], dt.format = dt.format)
    
    if (progbar) {
      res <- pblapply(locs, function(x, df) {as.matrix(ReadBasinOutput(filename = x, dt.format = df)[, -1])}, df = dt.format)
      res <- simplify2array(res)
    } else {
      res <- lapply(locs, function(x, df) {as.matrix(ReadBasinOutput(filename = x, dt.format = df)[, -1])}, df = dt.format)
      res <- simplify2array(res)
    }
    
    attr(res, "date") <- te[, 1]
    attr(res, "subid") <- subid
    attr(res, "variable") <- names(te)[-1]
    
  }

  return(res)
  
}

# debug
# library(pbapply)
# path <- "//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/calib_wbalance_fine_local/res_mc_in_51_50_daily/"
# path <- "//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/test/test_basin"
# hype.var <- "ccin"
# dt.format <- NULL
# dt.format <- "%Y"
# dt.format <- "%Y-%m"
# progbar <- T
# type <- "time"
# type <- "map"
# type <- "basin"
# subid <- 51