#' 
#' Read optimisation simulation results
#'
#' Read and combine HYPE optimisation simulation output files, generated with 'task WS' during HYPE optimisation runs. Outputs can 
#' consist of basin, time, or map output files.
#' 
#' @param path Character string, path to the directory holding simulation output files to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'.
#' @param type Character string, keyword for HYPE output file type to import. One of \code{"time"}, \code{"map"}, or 
#' \code{"basin"}. Can be abbreviated. The first two require specification of argument \code{hype.var}, the latter of argument 
#' \code{subid}. Format of return value depends on output type, see details.
#' @param hype.var Character string, keyword to specify HYPE output variable to import. Must include "RG"-prefix in case of output region files.
#' Not case-sensitive. Required in combination with \code{type} \code{"time"} or \code{"map"}.
#' @param id Integer, giving a single SUBID or OUTREGID for which to import basin output files. Required in combination with \code{type} 
#' \code{"basin"}.
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}, for conversion of date-time information in imported 
#' result files to POSIX dates, which are returned as attribute. Incomplete format strings for monthly and annual values allowed, e.g. 
#' '\%Y'. Defaults to \code{NULL}, which prevents date-time conversion, applicable e.g. for files containing just one column of 
#' summary values over the model period.
#' @param select Integer vector, column numbers to import, for use with \code{type = "time"}. Note: first column with dates must be 
#' imported.
#' @param from Integer. For partial imports, number of simulation iteration to start from.
#' @param to Integer. For partial imports, number of simulation iteration to end with.
#' @param progbar Logical, display a progress bar while importing HYPE output files. Adds overhead to calculation time but useful 
#' when many files are imported.
#' @param warn.nan Logical, check if imported results contain any \code{NaN} values. If \code{TRUE} and \code{NaN}s are found, 
#' a warning is thrown and affected SUBIDs and iterations are saved in an attribute \code{subid.nan}. Adds noticeable overhead to 
#' import time for large simulation file sets.
#' 
#' @details
#' HYPE optimisation routines optionally allow for generation of simulation output files for each iteration in the optimisation routine. 
#' For further details see documentation on 'task WS' in the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:optpar.txt}{optpar.txt online documentation}. 
#' 
#' \code{ReadWsOutput} imports and combines all simulation iterations in an \code{\link{array}}, which can then be easily used in 
#' further analysis, most likely in combination with performance and parameter values from an imported corresponding 'allsim.txt' file. 
#' 
#' The result folder containing HYPE WS results, argument \code{path}, can contain other files as well, \code{ReadWsOutput} searches for 
#' file name pattern to filter targeted result files. However, if files of the same type exist from different model runs, e.g. 
#' from another calibration run or from a standard model run, the pattern search cannot distinguish these from the targeted files 
#' and \code{ReadWsOutput} will fail.
#' 
#' For large numbers of result files, simulations can be partially imported using arguments \code{from} and \code{to}, in order to avoid 
#' memory exceedance problems.
#' 
#' @return
#' \code{ReadWsOutput} returns a 3-dimensional array with additional attributes. The array content depends on the HYPE output file type 
#' specified in argument \code{type}. Time and map output file imports return an array of class \code{\link{HypeSingleVar}} with 
#' \code{[time, subid, iteration]} dimensions, basin output file imports return an array of class \code{\link{HypeMultiVar}} with 
#' \code{[time, variable, iteration]} dimensions. An additional attribute \code{subid.nan} might be 
#' returned, see argument \code{warn.nan}, containing a list with SUBID vector elements. Vectors contain iterations where \code{NaN} 
#' values occur for the given subid.

#' 
#' Returned arrays contain additional \code{\link{attributes}}:
#' \describe{
#' \item{\strong{date}}{A vector of date-times, \code{POSIX} if argument \code{dt.format} is non-\code{NULL}. Corresponds to 1st array 
#' dimension.}
#' \item{\strong{subid}}{A (vector of) SUBID(s). Corresponds to 2nd array dimension for time and map output files. 
#' \code{NA} if not applicable.}
#' \item{\strong{outregid}}{A (vector of) OUTREGID(s). Corresponds to 2nd array dimension for time and map output files. 
#' \code{NA} if not applicable.}
#' \item{\strong{variable}}{A vector of HYPE output variables. Corresponds to 2nd array dimension for basin output files.}
#' \item{\strong{nan (optional)}}{A named list with SUBID or HYPE variable vector elements. Vectors contain iterations where \code{NaN} 
#' values occur for the given SUBID/HYPE variable.}
#' }
#' 
#' @examples
#' te <- ReadWsOutput(path = system.file("demo_model",
#' "results", package = "HYPEtools"), type = "map",
#' hype.var = "cout", dt.format = "%Y-%m")
#' te
#' 
#' @importFrom pbapply pblapply
#' @importFrom data.table fread is.data.table transpose
#' @importFrom utils glob2rx
#' @importFrom rlang .data
#' @export



ReadWsOutput <- function(path, type = c("time", "map", "basin"), hype.var = NULL, id = NULL, dt.format = NULL, 
                         select = NULL, from = NULL, to = NULL, progbar = TRUE, warn.nan = FALSE) {
  
  type <- match.arg(type)
  
  # create vector of time or map file locations
  if (type %in% c("map", "time")) {
    # check if required argument hype.var exists
    if (is.null(hype.var)) {
      stop("Argument 'hype.var' required with time and map output files.")
    }
    # import
    locs <- list.files(path = path, pattern = glob2rx(paste0(type, toupper(hype.var), "?*.txt")), full.names = TRUE)
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
    if (is.null(id)) {
      stop("Argument 'id' required with basin output files.")
    }
    # import
    locs <- list.files(path = path, pattern = paste0("^[0]{0,6}", id,  "_.*\\.txt$"), full.names = TRUE)
    # break if no files found
    if (length(locs) == 0) {
      stop(paste("No", type, "output files for SUBID/OUTREGID", id, "found in directory specified in argument 'path'."))
    }
  }
  
  # import routines, conditional on file type
  if (type == "time") {
    
    # dummy file to extract attributes from
    te <- ReadTimeOutput(filename = locs[1], dt.format = dt.format, select = select, type = "dt")
    
    # import
    if (progbar) {
      res <- pblapply(locs, function(x) {as.matrix(ReadTimeOutput(filename = x, dt.format = dt.format, select = select, type = "dt")[, !"DATE", with = FALSE])})
      res <- simplify2array(res)
    } else {
      res <- lapply(locs, function(x) {as.matrix(ReadTimeOutput(filename = x, dt.format = dt.format, select = select, type = "dt")[, !"DATE", with = FALSE])})
      res <- simplify2array(res)
    }
    # add attributes with information
    attr(res, "variable") <- toupper(hype.var)
    attr(res, "datetime") <- te[["DATE"]]
    attr(res, "subid") <- attr(te, "subid")
    attr(res, "outregid") <- attr(te, "outregid")
    class(res) <- c("HypeSingleVar", "array")
    
  } else if (type == "map") {
    
    # dummy file to extract attributes from
    te <- ReadMapOutput(filename = locs[1], dt.format = dt.format, type = "dt")
    
    # import
    if (progbar) {
      res <- pblapply(locs, function(x) {as.matrix(transpose(ReadMapOutput(filename = x, dt.format = dt.format, type = "dt", hype.var = hype.var)[, !"SUBID", with = FALSE]))})
      res <- simplify2array(res)
    } else {
      res <- lapply(locs, function(x) {t(as.matrix(ReadMapOutput(filename = x, dt.format = dt.format, type = "dt", hype.var = hype.var)[, !"SUBID", with = FALSE]))})
      res <- simplify2array(res)
    }
    # add attributes with information
    attr(res, "variable") <- toupper(hype.var)
    attr(res, "datetime") <- attr(te, "datetime")
    attr(res, "subid") <- te[["SUBID"]]
    attr(res, "outregid") <- attr(te, "outregid")
    dimnames(res)[[2]] <- paste0("X", te[["SUBID"]])
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
    
    attr(res, "datetime") <- te[, 1]
    attr(res, "subid") <- attr(te, "subid")
    attr(res, "outregid") <- attr(te, "outregid")
    attr(res, "variable") <- names(te)[-1]
    class(res) <- c("HypeMultiVar", "array")
    
  }
  
  # check for existence of NaN values
  if (warn.nan) {
    te <- apply(res, 2:3, function(x) any(is.nan(x)))
    if (any(te)) {
      warning("'NaN' values found in one or more SUBIDs. SUBIDs and iterations saved in attribute 'subid.nan'.")
      # convert to list of subids, with vector elements which contain iterations with NaN occurences
      te <- apply(te, 1, which)
      te <- te[lapply(te, length) > 0]
      attr(res, "nan") <- te
    }
  }
  
  return(res)
  
}
