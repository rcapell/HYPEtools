#' @export
#' 
#' @title
#' Plot a suite of evaluation plots from a HYPE basin output file
#'
#' @description
#' Plot a standard suite of evaluation plots from a basin output file, typically used for model performance inspection and/or 
#' during manual calibration
#'
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables and an attribute \code{timestep}. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param area Numeric, upstream area of sub-basin in km^2. Required for calculation of accumulated volume error. Optional argument, 
#' either this or arguments \code{subid}, \code{gd}, and \code{bd} are required.
#' @param subid HYPE SUBID of a target sub-catchment (must exist in \code{gd}). Mandatory in combination with \code{gd} and 
#' optionally \code{bd} if argument \code{area} is not defined. Used to calculate upstream area internally with function 
#' \code{\link{SumUpstreamArea}}.
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory with argument 
#' \code{subid}, details see there. 
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional with argument 
#' \code{subid}, details see there. 
#' 
#' @details
#' date, uprf, upsf, temp, uppe, upev, cout, rout, soim, sm13, upfp, snow, uppr, ccin, rein, ccon, reon, cctn, retn, ccsp, resp, ccpp, repp, cctp, retp, wcom, wstr
#' \url{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}
#' 
#' 
#' @return 
#' Returns a multi-panel plot in a new graphics device.
#' 
#' @examples
#' PlotBasinOutput(x = mybasin, area = 56.67)

PlotBasinOutput <- function(x, logQ = F, from = 1, to = nrow(x), sitename = "", area = NULL, subid = NULL, gd = NULL, bd = NULL) {
  
  ### Preliminaries
  
  ## conditional: argument area given or able to calculate with arguments gd, bd, subid?,  incl. error handling
  if (is.null(area) & is.null(gd)) {
    stop("Provide either argument 'area' or argument 'gd'.")
  } else {
    if (is.null(area)) {
      if (is.null(subid)) {
        stop("Argument 'subid' is mandatory with argument 'gd'.")
      }
      uarea <- SumUpstreamArea(subid = subid, gd = gd, bd = bd)[, 2]
    } else {
      uarea <- area * 10^6
    }
  }
  
  
  ## identify column indices of target variables and total number of variables to plot
  
  # force lower case for names in basin output file, for selecting target variables below
  names(x) <- tolower(names(x))
  
  # create vector over all target output variable names which are potentially used in the plot panels
  nm.t <- c("date", "uprf", "upsf", "temp", "uppe", "upev", "cout", "rout", "soim", "sm13", "upfp", "snow", "uppr", 
            "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", "repp", "cctp", "retp", "wcom", "wstr")
  
  
  # identify existing and non-empty variables for plotting in user-provided basin output table
  # and save existing variables to vectors
  for (i in 1:length(nm)) {
    te <- tryCatch(with(x, get(nm.t[i])), error = function (e) NULL)
    if(!(is.null(te) | all(suppressWarnings(is.na(te))))) {
      assign(x = nm.t[i], te)
    }
  }
  
  # define which panels are to be plotted based on existing variables
  if (exists())
  
  col.date <- which(nm == "date")
  col.uprf <- which(nm == "uprf")
  col.upsf <- which(nm == "upsf")
  col.temp <- which(nm == "temp")
  col.uppe <- which(nm == "uppe")
  col.upev <- which(nm == "upev")
  col.cout <- which(nm == "cout")
  col.rout <- which(nm == "rout")
  col.soim <- which(nm == "soim")
  col.sm13 <- which(nm == "sm13")
  col.upfp <- which(nm == "upfp")
  col.snow <- which(nm == "snow")
  col.uppr <- which(nm == "uppr")
  col.ccin <- which(nm == "ccin")
  col.rein <- which(nm == "rein")
  col.ccon <- which(nm == "ccon")
  col.reon <- which(nm == "reon")
  col.cctn <- which(nm == "cctn")
  col.retn <- which(nm == "retn")
  col.ccsp <- which(nm == "ccsp")
  col.resp <- which(nm == "resp")
  col.ccpp <- which(nm == "ccpp")
  col.repp <- which(nm == "repp")
  col.cctp <- which(nm == "cctp")
  col.retp <- which(nm == "retp")
  col.wcom <- which(nm == "wcom")
  col.wstr <- which(nm == "wstr")
  
  
  # conditional incl. error handling: argument area given or able to calculate with arguments gd, bd, subid?
  if (is.null(area) & is.null(gd)) {
    stop("Provide either argument 'area' or argument 'gd'.")
  } else {
    if (is.null(area)) {
      if (is.null(subid)) {
        stop("Argument 'subid' is mandatory with argument 'gd'.")
      }
      uarea <- SumUpstreamArea(subid = subid, gd = gd, bd = bd)[, 2]
    } else {
      uarea <- area * 10^6
    }
  }
  
}

# debug
x <- ReadBasinOutput("//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/cal_wbalance_coarse_open/res1/9548212.txt")
x <- ReadBasinOutput("../9548212.txt")
names(x)
x <- x[, -c(20:25)]
from <- 1
from <- "2005-01-01"
to <- nrow(x)
river <- ""
gauge <- ""
area <- NULL
gd <- gdf
logQ <- F
filename <- NA
subid <- 51
bd <- NULL
