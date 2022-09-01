#' Extract statistics from time series
#'
#' Calculate aggregated statistics from long-term time series, typically imported HYPE time output files.
#'
#' @param x Data frame, with column-wise equally-spaced time series. Date-times in \code{\link{POSIXct}} format in first column.
#' Typically an imported time output file from HYPE.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year.
#' @param aggperiod Character string, timestep for aggregated results. One of \code{"year"} for annual statistics, 
#' \code{"season1"} (winter/summer half-years), \code{"season2"} (4 seasons), or \code{"month"}. See details.
#' @param timestep Character string, timestep of data in \code{x}. Attribute \code{timestep} in \code{x} per default. 
#' Otherwise one of \code{"month"} (not allowed with \code{aggperiod = "month"}), \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours).
#' @param subid Integer, a vector of HYPE subbasin IDs for data in \code{x}. Attribute \code{subid} in \code{x} per default.
#' @param FUN A function to compute for each \code{aggperiod} in \code{x}.
#' @param ... Optional arguments to \code{FUN}.
#' 
#' @details
#' \code{ExtractStats} uses \code{\link{aggregate}} to calculate statistics for all data columns provided in \code{x}. Argument 
#' \code{start.mon} allows to define the start of the hydrological year. Hydrological seasons begin with winter (season1) or 
#' autumn (season2).
#' 
#' @return 
#' \code{ExtractStats} returns a dataframe with starting dates for each aggregation period in the first column, and a descriptive 
#' aggregation period name in the second. Remaining columns contain aggregated results as ordered in \code{x}. Additional attributes 
#' \code{subid} with subbasin IDs, \code{timestep} with time step of the source data, and \code{period} with a two-element POSIXct vector 
#' containing start and end dates of the source data.
#' 
#' @note 
#' If \code{FUN} returns several values per aggregation period, these are returned in nested columns in the resulting dataframe. See 
#' \code{Value} section for \code{\link{aggregate}} and example code below.
#' 
#' @examples
#' # Import example data
#' te1 <- ReadTimeOutput(filename = system.file("demo_model", "results",
#' "timeCOUT.txt", package = "HYPEtools"), dt.format = "%Y-%m")
#' # Extract maxima
#' ExtractStats(x = te1, start.mon = 1, FUN = max)
#' # Multiple result stats: Extract min, mean, and max in one go:
#' te2 <- ExtractStats(x = te1, start.mon = 1,
#' FUN = function(x) {c(min(x), mean(x), max(x))})
#' # extract mean from resulting nested dataframe:
#' data.frame(te2[, 1:2], sapply(te2[, -c(1:2)], function(x){x[, 2]}))
#' 
#' @importFrom stats aggregate
#' @export


ExtractStats <- function(x, start.mon = 1, aggperiod = c("year", "season1", "season2", "month"), timestep = attr(x, "timestep"), 
                         subid = attr(x, "subid"), FUN, ...) {
  
  ## input checks
  
  aggperiod <- match.arg(aggperiod)
  
  if (start.mon > 12 || start.mon < 1) {
    stop("'start.mon' not valid.")
  }
  
  # check if timestep is acceptable and shorter than aggregation period, abort otherwise
  if (aggperiod == "month") {
    if (!(length(grep("hour", timestep)) == 1 | timestep == "day" | timestep == "week")) {
      stop("Invalid 'timestep' argument.")
    }
  } else {
    if (!(length(grep("hour", timestep)) == 1 | timestep == "day" | timestep == "week" | timestep == "month")) {
      stop("Invalid 'timestep' argument.")
    }
  }
  
  
  ## calculate grouping vector, depending on aggregation period
  
  # start and end years of input data
  syear <- as.numeric(format(x[1,1], "%Y"))
  eyear <- as.numeric(format(x[nrow(x),1], "%Y"))
  
  # start month formatted for date conversion
  smon <- formatC(start.mon, width = 2, format = "d", flag = "0")
  
  if (aggperiod %in% c("year", "month")) {
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "UTC"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "UTC"), by = aggperiod)
    
  } else if (aggperiod == "season1") {
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "UTC"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "UTC"), by = "6 months")
    
  } else if (aggperiod == "season2") {
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "UTC"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "UTC"), by = "3 months")
    
  }
  
  # grouping vector
  group <- cut(x[, 1], breaks = breaks)
  
  # calculate flow statistics
  res <- aggregate(x[, -1], by = list(group), FUN = FUN, ..., simplify = TRUE)
  
  # format date and add aggregation period string
  res <- data.frame(DATE = as.POSIXct(res[, 1], tz = "UTC"), res)
  if (aggperiod == "year") {
    res[, 2] <- format(res[, 1], format = "%Y")
  } else if (aggperiod == "season1") {
    res[, 2] <- rep(c("winter", "summer"), length.out = nrow(res))
  } else if (aggperiod == "season2") {
    res[, 2] <- rep(c("autumn", "winter", "spring", "summer"), length.out = nrow(res))
  } else if (aggperiod == "month") {
    res[, 2] <- format(res[, 1], format = "%m")
  }
  names(res)[2] <- "aggperiod"
  
  # add attributes
  attr(res, "period") <- c(x[1, 1], x[nrow(x), 1])
  attr(res, "timestep") <- attr(x, "timestep")
  attr(res, "subid") <- attr(x, "subid")
  
  return(res)
}