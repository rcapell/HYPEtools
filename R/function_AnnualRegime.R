
#' @export
#' @title
#' Calculate annual regimes
#'
#' @description
#' Calculate annual regimes based on long-term time series, typically imported HYPE basin output and time output result files.
#'
#' @param x Data frame, with column-wise equally-spaced time series. Date-times in \code{\link{POSIXct}} format in first column.
#' Typically an imported basin or time output file from HYPE. See details.
#' @param stat Character string, either \code{"mean"} or \code{"sum"}. Defines the type of aggregation to be computed for output 
#' time periods, see Details. Defaults to \code{"mean"}.
#' @param ts.in Character string, timestep of \code{x}, searches for an attribute \code{timestep} in \code{x} per default. 
#' Otherwise one of \code{"month"}, \code{"week"}, \code{"day"}, or \code{"nhour"} (n = number of hours).
#' @param ts.out Character string, timestep for results, defaults to \code{ts.in}. This timestep must be equal to or longer than 
#' \code{ts.in}.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year, used to order the output.
#' @param incl.leap Logical, leap days (Feb 29) are removed from results per default, set to \code{TRUE} to keep them. Applies 
#' to daily and shorter time steps only.
#' @param na.rm Logical, indicating if \code{NA} values should be stripped before averages are calculated.
#' 
#' @details
#' \code{AnnualRegime} uses \code{\link{aggregate}} to calculate long-term average regimes for all data columns provided in \code{x}, 
#' including long-term arithmetic means, minima and maxima, and 25\% and 75\% percentiles. In a HYPE context, \code{AnnualRegime} 
#' is particularly applicable to model basin and time results imported using \code{\link{ReadBasinOutput}} and 
#' \code{\link{ReadTimeOutput}}. The function currently does not check if equally spaced time steps are provided in \code{x} or if the 
#' overall time period in \code{x} covers full years so that the calculated averages are based on the same number of values.
#' 
#' Values within each output time period can be aggregated either by arithmetic means or by sums within each period, e.g. typically 
#' means for temperatures and sums for precipitation. Long-term aggregated values are always computed as arithmetic means. 
#' 
#' @note
#' If weekly data are provided in \code{x}, \code{AnnualRegime} will inflate \code{x} to daily time steps before computing 
#' results. Values in \code{x} will be assigned to the preceeding week days, corresponding to HYPE file output, where weekly 
#' values are conventionally printed on the last day of the week. If \code{NA} values are present in the original weekly data, 
#' these will be filled with the next available value as a side effect of the inflation.
#' 
#' If weekly output time steps are computed in combination with a user-defined start month, the function will round up weeks to 
#' determine the first week of the hydrological year. Weeks are identified using Monday as first day of the week and the first Monday 
#' of the year as day 1 of week 1 (see conversion code \code{\%W} in \code{\link{strptime}}). Boundary weeks \code{'00'} and 
#' \code{'53'} are merged to week \code{'00'} prior to average computations.
#' 
#' @return 
#' \code{AnnualRegime} returns a list with 7 elements and two additional \code{\link{attributes}}. 
#' 
#' Each element contains a data frame with, in column-wise order: reference dates in \code{POSIXct} format, date information as string, 
#' mean values, min values, max values, and 25\% and 75\% percentiles.
#' Reference dates are given as dates in 1911/1912/1913 (just because a leap day and weeks '00'/'53' occurr during these years) and can 
#' be used for plots starting at the beginning of the hydrological year (with axis annotations set to months only). Daily and hourly 
#' time steps are given as is, weekly time steps are given as mid-week dates (Wednesday), monthly time steps as mid month dates (15th). 
#' 
#' Attribute \code{period} contains a two-element POSIXct vector containing start and end dates of the 
#' source data. Attribute \code{timestep} contains a timestep keyword corresponding to function argument \code{ts.out}.
#' 
#' 
#' @seealso
#' \code{\link{PlotAnnualRegime}}
#'
#' @examples
#' \dontrun{AnnualRegime(x = mybasinoutput)}

AnnualRegime <- function(x, stat = "mean", ts.in = NULL, ts.out = NULL, start.mon = 1, incl.leap = FALSE, na.rm = TRUE) {
  
  ## identify timestep of x and choose posix element for averaging
  # conditional: get timestep of x from attribute if argument ts.in is not provided, with error handling
  if (is.null(ts.in)) {
    ts.in <- attr(x, "timestep")
    if (is.null(ts.in)) {
      stop("No attribute 'timestep' found in 'x', and no argument 'ts.in' provided.")
    }
  }
  
  # check if input timestep is acceptable, abort otherwise
  if (!(length(grep("hour", ts.in)) == 1 | ts.in == "day" | ts.in == "week" | ts.in == "month")) {
    stop(paste("Timestep '", ts.in, "' not accepted.", sep = ""))
  }
  
  # assign and check output timestep
  if (is.null(ts.out)) {
    ts.out <- ts.in
  } else {
    if (ts.out != ts.in) {
      if ((ts.in == "month" & ts.out != "month") | 
          (ts.in == "week" & !(ts.out %in% c("week", "month"))) | 
          (ts.in == "day" & !(ts.out %in% c("day", "week", "month")))) {
        stop("Output timestep cannot be shorter than input timestep.")
      }
    }
  }

  # check if output timestep is acceptable, abort otherwise
  if (!(length(grep("hour", ts.out)) == 1 | ts.out == "day" | ts.out == "week" | ts.out == "month")) {
    stop(paste("Timestep '", ts.out, "' not accepted.", sep = ""))
  }
  
  # weekly data will be expanded to daily data for calculations and later re-collapsed, HYPE writes weekly values to 
  # last day of the week
  if (ts.in == "week") {
    # create daily date vector over time period
    days <- data.frame(DATE = seq(x[1, 1], x[nrow(x), 1], by = "day"))
    x <- merge(days, x, by = 1, all.x = T)
    # use internal function to fill NAs fith next available value, apply to all columns
    x <- data.frame(DATE = x[, 1], apply(x[, -1], 2, .FillWeek))
    tformat <- format(x[, 1], format = "%j")
  }
  
  
  ## format index vectors for calculations below
  # output period vector
  if (length(grep("hour", ts.out)) == 1) {
    tformat <- format(x[, 1], format = "%m-%d %H")
  } else if (ts.out == "day") {
    tformat <- format(x[, 1], format = "%m-%d")
  } else if (ts.out == "week") {
    tformat <- format(x[, 1], format = "%W")
    # merge boundary half-weeks, those which go across new year, to one group
    # this can be weeks "00", "52", "00", and "53"
    tformat[tformat %in% c("00", "52", "53")] <- "52"
  } else if (ts.out == "month") {
    tformat <- format(x[, 1], format = "%m")
  } else { # this should never occur, leave it just for safety...
    stop(paste("Timestep '", ts.out, "' not known.", sep = ""))
  }
  # Year vector
  yformat <- format(x[, 1], format = "%Y")
  
  # calculate results, conditional on type of aggregation for output periods
  if (stat == "mean") {
    res_ave <- aggregate(x[, -1], list(tformat), mean, na.rm = na.rm)
    res_min <- aggregate(x[, -1], list(tformat), min, na.rm = na.rm)
    res_max <- aggregate(x[, -1], list(tformat), max, na.rm = na.rm)
    res_25p <- aggregate(x[, -1], list(tformat), quantile, probs = 0.25, na.rm = na.rm)
    res_75p <- aggregate(x[, -1], list(tformat), quantile, probs = 0.75, na.rm = na.rm)
  } else if (stat == "sum") {
    te <- aggregate(x[, -1], list(tformat, yformat), sum, na.rm = na.rm)
    res_ave <- aggregate(te[, -c(1:2)], list(te[, 1]), mean, na.rm = na.rm)
    res_min <- aggregate(te[, -c(1:2)], list(te[, 1]), min, na.rm = na.rm)
    res_max <- aggregate(te[, -c(1:2)], list(te[, 1]), max, na.rm = na.rm)
    res_25p <- aggregate(te[, -c(1:2)], list(te[, 1]), quantile, probs = 0.25, na.rm = na.rm)
    res_75p <- aggregate(te[, -c(1:2)], list(te[, 1]), quantile, probs = 0.75, na.rm = na.rm)
  } else {
    # catch input errors
    stop(paste("Function argument stat: keyword '", stat, "' not known.", sep = ""))
  }
  
  # prettify header
  names(res_ave)[1] <- ts.out
  names(res_min)[1] <- ts.out
  names(res_max)[1] <- ts.out
  names(res_25p)[1] <- ts.out
  names(res_75p)[1] <- ts.out
  
  # remove leap day from daily results if requested by user (and if it exists in results)
  if (ts.out == "day" && !incl.leap && res_ave[60, 1] == "02-29") {
    res_ave <- res_ave[-60, ]
    res_min <- res_min[-60, ]
    res_max <- res_max[-60, ]
    res_25p <- res_25p[-60, ]
    res_75p <- res_75p[-60, ]
  }
  
  # remove leap day from sub-daily results if requested by user
  if (length(grep("hour", ts.out)) == 1 && !incl.leap) {
    te <- which(substr(res_ave[, 1], 1, 5) == "02-29")
    if (length(te) > 0) {
      res_ave <- res_ave[-te, ]
      res_min <- res_min[-te, ]
      res_max <- res_max[-te, ]
      res_25p <- res_25p[-te, ]
      res_75p <- res_75p[-te, ]
    }
  }
  
  # order results according to a user-requested starting month to reflect the hydrological year rather than the calender year, 
  # calculate reference dates, conditional on starting month
  if (start.mon != 1) {
    
    # catch user input errors
    if (start.mon > 12 || start.mon < 1) {
      stop("start.mon not valid.")
    }
    
    # create date string of first day in the hydrological year
    if (length(grep("hour", ts.out)) == 1) {
      sm <- paste(formatC(start.mon, width=2, flag = "0"), "-01 00", sep = "")
    } else if (ts.out == "day") {
      sm <- paste(formatC(start.mon, width=2, flag = "0"), "-01", sep = "")
    } else if (ts.out == "week") {
      # look-up table for starting weeks
      te <- data.frame(mon = 2:12, week = c(4,9,13,18,22,26,31,35,40,44,49))
      sm <- formatC(te[which(te[, 1] == start.mon), 2], width=2, flag = "0")
    } else if (ts.out == "month") {
      sm <- formatC(start.mon, width=2, flag = "0")
    }
    
    # find row index of start date string
    ind.sm <- which(res_ave[, 1] == sm)
    ind.nrow <- nrow(res_ave)
    
    # re-order all results
    res_ave <- res_ave[c(ind.sm:ind.nrow, 1:(ind.sm - 1)), ]
    res_min <- res_min[c(ind.sm:ind.nrow, 1:(ind.sm - 1)), ]
    res_max <- res_max[c(ind.sm:ind.nrow, 1:(ind.sm - 1)), ]
    res_25p <- res_25p[c(ind.sm:ind.nrow, 1:(ind.sm - 1)), ]
    res_75p <- res_75p[c(ind.sm:ind.nrow, 1:(ind.sm - 1)), ]
    
    # add a reference date column, format conditional on output time step
    if (length(grep("hour", ts.out)) == 1) {
      # construct reference datetimes, conditional on hydrological year starting month
      # so that a leap day is always included in the results
      if (start.mon == 2) {
        te <- as.POSIXct(strptime(paste(c(rep(1912, times = ind.nrow - ind.sm + 1), rep(1913, times = ind.sm - 1)), "-", res_ave[, 1], sep = ""), format = "%F %H", tz = "GMT"))
      } else {
        te <- as.POSIXct(strptime(paste(c(rep(1911, times = ind.nrow - ind.sm + 1), rep(1912, times = ind.sm - 1)), "-", res_ave[, 1], sep = ""), format = "%F %H", tz = "GMT"))
      }
    } else if (ts.out == "day") {
      # construct reference days
      # te <- as.POSIXct(strptime(paste(c(rep(1900, times = ind.nrow - ind.sm + 1), rep(1901, times = ind.sm - 1)), "-", res_ave[, 1], sep = ""), format = "%F", tz = "GMT"))
      if (start.mon == 2) {
        te <- as.POSIXct(strptime(paste(c(rep(1912, times = ind.nrow - ind.sm + 1), rep(1913, times = ind.sm - 1)), "-", res_ave[, 1], sep = ""), format = "%F", tz = "GMT"))
      } else {
        te <- as.POSIXct(strptime(paste(c(rep(1911, times = ind.nrow - ind.sm + 1), rep(1912, times = ind.sm - 1)), "-", res_ave[, 1], sep = ""), format = "%F", tz = "GMT"))
      }
    } else if (ts.out == "week") {
      # construct reference dates for each week, Wednesdays chosen
      if (start.mon == 2) {
        te <- as.POSIXct(strptime(paste(c(rep(1912, times = ind.nrow - ind.sm + 1), rep(1913, times = ind.sm - 1)), res_ave[, 1], "3", sep = ""), format = "%Y%W%u", tz = "GMT"))
      } else {
        te <- as.POSIXct(strptime(paste(c(rep(1911, times = ind.nrow - ind.sm + 1), rep(1912, times = ind.sm - 1)), res_ave[, 1], "3", sep = ""), format = "%Y%W%u", tz = "GMT"))
      }
    } else if (ts.out == "month") {
      # construct reference dates for each month, 15th chosen
      if (start.mon == 2) {
        te <- as.POSIXct(strptime(paste(c(rep(1912, times = ind.nrow - ind.sm + 1), rep(1913, times = ind.sm - 1)), "-", res_ave[, 1], "-15", sep = ""), format = "%F", tz = "GMT"))
      } else {
        te <- as.POSIXct(strptime(paste(c(rep(1911, times = ind.nrow - ind.sm + 1), rep(1912, times = ind.sm - 1)), "-", res_ave[, 1], "-15", sep = ""), format = "%F", tz = "GMT"))
      }
    }
    
  } else {
    # no start month adjustment necessary, just add a reference date column
    if (length(grep("hour", ts.out)) == 1) {
      # construct reference datetimes
      te <- as.POSIXct(strptime(paste(rep(1912, times = nrow(res_ave)), "-", res_ave[, 1], sep = ""), format = "%F %H", tz = "GMT"))
    } else if (ts.out == "day") {
      # construct reference days
      te <- as.POSIXct(strptime(paste(rep(1912, times = nrow(res_ave)), "-", res_ave[, 1], sep = ""), format = "%F", tz = "GMT"))
    } else if (ts.out == "week") {
      # construct reference dates for each week, Wednesdays chosen, 1901 because 1900 has no week "00"
      te <- as.POSIXct(strptime(paste(rep(1913, times = nrow(res_ave)), res_ave[, 1], "3", sep = ""), format = "%Y%W%u", tz = "GMT"))
    } else if (ts.out == "month") {
      # construct reference dates for each month, 15th chosen
      te <- as.POSIXct(strptime(paste(rep(1912, times = nrow(res_ave)), "-", res_ave[, 1], "-15", sep = ""), format = "%F", tz = "GMT"))
    }
  }
  
  # add reference dates to all results
  res_ave <- data.frame(refdate = te, res_ave)
  res_min <- data.frame(refdate = te, res_min)
  res_max <- data.frame(refdate = te, res_max)
  res_25p <- data.frame(refdate = te, res_25p)
  res_75p <- data.frame(refdate = te, res_75p)
  
  # combine to result list
  res <- list(mean = res_ave, minimum = res_min, maximum = res_max, p25 = res_25p, p75 = res_75p)
  
  # add period and timestep attributes
  attr(res, "timestep") <- ts.out
  attr(res, "period") <- as.POSIXct(c(x[1, 1], x[nrow(x), 1]), tz="GMT")
  return(res)
}

## DEBUG
# stat <- "mean"
# ts.in <- "day"
# ts.out <- "day"
# na.rm <- TRUE
# incl.leap <- T
# start.mon <- 2
# rm(.FillWeek); rm(list = ls(all.names = T))
# te <- AnnualRegime(x = x, ts.out = "day", start.mon = 1, ts.in = "day")
# plot(te[[1]][, c(1, 3)], type = "l")
