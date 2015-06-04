
#' @export
#' @title
#' Create an equally spaced time series from irregular observations
#'
#' @description
#' \code{EquallySpacedObs} creates equally spaced time series with missing observations from a data frame with irregular 
#' observations.
#' 
#' @param x A \code{data.frame}, containing columns with observations and a datetime column in \code{\link{POSIXt}} or 
#' \code{\link{Date}} format.
#' @param sort.data Logical, if \code{TRUE}, \code{x} will be sorted by datetime.
#' @param timestep Character string keyword, giving the target time step width. \code{"day"} or \code{"hour"} currently allowed.
#' @param ts.col Integer, column index of datetime column.
#' 
#' @details
#' \code{EquallySpacedObs} will preserve additional attributes present in \code{x}. If datetime column is of class 
#' \code{\link{Date}}, there may occur problems with daylight saving time shifts. To avoid problems, use class 
#' \code{\link{POSIXct}} and set time zone to \code{"GMT"}.
#' 
#' @return
#' \code{EquallySpacedObs} returns a dataframe.
#' 
#' @examples
#' \dontrun{
#' my.obs <- data.frame(date = as.POSIXct(c("2000-01-01", "2000-02-01"), tz = "gmt"), obs = c(1, 2))
#' EquallySpacedObs(x = my.obs, timestep = "day")
#' }
#' 


EquallySpacedObs <- function(x, sort.data = TRUE, timestep, ts.col = 1) {
  
  # input validity checks
  if (!(timestep %in% c("day", "hour"))) {
    stop("Argument 'timestep': keyword unknown.")
  }
  if (!(inherits(x[, ts.col], "POSIXt") | inherits(x[, ts.col], "Date"))) {
    stop("Datetime column neither of class 'POSIXt' nor of class 'Date'.")
  }
  
  # check if there are any duplicated dates in x or y
  if (anyDuplicated(x[, ts.col])) {
    stop("Duplicated dates in 'x'.")
  }
  
  # sort by datetime if requested
  if (sort.data) {
    x <- x[order(x[, ts.col]), ]
  }
  
  # truncate datetime to desired stepwidth
  x[, ts.col] <- as.POSIXct(trunc(x[, ts.col], units = paste(timestep, "s", sep = ""), tz = format(x[1, ts.col], format = "%Z")))
  
  # create data frame of target timesteps
  y <- data.frame(date = seq(min(x[, ts.col]), max(x[, ts.col]), by = timestep))
  
  # merge x and y to create daily result
  res <- merge(x, y, by.x = ts.col, by.y = 1, all.y = T)
  
  # add additional hype attributes of x to result, if any
  if (length(attributes(x)[-which(names(attributes(x)) %in% c("names", "row.names", "class"))]) > 0) {
    attributes(res) <- c(attributes(res), attributes(x)[-which(names(attributes(x)) %in% c("names", "row.names", "class"))])
  }
  
  # return result
  return(res)
}

