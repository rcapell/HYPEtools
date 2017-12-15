

AggregatedStats <- function(x, start.mon = 1, aggperiod = c("year", "season1", "season2", "month"), timestep = attr(x, "timestep"), FUN) {
  
  ## input checks
  
  aggperiod <- match.arg(aggperiod)
  
  if (start.mon > 12 || start.mon < 1) {
    stop("'start.mon' not valid.")
  }
  
  # check if timestep is acceptable and shorter than aggregation period, abort otherwise
  if (aggperiod == "year") {
    if (!(length(grep("hour", timestep)) == 1 | timestep == "day" | timestep == "week" | timestep == "month")) {
      stop("Invalid 'timestep' argument.")
    }
  } else {
    if (!(length(grep("hour", timestep)) == 1 | timestep == "day" | timestep == "week")) {
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
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "GMT"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "GMT"), by = aggperiod)
    
  } else if (aggperiod == "season1") {
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "GMT"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "GMT"), by = "6 months")
    
  } else if (aggperiod == "season2") {
    breaks <- seq(as.POSIXct(paste(syear - 1, smon, "01", sep = "-"), tz = "GMT"), as.POSIXct(paste(eyear + 1, smon, "01", sep = "-"), tz = "GMT"), by = "3 months")
    
  }
  
  # grouping vector
  group <- cut(x[, 1], breaks = breaks)
  
  # calculate flow statistics
  res <- aggregate(x[, -1], list(group), FUN)
  
  # add attributes
  attr(res, "period") <- c(x[1, 1], x[nrow(x), 1])
  attr(res, "timestep") <- attr(x, "timestep")
}