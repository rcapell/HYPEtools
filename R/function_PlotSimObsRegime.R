#' Plot annual regimes of simulated and observed variables
#' 
#' A combined plot for annual regimes with box plot elements for observed variables and ribbon elements 
#' for simulated variables. Particularly designed for comparisons of sparse observations with
#' high-density model results, e.g. for in-stream nutrients.
#' 
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param sim,obs Character string keywords, observed and simulated HYPE variable IDs to plot. Not case-sensitive, but must exist in \code{x}. 
#' Set to \code{NULL} to omit corresponding elements in plot.
#' @param ts.in Character string, timestep of \code{x}, searches for an attribute \code{timestep} in \code{x} per default. 
#' Otherwise one of \code{"month"}, \code{"week"}, \code{"day"}, or \code{"nhour"} (n = number of hours).
#' @param ts.out Character string, aggregation timestep for simulation results, defaults to \code{ts.in}. This timestep must be equal 
#' to or longer than \code{ts.in}.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year, used to order the output.
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot.
#' @param pos.legend Character string keyword for legend positioning. See Details in \code{link{legend}}.
#' @param inset Integer, legend inset as fraction of plot region, one or two values for x and y. See \code{link{legend}}.
#' @param l.legend Character vector of length 2 containing variable labels for legend, first for \code{sim}, then for \code{obs}. 
#' If non-NULL, variable labels are read from here instead of \code{sim} and \code{obs}.
#' @param log Logical, if \code{TRUE}, y-axis will be log-scaled.
#' @param ylim  Numeric vector of length two, giving y-axis limits. Defaults to min-max range of all plotted data.
#' @param xlab Character string or \code{\link{plotmath}} expression string, x-axis label. Defaults to a string giving the 
#' time period on which the regime is based.
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label. Defaults to a HYPE variable unit string taken from 
#' \code{x} \code{\link{attributes}} \code{'hypeunit'}.
#' @param mar Numeric vector of length 4, margin specification passed to \code{\link{par}}.
#' 
#' @details 
#' \code{PlotSimObsRegime} combines ribbons and box plot elements. Box plot elements are composed as defaults from \code{\link{boxplot}}, 
#' i.e. boxes with 25\% to 75\% percentile ranges and horizontal bar at median value, whiskers extending to 1.5 times standard deviation, and 
#' extreme values as points. Observation counts per month over the observation period are printed above the x-axis.
#' 
#' Aggregation time length of the simulated variable can be chosen in argument \code{ts.out}, resulting in more or less smoothed ribbons. 
#' For the observed variable, the aggregation is fixed to months, in order to aggregate enough values for each box plot element.
#' 
#' @return 
#' \code{PlotSimObsRegime} returns a plot to the currently active plot device, and invisibly a \code{\link{list}} object containing three 
#' elements with the plotted data and variable IDs. 
#' Element \code{obs} contains a list as returned by \code{\link{AnnualRegime}}. Element \code{obs} contains a list with two elements, a 
#' vector \code{refdate} with x positions of box plots elements, and a list \code{reg.obs} with observations for the monthly box plot elements. 
#' Element \code{variable} contains a named vector with HYPE variable IDs for observations and simulations. \code{sim} and \code{obs} returned 
#' empty if corresponding function argument was \code{NULL}.
#' 
#' @seealso 
#' \code{\link{PlotAnnualRegime}} for a more generic annual regime plot, \code{\link{AnnualRegime}} to compute annual regimes only.
#' 
#' @examples
#' # Plot observed and simulated discharge
#' te <- ReadBasinOutput(filename = system.file("demo_model",
#' "results", "0003587.txt", package = "HYPEtools"))
#' PlotSimObsRegime(x = te, sim = "cout", obs = "rout", start.mon = 10)
#' 
#' @importFrom graphics par grid abline axis axis.POSIXct box polygon lines boxplot text legend
#' @importFrom stats na.omit median
#' @importFrom utils modifyList
#' @export

# Exported function
PlotSimObsRegime <- function(x, sim, obs, ts.in = NULL, ts.out = "month", start.mon = 1, add.legend = TRUE, pos.legend = "topright", inset = 0, 
                             l.legend = NULL, log = FALSE, ylim = NULL, xlab = NULL, ylab = NULL, mar = c(3, 3, 1, 1) + .1) {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  # Call function and pass arguments
  .PlotSimObsRegime(x=x, sim=sim, obs=obs, ts.in=ts.in, ts.out=ts.out, start.mon=start.mon,
                    add.legend=add.legend, pos.legend=pos.legend, inset=inset, 
                    l.legend=l.legend, log=log, ylim=ylim, xlab=xlab, ylab=ylab, mar=mar)
  
}

# Internal function used for PlotBasinSummary
.PlotSimObsRegime <- function(x, sim, obs, ts.in = NULL, ts.out = "month", start.mon = 1, add.legend = TRUE, pos.legend = "topright", inset = 0, 
                             l.legend = NULL, log = FALSE, ylim = NULL, xlab = NULL, ylab = NULL, mar = c(3, 3, 1, 1) + .1) {
  
  # If user calls the internal function using HYPEtools:::.PlotSimObsRegime(), then make sure that par is reset on exit
  if(!length(sys.calls())>1){
    warning("Please use HYPEtools::PlotSimObsRegime() instead of the internal HYPEtools:::.PlotSimObsRegime() function")
    userpar <- par(no.readonly = TRUE) # Backup par
    on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  }
  
  ## check arguments and prepare data
  
  if (is.null(sim) && is.null(obs)) {
    stop("Provide at least one of 'sim' and 'obs'.")
  }
  
  # make data frames of obs and sim variables if they exist in x _and_ contain data
  if (!is.null(sim)) {
    col.sim <- which(toupper(names(x)) == toupper(sim))
    if (length(col.sim) != 0) {
      x.sim <- x[, c(1, col.sim)]
    } else {
      stop("Variable 'sim' does not exist in 'x'.")
    }
    if (all(is.na(x.sim[, 2]))) {
      x.sim <- NULL
    }
  } else {
    x.sim <- NULL
  }
  if (!is.null(obs)) {
    col.obs <- which(toupper(names(x)) == toupper(obs))
    if (length(col.obs) != 0) {
      x.obs <- x[, c(1, col.obs)]
    } else {
      stop("Variable 'obs' does not exist in 'x'.")
    }
    if (all(is.na(x.obs[, 2]))) {
      x.obs <- NULL
    }
  } else {
    x.obs <- NULL
  }
  
  # catch case where sim and/or obs columns are provided but none contain any data points
  if (is.null(x.sim) && is.null(x.obs)) {
    stop("No data in 'sim' and/or 'obs' columns.")
  }
  
  if (is.null(ts.in)) {
    ts.in <- attr(x, "timestep")
    if (is.null(ts.in)) {
      stop("No attribute 'timestep' found in 'x', and no argument 'ts.in' provided.")
    }
  }
  
  # assign output timestep if not user-provided
  if (is.null(ts.out)) {
    ts.out <- ts.in
  }
  
  # extract automatic y-axis limits by finding global maxima in all provided data series
  if (is.null(ylim)) {
    ylim <- range(c(x.sim[, 2], x.obs[, 2]), na.rm = TRUE)
  }
  
  # log-scaled y-axis
  if (log) {
    lg <- "y"
  } else {
    lg <- ""
  }
  
  # handle exceptions with log-scaled y axis
  if (log && ylim[1] <= 0 && ylim[2] <= 0) {
    
    # all values are 0 or negative
    warning("No positive values in data. Reverting to linear scaling.")
    lg <- ""
    
  } else if (log && ylim[1] <= 0) {
    
    # minimum value 0 or negative, raise axis minimum to smalles positive value
    warning("Zero or negative values in 'sim'/'obs', not shown in log-scaled plot.")
    ylim[1] <- min(c(x.sim[, 2], x.obs[, 2])[c(x.sim[, 2], x.obs[, 2]) > 0], na.rm = TRUE)
    
  }
  
  # add a bit of footspace for obs counts, conditional on log scaling
  if (log) {
    ylim[1] <- ylim[1] * .9
  } else {
    ylim[1] <- ylim[1] - (diff(ylim) / 50)
  }
               
  # prepare simulation regime, conditional on if sim is provided by user
  if (!is.null(x.sim)) {
    reg.sim <- AnnualRegime(x = x.sim, ts.in = ts.in, ts.out = ts.out, start.mon = start.mon)
  }
  
  # create axis labels if not provided
  if (is.null(xlab)) {
    xlab <- paste(format(x[c(1, nrow(x)), 1], format = "%Y"), collapse = " to ")
  }
  if (is.null(ylab)) {
    if (is.null(obs)) {
      ylab <- attr(x, "hypeunit")[col.sim - 1]
    } else {
      ylab <- attr(x, "hypeunit")[col.obs - 1]
    }
  }
  
  # prepare observation regime, remove NAs, group by month, construct dates for positioning in plot
  # conditional on if obs is provided by user
  if (!is.null(x.obs)) {
    reg.obs <- na.omit(x.obs)
    reg.obs <- tapply(reg.obs[, 2], format(reg.obs[, 1], format = "%m"), c, simplify = FALSE)
    # create sorting vector to match start.mon
    if (start.mon == 1) {
      sort.mon <- 1:12
    } else {
      sort.mon <- c(start.mon:12, 1:(start.mon - 1))
    }
    # conditional: if months without observations, add empty list elements for them
    if (length(reg.obs) < 12) {
      # create dummy list with all month elements
      te <- lapply(1:12, function(x){NA})
      names(te) <- sprintf("%02d", 1:12)
      # fill dummy with existing data
      reg.obs <- modifyList(te, reg.obs)
    }
    reg.obs <- reg.obs[sort.mon]
  }
  # dates for reg.obs box plot positioning, conditional on if reg.sim exists
  # also used for plot grid positioning
  if (exists("reg.sim")) {
    reg.obs.date <- seq(from = as.POSIXct(strptime(paste(format(reg.sim[[1]][1, 1], "%Y"), start.mon, 15, sep = "-"), "%F", tz = "UTC")), 
                        by = "month", length.out = 12)
  } else {
    reg.obs.date <- seq(from = as.POSIXct(strptime(paste("1910", start.mon, 15, sep = "-"), "%F", tz = "UTC")), 
                        by = "month", length.out = 12)
  }
  
  # set plot parameters
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  if (!is.null(sim)) {
    plot(reg.sim$mean[, c(1, 3)], axes = FALSE, type = "n", ylab = ylab, xlab = xlab, ylim = ylim, log = lg)
  } else {
    plot(reg.obs.date, sapply(reg.obs, mean), axes = FALSE, type = "n", ylab = ylab, xlab = xlab, ylim = ylim, log = lg)
  }
  
  # plot background grid
  grid(nx = NA, ny = NULL)
  vline <- reg.obs.date - 14*86400
  abline(v = vline, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  
  # manually add axes and framing box
  axis(side = 2)
  axis.POSIXct(side = 1, at = vline)
  box()
  
  # plot simulation regime ribbons, if sim was provided by user
  if (!is.null(sim)) {
    polcol <- .makeTransparent("red", 60)
    polcoor.minmax <- rbind(reg.sim$minimum[, c(1, 3)], reg.sim$maximum[nrow(reg.sim$maximum):1, c(1, 3)])
    polcoor.p25p75 <- rbind(reg.sim$p25[, c(1, 3)], reg.sim$p75[nrow(reg.sim$p75):1, c(1, 3)])
    polygon(polcoor.minmax[, 1], polcoor.minmax[, 2], col = polcol, border = NA)
    polygon(polcoor.p25p75[, 1], polcoor.p25p75[, 2], col = polcol, border = NA)
    # add mean and median lines, median needs to be calculated
    lines(reg.sim$mean[, c(1, 3)], col = .makeTransparent("red", 200))
    lines(reg.sim$median[, c(1, 3)], lty = 2, col = .makeTransparent("red", 200))
  }
  
  # plot observation box plots, if obs was provided by user
  if (!is.null(obs)) {
    boxplot(reg.obs, at = reg.obs.date, add = TRUE, boxwex = 1600000, show.names = FALSE, axes = FALSE, outpch = 20, outcol = .makeTransparent("black", 150), 
            col = .makeTransparent("grey30", 60), log = lg)
    # obs counts
    text(x = reg.obs.date, y = ylim[1], as.character(sapply(reg.obs, function(x) length(na.omit(x)))), adj = c(.5, .9), cex = .75, font = 3, col = "grey50")
    text(x = par("usr")[1] - diff(par("usr")[1:2])/25, y = ylim[1], "n obs.", adj = c(1, .9), cex = .75, font = 3, xpd = TRUE, col = "grey50")
    # add mean and median lines
    obs.mm <- data.frame(reg.obs.date, sapply(reg.obs, mean), sapply(reg.obs, median))
    obs.mm <- na.omit(obs.mm)
    lines(x = obs.mm[, 1], y = obs.mm[, 2], col = .makeTransparent("black", 200))
    lines(x = obs.mm[, 1], y = obs.mm[, 3], lty = 2, col = .makeTransparent("black", 200))
  }
  
  # add legend if requested
  if (add.legend) {
    
    # conditional if sim and/or obs exist
    if (is.null(sim)) {
      # create legend labels, conditional on if user provided names manually
      if (is.null(l.legend)) {
        lgnd <- c(toupper(obs), "mean", "median")
      } else {
        lgnd <- c(l.legend[1], "mean", "median")
        if (length(l.legend) > 1) {
          warning("Argument 'l.legend': only first element used.")
        }
        # create legend item properties
        lcol <- c(.makeTransparent(1, alpha = 200), rep("grey40", 2))
        lpch <- c(15, NA, NA)
        llty <- c(NA, 1, 2)
        llwd <- c(NA, 2, 2)
        # print legend
        legend(pos.legend, legend = lgnd, bty = "n", col = lcol, pch = lpch, lty = llty, cex=.75, pt.cex = 1.5, lwd = llwd, inset = inset)
      }
    } else if (is.null(obs)) {
      
      # create legend labels, conditional on if user provided names manually
      if (is.null(l.legend)) {
        lgnd <- c(toupper(sim), "mean", "median", "25 to 75 %ile", "min. to max.")
      } else {
        lgnd <- c(l.legend[1], "mean", "median", "25 to 75 %ile", "min. to max.")
        if (length(l.legend) > 1) {
          warning("Argument 'l.legend': only first element used.")
        }
      }
      # create legend item properties
      lcol <- c(.makeTransparent(2, alpha = 200), rep("grey40", 2), .makeTransparent(2, alpha = 120), .makeTransparent(2, alpha = 60))
      lpch <- c(15, NA, NA, 15, 15)
      llty <- c(NA, 1, 2, NA, NA)
      llwd <- c(NA, 2, 2, NA, NA)
      # print legend
      legend(pos.legend, legend = lgnd, bty = "n", col = lcol, pch = lpch, lty = llty, cex=.75, pt.cex = 1.5, lwd = llwd, inset = inset)
    } else {
      
      # create legend labels, conditional on if user provided names manually
      if (is.null(l.legend)) {
        lgnd <- c(toupper(c(sim, obs)), "mean", "median", "25 to 75 %ile", "min. to max.")
      } else {
        lgnd <- c(l.legend[1:2], "mean", "median", "25 to 75 %ile", "min. to max.")
        if (length(l.legend) > 2) {
          warning("Argument 'l.legend': only first 2 elements used.")
        }
      }
      # create legend item properties
      lcol <- c(.makeTransparent(2:1, alpha = 200), rep("grey40", 2), .makeTransparent(2, alpha = 120), .makeTransparent(2, alpha = 60))
      lpch <- c(15, 15, NA, NA, 15, 15)
      llty <- c(NA, NA, 1, 2, NA, NA)
      llwd <- c(NA, NA, 2, 2, NA, NA)
      # print legend
      legend(pos.legend, legend = lgnd, bty = "n", col = lcol, pch = lpch, lty = llty, cex=.75, pt.cex = 1.5, lwd = llwd, inset = inset)
    }
  }
  
  # return results as list invisibly
  if (is.null(sim)) {
    invisible(list(reg.sim = NULL, reg.obs = list(refdate = reg.obs.date, obs = reg.obs), variable = c(sim = sim, obs = obs)))
  } else if (is.null(obs)) {
    invisible(list(reg.sim, reg.obs = NULL, variable = c(sim = sim, obs = obs)))
  } else {
    invisible(list(reg.sim, reg.obs = list(refdate = reg.obs.date, obs = reg.obs), variable = c(sim = sim, obs = obs)))
  }
}

