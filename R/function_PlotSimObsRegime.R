#' Plot annual regimes of simulated and observed variables
#' 
#' A combined plot for annual regimes with box plot elements for observed and ribbon elements 
#' for simulated variables. Particularly designed for comparisons of sparse observations with
#' model high-density model results, e.g. for in-stream nutrients.
#' 
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param sim,obs Character string keywords, observed and simulated HYPE variable IDs to plot. Not case-sensitive, but must exist in \code{x}.
#' @param ts.in Character string, timestep of \code{x}, searches for an attribute \code{timestep} in \code{x} per default. 
#' Otherwise one of \code{"month"}, \code{"week"}, \code{"day"}, or \code{"nhour"} (n = number of hours).
#' @param ts.out Character string, timestep for results, defaults to \code{ts.in}. This timestep must be equal to or longer than 
#' \code{ts.in}.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year, used to order the output.
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot.
#' @param pos.legend Character string keyword for legend positioning. See Details in \code{link{legend}}.
#' @param l.legend Character vector of length 2. If non-NULL, variable labels are read from here instead of from \code{sim} and \code{obs}.
#' @param ylim  Numeric vector of length two, giving y-axis limits. Defaults to min-max range of all plotted data.
#' @param xlab Character string or \code{\link{plotmath}} expression string, x-axis label. Defaults to a string giving the 
#' time period on which the regime is based.
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label. Defaults to a measurement unit string taken from 
#' \code{x} \code{\link{attribute}} \code{'unit'}.
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
#' 
#' @details 
#' \code{PlotSimObsRegime}
#' 
#' @seealso 
#' \code{\link{PlotAnnualRegime}} for a more generic annual regime plot, \code{\link{AnnualRegime}} to compute annual regimes only.
#' 
#' @export
#' 


PlotSimObsRegime <- function(x, sim, obs, ts.in = NULL, ts.out = "month", start.mon = 1, add.legend = TRUE, pos.legend = "topright", 
                             l.legend = NULL, ylim = NULL, xlab = NULL, ylab = NULL, mar = c(3, 3, 1, 1) + .1) {
  
  ## check arguments and prepare data
  
  col.sim <- which(toupper(names(x)) == toupper(sim))
  col.obs <- which(toupper(names(x)) == toupper(obs))
  if (length(col.sim) != 0) {
    x.sim <- x[, c(1, col.sim)]
  } else {
    stop("Variable 'sim' does not exist in 'x'.")
  }
  if (length(col.obs) != 0) {
    x.obs <- x[, c(1, col.obs)]
  } else {
    stop("Variable 'obs' does not exist in 'x'.")
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
  # add a bit of footspace for obs counts
  ylim[1] <- ylim[1] - (diff(ylim)/50)
  
               
  # prepare simulation regime
  reg.sim <- AnnualRegime(x = x.sim, ts.in = ts.in, ts.out = ts.out, start.mon = start.mon)
  
  # create axis labels if not provided
  if (is.null(xlab)) {
    xlab <- paste(format(attr(reg.sim, "period"), format = "%Y"), collapse = " to ")
  }
  if (is.null(ylab)) {
    ylab <- attr(x, "unit")[col.sim]
    }
  
  # prepare observation regime, remove NAs, group by month, construct dates from sim regime for positioning in plot
  reg.obs <- na.omit(x.obs)
  reg.obs <- tapply(reg.obs[, 2], format(reg.obs[, 1], format = "%m"), c)
  # create sorting vector to match start.mon
  if (start.mon == 1) {
    sort.mon <- 1:12
  } else {
    sort.mon <- c(start.mon:12, 1:(start.mon - 1))
  }
  reg.obs <- reg.obs[sort.mon]
  reg.obs.date <- seq(from = as.POSIXct(strptime(paste(format(reg.sim[[1]][1, 1], "%Y"), start.mon, 15, sep = "-"), "%F", tz = "GMT")), 
                      by = "month", length.out = 12)
  
  # set plot parameters
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  plot(reg.sim$mean[, c(1, 3)], axes = F, type = "n", ylab = ylab, xlab = xlab, ylim = ylim)
  
  # plot background grid
  grid(nx = NA, ny = NULL)
  vline <- reg.obs.date - 14*86400
  abline(v = vline, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  
  # manually add axes and framing box
  axis(side = 2)
  axis.POSIXct(side = 1, at = vline)
  box()
  
  # plot simulation regime ribbons
  polcol <- .makeTransparent("red", 60)
  polcoor.minmax <- rbind(reg.sim$minimum[, c(1, 3)], reg.sim$maximum[nrow(reg.sim$maximum):1, c(1, 3)])
  polcoor.p25p75 <- rbind(reg.sim$p25[, c(1, 3)], reg.sim$p75[nrow(reg.sim$p75):1, c(1, 3)])
  polygon(polcoor.minmax[, 1], polcoor.minmax[, 2], col = polcol, border = NA)
  polygon(polcoor.p25p75[, 1], polcoor.p25p75[, 2], col = polcol, border = NA)
  # add mean and median lines, median needs to be calculated
  lines(reg.sim$mean[, c(1, 3)], col = .makeTransparent("red", 200))
  lines(reg.sim$median[, c(1, 3)], lty = 2, col = .makeTransparent("red", 200))
  
  # plot observation box plots
  boxplot(reg.obs, at = reg.obs.date, add = T, boxwex = 1600000, show.names = F, axes = F, outpch = 20, outcol = .makeTransparent("black", 150), col = .makeTransparent("grey30", 60))
  # obs counts
  text(x = reg.obs.date, y = ylim[1], as.character(sapply(reg.obs, length)), adj = c(.5, .9), cex = .75, font = 3, col = "grey50")
  text(x = par("usr")[1], y = ylim[1], "n obs.", adj = c(1, .9), cex = .75, font = 3, xpd = T, col = "grey50")
  # add mean and median lines
  lines(x = reg.obs.date, y = sapply(reg.obs, mean), col = .makeTransparent("black", 200))
  lines(x = reg.obs.date, y = sapply(reg.obs, median), lty = 2, col = .makeTransparent("black", 200))
  
  
  # add legend if requested
  if (add.legend) {
    
    # create legend labels, conditional on if user provided names manually and if number of observations for frequencies is known
    if (is.null(l.legend)) {
      lgnd <- c(sim, obs, "mean", "median", "25 to 75 %ile", "min. to max.")
    } else {
      lgnd <- c(l.legend[1:2], "mean", "median", "25 to 75 %ile", "min. to max.")
      if (length(l.legend) > 2) {
        warning("Argument 'l.legend': only first 2 elements used.")
      }
    }
    
    # print legend
    legend(pos.legend, legend = lgnd, bty = "n", lty = 1, col = col, cex=.9, lwd = llwd)
  }
  
}

