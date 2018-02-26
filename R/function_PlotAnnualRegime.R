#' Plot annual regimes
#'
#' Convenience wrapper function for a combined line \code{\link{plot}} with \code{\link{polygon}} variation ranges.
#'
#' @param x List, typically a result from \code{\link{AnnualRegime}}, containing data frames with aggregated long-term average 
#' regime data and two attributes \code{period} and \code{timestep}.
#' See Details and Value sections there.
#' @param line Character string, keyword for type of average line to plot. Either \code{"mean"} or \code{"median"}.
#' @param band Character string, keyword for variation bands. Either \code{"none"} to plot long-term averages only, or \code{"minmax"} or 
#' \code{"quartile"} to include bands of variation. See details.
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot.
#' @param l.legend Character vector. If non-NULL, legend labels are read from here instead of from column names in \code{x$mean}.
#' @param log Logical, if \code{TRUE}, y-axis will be log-scaled.
#' @param ylim  Numeric vector of length two, giving y-axis limits. Defaults to min-max range of all plotted data.
#' @param xlab Character string or \code{\link{plotmath}} expression string, x-axis label. Default prints the time period on which the 
#' regime is based, read from \code{x$period}.
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, with a default for discharge regimes.
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param lty Line type specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param lwd Line width specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
#' @param restore.par Logical, if \code{TRUE}, par settings will be restored to original state on function exit.
#' 
#' @details
#' If \code{"minmax"} or \code{"quartile"} are chosen for argument \code{band}, transparent bands of inter-annual variation are plotted along the 
#' long-term average line plots, either based on minimum and maximum values or on 25\% and 75\% percentiles.
#' 
#' Grid lines plotted in the background are mid-month lines.
#' 
#' @return
#' \code{PlotAnnualRegime} returns a plot to the currently active plot device.
#' 
#' @seealso
#' \code{\link{AnnualRegime}}, \code{\link{PlotSimObsRegime}}
#' 
#' @examples
#' \dontrun{PlotAnnualRegime(x = myregime)}
#' 
#' @export


PlotAnnualRegime <- function(x, line = "mean", band = "none", add.legend = FALSE, l.legend = NULL, log = FALSE, ylim = NULL, 
                             ylab = expression(paste("Q (m"^3, " s"^{-1}, ")")), 
                             xlab = paste(format(attr(x, "period"), format = "%Y"), collapse = " to "), col = "blue", 
                             lty = 1, lwd = 1, mar = c(3, 3, 1, 1) + .1, restore.par = FALSE) {
  
  # save current state of par() variables which are altered below, for restoring on function exit
  par.mar <- par("mar")
  par.xaxs <- par("xaxs")
  par.mgp <- par("mgp")
  par.tcl <- par("tcl")
  if (restore.par) {
    on.exit(par(mar = par.mar, xaxs = par.xaxs, mgp = par.mgp, tcl = par.tcl))
  }
  
  # number of time series in freq
  nq <- ncol(x$mean) - 2
  
  # input check for type specification arguments
  stopifnot(any(line == "mean", line == "median"))
  stopifnot(any(band == "minmax", band == "quartile", band == "none"))
  
  
  # input checks for line property specification arguments
  if (!any(length(col) == 1, length(col) == nq)) {
    stop("Length of 'col' argument and number of regime series in 'x' do not match.")
  }
  if (!any(length(lty) == 1, length(lty) == nq)) {
    stop("Length of 'lty' argument and number of regime series in 'x' do not match.")
  }
  if (!any(length(lwd) == 1, length(lwd) == nq)) {
    stop("Length of 'lwd' argument and number of regime series in 'x' do not match.")
  }
  
    
  # conditional: extract automatic y-axis limits by finding global maxima in all provided data series
  if (is.null(ylim) && line == "mean" && band == "none") {
    ylim <- range(unlist(x$mean[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && line == "median" && band == "none") {
    ylim <- range(unlist(x$median[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && band == "minmax") {
    ylim <- range(unlist(rbind(x$minimum, x$maximum)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && band == "quartile") {
    ylim <- range(unlist(rbind(x$p25, x$p75)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  }
  
  # log scaling for y axis, with treatment for negative lower limit in combination with log scaling
  if (log) {
    lg <- "y"
    if (ylim[1] <= 0) {
      warning("Negative or 0 values not allowed with log scaling. Reverting to linear scaling.")
      lg <- ""
    }
  } else {
    lg = ""
  }
  
  # conditional: create vectors of line properties if only single values were provided. assign to local object
  if (nq > 1 && length(col) == 1) {
    lcol <- rep(col, times = nq)
  } else {
    lcol <- col
  }
  
  if (nq > 1 && length(lty) == 1) {
    llty <- rep(lty, times = nq)
  } else {
    llty <- lty
  }
  
  if (nq > 1 && length(lwd) == 1) {
    llwd <- rep(lwd, times = nq)
  } else {
    llwd <- lwd
  }
  
  # set plot parameters
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0), tcl = .2)
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  plot(x$mean[, c(1, 3)], axes = F, type = "n", ylab = ylab, xlab = xlab, ylim = ylim, log = lg)
  
  # plot background grid
  grid(nx = NA, ny = NULL)
  if (attr(x, "timestep") == "month") {
    vline <- x$mean[, 1]
  } else if (attr(x, "timestep") == "day") {
    vline <- x$mean[which(format(x$mean[, 1], format = "%d") == "15"), 1]
  } else if (attr(x, "timestep") == "week") {
    # expand to daily dates and pick the 15th of each month
    te <- seq(from = x$mean[1, 1], to = x$mean[nrow(x$mean), 1], by = "day")
    vline <- te[which(format(te, format = "%d") == "15")]
  }
  abline(v = vline, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  
  # manually add axes and framing box
  axis(side = 2)
  axis.POSIXct(side = 1, at = vline)
  box()
  
  
  # plot regimes, incl transparent variation polygons if requested, using internal function
  if (band == "minmax") {
    
    polcol <- .makeTransparent(lcol, 30)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$minimum[, c(1, i)], x$maximum[nrow(x$maximum):1, c(1, i)])
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
    
  }
  if (band == "quartile") {
    
    polcol <- .makeTransparent(lcol, 30)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$p25[, c(1, i)], x$p75[nrow(x$p75):1, c(1, i)])
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
  }
  if (line == "mean") {
    for (i in 3:(nq + 2)) {
      lines(x$mean[, 1], x$mean[, i], type = "l", pch = 16, cex = 0.4, col = lcol[i - 2], lty = llty[i - 2], lwd = llwd[i - 2])
    }
  }
  if (line == "median") {
    for (i in 3:(nq + 2)) {
      lines(x$median[, 1], x$median[, i], type = "l", pch = 16, cex = 0.4, col = lcol[i - 2], lty = llty[i - 2], lwd = llwd[i - 2])
    }
  }
  
  # add legend if requested
  if (add.legend) {
    
    # create legend labels, conditional on if user provided names manually and if number of observations for frequencies is known
    if (is.null(l.legend)) {
      lgnd <- names(x$mean)[-c(1:2)]
    } else {
      lgnd <- l.legend
    }
    
    # print legend
    legend("topright", legend = lgnd, bty = "n", lty = llty, col = lcol, cex=.9, lwd = llwd)
  }
    
}

# debug
# x <- AnnualRegime(cctn, ts.in = "day")
# mar <- c(3, 3, 1, 1) + .1
# ylab <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
# xlab <- paste(format(attr(x, "period"), format = "%Y"), collapse = " to ")
# col <- 1:5
# lty <- 1
# lwd <- 1
# line <- "mean"
# band <- "none"
# add.legend <- FALSE
# l.legend <- NULL
# ylim <- NULL
# log <- F
