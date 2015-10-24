#'
#' @export
#' 
#' @title
#' Plot annual regimes
#'
#' @description
#' Convenience wrapper function for a combined line \code{\link{plot}} with \code{\link{polygon}} variation ranges.
#'
#' @param x List, typically a result from \code{\link{AnnualRegime}}, containing data frames with aggregated long-term average regime data. 
#' See details in description there.
#' 
#' @param type Character string, keyword for plot type. Either \code{"mean"} to plot long-term averages only, or \code{"minmax"} or 
#' \code{"p25p75"} to include bands of variation. See details.
#' probability plot, which allows for for better comparison of low flow and high flow frequencies.
#' 
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot.
#' 
#' @param l.legend Character vector. If non-NULL, legend labels are read from here instead of from column names in \code{x$mean}.
#' 
#' @param ylim  Numeric vector of length two, giving y-axis limits. \code{NULL} for default values.
#' 
#' @param xlab Character string or \code{\link{plotmath}} expression string, x-axis label. Default prints the time period on which the 
#' regime is based, read from \code{x$period}.
#' 
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, with a default for discharge regimes.
#' 
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' 
#' @param lty Line type specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#'
#' @param lwd Line width specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' 
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
#' 
#' @details
#' If \code{"minmax"} or \code{"p25p75"} are chosen for argument \code{type}, transparent bands of inter-annual variation are plotted along the 
#' long-term average line plots, either based on minimum and maximum values or on 25\% and 75\% percentiles.
#' 
#' @return
#' \code{PlotAnnualRegime} returns a plot to the currently active plot device.
#' 
#' @seealso
#' \code{\link{AnnualRegime}}
#' 
#' @examples
#' \dontrun{PlotAnnualRegime(x = myregime)}


PlotAnnualRegime <- function(x, type = "mean", add.legend = FALSE, l.legend = NULL, ylim = NULL, ylab = expression(paste("Q (m"^3, " s"^{-1}, ")")), 
                 xlab = paste(format(x$period, format = "%Y"), collapse = " to "), col = "blue", lty = 1, lwd = 1, mar = c(3, 3, 1, 1) + .1) {
  
  # save current state of par() variables which are altered below, for restoring on function exit
  par.mar <- par("mar")
  par.xaxs <- par("xaxs")
  par.mgp <- par("mgp")
  par.tcl <- par("tcl")
  on.exit(par(mar = par.mar, xaxs = par.xaxs, mgp = par.mgp, tcl = par.tcl))
  
  # number of time series in freq
  nq <- ncol(x$mean) - 2
  
  # input check for type specification arguments
  stopifnot(any(type == "mean", type == "minmax", type == "p25p75"))
  
  
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
  if (is.null(ylim) && type == "mean") {
    ylim <- range(unlist(x$mean[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && type == "minmax") {
    ylim <- range(unlist(rbind(x$min, x$max)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && type == "p25p75") {
    ylim <- range(unlist(rbind(x$p25, x$p75)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
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
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  plot(x$mean[, c(1, 3)], axes = F, type = "n", ylab = ylab, xlab = xlab, ylim = ylim)
  
  # plot background grid
  grid(nx = NA, ny = NULL)
  abline(v = x$mean[, 1], col = "lightgray", lty = "dotted", lwd = par("lwd"))
  
  # manually add axes and framing box
  axis(side = 2)
  axis.POSIXct(side = 1, at = x$mean[, 1])
  box()
  
  # internal function to calculate transparent colors for variation polygon, used below
  # from: http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
  makeTransparent <- function(someColor, alpha=60) {
    newColor<-col2rgb(someColor)
    apply(newColor, 2, function(curcoldata){rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3],alpha = alpha, maxColorValue = 255)})
  }
  
  # plot regimes, incl transparent variation polygons if requested
  if (type == "minmax") {
    
    polcol <- makeTransparent(lcol, 30)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$minimum[, c(1, i)], x$maximum[nrow(x$maximum):1, c(1, i)])
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
    
  } else if (type == "p25p75") {
    
    polcol <- makeTransparent(lcol, 30)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$p25[, c(1, i)], x$p75[nrow(x$maximum):1, c(1, i)])
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
  }
  for (i in 3:(nq + 2)) {
      lines(x$mean[, 1], x$mean[, i], type = "l", pch = 16, cex = 0.4, col = lcol[i - 2], lty = llty[i - 2], lwd = llwd[i - 2])
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
    legend("topright", legend = lgnd, bty = "n", lty = 1, col = col, cex=.9)
  }
    
}

# # debug
# x <- AnnualRegime(data.frame(date, rout, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon)
# mar <- c(3, 3, 1, 1) + .1
# ylab <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
# xlab <- paste(format(x$period, format = "%Y"), collapse = " to ")
# col <- "blue"
# lty <- 1
# lwd <- 1
# type <- "mean"
# add.legend <- FALSE
# l.legend <- NULL
