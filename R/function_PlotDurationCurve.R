
#'
#' @export
#' 
#' @title
#' Plot a duration curve
#'
#' @description
#' Convenience wrapper function for a line \code{\link{plot}}, with pretty defaults for axis annotation and a Gaussian scaling option for the x-axis.
#'
#' @param freq Data frame with two columns, containing probabilities in the first column and data quantiles in the second. Typically an object as 
#' returned by \code{\link{ExtractFreq}}, or a subset thereof.
#' 
#' @param xscale Character string, keyword for x-axis scaling. Either \code{"lin"} for linear scaling or \code{"gauss"} for gaussian scaling as in a normal 
#' probability plot, which allows for for better comparison of low flow and high flow frequencies.
#' 
#' @param yscale Character string, keyword for y-axis scaling. Either \code{"lin"} for linear scaling or \code{"log"} for common logarithm scaling.
#' 
#' @param print.n.obs Logical. If \code{TRUE} and \code{freq} is a result from \code{\link{ExtractFreq}}, the number of observations on which the quantiles 
#' are based will be plotted on the outer margin. 
#' 
#' @param ylim  Numeric vector of length two, giving y-axis limits. \code{NULL} for default values.
#' 
#' @param xlab Character string, x-axis label.
#' 
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, either as keyword \code{"m3s"} or \code{"mmd"} for pre-defined pretty 
#' discharge labels, or any other string which will be plotted unchanged.
#' 
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue.
#' 
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
#' 
#' @param ... Further graphical parameters (see \code{\link{par}}) to be supplied as arguments to \code{\link{lines}}, e.g. line type and width, for 
#' details see there.
#' 
#' @details
#' \code{PlotDurationCurve} plots a duration curve with pretty formatting defaults. The function sets \code{\link{par}} parameters \code{tcl} and \code{mgp} 
#' internally and will override previously set values for the returned plot. It typically uses results from \code{\link{ExtractFreq}} as input data and via that 
#' function it can be used to visualise and compare time series properties.
#' 
#' @return
#' \code{PlotDurationCurve} returns a plot to the currently active plot device.
#' 
#' @seealso
#' \code{\link{ExtractFreq}}
#' 
#' @examples
#' \dontrun{PlotDurationCurve(freq = myfreq, print.n.obs = T)}



PlotDurationCurve <- function(freq, xscale = "lin", yscale = "log", print.n.obs = FALSE, ylim = NULL, xlab = "Flow exceedance percentile", 
                              ylab = "m3s", col = "blue", mar = c(3,3,1,1)+.1, ...) {
  
  # input checks for scale specification arguments
  stopifnot(any(xscale == "lin", xscale == "gauss"), any(yscale == "lin", yscale == "log"))
  
  # conditional: y-axis scaling. Available options are linear and log-scaled
  if (yscale == "lin") {
    plot.log <- ""
  } else {
    plot.log <- "y"
  }
  
  ## create plot dataframe
  # calculate exceedance values from probabilities
  data <- freq
  data[, 1] <- 1 - freq[, 1]
  # remove 0 rows with 0 quantiles which conflict with log-scaled y-axis, throw warning to notify user
  if (all(length(which(data[, 2] < 0)) > 0, yscale == "log")) {
    data <- data[data[, 2] > 0, ]
    warning("Quantiles where value == 0 removed prior to plotting.")
  }
  
  # conditional: create y-axis label strings
  if (ylab == "m3s") {
    ylabel <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
  } else if (ylab == "mmd") {
    ylabel <- expression(paste("Q (mmm d"^{-1}, ")"))
  } else {
    ylabel <- ylab
  }
  
  
  # set plot parameters
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  
  
  # conditional: plot with linear or gaussian x-scale
  if (xscale == "lin") {
    
    # set up the plot region, but do not plot yet. Background grid will be plotted first
    plot(data, log = plot.log, axes = F, type = "n", ylab = ylabel, xlab = xlab, ylim = ylim)
    
    # plot background grid
    grid(equilogs = F)
    
    # manually add axes and framing box
    axis(side = 2)
    axis(side = 1)
    box()
    
    # plot duration curve
    lines(data, type = "l", pch = 16, cex = 0.4, col = col, ...)
    
  } else {
    
    # add gaussian x-axis values, ie for an axis where the outer quantiles are increasingly spread out
    data <- cbind(data, x.gaussian = qnorm(data[,1]))
    
    # create x-axis tick positions in gaussian scaling
    x.gauss <- qnorm(c(.0001, .001, .01, .1, .2, .4, .6, .8, .9, .99, .999, .9999))
    # create x-axis labels for gaussian scale
    xlab.gauss <- c("0.0001", "0.001", "0.01", "0.1", "0.2", "0.4", "0.6", "0.8", "0.9", "0.99", "0.999", "0.9999")
    
    # set up the plot region, but do not plot yet. Background grid will be plotted first. Axes omitted as well, because gaussian axis is created manually
    plot(data[, 3], data[, 2], axes = F, log = plot.log, type = "n", ylab = ylabel, xlab = xlab, ylim = ylim)
    
    # plot background grid (manually in this case, because grid() will not plot on gaussian tick marks)
    abline(h = axTicks(2), col = "lightgray", lty = "dotted")
    abline(v = x.gauss, col = "lightgray", lty = "dotted")
    
    # manually add axes and framing box
    axis(side = 2)
    axis(side = 1, at = x.gauss, labels = xlab.gauss)
    box()
    
    # plot duration curve with gaussian-scaled x positions
    lines(data[, 3], data[, 2], col = col, ...)
      
  }
  
  
  # add number of observations if requested
  if (print.n.obs) {
    if (!is.null(attr(freq, which = "n.obs"))) {
      mtext(text = paste(attr(freq, which = "n.obs"), "observations"), side = 3, adj = 1, cex = .8, font = 3)
    } else {
      warning("Printing of number of observations requested, but attribute not available in 'freq'.")
    }
  }
}
