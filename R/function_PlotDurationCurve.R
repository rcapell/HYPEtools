#' Plot duration curves
#'
#' Convenience wrapper function for a (multiple) line \code{\link{plot}}, with pretty defaults for axis annotation and a Gaussian scaling option for the x-axis.
#'
#' @param freq Data frame with at least two columns, containing probabilities in the first and series of data quantiles in the remaining columns. Typically 
#' an object as returned by \code{\link{ExtractFreq}} or a subset thereof.
#' @param xscale Character string, keyword for x-axis scaling. Either \code{"lin"} for linear scaling or \code{"gauss"} for gaussian scaling as in a normal 
#' probability plot, which allows for for better comparison of low flow and high flow frequencies.
#' @param yscale Character string, keyword for y-axis scaling. Either \code{"lin"} for linear scaling or \code{"log"} for common logarithm scaling.
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot, including the number of observations on which the quantiles are based for 
#' each curve if \code{freq} is a result from \code{\link{ExtractFreq}}.
#' @param l.legend Character vector. If non-NULL, legend labels are read from here instead of from column names in \code{freq}.
#' @param ylim  Numeric vector of length two, giving y-axis limits. \code{NULL} for default values.
#' @param xlab Character string, x-axis label.
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, either as keyword \code{"m3s"} or \code{"mmd"} for pre-defined pretty 
#' discharge labels, or any other string which will be plotted unchanged.
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param lty Line type specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param lwd Line width specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
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
#' # Import source data
#' te1 <- ReadBasinOutput(filename = system.file("demo_model", "results", "0003587.txt", 
#'                        package = "HYPEtools"))
#' te2 <- ExtractFreq(te1[, c("COUT", "ROUT")])
#' # Plot flow duration curves for simulated and observed discharge
#' PlotDurationCurve(freq = te2, add.legend = TRUE, col = c("red", "blue"))
#' 
#' @importFrom graphics par grid axis box lines abline axTicks legend
#' @importFrom stats qnorm
#' @export

# Exported function
PlotDurationCurve <- function(freq, xscale = "lin", yscale = "log", add.legend = FALSE, l.legend = NULL, ylim = NULL, 
                              xlab = "Flow exceedance percentile", ylab = "m3s", col = "blue", lty = 1, lwd = 1, 
                              mar = c(3, 3, 1, 1) + .1) {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  # Call function and pass arguments
  .PlotDurationCurve(freq=freq, xscale=xscale, yscale=yscale, add.legend=add.legend,
                     l.legend=l.legend, ylim=ylim, xlab=xlab, ylab=ylab, col=col, lty=lty, lwd=lwd, mar=mar)
  
}

# Internal function used for PlotBasinOutput and PlotBasinSummary
.PlotDurationCurve <- function(freq, xscale = "lin", yscale = "log", add.legend = FALSE, l.legend = NULL, ylim = NULL, 
                               xlab = "Flow exceedance percentile", ylab = "m3s", col = "blue", lty = 1, lwd = 1, 
                               mar = c(3, 3, 1, 1) + .1) {
  
  # If user calls the internal function using HYPEtools:::.PlotDurationCurve(), then make sure that par is reset on exit
  if(!length(sys.calls())>1){
    warning("Please use HYPEtools::PlotDurationCurve() instead of the internal HYPEtools:::.PlotDurationCurve() function")
    userpar <- par(no.readonly = TRUE) # Backup par
    on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  }
  
  # number of quantile series in freq
  nq <- ncol(freq) - 1
  
  # input check for scale specification arguments
  stopifnot(any(xscale == "lin", xscale == "gauss"), any(yscale == "lin", yscale == "log"))
  
  # input checks for line property specification arguments
  if (!any(length(col) == 1, length(col) == nq)) {
    stop("Length of 'col' argument and number of quantile series in 'freq' do not match.")
  }
  if (!any(length(lty) == 1, length(lty) == nq)) {
    stop("Length of 'lty' argument and number of quantile series in 'freq' do not match.")
  }
  if (!any(length(lwd) == 1, length(lwd) == nq)) {
    stop("Length of 'lwd' argument and number of quantile series in 'freq' do not match.")
  }
  
  
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
  
  # replace 0 quantiles in data with NAs when a log-scaled y-axis is used, throw warning to notify user
  if (yscale == "log" && any(unlist(data[, -1], use.names = FALSE) == 0, na.rm = TRUE)){
    
    warning("Quantiles where value == 0 removed prior to plotting.")
    for (i in 2:(nq + 1)) {
      te <- which(data[, i] == 0)
      if (length(te) > 0){
        data[which(data[, i] == 0), i] <- NA
      }
    }
  }
  
  
  # conditional: create y-axis label strings
  if (!is.expression(ylab) && ylab == "m3s") {
    ylabel <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
  } else if (!is.expression(ylab) && ylab == "mmd") {
    ylabel <- expression(paste("Q (mmm d"^{-1}, ")"))
  } else {
    ylabel <- ylab
  }
  
  # conditional: extract automatic y-axis limits by finding global maxima in all provided data series
  if (is.null(ylim)) {
    ylim <- range(unlist(data[, -1], use.names = FALSE), na.rm = TRUE)
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
  
  
  ## conditional: plot with linear or gaussian x-scale
  # linear
  if (xscale == "lin") {
    
    # set up the plot region, but do not plot yet. Background grid will be plotted first
    plot(data[, c(1, 2)], log = plot.log, axes = FALSE, type = "n", ylab = ylabel, xlab = xlab, ylim = ylim)
    
    # plot background grid
    grid(equilogs = FALSE)
    
    # manually add axes and framing box
    axis(side = 2)
    axis(side = 1)
    box()
    
    # plot duration curves
    for (i in 2:(nq + 1)) {
      lines(data[, 1], data[, i], type = "l", pch = 16, cex = 0.4, col = lcol[i - 1], lty = llty[i - 1], lwd = llwd[i - 1])
    }
    
  # gaussian
  } else {
    
    # calculate gaussian x-axis values, ie for an axis where the outer quantiles are increasingly spread out
    x.gaussian <- qnorm(data[,1])
    
    # create x-axis tick positions in gaussian scaling
    x.gauss <- qnorm(c(.0001, .001, .01, .1, .2, .4, .6, .8, .9, .99, .999, .9999))
    # create x-axis labels for gaussian scale
    xlab.gauss <- c("0.0001", "0.001", "0.01", "0.1", "0.2", "0.4", "0.6", "0.8", "0.9", "0.99", "0.999", "0.9999")
    
    # set up the plot region, but do not plot yet. Background grid will be plotted first. Axes omitted as well, because gaussian axis is created manually
    plot(x.gaussian, data[, 2], axes = FALSE, log = plot.log, type = "n", ylab = ylabel, xlab = xlab, ylim = ylim)
    
    # plot background grid (manually in this case, because grid() will not plot on gaussian tick marks)
    abline(h = axTicks(2), col = "lightgray", lty = "dotted")
    abline(v = x.gauss, col = "lightgray", lty = "dotted")
    
    # manually add axes and framing box
    axis(side = 2)
    axis(side = 1, at = x.gauss, labels = xlab.gauss)
    box()
    
    # plot duration curves
    for (i in 2:(nq + 1)) {
      lines(x.gaussian, data[, i], type = "l", pch = 16, cex = 0.4, col = lcol[i - 1], lty = llty[i - 1], lwd = llwd[i - 1])
    }
    
  }
  
  # add legend if requested
  if (add.legend) {
    
    # create legend labels, conditional on if user provided names manually and if number of observations for frequencies is known
    if (is.null(l.legend)) {
      lgnd <- names(freq)[-1]
    } else {
      lgnd <- l.legend
    }
    
    if (!is.null(attr(freq, which = "n.obs"))) {
      lgnd <- paste(lgnd, " (", attr(freq, "n.obs"), " obs.)", sep = "")
    }
    # print legend
    legend("topright", legend = lgnd, bty = "n", lty = llty, col = col, cex=.9)
  }
  
}
