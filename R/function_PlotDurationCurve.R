
#'
#' @export
#' 
#' @title
#' Plot a duration curve
#'
#' @description
#' Convenience wrapper function for a line \code{\link{plot}}, with pretty defaults for axis annotation.
#'
#' @param freq Data frame with two columns, containing probabilities in the first column and data quantiles in the second. Typically an object as 
#' returned by \code{\link{ExtractFreq}}, or a subset thereof.
#' 
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, either as keyword \code{"m3s"} or \code{"mmd"} for pre-defined pretty 
#' discharge labels, or any other string which will be plotted unchanged.
#' 
#' @param print.n.obs Logical. If \code{TRUE} and \code{freq} is a result from \code{\link{ExtractFreq}}, the number of observations on which the quantiles are based 
#' will be plotted. 
#' 
# @param df.up.area A data frame as returned from \code{\link{SumUpstreamAreas}}. Two columns with SUBIDs and upstream areas. Can be provided to reduce 
# overall computation time.
#' 
#' @param ylim  Numeric vector of length two, giving y-axis limits. \code{NULL} for default values.
#' 
#' @param xlab Character string, x-axis label.
#' 
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue.
#' 
#' @param ... Further graphical parameters (see \code{\link{par}}) to be supplied as arguments to \code{\link{lines}}, e.g. line type and width, for 
#' details see there.
#' 
#' @details
#' \code{PlotDurationCurve} plots a duration curve on a log-scaled y-axis with pretty formatting defaults. The function sets 
#' \code{\link{par}} parameters \code{mar}, \code{tcl}, and \code{mgp} internally and will override previously set values for the returned plot.
#' 
#' @return
#' \code{PlotDurationCurve} returns a plot to the currently active plot device.
#' 
#' @seealso
#' \code{\link{ExtractFreq}}
#' 
#' @examples
#' \dontrun{PlotDurationCurve(freq = myfreq, print.n.obs = T)}



PlotDurationCurve <- function(freq, ylab = "m3s", print.n.obs = FALSE, ylim = NULL, xlab = "Flow exceedance percentile", col = "blue", ...) {
  
  # yaxis label
  if (ylab == "m3s") {
    ylabel <- expression(paste("Q (m"^3, " s"^{-1}, ")"))
  } else if (ylab == "mmd") {
    ylabel <- expression(paste("Q (mmm d"^{-1}, ")"))
  } else {
    ylabel <- ylab
  }
  
  ## create plot dataframe
  # get exceedance values from probabilities
  data <- freq
  data[, 1] <- 1 - freq[, 1]
  # remove 0 rows with 0 quantiles which conflict with log-scaled y-axis, throw warning to notify user
  if (length(which(data[, 2] < 0)) > 0) {
    data <- data[data[, 2] > 0, ]
    warning("Quantiles where value == 0 removed prior to plotting.")
  }
  
  
  # set plot parameters
  par(mar = c(3,3,1,1)+.1, tcl = -0.2, mgp = c(1.8, 0.3, 0))
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  plot(data, log = "y", type = "n", ylab = ylabel, xlab = xlab, ylim = ylim)
  
  # plot background grid
  grid(equilogs = F)
  
  # plot duration curve
  lines(data, type = "l", pch = 16, cex = 0.4, col = col, ...)
  
  # add number of observations if requested
  if (print.n.obs) {
    if (!is.null(attr(freq, which = "n.obs"))) {
      mtext(text = paste(attr(freq, which = "n.obs"), "observations"), side = 3, adj = 1, cex = .8, font = 3)
    } else {
      warning("Printing of number of observations requested, but attribute not available in 'freq'.")
    }
  }
}
