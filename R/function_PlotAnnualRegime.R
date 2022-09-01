#' Plot annual regimes
#'
#' Convenience wrapper function for a combined line \code{\link{plot}} with \code{\link{polygon}} variation ranges.
#'
#' @param x List, typically a result from \code{\link{AnnualRegime}}, containing data frames with aggregated long-term average 
#' regime data and two attributes \code{period} and \code{timestep}.
#' See Details and Value sections there.
#' @param line Character string, keyword for type of average line to plot. Either \code{"mean"} or \code{"median"}.
#' @param band Character vector, keyword for variation bands. If \code{"none"} (default), plot average line(s) only. \code{"minmax"}, 
#' \code{"p25p75"}, or \code{p5p95} to include bands of variation. Combinations of bands are allowed, but providing \code{"none"} 
#' will always prevent plotting of any band. See details.
#' @param add.legend Logical. If \code{TRUE}, a legend will be added to the plot.
#' @param l.legend Character vector. If non-NULL, legend labels are read from here instead of from column names in \code{x$mean}.
#' @param l.position Legend position, keyword string. One of \code{"left"}, \code{"topleft"}, \code{"topright"}, 
#' \code{"right"}, \code{"bottomright"}, \code{"bottomleft"}.
#' @param log Logical, if \code{TRUE}, y-axis will be log-scaled.
#' @param ylim  Numeric vector of length two, giving y-axis limits. Defaults to min-max range of all plotted data.
#' @param xlab Character string or \code{\link{plotmath}} expression string, x-axis label. Default prints the time period on which the 
#' regime is based, read from \code{x$period}.
#' @param ylab Character or \code{\link{plotmath}} expression string. Y-axis label, with a default for discharge regimes.
#' @param col Line color specification, see \code{\link{par}} for details. Defaults to blue. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param alpha Numeric, alpha transparency value for variation bands. Value between \code{0} (transparent) and \code{255} (opaque), see 
#' also \code{\link{rgb}}
#' @param lty Line type specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param lwd Line width specification, see \code{\link{par}} for details. Either a single value or a vector of the same length as quantile 
#' series in \code{freq}.
#' @param mar Numeric vector of length 4, margin specification as in \code{\link{par}} with modified default. Details see there.
#' @param verbose Logical, print warnings if \code{NA} values are found in \code{x}. Defaults to \code{TRUE}.
#' 
#' @details
#' \code{PlotAnnualRegime} plots contents from lists as returned by \code{\link{AnnualRegime}} (for format details, see there). If 
#' \code{NA} values are present in the plot data, the function will throw a warning if \code{verbose = TRUE} and proceed with plotting 
#' all available data. 
#' 
#' Argument \code{band} allows to plot variation bands to be plotted in addition to average lines. These can be (combinations of) ranges 
#' between minima and maxima, 5th and 95th percentiles, and 25th and 75th percentiles, i.e. all moments available in \code{AnnualRegime} 
#' results. 
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
#' # Source data, HYPE basin output with a number of result variables
#' te1 <- ReadBasinOutput(filename = system.file("demo_model",
#' "results", "0003587.txt", package = "HYPEtools"))
#' # Daily discharge regime, computed and observed,
#' # hydrologigical year from October, aggregated to weekly means
#' te2 <- AnnualRegime(te1[, c("DATE", "COUT", "ROUT")], ts.in = "day",
#' ts.out = "week", start.mon = 10)
#' PlotAnnualRegime(x = te2)
#' PlotAnnualRegime(x = te2, line = "median", band = "p05p95",
#' add.legend = TRUE, col = c("red", "blue"))
#' 
#' @importFrom graphics par grid abline axis axis.POSIXct box polygon lines legend
#' @importFrom stats na.omit
#' @export

# Exported function
PlotAnnualRegime <- function(x, line = c("mean", "median"), band = c("none", "p05p95", "p25p75", "minmax"), add.legend = FALSE, 
                             l.legend = NULL, l.position = c("topright", "bottomright", "right", "topleft", "left", "bottomleft"), 
                             log = FALSE, ylim = NULL, ylab = expression(paste("Q (m"^3, " s"^{-1}, ")")), 
                             xlab = paste(format(attr(x, "period"), format = "%Y"), collapse = " to "), col = "blue", alpha = 30, 
                             lty = 1, lwd = 1, mar = c(3, 3, 1, 1) + .1, verbose = TRUE) {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  # Call function and pass arguments
  .PlotAnnualRegime(x=x, line=line, band=band, add.legend=add.legend,l.legend=l.legend,
                    l.position=l.position, log=log,ylim=ylim, ylab=ylab, xlab=xlab,
                    col=col, alpha=alpha, lty=lty, lwd=lwd, mar=mar, verbose=verbose)
  
}

# Internal function used for PlotBasinOutput and PlotBasinSummary
.PlotAnnualRegime <- function(x, line = c("mean", "median"), band = c("none", "p05p95", "p25p75", "minmax"), add.legend = FALSE, 
                             l.legend = NULL, l.position = c("topright", "bottomright", "right", "topleft", "left", "bottomleft"), 
                             log = FALSE, ylim = NULL, ylab = expression(paste("Q (m"^3, " s"^{-1}, ")")), 
                             xlab = paste(format(attr(x, "period"), format = "%Y"), collapse = " to "), col = "blue", alpha = 30, 
                             lty = 1, lwd = 1, mar = c(3, 3, 1, 1) + .1, verbose = TRUE) {
  
  # If user calls the internal function using HYPEtools:::.PlotAnnualRegime(), then make sure that par is reset on exit
  if(!length(sys.calls())>1){
    warning("Please use HYPEtools::PlotAnnualRegime() instead of the internal HYPEtools:::.PlotAnnualRegime() function")
    userpar <- par(no.readonly = TRUE) # Backup par
    on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  }

  # number of time series in x
  nq <- ncol(x$mean) - 2
  
  # input check for type specification arguments
  line <- match.arg(line)
  band <- match.arg(band, several.ok = TRUE)
  # default is "none"
  if (any(band == "none")) {
    band <- "none"
  }
  l.position <- match.arg(l.position)
  stopifnot(is.logical(verbose))
  
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
  if (is.null(ylim) && line == "mean" && any(band == "none")) {
    ylim <- range(unlist(x$mean[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && line == "median" && any(band == "none")) {
    ylim <- range(unlist(x$median[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && any(band == "minmax")) {
    ylim <- range(unlist(rbind(x$minimum, x$maximum)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && any(band == "p05p95")) {
    ylim <- range(unlist(rbind(x$p05, x$p95)[, -c(1:2)], use.names = FALSE), na.rm = TRUE)
  } else if (is.null(ylim) && any(band == "p25p75")) {
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
  
  # set plot parameters, lend for square background bands in legend
  par(mar = mar, tcl = -0.2, mgp = c(1.8, 0.3, 0), tcl = .2, lend = 1)
  
  # set up the plot region, but do not plot yet. Background grid will be plotted first
  plot(x$mean[, c(1, 3)], axes = FALSE, type = "n", ylab = ylab, xlab = xlab, ylim = ylim, log = lg)
  
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
  } else if (length(grep("hour", attr(x, "timestep"))) == 1) {
    vline <- x$mean[which(format(x$mean[, 1], format = "%d %H") == "15 12"), 1]
  }
  abline(v = vline, col = "lightgray", lty = "dotted", lwd = par("lwd"))
  
  # manually add axes and framing box
  axis(side = 2)
  axis.POSIXct(side = 1, at = vline)
  box()
  
  
  
  ## plot regimes, incl transparent variation polygons if requested, using internal function
  
  # initialise vector to hold variable names with NA values
  na.warn <- NULL
  
  # plot requested bands and lines, iterate through variables
  if (any(band == "minmax")) {
    
    polcol <- .makeTransparent(lcol, alpha = alpha)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$minimum[, c(1, i)], x$maximum[nrow(x$maximum):1, c(1, i)])
      # remove NAs and throw warning if any were found
      polcoor <- na.omit(polcoor)
      if (!is.null(attr(polcoor, "na.action")) && verbose) {
        na.warn <- c(na.warn, names(x$mean)[i])
      }
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
    
  }
  if (any(band == "p05p95")) {
    
    polcol <- .makeTransparent(lcol, alpha = alpha)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$p05[, c(1, i)], x$p95[nrow(x$p95):1, c(1, i)])
      # remove NAs and throw warning if any were found
      polcoor <- na.omit(polcoor)
      if (!is.null(attr(polcoor, "na.action")) && verbose) {
        na.warn <- c(na.warn, names(x$mean)[i])
      }
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
  }
  if (any(band == "p25p75")) {
    
    polcol <- .makeTransparent(lcol, alpha = alpha)
    for (i in 3:(nq + 2)) {
      polcoor <- rbind(x$p25[, c(1, i)], x$p75[nrow(x$p75):1, c(1, i)])
      # remove NAs and throw warning if any were found
      polcoor <- na.omit(polcoor)
      if (!is.null(attr(polcoor, "na.action")) && verbose) {
        na.warn <- c(na.warn, names(x$mean)[i])
      }
      polygon(polcoor[, 1], polcoor[, 2], col = polcol[i - 2], border = NA)
    }
  }
  if (line == "mean") {
    for (i in 3:(nq + 2)) {
      mn <- x$mean[, c(1, i)]
      # remove NAs and throw warning if any were found
      mn <- na.omit(mn)
      if (!is.null(attr(mn, "na.action")) && verbose) {
        na.warn <- c(na.warn, names(x$mean)[i])
      }
      lines(mn, type = "l", pch = 16, cex = 0.4, col = lcol[i - 2], lty = llty[i - 2], lwd = llwd[i - 2])
    }
  }
  if (line == "median") {
    for (i in 3:(nq + 2)) {
      md <- x$median[, c(1, i)]
      # remove NAs and throw warning if any were found
      md <- na.omit(md)
      if (!is.null(attr(md, "na.action")) && verbose) {
        na.warn <- c(na.warn, names(x$mean)[i])
      }
      lines(md, type = "l", pch = 16, cex = 0.4, col = lcol[i - 2], lty = llty[i - 2], lwd = llwd[i - 2])
    }
  }
  
  # throw warning with variable names if NAs were found
  if (length(na.warn) > 0 && verbose) {
    warning(paste0("NA values found in variable(s) '", paste(unique(na.warn), collapse = "', '"), "'."))
  }
  
  # add legend if requested
  if (add.legend) {
    
    # create legend labels, conditional on if user provided names manually and if number of observations for frequencies is known
    if (is.null(l.legend)) {
      lgnd <- names(x$mean)[-c(1:2)]
    } else {
      lgnd <- l.legend
    }
    
    # print legend, conditional on presence of bands in plot
    if (any(band != "none")) {
      # with band, plot fat transparent lines as bands first, then annotated lines
      #legend(l.position, legend = rep("", length(lgnd)), bty = "n", lty = 1, col = polcol, cex=.9, lwd = 14, inset = c(max(strwidth(lgnd, units = "figure")), 0))
      legend(l.position, legend = lgnd, bty = "n", lty = 1, col = polcol, cex=.9, lwd = 14, text.col = .makeTransparent(1, 0))
      for (i in seq_along(band)) {
        legend(l.position, legend = lgnd, bty = "n", lty = 1, col = polcol, cex=.9, lwd = 14 * i / length(band), text.col = .makeTransparent(1, 0))
      }
      legend(l.position, legend = lgnd, bty = "n", lty = llty, col = lcol, cex=.9, lwd = llwd)
    } else {
      # only lines
      legend(l.position, legend = lgnd, bty = "n", lty = llty, col = lcol, cex=.9, lwd = llwd)
    }
  }
}
