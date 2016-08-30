
#' Bar plots of upstream-averaged classes of HYPE sub-basins
#' 
#' Function to plot upstream-averaged landscape property classes of one or several sub-basins as bar plots, e.g. 
#' land use or soils.
#' 
#' @param x Data frame, containing column-wise class group fractions with SUBIDs in first column. Typically a result 
#' from \code{\link{UpstreamGroupSLCClasses}}.
#' @param type Character string keyword for use with \code{desc}. Type of class groups, either \code{"landuse"}, 
#' \code{"soil"}, or \code{"crop"}, can be abbreviated.
#' @param desc List for use with \code{type}. Class description labels imported from a 'description.txt' file, for bar labeling.
#' See \code{\link{ReadDescription}} for formatting details.
#' @param xlab Character string, x-axis label, with defaults for standard groups land use, soil, and crops. 
#' @param ylab Character string, y-axis label.
#' @param ylim Numeric, two element vector with limits for the y-axis. Defaults to values which give ample space for bar labels.
#' @param class.names Character vector of class group names, with same length as number of class group fractions in \code{x}. 
#' Specification of bar labels, alternative to arguments \code{type} and \code{desc}, in particular for cases where a non-standard
#' grouping was used for \code{x}.
#' @param names.arg Character vector, see \code{\link{barplot}}. Defaults to no labeling below bars (text labels within plot through 
#' arguments above).
#' @param cex.axis Numeric, character expansion factor for axis annotation and labels.
#' @param cex.names Numeric, character expansion factor for class group labels.
#' @param col Colors for bars. Defaults to pre-defined color (sequences if multiple catchments). 
#' @param legend.text Character, if provided, a legend will be plotted. Defaults to none if one sub-basin is plotted, and SUBIDs 
#' if several sub-basins are plotted. Set to \code{NA} to prevent legend plotting in any case.
#' @param legend.pos Character keyword for legend positioning, most likely \code{"left"} or \code{"right"}. For details, see 
#' \code{\link{legend}}.
#' @param pars List of tagged values which are passed to \code{\link{par}}.
#' 
#' @details 
#' \code{BarplotUpstreamClasses} is a wrapper for \code{\link{barplot}}, with vertical labels plotted over the class group bars. 
#' Most arguments have sensible defaults, but can be adapted for fine-tuning if necessary.
#' 
#' @return 
#' The function returns bar midpoints, see description in \code{\link{barplot}}.
#' 
#' @seealso 
#' \code{\link{UpstreamGroupSLCClasses}}
#' \code{\link{barplot}}
#' 
#' @examples 
#' \dontrun{res <- UpstreamGroupSLCClasses(subid = 21, gd = mygeodata, gc = mygeoclass, bd = mybranchdata, type = "soil")}
#' \dontrun{BarplotUpstreamClasses(x = res, type = "s", desc = mydescription)}
#' 
#' @export

BarplotUpstreamClasses <- function (x, type = NULL, desc = NULL, xlab = NULL, ylab = "Area fraction (%)", 
                                    ylim = c(0,  max(x[, -1] * 1.5)), class.names = NULL, names.arg = rep("", ncol(x) - 1), 
                                    cex.axis = 1, cex.names = .9, col = NULL, legend.text = NULL, 
                                    legend.pos = "left", 
                                    pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA)) {
  # input argument checks
  if (!is.null(class.names) && ncol(x) -1 != length(class.names)) {
    stop("Number of classes in arguments 'x' and 'class.names' do not match.")
  }
  
  # set plot parameters
  par(pars)
  
  # axis and bar label and color construction, conditional on function arguments
  if (type == "landuse" || type == "l") {
    # bar labels
    if (!is.null(desc)) {
      lgroup <- desc$lu
    } else if (!is.null(class.names)) {
      lgroup <- class.names
    } else {
      lgroup <- names(x[[, -1]])
    }
    # default value for x axis label
    if (is.null(xlab)) {
      xlab <- "Land use group"
    }
    # color for bars
    if (is.null(col)) {
      col <- ColGreens(nrow(x))
    }
    
  } else if (type == "soil" || type == "s") {
    if (!is.null(desc)) {
      lgroup <- desc$so
    } else if (!is.null(class.names)) {
      lgroup <- class.names
    } else {
      lgroup <- names(x[[, -1]])
    }
    if (is.null(xlab)) {
      xlab <- "Soil group"
    }
    if (is.null(col)) {
      col <- ColReds(nrow(x))
    }
    
  } else if (type == "crop" || type == "c") {
    if (!is.null(desc)) {
      lgroup <- desc$cr
    } else if (!is.null(class.names)) {
      lgroup <- class.names
    } else {
      lgroup <- names(x[[, -1]])
    }
    if (is.null(xlab)) {
      xlab <- "Crop group"
    }
    if (is.null(col)) {
      col <- ColPurples(nrow(x))
    }
    
  } else {
    if (!is.null(type)) {
      warning(paste0("Unknown keyword ", type, " provided in argument 'type'."))
    }
    if (!is.null(class.names)) {
      lgroup <- class.names
    } else {
      lgroup <- names(x[[, -1]])
    }
    if (is.null(col)) {
      col <- ColBlues(nrow(x))
    }
  }
  
  # legend text construction
  if (is.null(legend.text)) {
    if (nrow(x) == 1) {
      leg.t <- NULL
    } else {
      leg.t <- x[, 1]
    }
  } else if (is.na(legend.text)) {
    leg.t <- NULL
  } else {
    leg.t <- legend.text
    if (length(leg.t) != nrow(x)) {
      warning("Number of elements in 'legend.text' do not match number of rows in 'x'.")
    }
  }
  
  # plot bars and labels
  res <- barplot(height = as.matrix(x[, -1]), names.arg = names.arg, beside = T, xlab = xlab, ylab = ylab, 
                 ylim = ylim, border = NA, space = c(0, .2), col = col, cex.axis = cex.axis, legend.text = leg.t, 
                 args.legend = list(border = NA, bty = "n", x = legend.pos, cex = cex.names))
  mtext(text = lgroup, side = 3, at = colMeans(res), line = -.1, padj = .5, cex = cex.names, las = 3, adj = 1)
  mtext(xlab, side = 1, line = .5, cex = cex.axis)
  box()
  
  # return barplot value invisibly
  invisible(res)
}

# # DEBUG
# gd <- ReadGeoData("../PlotBasinSummary/GeoData.txt")
# gcl <- ReadGeoClass("../PlotBasinSummary/GeoClass.txt", headrow = 4)
# type <- "landuse"
# x <- UpstreamGroupSLCClasses(subid = c(8000152, 8127943), gd = gd, gc = gcl, type = type)
# desc <- ReadDescription("../PlotBasinSummary/description.txt")
# pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA)
# ylab <- "Area fraction (%)"
# ylim <- c(0,  max(x[, -1] * 1.5))
# names.arg = rep("", ncol(x) - 1)
# cex.axis = 1
# cex.names = .9
# class.names = NULL
# xlab = NULL
# col = NULL
# legend.text = NULL
# legend.pos <- "left"
