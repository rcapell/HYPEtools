
#' Bar plots of upstream-averaged classes of HYPE sub-basins
#' 
#' Function to plot upstream-averaged landscape property classes of one or several sub-basins as bar plots, e.g. 
#' land use or soils.
#' 
#' @param x Data frame, containing column-wise class group fractions with SUBIDs in first column. Typically a result 
#' from \code{\link{UpstreamGroupSLCClasses}}. Column names of class group fractions _must_ end with \code{_x}, with x giving the
#' class group ID, see details.
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
#' @param col Colors for bars. Defaults to pre-defined color (use vector of same length as number of SUBIDs if multiple catchments). 
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
#' Column names of \code{x} are used to link class groups to class IDs in \code{desc}. HYPE has no formal 
#' requirements on how class IDs are numbered and when one of the standard groups land use, soil, or crop are provided in \code{x}, 
#' there might be missing class IDs. Class names in \code{desc} are matched against column name endings \code{'_x'} in \code{x}. 
#' If manual names are provided in \code{class.names}, the column name endings must be a consecutive sequence from 1 to number of elements 
#' in \code{class.names}.
#' 
#' @return 
#' The function returns bar midpoints, see description in \code{\link{barplot}}.
#' 
#' @seealso 
#' \code{\link{UpstreamGroupSLCClasses}}
#' \code{\link{barplot}}
#' 
#' @examples 
#' \dontrun{res <- UpstreamGroupSLCClasses(subid = 21, gd = mygeodata, gcl = mygeoclass, bd = mybranchdata, type = "soil")}
#' \dontrun{BarplotUpstreamClasses(x = res, type = "s", desc = mydescription)}
#' 
#' @export

BarplotUpstreamClasses <- function (x, type = NULL, desc = NULL, xlab = NULL, ylab = "Area fraction (%)", 
                                    ylim = c(0,  max(x[, -1] * 150)), class.names = NULL, names.arg = rep("", ncol(x) - 1), 
                                    cex.axis = 1, cex.names = .9, col = NULL, legend.text = NULL, 
                                    legend.pos = "left", 
                                    pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")) {
  # input argument checks
  if (!is.null(class.names) && ncol(x) -1 != length(class.names)) {
    stop("Number of classes in arguments 'x' and 'class.names' do not match.")
  }
  
  # axis and bar label and color construction, conditional on function arguments
  if (type == "landuse" || type == "l") {
    # bar labels
    if (!is.null(desc)) {
      lgroup <- desc$lu
      lid <- desc$lu.id
    } else if (!is.null(class.names)) {
      lgroup <- class.names
      lid <- 1:length(class.names)
    } else {
      lgroup <- names(x)[-1]
      lid <- sapply(strsplit(names(x)[-1], "_"), function(x) {as.numeric(x[2])})
    }
    # default value for x axis label
    if (is.null(xlab)) {
      xlab <- "Land use group"
    }
    # color for bars
    if (is.null(col)) {
      col <- "chartreuse3"
    }
    
  } else if (type == "soil" || type == "s") {
    if (!is.null(desc)) {
      lgroup <- desc$so
      lid <- desc$so.id
    } else if (!is.null(class.names)) {
      lgroup <- class.names
      lid <- 1:length(class.names)
    } else {
      lgroup <- names(x[, -1])
      lid <- sapply(strsplit(names(x)[-1], "_"), function(x) {as.numeric(x[2])})
    }
    if (is.null(xlab)) {
      xlab <- "Soil group"
    }
    if (is.null(col)) {
      col <- "indianred"
    }
    
  } else if (type == "crop" || type == "c") {
    if (!is.null(desc)) {
      lgroup <- desc$cr
      lid <- desc$cr.id
    } else if (!is.null(class.names)) {
      lgroup <- class.names
      lid <- 1:length(class.names)
    } else {
      lgroup <- names(x[, -1])
      lid <- sapply(strsplit(names(x)[-1], "_"), function(x) {as.numeric(x[2])})
    }
    if (is.null(xlab)) {
      xlab <- "Crop group"
    }
    if (is.null(col)) {
      col <- "plum3"
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
  
  # set plot parameters
  par(pars)
  
  # get class IDs from column names in x
  cid <- sapply(strsplit(names(x)[-1], "_"), function(x) {as.numeric(x[2])})
  
  # plot bars and labels
  res <- barplot(height = as.matrix(x[, -1]) * 100, names.arg = names.arg, beside = T, xlab = xlab, ylab = ylab, 
                 ylim = ylim, border = NA, space = c(0, .2), col = col, cex.axis = cex.axis, legend.text = NULL)
  mtext(text = lgroup[match(cid, lid)], side = 3, at = colMeans(res), line = -.2, padj = .3, cex = cex.names, las = 3, adj = 1)
  mtext(xlab, side = 1, line = .5, cex = cex.names)
  abline(v = diff(colMeans(res))/2 + colMeans(res)[-ncol(res)] - .05, col = "grey90")
  if (!is.null(leg.t)) {
    legend(x = legend.pos, legend = leg.t, border = NA, cex = cex.names, fill = col, bg = "#FFFFFFB3", box.lty = 0)
  }
  box()
  
  # return barplot value invisibly
  invisible(res)
}

# # DEBUG
# gd <- ReadGeoData("../PlotBasinSummary/GeoData.txt")
# gcl <- ReadGeoClass("../PlotBasinSummary/GeoClass.txt", headrow = 4)
# type <- "landuse"
# x <- UpstreamGroupSLCClasses(subid = c(8000152, 8127943, 8213537), gd = gd, gc = gcl, type = type)
# desc <- ReadDescription("../PlotBasinSummary/description.txt")
# pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")
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

