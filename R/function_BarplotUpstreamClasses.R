
#' Bar plots of upstream-averaged classes of HYPE sub-basins
#' 
#' Function to plot upstream-averaged landscape property classes of one or several sub-basins as bar plots, e.g. 
#' land use or soils. Builds on \code{\link{barplot}}.
#' 
#' @param x Data frame, containing column-wise class group fractions with SUBIDs in first column. Typically a result 
#' from \code{\link{UpstreamGroupSLCClasses}}. Column names of class group fractions _must_ end with \code{_x}, with x giving the
#' class group ID, see details.
#' @param type Character string keyword for class group labeling, used in combination with \code{desc}. Type of class groups, 
#' either \code{"landuse"}, \code{"soil"}, or \code{"crop"} (abbreviations allowed). If \code{"custom"} (default),  labeling can be fine-
#' tuned with argument \code{class.names}.
#' @param desc List for use with \code{type}. Class description labels imported from a 'description.txt' file, for labeling of 
#' standard class groups. See \code{\link{ReadDescription}} for formatting details.
#' @param class.names Character vector of class group names, with same length as number of class group fractions in \code{x}. 
#' Specification of bar labels, alternative to arguments \code{type} and \code{desc}, in particular for cases where a non-standard
#' grouping was used for \code{x}.
#' @param xlab Character string, x-axis label, with defaults for standard groups land use, soil, and crops. 
#' @param ylab Character string, y-axis label.
#' @param ylim Numeric, two element vector with limits for the y-axis. Defaults to values which give ample space for bar labels.
#' @param names.arg Character vector, see \code{\link{barplot}}. Defaults to no labeling below bars (text labels within plot through 
#' arguments above).
#' @param cex.axis Numeric, character expansion factor for axis annotation and labels.
#' @param cex.names Numeric, character expansion factor for class group labels.
#' @param col Colors for bars. Defaults to \code{type}-specific pre-defined color. 
#' @param border Colors for bar borders. Defaults to no borders.
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
#' # Import source data
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te2 <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' te3 <- ReadDescription(filename = system.file("demo_model", "description.txt", 
#'                        package = "HYPEtools"))
#' # Calculate plot data, upstream soil fractions
#' te4 <- UpstreamGroupSLCClasses(subid = 63794, gd = te1, gcl = te2, type = "soil")
#' # Function call
#' BarplotUpstreamClasses(x = te4, type = "s", desc = te4, ylim = c(0,100))
#' 
#' @importFrom stats aggregate na.fail
#' @importFrom graphics par barplot mtext abline legend box
#' @export

# Exported function
BarplotUpstreamClasses <- function (x, type = c("custom", "landuse", "soil", "crop"), desc = NULL, class.names = NULL, xlab = NULL, 
                                    ylab = "Area fraction (%)", ylim = c(-.05,  max(x[, -1] * 150)), names.arg = rep("", ncol(x) - 1), 
                                    cex.axis = 1, cex.names = .9, col = NULL, border = NA, legend.text = NULL, legend.pos = "left", 
                                    pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")) {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  # Call function and pass arguments
  .BarplotUpstreamClasses(x=x, type=type, desc=desc, class.names=class.names, xlab=xlab,
                          ylab=ylab, ylim=ylim, names.arg=names.arg, cex.axis=cex.axis,
                          cex.names=cex.names, col=col, border=border, legend.text=legend.text,
                          legend.pos=legend.pos, pars=pars)
  
  
}

# Internal function used for PlotBasinSummary
.BarplotUpstreamClasses <- function (x, type = c("custom", "landuse", "soil", "crop"), desc = NULL, class.names = NULL, xlab = NULL, 
                                    ylab = "Area fraction (%)", ylim = c(-.05,  max(x[, -1] * 150)), names.arg = rep("", ncol(x) - 1), 
                                    cex.axis = 1, cex.names = .9, col = NULL, border = NA, legend.text = NULL, legend.pos = "left", 
                                    pars = list(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")) {
  
  # If user calls the internal function using HYPEtools:::.BarplotUpstreamClasses(), then make sure that par is reset on exit
  if(!length(sys.calls())>1){
    warning("Please use HYPEtools::BarplotUpstreamClasses() instead of the internal HYPEtools:::.BarplotUpstreamClasses() function")
    userpar <- par(no.readonly = TRUE) # Backup par
    on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  }
  
  # input argument checks
  type <- match.arg(type)
  
  if (!is.null(class.names) && ncol(x) -1 != length(class.names)) {
    stop("Number of classes in arguments 'x' and 'class.names' do not match.")
  }
  
  
  ## axis and bar label and color construction, conditional on function arguments
  
  if (type == "custom") {
    # none of the standard types, desc ignored
    if(!is.null(desc)) {
      warning("Argument 'desc' ignored because argument 'type' is 'custom'.")
    }
    # class labels
    if (!is.null(class.names)) {
      lgroup <- class.names
    } else {
      lgroup <- names(x)[-1]
    }
    # bar colour(s)
    if (is.null(col)) {
      if (nrow(x) == 1) {
        col <- "#9292D3"
      } else {
        col <- ColBlues(nrow(x))
      }
    }
  } else {
    # one of the three standard class types 
    if (type == "landuse") {
      # class labels
      if (!is.null(desc)) {
        lgroup <- desc$lu
      } else if (!is.null(class.names)) {
        lgroup <- class.names
      } else {
        lgroup <- names(x)[-1]
      }
      # default value for x axis label
      if (is.null(xlab)) {
        xlab <- "Land use group"
      }
      # color for bars
      if (is.null(col)) {
        if (nrow(x) == 1) {
          col <- "chartreuse3"
        } else {
          col <- ColGreens(nrow(x))
        }
      }
    } else if (type == "soil") {
      if (!is.null(desc)) {
        lgroup <- desc$so
      } else if (!is.null(class.names)) {
        lgroup <- class.names
      } else {
        lgroup <- names(x[, -1])
      }
      if (is.null(xlab)) {
        xlab <- "Soil group"
      }
      if (is.null(col)) {
        if (nrow(x) == 1) {
          col <- "indianred"
        } else {
          col <- ColReds(nrow(x))
        }
      }
      
    } else if (type == "crop") {
      if (!is.null(desc)) {
        lgroup <- desc$cr
      } else if (!is.null(class.names)) {
        lgroup <- class.names
      } else {
        lgroup <- names(x[, -1])
      }
      if (is.null(xlab)) {
        xlab <- "Crop group"
      }
      if (is.null(col)) {
        col <- "plum3"
        if (nrow(x) == 1) {
          col <- "plum3"
        } else {
          col <- ColPurples(nrow(x))
        }
      }
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
  
  
  # if group labels come from a description file, match class IDs with description labels 
  # (individual classes can be missing in x)
  if (!is.null(desc)) {
    # get class IDs from column names in x
    cid <- sapply(strsplit(names(x)[-1], "_"), function(x) {as.numeric(x[2])})
    # select labels for existing groups existing in x
    lgroup <- lgroup[cid]
  }
  
  # plot bars and labels
  res <- barplot(height = as.matrix(x[, -1]) * 100, names.arg = names.arg, beside = TRUE, xlab = xlab, ylab = ylab, 
                 ylim = ylim, border = border, space = c(0, .2), col = col, cex.axis = cex.axis, legend.text = NULL)
  mtext(text = lgroup, side = 3, at = colMeans(res), line = -.2, padj = .3, cex = cex.names, las = 3, adj = 1)
  mtext(xlab, side = 1, line = .5, cex = cex.names)
  abline(v = diff(colMeans(res))/2 + colMeans(res)[-ncol(res)] - .05, col = "grey90", lwd = 1)
  if (!is.null(leg.t)) {
    legend(x = legend.pos, legend = leg.t, border = NA, cex = cex.names, fill = col, bg = "#FFFFFFB3", box.lty = 0)
  }
  box(lwd = 1)
  
  # return barplot value invisibly
  invisible(res)
}

