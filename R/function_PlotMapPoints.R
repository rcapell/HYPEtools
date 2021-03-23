#'
#' Plot function for mapped point information
#'
#' Plot mapped point information, e.g. model performances at observation sites.
#' 
#' @param x Information to plot, typically model performances from imported HYPE 'subassX.txt' files. Data frame object 
#' with two columns, first column containing SUBIDs and second column containing model results to plot. See details.
#' @param sites A \code{SpatialPointsDataFrame} object. Typically an imported outlet point vector point file. Import of vector points  
#' requires additional packages, e.g. \code{\link[rgdal:readOGR]{rgdal}}.
#' @param sites.subid.column Integer, column index in the \code{sites} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param bg A \code{SpatialPolygonsDataFrame} object to plot as grey background. Typically an imported sub-basin vector polygon file. 
#' For plots with several background layers, use \code{add = TRUE} and plot background layer(s) first.
#' @param map.adj Numeric, map adjustion in direction where it is smaller than the plot window. A value of \code{0} means left-justified 
#' or bottom-justified, \code{0.5} (the default) means centered, and \code{1} means right-justified or top-justified.
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}}.
#' @param legend.pos Legend, scale, and north arrow position, keyword string. One of \code{"left"}, \code{"topleft"}, \code{"topright"}, 
#' \code{"right"}, \code{"bottomright"}, \code{"bottomleft"}.
#' @param legend.title Character string or mathematical expression. An optional title for the legend. If none is provided here, \code{var.name}
#' is used as legend title string. For select HYPE variables, pretty legend titles are in-built.
#' @param legend.outer Logical. If \code{TRUE}, outer break point values will be plotted in legend.
#' @param legend.inset Numeric, inset distance(s) from the margins as a fraction of the plot region for legend, scale and north arrow. 
#' See \code{\link{legend}} and details below.
#' @param col Colors to use on the map. One of the following: \itemize{
#' \item \code{NULL}, to use a default purple-red-yellow-blue color ramp, best used with \code{col.breaks = NULL}.
#' \item A color ramp palette function, e.g. as returned from a call to \code{\link{colorRampPalette}}
#' \item A vector of colors. This can be a character vector of R's built-in color names or hexadecimal strings as returned by 
#' \code{\link{rgb}}, or an integer vector of current \code{\link{palette}} indices.
#' }
#' @param col.breaks A numeric vector, specifying break points for discretisation of model result values into classes. Class boundaries will be
#' interpreted as right-closed, i.e upper boundaries included in class. Lowest class boundary included in lowest class as well.
#' Meaningful results require the lowest and uppermost breaks to bracket all model result values, otherwise there will be 
#' unclassified white spots on the map plot. If \code{NULL} (the default), \code{col.breaks} covers a range from 0 to 1 
#' with 9 intervals, and an additional interval for negative values. This is suitable for e.g. NSE performances.
#' @param plot.scale Logical, plot a scale bar below legend (i.e. position defined by legend position). NOTE: works only with 
#' projected maps based on meter units, not geographical projections
#' @param plot.arrow Logical, plot a North arrow below legend (i.e. position defined by legend position).
#' @param pt.cex Numeric, plot point size expansion factor, works on top of \code{par.cex}.
#' @param par.cex Numeric, character expansion factor. See description of \code{cex} in \code{\link{par}}.
#' @param par.mar Plot margins as in \code{\link{par}} argument \code{mar}. Defaults to a nearly margin-less plot. 
#' In standard use cases of this function, plot margins do not need to be changed.
#' @param pch Integer, plotting symbol. See \code{\link{points}}.
#' @param add Logical, default \code{FALSE}. If \code{TRUE}, add to existing plot. In that case \code{map.adj} has no effect.
#' @param restore.par Logical, if \code{TRUE}, par settings will be restored to original state on function exit.

#' @details
#' \code{PlotMapPoints} can be used to print point information on a mapped surface. The primary target are model performance 
#' measures as written to 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:subassx.txt}{HYPE 'subassX.txt' files}, but 
#' color scale and break point arguments are flexible enough to also be used with e.g. HYPE output variables or other data.
#' 
#' \code{PlotMapPoints} per default works with a margin-less figure and positions map and legend items close to the plot boundaries. 
#' In order to move map and legend closer to each other, change the plot device width. 
#' 
#' Legends are positioned by keyword through argument \code{legend.pos}, defaulting to the right side of the map. \code{legend.pos} and 
#' \code{map.adj} should be chosen so that legend and map do not overlap. Additionally, the legend position can be fine-tuned using 
#' argument \code{legend.inset}. This is particularly useful for legend titles with more than one line. For details on inset 
#' specification see \code{inset} in \code{\link{legend}}. 
#' 
#' @return 
#' \code{PlotMapPoints} returns a plot to the currently active plot device, and invisibly an object of class 
#' \code{\link{SpatialPointsDataFrame}} as provided in argument \code{sites}, with plotted values and color codes added as columns 
#' in the data slot.
#'
#' @seealso 
#' \code{\link{ReadSubass}} for HYPE result import; \code{\link{ReadMapOutput}} for a similar plot function

#' @examples
#' \dontrun{require(rgdal)
#' x11(width = 5, height = 8)
#' mysites <- readOGR(dsn = "../gisdata", layer = "myHYPEoutlets")
#' mybg <- readOGR(dsn = "../gisdata", layer = "myHYPEsubids")
#' PlotMappoints(x = mysubass, sites = mysites, map.subid.column = 2, bg = mybg)}
#' 
#' @export
#' @import sp


PlotMapPoints <- function(x, sites, sites.subid.column = 1, bg = NULL, map.adj = 0, plot.legend = T, 
                          legend.pos = "right", legend.title = NULL, legend.outer = F, legend.inset = c(0, 0), col = NULL, 
                          col.breaks = NULL, plot.scale = T, plot.arrow = T, pt.cex = 1, 
                          par.cex = 1, par.mar = rep(0, 4) + .1, pch = 21, add = FALSE, restore.par = FALSE) {
  
  # input argument checks
  stopifnot(is.data.frame(x), dim(x)[2] == 2, class(sites)=="SpatialPointsDataFrame", 
            is.null(col.breaks) || is.numeric(col.breaks))
  stopifnot(map.adj %in% c(0, .5, 1))
  stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
  # if (length(col.breaks) == 1) {
  #   col.breaks <- range(x[, 2], na.rm = T)
  #   warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
  # }
  if (!is.null(col.breaks) && (min(col.breaks) > min(x[, 2], na.rm = T) || max(col.breaks) < max(x[, 2], na.rm = T))) {
    warning("Range of user-provided argument 'col.breaks' does not cover range of 'x[, 2]. 
            Areas outside range will be excluded from plot.")
  }
  
  # sort col.breaks to make sure breaks are in increasing order
  if (!is.null(col.breaks)) {
    col.breaks <- sort(col.breaks, decreasing = FALSE)
  }
  
  # add y to legend inset if not provided by user
  if (length(legend.inset) == 1) {
    legend.inset[2] <- 0
  }
  
  # save current state of par() variables which are altered below, for restoring on function exit
  # conditional on argument reset.par'
  if (restore.par) {
    par.mar0 <- par("mar")
    par.xaxs <- par("xaxs")
    par.yaxs <- par("yaxs")
    par.lend <- par("lend")
    par.xpd <- par("xpd")
    par.cex0 <- par("cex")
    on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex0))
  }
  
  
  ## data preparation and conditional assignment of break point vectors and colors to x 
  
  # create color breaks vector from user input or internally
  if (!is.null(col.breaks)) {
    cbrks <- col.breaks
    mnx <- min(cbrks)
    # special treatment for single-value maps
    if (length(cbrks) == 1) {
      cbrks <- range(cbrks) + c(-1, 1)
    }
  } else {
    if (max(x[, 2], na.rm = TRUE) > 1) {
      warning("Plot values in 'x' outside range of automatic color range. Provide suitable range in 'col.breaks' to plot them.")
    }
    mnx <- min(x[, 2], na.rm = TRUE)
    if (mnx < 0) {
      cbrks <- c(mnx, seq(0, 1, by = 0.1))
    } else {
      cbrks <- seq(0, 1, by = 0.1)
    }
  }
  
  # define colors for classes
  if (is.function(col) || is.null(col)) {
    # color definition through color ramp function, either automatic or user-supplied
    if (is.function(col)) {
      # user-supplied color ramp function
      crfun <- col
      col.class <- crfun(length(cbrks) - 1)
    } else {
      # no color ramp function supplied, create default, add purple for negative values if they exist in x
      crfun <- colorRampPalette(c("#e81515", "#EEEE00", "#2892c7"))
      if (mnx < 0){
        col.class <- c("purple", crfun(length(cbrks) - 2))
      } else {
        col.class <- crfun(length(cbrks) - 1)
      }
    }
  } else if(is.vector(col)) {
    # a vector of colors is supplied
    if (length(col) == length(col.breaks) - 1) {
      col.class <- col
    } else {
      stop("If colors are specified as vector in 'col', the number of colors in 'col' must be one less than the number of breakpoints in 'col.breaks'.")
    }
  } else {
    # Error treatment for all other types of user input
    stop("Invalid 'col' argument.")
  }
  
  # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
  x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(x[, 3]) <- col.class
  # convert to character to make it conform to plotting requirements below
  x[, 3] <- as.character(x[, 3])
  # give it a name
  names(x)[3] <- "color"
  
  # number of columns in orginial sites map
  nc.sites <- ncol(sites@data)
  # add x to subid map table (in data slot, indicated by @), merge by SUBID
  sites@data <- data.frame(sites@data, x[match(sites@data[, sites.subid.column], x[, 1]),])
  # select sites for which x exists
  sts <- sites[!is.na(sites@data[, nc.sites + 1]), ]
  
  # update legend title if none was provided by user or "auto" selection
  if (is.null(legend.title)) {
    legend.title <- toupper(names(x)[2])
  }
  
  
  ## plot settings
  if (!add) {
    plot.new()
    par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = T, cex = par.cex)
    frame()
  } else {
    par(xpd = T, cex = par.cex, lend = 1)
  }
  
  
  ## the positioning of all plot elements works with three scales for the device's plot region: 
  ## inches, fraction, and map coordinates
  
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  
  # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.pos <- legend(legend.pos, legend = rep(NA, length(cbrks) - 1),
                       col = col.class, lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
  # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.wd <- leg.fr.pos$rect$w
  # legend box element height (fraction), with workaround for single-class maps
  if (length(leg.fr.pos$text$y) == 1) {
    te <- legend(legend.pos, legend = rep(NA, length(cbrks)),
                 col = crfun(length(cbrks)), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
    legbx.fr.ht <- diff(c(te$text$y[length(cbrks)], te$text$y[length(cbrks) - 1]))
  } else {
    legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
  }
  
  
  ## prepare legend annotation
  
  # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
  ann.txt <- signif(cbrks, digits = 2)
  # conditional: remove outer break points
  if (!legend.outer) {
    ann.txt[c(1, length(ann.txt))] <- ""
  }
  # annotation width (inches)
  ann.in.wd <- max(strwidth(ann.txt, "inches"))
  # legend inset required to accomodate text annotation, and scalebar (always below legend)
  leg.inset <- c(ann.in.wd/p.in.wd, if(legend.pos %in% c("bottomright", "bottomleft")) {0.1} else {0})
  
  # conditional on legend placement side (legend annotation always right of color boxes)
  if (legend.pos %in% c("bottomright", "right", "topright")) {
    
    # update legend inset
    legend.inset <- legend.inset + leg.inset
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) - legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomright") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "right") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
    
  } else {
    # left side legend
    # update legend inset
    legend.inset[2] <- legend.inset[2] + leg.inset[2]
    ## annotation positions (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # inset scaling factor, used if 'add' is TRUE, otherwise 1 (explicitly because usr does not get updated directly when set)
    if (add) {
      f.inset.x <- par("usr")[2] - par("usr")[1]
      f.inset.y <- par("usr")[4] - par("usr")[3]
    } else {
      f.inset.x <- 1
      f.inset.y <- 1
    }
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] * f.inset.x - 0.01
    if (legend.pos == "bottomleft") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
    } else if (legend.pos == "left") {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks)))
    } else {
      ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
    }
  }
  
  
  ## calculate coordinates for map positioning
  
  # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
  if (!is.null(bg)) {
    if (is.projected(bg)) {
      bbx <- bbox(bg)
      # map side ratio (h/w)
      msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
      # plot area side ratio (h/w)
      psr <- par("pin")[2] / par("pin")[1]
    } else {
      bbx <- bbox(bg)
      # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
      par(new = T)
      plot(sites, col = NULL, xlim = bbx[1, ], ylim = bbx[2, ])
      # create a map side ratio based on the device region in user coordinates and the map bounding box
      p.range.x <- diff(par("usr")[1:2])
      p.range.y <- diff(par("usr")[3:4])
      m.range.x <- diff(bbx[1, ])
      m.range.y <- diff(bbx[2, ])
      # map side ratio (h/w)
      msr <- m.range.y / m.range.x
      # plot area side ratio (h/w)
      psr <- p.range.y / p.range.x
    }
  } else {
    if (is.projected(sites)) {
      bbx <- bbox(sites)
      # map side ratio (h/w)
      msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
      # plot area side ratio (h/w)
      psr <- par("pin")[2] / par("pin")[1]
    } else {
      bbx <- bbox(sites)
      # set user coordinates using a dummy plot
      par(new = T)
      plot(sites, col = NULL, add = add)
      # create a map side ratio based on the device region in user coordinates and the map bounding box
      p.range.x <- diff(par("usr")[1:2])
      p.range.y <- diff(par("usr")[3:4])
      m.range.x <- diff(bbx[1, ])
      m.range.y <- diff(bbx[2, ])
      # map side ratio (h/w)
      msr <- m.range.y / m.range.x
      # plot area side ratio (h/w)
      psr <- p.range.y / p.range.x
    }
  }
  
  
  # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
  if (msr > psr) {
    # map is smaller than plot window in x direction, map can be moved left or right
    if (map.adj == 0) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
    } else if (map.adj == .5) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
    } else {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
    }
  } else {
    # map is smaller than plot window in y direction, map can be moved up or down
    if (map.adj == 0) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
    } else if (map.adj == .5) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
    } else {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
    }
  }
  
  
  ## plot the map and add legend using the positioning information derived above
  
  # map
  if (!is.null(bg)) {
    plot(bg, col = "grey90", border = "grey70", ylim = pylim, xlim = pxlim, add = add)
    plot(sts, bg = sts$color, border = 1, pch = pch, lwd = .8, cex = 1.2 * pt.cex, add = T)
  } else {
    plot(sts, bg = sts$color, col = 1, pch = pch, lwd = .8, cex = 1.2 * pt.cex, ylim = pylim, xlim = pxlim, add = add)
  }
  # legend
  if (plot.legend) {
    legend(legend.pos, legend = rep(NA, length(cbrks) - 1), inset = legend.inset, 
           col = rev(col.class), lty = 1, lwd = 14,  bty = "n", title = legend.title, pt.cex = pt.cex)
    # convert annotation positioning to map coordinates, only if 'add' is FALSE
    # then plot annotation text
    if (!add) {
      ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
      ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
      text(x = ann.mc.x, y = ann.mc.y, labels = rev(ann.txt), adj = c(0, .5), cex = 0.8)
    } else {
      text(x = ann.fr.x, y = ann.fr.y, labels = rev(ann.txt), adj = c(0, .5), cex = 0.8)
    }
  }
  
  ## scale position (reference point: lower left corner), also used as reference point for north arrow
  ## conditional on 'add'
  
  if (add) {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- par("usr")[2] - signif(diff(par("usr")[1:2])/4, 0) - legend.inset[1] * diff(par("usr")[1:2])
    } else {
      lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
    }
    
    # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]*f.inset.y/2)
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2]/2 - .1) * f.inset.y)
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2]/2 - .1) * f.inset.y)
    }
  } else {
    
    # x position conditional on legend placement side
    if (legend.pos %in% c("bottomright", "right", "topright")) {
      lx <- pxlim[2] - signif(diff(bbx[1,])/4, 0) - legend.inset[1] * diff(pxlim)
    } else {
      lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
    }
    
    # y position conditional legend placement position
    if (legend.pos %in% c("bottomright", "bottomleft")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2) * diff(pylim) + pylim[1]
    } else if (legend.pos %in% c("right", "left")) {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    } else {
      ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2]/2 - .1) * diff(pylim) + pylim[1]
    }
  }
  
  if (plot.scale) {
    if (!is.projected(sites)) {
      warning("Scale bar meaningless with un-projected maps. Set 'plot.scale = F' to remove it.")
    }
    if (!add) {
      ldistance <- signif(diff(bbx[1,])/4, 0)
    } else {
      ldistance <- signif(diff(par("usr")[1:2])/4, 0)
    }
    .Scalebar(x = lx, 
              y = ly, 
              distance = ldistance, 
              scale = 0.001, t.cex = 0.8)
  }
  
  if (plot.arrow) {
    
    if (add) {
      nlen <- diff(par("usr")[1:2])/70
      # north arrow x position conditional on side where legend is plotted
      if (legend.pos %in% c("bottomright", "right", "topright")) {
        nx <- lx - 0.02 * diff(par("usr")[1:2])
      } else {
        nx <- lx + signif(diff(par("usr")[1:2])/4, 0) + 0.055 * diff(par("usr")[1:2])
      }
    } else {
      nlen <- diff(bbx[1,])/70
      # north arrow x position conditional on side where legend is plotted
      if (legend.pos %in% c("bottomright", "right", "topright")) {
        nx <- lx - 0.02 * diff(pxlim)
      } else {
        nx <- lx + signif(diff(bbx[1,])/4, 0) + 0.055 * diff(pxlim)
      }
    }
    
    .NorthArrow(xb = nx, 
                yb = ly, 
                len = nlen, cex.lab = .8)
  }
  
  # invisible unless assigned: return map with added data and color codes
  invisible(sites)
}

# # DEBUG
# x <- ReadSubass("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_miracle/res_wq_baseline/subass2.txt")[, c(1, 3)]
# x <- ReadGeoData("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_miracle/GeoData.txt")[, c("SUBID", "PARREG")]
# x <- read.table(file = "//winfs-proj/data/proj/Fouh/Europe/E-HYPE/EHYPEv3.xDev/New Data/Xobs_WQ/Xobsar/RDir/subid_xobs.txt", sep = "\t", header = T)
# sites <- readOGR("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/gis", layer = "helgean_outlet_points")
# sites <- readOGR(dsn = "//winfs-proj/data/proj/Fouh/Europe/E-HYPE/EHYPEv3.0/Data/RepurposedData/WHIST/Current_shapefiles/Utloppspunkter", layer = "EHYPE3_utlopp_20141211_rev20150325")
# bg <- readOGR("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/gis/helgean/subbasin", layer = "helgean_shype_aro_y")
# bg <- NULL
# add <- F
# map.adj <- 0
# plot.legend <- T
# plot.scale <- T
# plot.arrow <- T
# legend.pos <- "right"
# legend.title <- "test"
# legend.inset <- c(0,0)
# col.breaks <- NULL
# col <- NULL
# sites.subid.column <- 5
# par.mar <- rep(0, 4) + .1
# par.cex <- 1
# pt.cex <- 1
