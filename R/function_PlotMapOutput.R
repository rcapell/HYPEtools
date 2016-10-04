#' 
#' Plot function for HYPE map results.
#'
#' Draw HYPE map results, with pretty scale discretisations and color ramp defaults for select HYPE variables. 
#' 
#' @param x HYPE model results, typically 'map output' results. Data frame object with two columns, first column containing SUBIDs and 
#' second column containing model results to plot. See details.
#' @param map A \code{SpatialPolygonsDataFrame} object. Typically an imported sub-basin shape file, shape file import 
#' requires additional packages, e.g. \code{rgdal}.
#' @param map.subid.column Integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param var.name Character string. HYPE variable name to be plotted. Mandatory for automatic color ramp selection of pre-defined
#' HYPE variables (\code{col.ramp.fun = "auto"}). Not case-sensitive. See details.
#' @param map.adj Numeric, map adjustion in direction where it is smaller than the plot window. A value of \code{0} means left-justified 
#' or bottom-justified, \code{0.5} (the default) means centered, and \code{1} means right-justified or top-justified.
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}}.
#' @param legend.pos Legend, scale, and north arrow position, keyword string. One of \code{"left"}, \code{"topleft"}, \code{"topright"}, 
#' \code{"right"}, \code{"bottomright"}, \code{"bottomleft"}.
#' @param legend.title Character string or mathematical expression. An optional title for the legend. If none is provided here, \code{var.name}
#' is used as legend title string. For select HYPE variables, pretty legend titles are in-built.
#' @param legend.inset Numeric, inset distance(s) from the margins as a fraction of the plot region for legend, scale and north arrow. 
#' See \code{\link{legend}} and details below.
#' @param col.ramp.fun Color ramp palette to use for the map. One of the following: \itemize{
#' \item \code{"auto"} to allow for automatic selection from pre-defined color ramp palettes and break points based on argument \code{var.name}, 
#' see details
#' \item One of the pre-defined palette functions for HYPE output variables or user-calculated differences between model results: 
#' \code{"ColNitr"} for nitrogen, \code{"ColPhos"} for phosphorus, \code{"ColPrec"} for precipitation, \code{"ColTemp"} for 
#' temperatures, \code{"ColQ"} for runoff, \code{"ColDiffTemp"} for temperature differences,  \code{"ColDiffGeneric"} for generic
#' differences, see details
#' \item A color ramp palette function, e.g. as returned from a call to \code{\link{colorRampPalette}}
#' }
#' @param col.breaks A numeric vector, specifying break points for discretisation of model result values into classes. Class boundaries will be
#' interpreted as right-closed, i.e upper boundaries included in class. Lowest class boundary included in lowest class as well.
#' Meaningful results require the lowest and uppermost breaks to bracket all model result values, otherwise there will be 
#' unclassified white spots on the map plot. Not mandatory, can optionally 
#' be combined with one of the pre-defined palettes, including \code{"auto"} selection. Per default, a generic
#' classification will be applied (see details).
#' @param plot.scale Logical, plot a scale bar below legend (i.e. position defined by legend position). NOTE: works only with 
#' projected maps based on meter units, not geographical projections
#' @param plot.arrow Logical, plot a North arrow below legend (i.e. position defined by legend position).
#' @param par.cex Numeric, character expansion factor. See description of \code{cex} in \code{\link{par}}.
#' @param par.mar Plot margins as in \code{\link{par}} argument \code{mar}. Defaults to a nearly margin-less plot. 
#' In standard use cases of this function, plot margins do not need to be changed.
#' @param add Logical, default \code{FALSE}. If \code{TRUE}, add to existing plot. In that case \code{map.adj} has no effect.
#' @param restore.par Logical, if \code{TRUE}, par settings will be restored to original state on function exit.
#' 
#' @details
#' \code{PlotMapOutput} plots HYPE results from 'map[variable name].txt' files, typically imported using \code{\link{ReadMapOutput}}. 
#' \code{x} arguments \strong{must} contain the variable of interest in the second column. For multicolumn map results, i.e. with 
#' several time periods, pass index selections to \code{x}, e.g. \code{mymapresult[, c(1, 3)]}. 
#' 
#' Mapped variables are visualised using color-coded data intervals. \code{PlotMapOutput} can use one of several internal color ramp functions 
#' suitable for some common HYPE result variables as listed under argument \code{col.ramp.fun}. These are called by keyword. Alternatively,
#' any color ramp function can be provided. The internal color ramps are mostly single color ramps with less saturated colors for smaller values
#' and more saturated values for higher values, suitable for e.g. concentration or volume ranges. \code{ColTemp} is an exception to this rule, 
#' with blue-to-turquoise colors and yellow-to-red 
#' colors to represent temperature ranges below and above zero. Two further ramps provided are suitable to represent calculated differences, 
#' e.g. between two model runs, a generic ramp with reds on the low end and blues on the high end, and a temperature-specific one in reversed
#' order. 
#' 
#' Break points between color classes of in-built or user-provided color ramp palettes can optionally be provided in argument 
#' \code{col.breaks}. This is particularly useful when specific pretty class boundaries are needed, e.g. for publication figures. Per default, 
#' break points for internal single color ramps and user-provided ramps are calculated based on 10\% percentiles of HYPE results given in 
#' \code{x}. Default break points for internal color ramp \code{"ColDiffGeneric"} are based on an equal distance classification of log-scaled 
#' \code{x} ranges, centered around zero. For internal color ramp \code{"ColDiffTemp"}, they are breaks in an interval from -7.5 to 7.5 K.
#' 
#' For select common HYPE variables, given in argument \code{var.name}, an automatic color ramp selection including pretty breaks and legend titles 
#' is built into 
#' \code{PlotMapOutput}. These are 'CCTN', 'CCTP', 'COUT', and 'TEMP'. Automatic selection is activated by chosing keyword \code{"auto"}
#' in \code{col.ramp.fun}. All other HYPE variables will be plotted using a generic color ramp palette and generic break points.
#' 
#' \code{PlotMapOutput} per default works with a margin-less figure and positions map and legend items close to the plot boundaries. 
#' In order to move map and legend closer to each other, change the plot device width.
#' 
#' Legends are positioned by keyword through argument \code{legend.pos}, defaulting to the right side of the map. \code{legend.pos} and 
#' \code{map.adj} should be chosen so that legend and map do not overlap. Additionally, the legend position can be fine-tuned using 
#' argument \code{legend.inset}. This is particularly useful for legend titles with more than one line. For details on inset 
#' specification see \code{inset} in \code{\link{legend}}. 
#' 
#' @return 
#' \code{PlotMapOutput} returns a plot to the currently active plot device, and invisibly an object of class \code{\link{SpatialPolygonsDataFrame}} 
#' as provided in argument \code{map}, with plotted values and color codes added as columns in the data slot.
#' 
#' @seealso 
#' \code{\link{ReadMapOutput}} for HYPE result import; \code{\link{PlotMapPoints}} for a similar plot function
#' 
#' @examples
#' \dontrun{require(rgdal)
#' x11(width = 5, height = 8)
#' PlotMapOutput(x = mymapresult, map = readOGR(dsn = "../gisdata", layer = "myHYPEsubids"), map.subid.column = 2, var.name = "CCTN")}
#' 
#' @export
#' @import sp
# @importFrom sp SpatialPolygonsDataFrame SpatialPolygons


PlotMapOutput <- function(x, map, map.subid.column = 1, var.name = "", map.adj = 0, plot.legend = T, 
                          legend.pos = "right", legend.title = NULL, legend.inset = c(0, 0), 
                          col.ramp.fun = "auto", col.breaks = NULL, plot.scale = T, plot.arrow = T, 
                          par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, restore.par = FALSE) {
  
  # input argument checks
  stopifnot(is.data.frame(x), dim(x)[2] == 2, class(map)=="SpatialPolygonsDataFrame", 
            is.null(col.breaks) || is.numeric(col.breaks))
  stopifnot(map.adj %in% c(0, .5, 1))
  stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
  if (length(col.breaks) == 1) {
    col.breaks <- range(x[, 2], na.rm = T)
    warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
  }
  if (!is.null(col.breaks) && (min(col.breaks) >= min(x[, 2], na.rm = T) || max(col.breaks) <= max(x[, 2], na.rm = T))) {
    warning("Range of user-provided argument 'col.breaks' does not cover range of 'x[, 2]. 
            Areas outside range will be excluded from plot.")
  }
  
  # add y to legend inset if not provided by user
  if (length(legend.inset) == 1) {
    legend.inset[2] <- 0
  }
  
  # save current state of par() variables which are altered below, for restoring on function exit
  par.mar0 <- par("mar")
  par.xaxs <- par("xaxs")
  par.yaxs <- par("yaxs")
  par.lend <- par("lend")
  par.xpd <- par("xpd")
  par.cex0 <- par("cex")
  if (restore.par) {
    on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex0))
  }
  
  # data preparation and conditional assignment of color ramp functions and break point vectors 
  # to internal variables crfun and cbrks
  
  if (is.function(col.ramp.fun)) {
    # Case 1: a color ramp palette function is supplied
    crfun <- col.ramp.fun
    if (!is.null(col.breaks)) {
      cbrks <- col.breaks
    } else {
      cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
    }
  } else if (is.character(col.ramp.fun)) {
    # Case 2: no color ramp palette function is supplied and one of the predefined is requested
    # First treat the specific palette function strings, then "auto" requests, and last error handling for all other strings.
    # Specific palettes get a generic class break points if not provided with another by the user
    # THIS CODE IS REPETITIVE, COULD BE STREAMLINED BY BREAKING OUT cbrks ASSIGNMENT
    if (col.ramp.fun == "ColNitr") {
      crfun <- ColNitr
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPhos") {
      crfun <- ColPhos
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColTemp") {
      crfun <- ColTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPrec") {
      crfun <- ColPrec
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColQ") {
      crfun <- ColQ
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColDiffTemp") {
      crfun <- ColDiffTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- c(ifelse(min(x[,2]) < 7.5, min(x[,2]) - 1, 30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
        #cbrks <- quantile(x[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "ColDiffGeneric") {
      crfun <- ColDiffGeneric
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        # create a break point sequence which is centered around zero, with class withs based on equal intervals of the log-scaled
        # variable distribution
        cbrks <- c(rev(exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)) * -1), exp(seq(0, log(max(abs(range(x[,2]))) + 1), length.out = 5)))
        #cbrks <- quantile(x[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "auto") {
      # Here follows a limited set of pre-defined color ramps and break point vectors for select HYPE variables, and
      # at the end a generic "catch the rest" treatment for undefined variables
      if (toupper(var.name) == "CCTN") {
        crfun <- ColNitr
        cbrks <- c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000, ifelse(max(x[,2]) > 5000, max(x[,2]) + 1, 10000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "CCTP") {
        crfun <- ColPhos
        cbrks <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, ifelse(max(x[,2]) > 250, max(x[,2]) + 1, 1000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "COUT") {
        crfun <- ColQ
        cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(x[,2]) > 500, max(x[,2]) + 1, 2000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
        }
      } else if (toupper(var.name) == "TEMP") {
        crfun <- ColTemp
        cbrks <- c(ifelse(min(x[,2]) < -7.5, min(x[,2]) - 1, -30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[,2]) > 7.5, max(x[,2]) + 1, 30))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Air Temp. ("*degree, "C)"))
        }
      } else {
        crfun <- ColDiffGeneric
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else {
      # Error treatment for all other strings
      stop("Invalid 'col.ramp.fun' argument. Neither a function nor a recognised character string.")
    }
  } else {
    # Error treatment for all other types of user input
    stop("Invalid 'col.ramp.fun' argument. Neither a function nor a character string.")
  }
  
  
  # in variables with large numbers of "0" values, the lower 10%-percentiles can be repeatedly "0", which leads to an error with cut,
  # so cbrks is shortened to unique values (this affects only the automatic quantile-based breaks)
  # if just one value remains (or was requested by user), replace crbks by minmax-based range (this also resolves unexpected behaviour
  # with single-value cbrks in 'cut' below).
  cbrks <- unique(cbrks)
  if (length(cbrks) == 1) {
    cbrks <- range(cbrks) + c(-1, 1)
  }
  # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
  x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(x[, 3]) <- crfun(length(cbrks) - 1)
  # convert to character to make it conform to plotting requirements below
  x[, 3] <- as.character(x[, 3])
  # give it a name
  names(x)[3] <- "color"
  
  # add x to subid map table (in data slot, indicated by @), merge by SUBID
  map@data <- data.frame(map@data, x[match(map@data[, map.subid.column], x[,1]),])
  
  # update legend title if none was provided by user or "auto" selection
  if (is.null(legend.title)) {
    legend.title <- toupper(var.name)
  }
  #x11(width = 4.5, height = 9)
  # par settings: lend set to square line endings because the legend below works with very thick lines 
  # instead of boxes (a box size limitation work-around); xpd set to allow for plotting a legend on the margins
  if (!add) {
    plot.new()
    par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = T, cex = par.cex)
    frame()
  } else {
    par(lend = 1, xpd = T, cex = par.cex)
  }
  
  
  ## the positioning of all plot elements works with three scales for the device's plot region: 
  ## inches, fraction, and map coordinates
  
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  
  # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates) 
  leg.fr.pos <- legend(legend.pos, legend = rep(NA, length(cbrks) - 1),
               col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
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
  if (is.projected(map)) {
    bbx <- bbox(map)
    # map side ratio (h/w)
    msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
    # plot area side ratio (h/w)
    psr <- par("pin")[2] / par("pin")[1]
  } else {
    bbx <- bbox(map)
    # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
    plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ])
    # create a map side ratio based on the device region in user coordinates and the map bounding box
    p.range.x <- diff(par("usr")[1:2])
    p.range.y <- diff(par("usr")[3:4])
    m.range.x <- diff(bbox(map)[1, ])
    m.range.y <- diff(bbox(map)[2, ])
    # map side ratio (h/w)
    msr <- m.range.y / m.range.x
    # plot area side ratio (h/w)
    psr <- p.range.y / p.range.x
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
  plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
  # legend
  if (plot.legend) {
    legend(legend.pos, legend = rep(NA, length(cbrks) - 1), inset = legend.inset, 
           col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title)
    # convert annotation positioning to map coordinates, only if 'add' is FALSE
    # then plot annotation text
    if (!add) {
      ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
      ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
      text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
    } else {
      text(x = ann.fr.x, y = ann.fr.y, labels = ann.txt, adj = c(0, .5), cex = 0.8)
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
    if (!is.projected(map)) {
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
  invisible(map)
}

# # DEBUG
# library(rgdal)
# x <- ReadMapOutput("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_shype/res_test/mapCOUT.txt")[, 1:2]
# map <- readOGR(dsn = "//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_shype/gis", layer = "helgean_shype_aro_y")
# map.subid.column <- 3
# var.name <- "COUT"
# plot.scale <- T
# map.adj <- 0
# plot.legend <- T
# legend.pos <- "bottomleft"
# legend.title <- "rhrhshfhfhs"
# #col.ramp.fun <- "ColQ"
# col.ramp.fun <- colorRampPalette(c("yellow", "green"))
# col.breaks <- NULL
# par.mar <- rep(0, 4) + .1
# legend.inset <- c(0,0)
# par.cex <- 1
# plot.arrow <- T
# plot.legend <- T
# add <- T
# rm(list = ls(all.names = T))
