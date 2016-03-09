#' 
#' Plot function for HYPE map results.
#'
#' Draw HYPE map results, with pretty scale discretisations and color ramp defaults for select HYPE variables. 
#' 
#' @param data HYPE map results, data frame object with two columns, first column containing SUBIDs and 
#' second column containing model results. See details.
#' @param map A \code{SpatialPolygonsDataFrame} object. Typically an imported SUBID shape file, shape file import requires additional packages, 
#' e.g. \code{rgdal}.
#' @param map.subid.column Integer, index of the column in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param var.name Character string. HYPE variable name to be plotted. Mandatory for automatic color ramp selection of pre-defined
#' HYPE variables (\code{col.ramp.fun = "auto"}). Not case-sensitive. See details.
#' @param plot.scale Logical, plot a scale bar in the lower right corner. NOTE: works only with projected maps 
#' based on meter units, not geographical projections
#' @param plot.arrow Logical, plot a North arrow in the lower right corner.
#' @param map.pos Numeric, map positioning in direction where it is smaller than the plot window. A value of \code{0} means left-justified 
#' or bottom-justified, \code{0.5} (the default) means centered, and \code{1} means right-justified or top-justified.
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}}.
#' @param legend.pos Legend position, keyword string. One of \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, 
#' \code{"right"}, \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}.
#' @param legend.title Character string or mathematical expression. An optional title for the legend. If none is provided here, \code{var.name}
#' is used as legend title string. For select HYPE variables, pretty legend titles are in-built.
#' @param legend.inset Numeric, inset distance(s) from the margins as a fraction of the plot region. See \code{\link{legend}} and details below.
#' @param legend.cex Numeric, character expansion factor for legend. See description of \code{cex} in \code{\link{legend}}.
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
#' @param par.mar Plot margins as in \code{\link{par}} argument \code{mar}. Defaults to a nearly margin-less plot.
#' 
#' @details
#' \code{PlotMapOutput} plots HYPE results from 'map[variable name].txt' files, typically imported using \code{\link{ReadMapOutput}}. 
#' \code{data} arguments \strong{must} contain the variable of interest in the second column. For multicolumn map results, i.e. with 
#' several time periods, pass index selections to \code{data}, e.g. \code{mymapresult[, c(1, 3)]}. 
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
#' \code{data}. Default break points for internal color ramp \code{"ColDiffGeneric"} are based on an equal distance classification of log-scaled 
#' \code{data} ranges, centered around zero. For internal color ramp \code{"ColDiffTemp"}, they are breaks in an interval from -7.5 to 7.5 K.
#' 
#' For select common HYPE variables, given in argument \code{var.name}, an automatic color ramp selection including pretty breaks and legend titles 
#' is built into 
#' \code{PlotMapOutput}. These are 'CCTN', 'CCTP', 'COUT', and 'TEMP'. Automatic selection is activated by chosing keyword \code{"auto"}
#' in \code{col.ramp.fun}. All other HYPE variables will be plotted using a generic color ramp palette and generic break points.
#' 
#' Legends are positioned by keyword, defaulting to the right side of the map. Depending on the form of the displayed area and 
#' the size of the legend, there might be no optimal place inside the plot area. In that case, the legend can be moved to the 
#' outer margins by specifying negative \code{legend.inset} values. For details on inset specification see \code{inset} in \code{\link{legend}}. 
#' Make sure to also adapt the plot margin sizes appropriately through \code{par.mar}, see \code{mar} in \code{\link{par}}.
#' 
#' @examples
#' \dontrun{require(rgdal)
#' PlotMapOutput(data = mymapresult, map = readOGR(dsn = "../gisdata", layer = "myHYPEsubids"), map.subid.column = 2, var.name = "CCTN")}
#' 
#' @export
#' @import sp
# @importFrom sp SpatialPolygonsDataFrame SpatialPolygons


PlotMapOutput <- function(data, map, map.subid.column = 1, var.name = "", plot.scale = T, plot.arrow = T, map.pos = 0, plot.legend = T, 
                          legend.pos = "right", legend.title = NULL, legend.inset = c(0, 0), legend.cex = 1, 
                          col.ramp.fun = "auto", col.breaks = NULL, par.mar = rep(0, 4) + .1) {
  
  # input argument checks
  stopifnot(is.data.frame(data), dim(data)[2] == 2, class(map)=="SpatialPolygonsDataFrame", 
            is.null(col.breaks) || is.numeric(col.breaks))
  stopifnot(map.pos %in% c(0, .5, 1))
  stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
  
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
  par.cex <- par("cex")
  on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex))
  
  # data preparation and conditional assignment of color ramp functions and break point vectors 
  # to internal variables crfun and cbrks
  
  if (is.function(col.ramp.fun)) {
    # Case 1: a color ramp palette function is supplied
    crfun <- col.ramp.fun
    if (!is.null(col.breaks)) {
      cbrks <- col.breaks
    } else {
      cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
    }
  } else if (is.character(col.ramp.fun)) {
    # Case 2: no color ramp palette function is supplied and one of the predefined is requested
    # First treat the specific palette function strings, then "auto" requests, and last error handling for all other strings.
    # Specific palettes get a generic class break points if not provided with another by the user
    # THIS CODE IS REPETITIVE, COULD BE STREAMLINED BY BREAKING OUT cbrks ASSIGNMENT
    if (col.ramp.fun == "ColNitr") {
      crfun <- .ColNitr
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPhos") {
      crfun <- .ColPhos
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColTemp") {
      crfun <- .ColTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColPrec") {
      crfun <- .ColPrec
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColQ") {
      crfun <- .ColQ
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
      }
    } else if (col.ramp.fun == "ColDiffTemp") {
      crfun <- .ColDiffTemp
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- c(ifelse(min(data[,2]) < 7.5, min(data[,2]) - 1, 30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(data[,2]) > 7.5, max(data[,2]) + 1, 30))
        #cbrks <- quantile(data[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "ColDiffGeneric") {
      crfun <- .ColDiffGeneric
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        # create a break point sequence which is centered around zero, with class withs based on equal intervals of the log-scaled
        # variable distribution
        cbrks <- c(rev(exp(seq(0, log(max(abs(range(data[,2]))) + 1), length.out = 5)) * -1), exp(seq(0, log(max(abs(range(data[,2]))) + 1), length.out = 5)))
        #cbrks <- quantile(data[, 2], probs = seq(0, 1, .1))
      }
      
    } else if (col.ramp.fun == "auto") {
      # Here follows a limited set of pre-defined color ramps and break point vectors for select HYPE variables, and
      # at the end a generic "catch the rest" treatment for undefined variables
      if (toupper(var.name) == "CCTN") {
        crfun <- .ColNitr
        cbrks <- c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000, ifelse(max(data[,2]) > 5000, max(data[,2]) + 1, 10000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "CCTP") {
        crfun <- .ColPhos
        cbrks <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, ifelse(max(data[,2]) > 250, max(data[,2]) + 1, 1000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
        }
      } else if (toupper(var.name) == "COUT") {
        crfun <- .ColQ
        cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(data[,2]) > 500, max(data[,2]) + 1, 2000))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
        }
      } else if (toupper(var.name) == "TEMP") {
        crfun <- .ColTemp
        cbrks <- c(ifelse(min(data[,2]) < -7.5, min(data[,2]) - 1, -30), -7.5, -5, -2.5, 1, 0, 1, 2.5, 5, 7.5, ifelse(max(data[,2]) > 7.5, max(data[,2]) + 1, 30))
        if (is.null(legend.title)) {
          legend.title <- expression(paste("Air Temp. ("*degree, "C)"))
        }
      } else {
        crfun <- .ColDiffGeneric
        cbrks <- quantile(data[, 2], probs = seq(0, 1, .1), na.rm = T)
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
  cbrks <- unique(cbrks)
  # discretise the modeled values in data into classed groups, add to data as new column (of type factor)
  data[, 3] <- cut(data[, 2], breaks = cbrks, include.lowest = T)
  # replace the factor levels with color codes using the color ramp function assigned above
  levels(data[, 3]) <- crfun(length(cbrks) - 1)
  # convert to character to make it conform to plotting requirements below
  data[, 3] <- as.character(data[, 3])
  # give it a name
  names(data)[3] <- "color"
  
  # add data to subid map table (in data slot, indicated by @), merge by SUBID
  map@data <- data.frame(map@data, data[match(map@data[, map.subid.column], data[,1]),])
  
  # update legend title if none was provided by user or "auto" selection
  if (is.null(legend.title)) {
    legend.title <- toupper(var.name)
  }
  #x11(width = 4.5, height = 9)
  # par settings: lend set to square line endings because the legend below works with very thick lines 
  # instead of boxes (a box size limitation work-around); xpd set to allow for plotting a legend on the margins
  par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = T)
  plot.new()
  
  ## the positioning of all plot elements works with three scales for the device's plot region: 
  ## inches, fraction, and map coordinates
  
  # plot width (inches)
  p.in.wd <- par("pin")[1]
  
  # legend position (fraction) 
  leg.fr.pos <- legend(legend.pos, legend = rep(NA, length(cbrks) - 1), cex = legend.cex,
               col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title, plot = F)
  # legend width (fraction) 
  leg.fr.wd <- leg.fr.pos$rect$w
  # legend box element height (fraction)
  legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
  
  
  ## prepare legend annotation
  
  # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
  ann.txt <- signif(cbrks, digits = 2)
  # annotation width (inches)
  ann.in.wd <- max(strwidth(ann.txt, "inches"))
  # legend inset required to accomodate text annotation
  leg.inset <- c(ann.in.wd/p.in.wd, 0)
  
  # conditional on legend placement side (legend annotation always right of color boxes)
  if (legend.pos %in% c("bottomright", "right", "topright")) {
    
    # annotation positions (fraction)
    ann.fr.x <- 1 - rep((1 - leg.fr.pos$text$x[1]), length(ann.txt)) - legend.inset[1] - 0.01
    ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2]
    # update legend inset
    legend.inset <- legend.inset + leg.inset
    
  } else {
    
    # annotation positions (fraction)
    ann.fr.x <- rep(leg.fr.pos$text$x[1], length(ann.txt)) + legend.inset[1] - 0.01
    ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht/2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2]
  }
  
  
  ## calculate coordinates for map positioning
  
  # map coordinates
  bbx <- bbox(map)
  # map side ratio (h/w)
  msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
  # plot area side ratio (h/w)
  psr <- par("pin")[2] / par("pin")[1]
  
  # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
  if (msr > psr) {
    # map is smaller than plot window in x direction, map can be moved left or right
    if (map.pos == 0) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim)/psr)
    } else if (map.pos == .5) {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim)/psr/2, mean(as.numeric(bbx[1, ])) + diff(pylim)/psr/2)
    } else {
      pylim <- as.numeric(bbx[2, ])
      pxlim <- c(bbx[1, 2] - diff(pylim)/psr, bbx[1, 2])
    }
  } else {
    # map is smaller than plot window in y direction, map can be moved up or down
    if (map.pos == 0) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim)*psr)
    } else if (map.pos == .5) {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim)*psr/2, mean(as.numeric(bbx[2, ])) + diff(pxlim)*psr/2)
    } else {
      pxlim <- as.numeric(bbx[1, ])
      pylim <- c(bbx[2, 2] - diff(pxlim)*psr, bbx[2, 2])
    }
  }
  
  
  ## plot the map and add legend using the positioning information reived above
  
  # map
  plot(map, col = map$color, border = NA, ylim = pylim, xlim = pxlim)
  # legend
  legend(legend.pos, legend = rep(NA, length(cbrks) - 1), cex = legend.cex, inset = legend.inset, 
         col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", title = legend.title)
  # convert annotation positioning to map coordinates
  ann.mc.x <- ann.fr.x * diff(pxlim) + pxlim[1]
  ann.mc.y <- ann.fr.y * diff(pylim) + pylim[1]
  text(x = ann.mc.x, y = ann.mc.y, labels = ann.txt, adj = c(0, .5))
  
  
  if (plot.scale) {
    .Scalebar(x = bbx[1,2] - 1.5 * (if (diff(bbx[1,])/4 >= 1000) {signif(diff(bbx[1,])/4, 0)} else {1000}), 
              y = bbx[2,1] + diff(bbx[2, ]) * .01, 
              distance = if (diff(bbx[1,])/4 >= 1000) {signif(diff(bbx[1,])/4, 0)} else {1000}, 
              scale = 0.001, t.cex = 0.6)
  }
  if (plot.arrow) {
    .NorthArrow(xb = bbx[1,2], 
                yb = bbx[2,1] + diff(bbx[2, ]) * .02, 
                len = diff(bbx[1,])/70, cex.lab = .8)
    
  }
  
  
  # invisible unless assigned: return the color codes for all values in data
  invisible(data[, 3])
}


# DEBUG
library(rgdal)
data <- ReadMapOutput("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_shype/res_test/mapCOUT.txt")[, 1:2]
map <- readOGR(dsn = "//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_shype/gis", layer = "helgean_shype_aro_y")
map.subid.column <- 3
var.name <- "COUT"
plot.scale <- T
map.pos <- 0
plot.legend <- T
legend.pos <- "right"
legend.title <- "rhrhshfhfhs"
col.ramp.fun <- "auto"
col.ramp.fun <- colorRampPalette(c("yellow", "green"))
col.breaks <- NULL
par.mar <- rep(0, 4) + .1
par.mar <- c(0,0,0,3) + .1
legend.inset <- c(0,0)
legend.cex <- 1
rm(.ColQ)