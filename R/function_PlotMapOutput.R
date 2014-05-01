# function to plot a map of common HYPE variables, with pretty discretisations and color ramp defaults


#' @export

#' @title
#' Plot function for HYPE map results.
#'
#' @description
#' Draw HYPE map results, with pretty scale discretisations and color ramp defaults. 
#' Data typically imported using \code{\link{ReadMapOutput}}.
#' 
#' @param data Map results, data frame object with two columns, first column containing SUBIDs and 
#' second column containing model results. See details.
#' @param map.subid.column  
#' @param map A \code{SpatialPolygonsDataFrame} object, typically an imported SUBID map, requires package \code{rgdal}.
#' @param var.name Character string. HYPE variable name
#' @param plot.scale Logical, plot a scale bar and a North arrow in the lower right corner. NOTE: works only with projected maps 
#' based on meter units, not geographical projections
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}}.
#' @param legend.pos Legend position, keyword string. One of \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"}, 
#' \code{"right"}, \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"}.
#' @param legend.title Character string. An optional title for the legend.
#' @param col.ramp.fun Color ramp palette to use for the map. One of the following: \itemize{
#' \item \code{"auto"} to allow for automatic selection from pre-defined color ramp palettes based on argument \code{var.name}
#' \item One of the pre-defined palette functions for HYPE output variables: \code{"ColNitr"} for nitrogen, \code{"ColPhos"} for 
#' phosphorus, \code{"ColPrec"} for precipitation, \code{"ColTemp"} for temperatures, \code{"ColQ"} for runoff, 
#' or user-calculated differences: \code{"ColDiff"} (see details)
#' \item A color ramp palette function, e.g. as returned from a call to \code{\link{colorRampPalette}}
#' }
#' @param col.breaks Break points for discretisation of model result values into classes. Numeric vector of break points, will be
#' interpreted as right-closed, i.e upper boundaries included in class. Lowest class boundary included in lowest class as well.
#' Meaningful results require the lowest and uppermost breaks to bracket all model result values, otherwise there will be 
#' unclassified white spots on the map plot. Only necessary if a user-defined \code{col.ramp.fun} is supplied (otherwise a generic
#' classification with 10 classes based on 10% quantiles will be applied), but can optionally 
#' be combined with one of the pre-defined palettes.
#' 
#' @details
#' \code{PlotMapOutput} plots HYPE results from 'map[variable name].txt' files. \code{data} arguments must contain the variable 
#' of interest in the second column. For multicolumn map results, i.e. with several time periods, pass index selections to 
#' \code{data}, e.g. \code{mymapresult[, c(1, 3)]}.
#' 
#' @examples
#' \dontrun{require(rgdal); PlotMapOutput(data = mymapresult, map = readOGR(dsn = "../gisdata", layer = "myHYPEsubids", var.name = "CCTN"))}
#' 


PlotMapOutput <- function(data, map, map.subid.column, var.name, plot.scale = T, plot.legend = T, legend.pos = "right", legend.title = "", 
                          col.ramp.fun = "auto", col.breaks = NA) {
  
  # load required libraries
  require(rgdal)
  
  # input argument checks
  stopifnot(is.data.frame(data), dim(data)[2] == 2, class(map)=="SpatialPolygonsDataFrame", exists("var.name"), 
            is.na(col.breaks) || is.numeric(col.breaks))
  
  # pre-defined break point vectors (defining them all is unnecessary but makes it easier to change and extend)
  breaks.TN <- c(0, 250, 500, 1000, 2500, 5000, max(res[,2]))
  
  
  # data preparation and conditional assignment of color ramp functions and break point vectors 
  # to internal variables crfun and cbrks
  
  if (is.function(col.ramp.fun)) {
    # Case 1: a color ramp palette function is supplied
    crfun <- col.ramp.fun
    if (!is.na(col.breaks)) {
      cbrks <- col.breaks
    } else {
      cbrks <- quantile(data[, 2], probs = seq(0, 1, .1))
    }
  } else if (is.character(col.ramp.fun)) {
    # Case 2: no color ramp palette function is supplied and one of the predefined is requested
    # First treat the specific palette function strings, then "auto" requests, and last error handling for all other strings.
    # Specific palettes get a generic class break points if not provided with another by the user
    # THIS CODE IS REPETITIVE, COULD BE STREAMLINED BY BREAKING OUT cbrks ASSIGNMENT
    if (col.ramp.fun == "ColNitr") {
      crfun <- .ColNitr
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    } else if (col.ramp.fun == "ColPhos") {
      crfun <- .ColPhos
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    } else if (col.ramp.fun == "ColTemp") {
      crfun <- .ColTemp
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    } else if (col.ramp.fun == "ColPrec") {
      crfun <- .ColPrec
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    } else if (col.ramp.fun == "ColQ") {
      crfun <- .ColQ
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    } else if (col.ramp.fun == "ColDiff") {
      crfun <- .ColDiff
      if (!is.na(col.breaks)) {
        cbrks <- col.breaks
      } else {
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
      
    } else if (col.ramp.fun == "auto") {
      # Here follows a limited set of pre-defined color ramps and break point vectors for select HYPE variables, and
      # at the end a generic "catch the rest" treatment for undefined variables
      if (toupper(var.name) == "CCTN") {
        crfun <- .ColNitr
        cbrks <- c(0, 100, 250, 500, 1000, 2500, 5000, ifelse(max(data[,2]) > 5000, max(data[,2]), 10000))
        legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
      } else if (toupper(var.name) == "CCTP") {
        crfun <- .ColPhos
        cbrks <- c(0, 25, 50, 100, 150, 200, 250, ifelse(max(data[,2]) > 250, max(data[,2]), 1000))
        legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
      } else if (toupper(var.name) == "COUT") {
        crfun <- .ColQ
        cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(data[,2]) > 500, max(data[,2]), 2000))
        legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
      } else if (toupper(var.name) == "TEMP") {
        crfun <- .ColTemp
        cbrks <- c(-10, -7.5, -5. -2.5, 0, 2.5, 5, 7.5, ifelse(max(data[,2]) > 7.5, max(data[,2]), 30))
        legend.title <- expression(paste("Air Temp. ("*degree, "C)"))
      } else {
        crfun <- .ColDiff
        cbrks <- seq(min(data[, 2]), max(data[, 2]), length.out = 10)
      }
    }
  } else {
    # Error treatment for all other types of user input
    stop("Invalid 'col.ramp.fun' argument. Neither a function nor a character string.")
  }
  
  ## translate modelled variable to fixed colour mapping (this should become a separate function)
  
  data[,3] <- cut(data[, 2], breaks = cbrks, include.lowest = T)
  data[,4] <- data[,3]
  levels(data[,4]) <- crfun(length(cbrks) - 1)
  data[,4] <- as.character(data[,4])
  names(data)[3:4] <- c("group", "color")
  
  # add data to subid map table, merge by SUBID
  map@data <- data.frame(map@data, data[match(map@data[, map.subid.column], data[,1]),])
  
  
  #x11(width = 4.5, height = 9)
  par(mar = rep(0, 4) + .1, xaxs = "i", yaxs = "i", lend = 1)
  plot(map, col = map$color, border = NA)
  bbx <- bbox(map)
  .Scalebar(x = bbx[1,2] - 1.5 * (if (diff(bbx[1,])/4 >= 1000) {signif(diff(bbx[1,])/4, 0)} else {1000}), 
            y = bbx[2,1] + diff(bbx[2, ]) * .01, 
            distance = if (diff(bbx[1,])/4 >= 1000) {signif(diff(bbx[1,])/4, 0)} else {1000}, 
            scale = 0.001, t.cex = 1)
  .NorthArrow(xb = bbx[1,2], 
              yb = bbx[2,1] + diff(bbx[2, ]) * .02, 
              len = diff(bbx[1,])/70, cex.lab = .8)
  legend(legend.pos, legend = .CreateLabelsFromBreaks(cbrks), 
         col = crfun(length(cbrks) - 1), lty = 1, lwd = 14,  bty = "n", cex = .8, title = legend.title)
  
  # invisible unless assigned: return 
  #invisible()
}


# # DEBUG
# data <- ReadMapOutput("//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/echam/BUS/period1/res/mapCCTN.txt")
# data <- ReadMapOutput("//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/echam/BUS/period1/res/mapCCTP.txt")
# data <- ReadMapOutput("//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/echam/BUS/period1/res/mapCOUT.txt")
# data <- ReadMapOutput("//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/Projekt/cleo/WP_3/2014-04_SHYPE_combined_scenarios/echam/BUS/period1/res/mapTEMP.txt")
# map <- readOGR(dsn = "//winfs/data/arkiv/proj/FoUhArkiv/Sweden/S-HYPE/S-HYPE2012B/gis", layer = "SHYPE2012B_aro_y")
# # Clean map from Norwegian area
# map <- map[map$SUBIDnew < 51000, ]
# map.subid.column <- 3
# var.name <- "CCTN"
# plot.scale <- T
# plot.legend <- T
# legend.pos <- "right"
# legend.title <- "" 
# col.ramp.fun <- "auto"
# col.ramp.fun <- colorRampPalette(c("yellow", "green"))
# col.breaks <- NA
# 
# # re-set map data
# map@data <- map@data[, 1:7]
# rm(data, map, map.subid.column, var.name, plot.scale, plot.legend, legend.pos, legend.title, col.ramp.fun, col.breaks, .CreateLabelsFromBreaks, cbrks, crfun,
#    .ColNitr, .NorthArrow, .Scalebar, bbx)
