#'
#' Plot function for HYPE map results.
#'
#' Draw HYPE map results, with pretty scale discretisations and color ramp defaults for select HYPE variables.
#'
#' @param x HYPE model results, typically 'map output' results. Data frame object with two columns, first column containing SUBIDs and
#' second column containing model results to plot. See details.
#' @param map A \code{SpatialPolygonsDataFrame} or \code{sf} object. Typically an imported sub-basin vector polygon file. Import of vector polygons
#' requires additional packages, e.g. [sf::st_read]. For interactive Leaflet maps a small/simplified polygon file should be used as larger
#' files can take an excessive amount of time to render.
#' @param map.subid.column Integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param var.name Character string. HYPE variable name to be plotted. Mandatory for automatic color ramp selection of pre-defined
#' HYPE variables (\code{col = "auto"}). Not case-sensitive. See details.
#' @param map.type Map type keyword string. Choose either \code{"default"} for the default static plots or \code{"leaflet"} for interactive Leaflet maps.
#' @param map.adj Numeric, map adjustment in direction where it is smaller than the plot window. A value of \code{0} means left-justified
#' or bottom-justified, \code{0.5} (the default) means centered, and \code{1} means right-justified or top-justified. Only used for default maps.
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}} for default maps.
#' @param legend.pos Legend, scale, and north arrow position, keyword string. For static plots, one of: \code{"left"}, \code{"topleft"}, \code{"topright"},
#' \code{"right"}, \code{"bottomright"}, \code{"bottomleft"}. For interactive Leaflet maps, one of: \code{"topleft"}, \code{"topright"}, \code{"bottomright"}, \code{"bottomleft"}.
#' @param legend.title Character string or mathematical expression. An optional title for the legend. If none is provided here, \code{var.name}
#' is used as legend title string. For select HYPE variables, pretty legend titles are in-built.
#' @param legend.outer Logical. If \code{TRUE}, outer break point values will be plotted in legend.
#' @param legend.inset Numeric, inset distance(s) from the margins as a fraction of the plot region for legend, scale and north arrow.
#' See \code{\link{legend}} and details below.
#' @param legend.signif Integer, number of significant digits to display in legend labels.
#' @param col Colors to use on the map. One of the following: \itemize{
#' \item \code{"auto"} to allow for automatic selection from tailored color ramp palettes and break points based on argument \code{var.name},
#' see details
# \item One of the pre-defined palette functions for HYPE output variables or user-calculated differences between model results:
# \code{"ColNitr"} for nitrogen, \code{"ColPhos"} for phosphorus, \code{"ColPrec"} for precipitation, \code{"ColTemp"} for
# temperatures, \code{"ColQ"} for runoff, \code{"ColDiffTemp"} for temperature differences,  \code{"ColDiffGeneric"} for generic
# differences, see details
#' \item A color ramp palette function, e.g. as returned from a call to \code{\link{colorRampPalette}}. A number of tailored functions are
#' available in \code{HYPEtools}, see \code{\link{CustomColors}}
#' \item A vector of colors. This can be a character vector of R's built-in color names or hexadecimal strings as returned by
#' \code{\link{rgb}}, or an integer vector of current \code{\link{palette}} indices.
#' }
#' @param col.ramp.fun DEPRECATED, for backwards compatibility only.
#' @param col.breaks A numeric vector, specifying break points for discretisation of model result values into classes. Used if a color palette is specified with \code{col} argument.
#' Class boundaries will be interpreted as right-closed, i.e upper boundaries included in class. Lowest class boundary included in lowest class as well.
#' Meaningful results require the lowest and uppermost breaks to bracket all model result values, otherwise there will be
#' unclassified white spots on the map plot. Not mandatory, can optionally
#' be combined with one of the pre-defined palettes, including \code{"auto"} selection. Per default, a generic
#' classification will be applied (see details).
#' @param col.rev Logical, If \code{TRUE}, then color palette will be reversed.
#' @param plot.scale Logical, plot a scale bar below legend (i.e. position defined by legend position). NOTE: works only with
#' projected maps based on meter units, not geographical projections
#' @param plot.arrow Logical, plot a North arrow below legend for default maps (i.e. position defined by legend position).
#' @param par.cex Numeric, character expansion factor. See description of \code{cex} in \code{\link{par}}. Only used for default maps.
#' @param par.mar Plot margins as in \code{\link{par}} argument \code{mar}. Defaults to a nearly margin-less plot.
#' In standard use cases of this function, plot margins do not need to be changed. Only used for default maps.
#' @param add Logical, default \code{FALSE}. If \code{TRUE}, add to existing plot. In that case \code{map.adj} has no effect. Only used for default maps.
#' @param graphics.off Logical, default \code{TRUE}. If \code{TRUE}, HYPEtools will turn off any existing plotting devices before generating a map. Set this to \code{FALSE} when 
#' adding default maps to a plotting device. See \code{\link{graphics.off}}.
#' @param weight Numeric, weight of subbasin boundary lines in Leaflet maps. See [leaflet::addPolygons()].
#' @param opacity Numeric, opacity of subbasin boundary lines in Leaflet maps. See [leaflet::addPolygons()].
#' @param fillOpacity Numeric, opacity of subbasin polygons in Leaflet maps. See [leaflet::addPolygons()].
#' @param na.color Character string of color to use to symbolize subbasin polygons in Leaflet maps which correspond to \code{NA} values.
#' @param plot.searchbar Logical, if \code{TRUE}, then a search bar will be included within Leaflet maps. See [leaflet.extras::addSearchFeatures()].
#' @param plot.label Logical, if \code{TRUE}, then labels will be displayed in Leaflet maps when the cursor hovers over subbasins. See [leaflet::addPolygons()].
#' @param file Save Leaflet map to an image file by specifying the path to the desired output file using this argument. File extension must be specified. See [mapview::mapshot()].
#' You may need to run [webshot::install_phantomjs()] the first time you save a Leaflet map to an image file. See [webshot::install_phantomjs()].
#' @param vwidth Numeric, width of the exported Leaflet map image in pixels. See [mapview::mapshot()].
#' @param vheight Numeric, height of the exported Leaflet map image in pixels. See [mapview::mapshot()].
#' @param html.name Save Leaflet map to an interactive HTML file by specifying the path to the desired output file using this argument. 
#' File extension must be specified. See [htmlwidgets::saveWidget()].
#'
#' @details
#' \code{PlotMapOutput} plots HYPE results from 'map\[variable name\].txt' files, typically imported using \code{\link{ReadMapOutput}}.
#' \code{x} arguments \strong{must} contain the variable of interest in the second column. For map results with multiple columns, i.e.
#' several time periods, pass index selections to \code{x}, e.g. \code{mymapresult[, c(1, 3)]}.
#'
#' \code{PlotMapOutput} can return static plots or interactive Leaflet maps depending on value provided for the argument \code{map.type}.
#'
#' Mapped variables are visualised using color-coded data intervals. \code{HYPEtools} provides a number of color ramps functions for HYPE variables,
#' see \code{\link{CustomColors}}. These are either single-color ramps with less saturated colors for smaller values
#' and more saturated values for higher values, suitable for e.g. concentration or volume ranges, or multi-color ramps suitable for calculated
#' differences, e.g. between two model runs.
#'
#' Break points between color classes of in-built or user-provided color ramp palettes can optionally be provided in argument
#' \code{col.breaks}. This is particularly useful when specific pretty class boundaries are needed, e.g. for publication figures. Per default,
#' break points for internal single color ramps and user-provided ramps are calculated based on 10\% percentiles of HYPE results given in
#' \code{x}. Default break points for internal color ramp \code{ColDiffGeneric} are based on an equal distance classification of log-scaled
#' \code{x} ranges, centered around zero. For internal color ramp \code{ColDiffTemp}, they are breaks in an interval from -7.5 to 7.5 K.
#'
#' For select common HYPE variables, given in argument \code{var.name}, an automatic color ramp selection including pretty breaks and legend titles
#' is built into \code{PlotMapOutput}. These are 'CCTN', 'CCTP', 'COUT', and 'TEMP'. Automatic selection is activated by chosing keyword
#' \code{"auto"} in \code{col}. All other HYPE variables will be plotted using a generic color ramp palette and generic break points with
#' \code{"auto"} color selection.
#'
#' \code{PlotMapOutput} per default works with a margin-less figure and positions map and legend items close to the plot boundaries.
#' In order to move map and legend closer to each other, change the plot device width.
#'
#' Legends are positioned by keyword through argument \code{legend.pos}, defaulting to the bottom right side of the map. For static plots, \code{legend.pos} and
#' \code{map.adj} should be chosen so that legend and map do not overlap, and the legend position can be fine-tuned using
#' argument \code{legend.inset}. This is particularly useful for legend titles with more than one line. For details on inset
#' specification see \code{inset} in \code{\link{legend}}.
#'
#' @return
#' For static plots \code{PlotMapOutput} returns a plot to the currently active plot device, and invisibly an object of class \code{SpatialPolygonsDataFrame}
#' as provided in argument \code{map}, with plotted values and color codes added as columns in the data slot. For interactive Leaflet maps, \code{PlotMapOutput} returns
#' an object of class \code{leaflet}.
#'
#' @seealso
#' \code{\link{ReadMapOutput}} for HYPE result import; \code{\link{PlotMapPoints}} for plotting HYPE results at points, e.g. sub-basin outlets.
#'
#' @examples
#' \donttest{
#' # Import plot data and subbasin polygons
#' require(sf)
#' te1 <- ReadMapOutput(filename = system.file("demo_model",
#' "results", "mapCRUN.txt", package = "HYPEtools"), dt.format = NULL)
#' te2 <- st_read(dsn = system.file("demo_model",
#' "gis", "Nytorp_map.gpkg", package = "HYPEtools"))
#' # plot runoff map
#' PlotMapOutput(x = te1, map = te2, map.subid.column = 25,
#' var.name = "CRUN", col = ColQ)
#' }
#' 
#'
#' @importFrom dplyr right_join %>% mutate filter across
#' @importFrom grDevices dev.list
#' @importFrom graphics par frame legend strwidth text
#' @importFrom stats quantile setNames
#' @importFrom rlang .data
#' @export


PlotMapOutput <- function(x, map, map.subid.column = 1, var.name = "", map.type = "default", map.adj = 0, plot.legend = TRUE,
                          legend.pos = "bottomright", legend.title = NULL, legend.outer = FALSE, legend.inset = c(0, 0), legend.signif = 2,
                          col = "auto", col.ramp.fun, col.breaks = NULL, col.rev = FALSE, plot.scale = TRUE, plot.arrow = TRUE,
                          par.cex = 1, par.mar = rep(0, 4) + .1, add = FALSE, graphics.off = TRUE,
                          weight = 0.15, opacity = 0.75, fillOpacity = 0.5, na.color = "#808080",
                          plot.searchbar = FALSE, plot.label = FALSE, file = "", vwidth = 1424,
                          vheight = 1000, html.name = "") {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit - suppress warnings because par not updated when using leaflet map so you get warning on exit that you call par with no plot

  # Check/Load Dependencies for interactive mapping features - do this here so that these packages are not required for the base HYPEtools installation
  if (map.type == "leaflet" & !all(
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("leaflet", quietly = TRUE),
    requireNamespace("leaflet.extras", quietly = TRUE),
    requireNamespace("mapview", quietly = TRUE),
    requireNamespace("htmlwidgets", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop("To use the interactive mapping features, please ensure that the following packages are installed: sf, leaflet, leaflet.extras, mapview, htmlwidgets", call.=FALSE)
    
    # Perform function
  } else {
    # Clear plotting devices if graphics.off argument is true - prevents R fatal errors caused if PlotMapPoints tries to add default plot to existing Leaflet map
    if (graphics.off == TRUE & !is.null(dev.list())) graphics.off()
    
    # input argument checks
    stopifnot(is.data.frame(x), dim(x)[2] == 2, is.null(col.breaks) || is.numeric(col.breaks), ("sf" %in% class(map) | "SpatialPolygonsDataFrame" %in% class(map)))
    if (map.type == "default") {
      if ("sf" %in% class(map)) {
        map <- sf::as_Spatial(map)
      }
    } else if (map.type == "leaflet") {
      if ("SpatialPolygonsDataFrame" %in% class(map)) {
        map <- sf::st_as_sf(map)
      }
    }
    
    stopifnot(map.adj %in% c(0, .5, 1))
    if (map.type == "default") {
      stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
    } else if (map.type == "leaflet") {
      stopifnot(legend.pos %in% c("bottomright", "topright", "topleft", "bottomleft"))
    }
    if (length(col.breaks) == 1) {
      col.breaks <- range(x[, 2], na.rm = TRUE)
      warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
    }
    if (!is.null(col.breaks) && (min(col.breaks, na.rm = TRUE) > min(x[, 2], na.rm = TRUE) || max(col.breaks, na.rm = TRUE) < max(x[, 2], na.rm = TRUE))) {
      warning("Range of user-provided argument 'col.breaks' does not cover range of 'x[, 2].
            Areas outside range will be excluded from plot.")
    }
    
    # check if deprecated argument col.ramp.fun was used
    if (!missing(col.ramp.fun)) {
      warning("Deprecated argument 'col.ramp.fun' used. Please use 'col' instead.")
    }
    
    # add y to legend inset if not provided by user
    if (length(legend.inset) == 1) {
      legend.inset[2] <- 0
    }
    
    # sort col.breaks to make sure breaks are in increasing order
    if (!is.null(col.breaks)) {
      col.breaks <- sort(col.breaks, decreasing = FALSE)
    }
    
    # data preparation and conditional assignment of color ramp functions and break point vectors
    # to internal variables crfun and cbrks
    
    if (is.function(col)) {
      # Case 1: a color ramp palette function is supplied
      crfun <- col
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
      } else {
        # color breaks: special defaults for some of the inbuilt color ramp functions
        if (identical(col, ColDiffTemp)) {
          # temperature differences
          cbrks <- c(ifelse(min(x[, 2] ,na.rm = TRUE) < 7.5, min(x[, 2], na.rm = TRUE) - 1, 30), -7.5, -5, -2.5, -1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[, 2], na.rm = TRUE) > 7.5, max(x[, 2], na.rm = TRUE) + 1, 30))
        } else if (identical(col, ColDiffGeneric)) {
          # create a break point sequence which is centered around zero, with class widths based on equal intervals of the log-scaled
          # variable distribution
          cbrks <- c(rev(exp(seq(0, log(max(abs(range(x[, 2]))) + 1), length.out = 5)) * -1), exp(seq(0, log(max(abs(range(x[, 2]))) + 1), length.out = 5)))
        } else {
          # generic, quantile-based breaks for all other functions
          cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = TRUE)
        }
      }
    } else if (col[1] == "auto") {
      # Case 2: limited set of pre-defined color ramps and break point vectors for select HYPE variables, with a generic "catch the rest" treatment
      # for undefined variables
      if (toupper(var.name) == "CCTN") {
        crfun <- ColNitr
        cbrks <- c(0, 10, 50, 100, 250, 500, 1000, 2500, 5000, ifelse(max(x[, 2], na.rm = TRUE) > 5000, max(x[, 2], na.rm = TRUE) + 1, 10000))
        if (is.null(legend.title)) {
          if (map.type == "default") {
            legend.title <- expression(paste("Total N (", mu, "g l"^"-1", ")"))
          } else if (map.type == "leaflet") {
            legend.title <- paste("Total N (ug/L)")
          }
        }
      } else if (toupper(var.name) == "CCTP") {
        crfun <- ColPhos
        cbrks <- c(0, 5, 10, 25, 50, 100, 150, 200, 250, ifelse(max(x[, 2], na.rm = TRUE) > 250, max(x[, 2], na.rm = TRUE) + 1, 1000))
        if (is.null(legend.title)) {
          if (map.type == "default") {
            legend.title <- expression(paste("Total P (", mu, "g l"^"-1", ")"))
          } else if (map.type == "leaflet") {
            legend.title <- paste("Total P (ug/L)")
          }
        }
      } else if (toupper(var.name) == "COUT") {
        crfun <- ColQ
        cbrks <- c(0, .5, 1, 5, 10, 50, 100, 500, ifelse(max(x[, 2], na.rm = TRUE) > 500, max(x[, 2], na.rm = TRUE) + 1, 2000))
        if (is.null(legend.title)) {
          if (map.type == "default") {
            legend.title <- expression(paste("Q (m"^3, "s"^"-1", ")"))
          } else if (map.type == "leaflet") {
            legend.title <- paste("Q (m3/s)")
          }
        }
      } else if (toupper(var.name) == "TEMP") {
        crfun <- ColTemp
        cbrks <- c(ifelse(min(x[, 2]) < -7.5, min(x[, 2]) - 1, -30), -7.5, -5, -2.5, -1, 0, 1, 2.5, 5, 7.5, ifelse(max(x[, 2], na.rm = TRUE) > 7.5, max(x[, 2], na.rm = TRUE) + 1, 30))
        if (is.null(legend.title)) {
          if (map.type == "default") {
            legend.title <- expression(paste("Air Temp. (" * degree, "C)"))
          } else if (map.type == "leaflet") {
            legend.title <- paste("Air Temp. (C)")
          }
        }
      } else {
        crfun <- ColDiffGeneric
        cbrks <- quantile(x[, 2], probs = seq(0, 1, .1), na.rm = TRUE)
      }
    } else if (is.vector(col)) {
      # Case 3: a vector of colors
      crfun <- NULL
      if (!is.null(col.breaks)) {
        cbrks <- col.breaks
        if (map.type == "default" & length(col) != (length(cbrks) - 1)) {
          stop("For default maps, if colors are specified as vector in 'col', the number of colors in 'col' must be one less than the number of breakpoints in 'col.breaks'.")
        } else if (map.type == "leaflet" & length(col) != (length(cbrks))) {
          stop("For leaflet maps, if colors are specified as vector in 'col', the number of colors in 'col' must be equal to the number of breakpoints in 'col.breaks'.")
        }
      } else {
        cbrks <- quantile(x[, 2], probs = seq(0, 1, length.out = length(col) + 1), na.rm = TRUE)
      }
    } else {
      # Error treatment for all other user input
      stop("Invalid 'col' argument.")
    }
    
    # in variables with large numbers of "0" values, the lower 10%-percentiles can be repeatedly "0", which leads to an error with cut,
    # so cbrks is shortened to unique values (this affects only the automatic quantile-based breaks)
    # if just one value remains (or was requested by user), replace crbks by minmax-based range (this also resolves unexpected behaviour
    # with single-value cbrks in 'cut' below).
    if (is.null(crfun) && length(unique(cbrks)) < length(cbrks)) {
      # warn, if user defined colors are discarded because of removal of quantile-based classes
      warning("User-defined colors in 'col' truncated because of non-unique values in quantile-based color breaks. Provide breaks in
            'col.breaks' to use all colors.")
    }
    cbrks <- unique(cbrks)
    if (length(cbrks) == 1) {
      cbrks <- range(cbrks) + c(-1, 1)
    }
    if (is.null(crfun)) {
      # truncate user-defined colors
      col <- col[1:(length(cbrks) - 1)]
    }
    # discretise the modeled values in x into classed groups, add to x as new column (of type factor)
    x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = TRUE)
    
    # For leaflet mapping add NA Factor Level if any MapOutput data is NA
    if (map.type == "leaflet" & any(is.na(x[[2]]))) {
      x[, 3] <- addNA(x[, 3])
    }
    
    # replace the factor levels with color codes using the color ramp function assigned above or user-defined colors
    if (is.null(crfun)) {
      if (map.type == "leaflet" & any(is.na(x[[2]]))) {
        if (col.rev == FALSE) {
          levels(x[, 3]) <- c(col, na.color) # Add extra color for NA in leaflet maps
        } else if (col.rev == TRUE) {
          levels(x[, 3]) <- c(rev(col), na.color) # Add extra color for NA in leaflet maps
        }
      } else {
        if (col.rev == FALSE) {
          levels(x[, 3]) <- col
        } else if (col.rev == TRUE) {
          levels(x[, 3]) <- rev(col) # Reverse color palette
        }
      }
    } else {
      if (map.type == "leaflet" & any(is.na(x[[2]]))) {
        if (col.rev == FALSE) {
          levels(x[, 3]) <- crfun(length(cbrks)) # Add extra legend break for NA in leaflet maps
        } else if (col.rev == TRUE) {
          rev.col <- rev(crfun(length(cbrks))) # Add extra legend break for NA in leaflet maps, reverse color palette
          levels(x[, 3]) <- rev.col[c(2:length(rev.col), 1)] # Reorder colors so that NA color is still last
        }
      } else {
        if (col.rev == FALSE) {
          levels(x[, 3]) <- crfun(length(cbrks) - 1)
        } else if (col.rev == TRUE) {
          levels(x[, 3]) <- rev(crfun(length(cbrks) - 1)) # Reverse color palette
        }
      }
    }
    
    # convert to character to make it conform to plotting requirements below
    x[, 3] <- as.character(x[, 3])
    # give it a name
    names(x)[3] <- "color"
    
    if (map.type == "default") {
      # add x to subid map table (in data slot, indicated by @), merge by SUBID
      map@data <- data.frame(map@data, x[match(map@data[, map.subid.column], x[, 1]), ])
    } else if (map.type == "leaflet") {
      message(paste0('Joining "', colnames(map)[map.subid.column], '" from GIS Data (map) To "', colnames(x)[1], '" from MapOutput (x)'))
      
      # Check for duplicate SUBIDS
      if(any(duplicated(map[, map.subid.column]%>%sf::st_drop_geometry()))){warning("Duplicate SUBIDS exist in GIS Data (map)")}
      if(any(duplicated(x[,1]))){warning("Duplicate SUBIDS exist in MapOutput (x)")}
      
      x <- right_join(map[, map.subid.column]%>%mutate(across(1,~as.character(.x))), x%>%mutate(across(1,~as.character(.x))), by = setNames(nm = colnames(map)[map.subid.column], colnames(x)[1])) # Join GIS Data with MapOutput Data in a manner in which column names don't have to be identical (e.g. "SUBID" and "subid" is okay, character and integer is okay)
    }
    
    # update legend title if none was provided by user or "auto" selection
    if (is.null(legend.title)) {
      legend.title <- toupper(var.name)
    }
    
    if (map.type == "default") {
      # par settings: lend set to square line endings because the legend below works with very thick lines
      # instead of boxes (a box size limitation work-around); xpd set to allow for plotting a legend on the margins
      if (!add) {
        # x11(width = 15, height = 5)
        # par(mfcol = c(1, 3))
        # plot(0,type='n',axes=FALSE,ann=FALSE)
        par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = TRUE, cex = par.cex)
        # plot.window(xlim = 0:1, ylim = 0:1)
        frame()
      } else {
        par(lend = 1, xpd = TRUE, cex = par.cex)
      }
      
      
      ## the positioning of all plot elements works with three scales for the device's plot region:
      ## inches, fraction, and map coordinates
      
      # plot width (inches)
      p.in.wd <- par("pin")[1]
    }
    
    # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates)
    # legend colors
    if (is.null(crfun)) {
      if (map.type == "leaflet" & any(is.na(x[[2]]))) {
        if (col.rev == FALSE) {
          lcol <- c(col, na.color) # Add extra legend color for NA for leaflet maps
        } else if (col.rev == TRUE) {
          lcol <- c(rev(col), na.color) # Add extra legend color for NA for leaflet maps and reverse color palette
        }
      } else {
        if (col.rev == FALSE) {
          lcol <- col
        } else if (col.rev == TRUE) {
          lcol <- rev(col) # Reverse color palette
        }
      }
    } else {
      if (map.type == "leaflet" & any(is.na(x[[2]]))) {
        if (col.rev == FALSE) {
          lcol <- crfun(length(cbrks)) # Add extra legend color for NA for leaflet maps
        } else if (col.rev == TRUE) {
          rev.col <- rev(crfun(length(cbrks))) # Add extra legend color for NA for leaflet maps, reverse color palette
          lcol <- rev.col[c(2:length(rev.col), 1)] # Reorder colors so that NA color is still last
        }
      } else {
        if (col.rev == FALSE) {
          lcol <- crfun(length(cbrks) - 1)
        } else if (col.rev == TRUE) {
          lcol <- rev(crfun(length(cbrks) - 1)) # Reverse color palette
        }
      }
    }
    
    # Generate default plot
    if (map.type == "default") {
      leg.fr.pos <- legend(legend.pos,
                           legend = rep(NA, length(cbrks) - 1),
                           col = lcol, lty = 1, lwd = 14, bty = "n", title = legend.title, plot = FALSE
      )
      
      # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates)
      leg.fr.wd <- leg.fr.pos$rect$w
      # legend box element height (fraction), with workaround for single-class maps
      if (length(leg.fr.pos$text$y) == 1) {
        te <- legend(legend.pos,
                     legend = rep(NA, length(cbrks)),
                     col = ColQ(length(cbrks)), lty = 1, lwd = 14, bty = "n", title = legend.title, plot = FALSE
        )
        legbx.fr.ht <- diff(c(te$text$y[length(cbrks)], te$text$y[length(cbrks) - 1]))
      } else {
        legbx.fr.ht <- diff(c(leg.fr.pos$text$y[length(cbrks) - 1], leg.fr.pos$text$y[length(cbrks) - 2]))
      }
      
      ## prepare legend annotation
      
      # formatted annotation text (to be placed between legend boxes which is not possible with legend() directly)
      ann.txt <- signif(cbrks, digits = legend.signif)
      # conditional: remove outer break points
      if (!legend.outer) {
        ann.txt[c(1, length(ann.txt))] <- ""
      }
      # annotation width (inches)
      ann.in.wd <- max(strwidth(ann.txt, "inches"))
      # legend inset required to accomodate text annotation, and scalebar (always below legend)
      leg.inset <- c(ann.in.wd / p.in.wd, if (legend.pos %in% c("bottomright", "bottomleft")) {
        0.1
      } else {
        0
      })
      
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
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
        } else if (legend.pos == "right") {
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks)))
        } else {
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
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
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks))) + legend.inset[2] * f.inset.y
        } else if (legend.pos == "left") {
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks)))
        } else {
          ann.fr.y <- rev(seq(from = leg.fr.pos$text$y[length(cbrks) - 1] - legbx.fr.ht / 2, by = legbx.fr.ht, length.out = length(cbrks))) - legend.inset[2] * f.inset.y
        }
      }
      
      ## calculate coordinates for map positioning
      
      # map coordinates,unprojected maps need a workaround with dummy map to calculate map side ratio
      if(sf::st_is_longlat(map) == FALSE){
        bbx <- matrix(sf::st_bbox(map),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
        # map side ratio (h/w)
        msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
        # plot area side ratio (h/w)
        psr <- par("pin")[2] / par("pin")[1]
      } else {
        bbx <- matrix(sf::st_bbox(map),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
        # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
        par(new = TRUE)
        # plot(SpatialPoints(coordinates(map), proj4string = CRS(proj4string(map))), col = NULL, xlim = bbx[1, ], ylim = bbx[2, ])
        plot(sf::st_geometry(sf::st_as_sf(map)), xlim = bbx[1, ], ylim = bbx[2, ])
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
      
      # define plot limits, depending on (a) map and plot ratios (plot will be centered if left to automatic) and (b) user choice
      if (msr > psr) {
        # map is smaller than plot window in x direction, map can be moved left or right
        if (map.adj == 0) {
          pylim <- as.numeric(bbx[2, ])
          pxlim <- c(bbx[1, 1], bbx[1, 1] + diff(pylim) / psr)
        } else if (map.adj == .5) {
          pylim <- as.numeric(bbx[2, ])
          pxlim <- c(mean(as.numeric(bbx[1, ])) - diff(pylim) / psr / 2, mean(as.numeric(bbx[1, ])) + diff(pylim) / psr / 2)
        } else {
          pylim <- as.numeric(bbx[2, ])
          pxlim <- c(bbx[1, 2] - diff(pylim) / psr, bbx[1, 2])
        }
      } else {
        # map is smaller than plot window in y direction, map can be moved up or down
        if (map.adj == 0) {
          pxlim <- as.numeric(bbx[1, ])
          pylim <- c(bbx[2, 1], bbx[2, 1] + diff(pxlim) * psr)
        } else if (map.adj == .5) {
          pxlim <- as.numeric(bbx[1, ])
          pylim <- c(mean(as.numeric(bbx[2, ])) - diff(pxlim) * psr / 2, mean(as.numeric(bbx[2, ])) + diff(pxlim) * psr / 2)
        } else {
          pxlim <- as.numeric(bbx[1, ])
          pylim <- c(bbx[2, 2] - diff(pxlim) * psr, bbx[2, 2])
        }
      }
      
      ## plot the map and add legend using the positioning information derived above
      
      # map, plot in current frame if not added because a new frame was already created above for calculating all the coordinates
      if (!add) {
        par(new = TRUE)
      }
      plot(sf::st_geometry(sf::st_as_sf(map)), col = map$color, border = NA, ylim = pylim, xlim = pxlim, add = add)
      # legend
      if (plot.legend) {
        legend(legend.pos,
               legend = rep(NA, length(cbrks) - 1), inset = legend.inset,
               col = lcol, lty = 1, lwd = 14, bty = "n", title = legend.title
        )
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
          lx <- par("usr")[2] - signif(diff(par("usr")[1:2]) / 4, 0) - legend.inset[1] * diff(par("usr")[1:2])
        } else {
          lx <- par("usr")[1] + (legend.inset[1] + 0.02) * diff(par("usr")[1:2])
        }
        
        # y position conditional legend placement position (leg.fr.pos here is already in map coordinates)
        if (legend.pos %in% c("bottomright", "bottomleft")) {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2] * f.inset.y / 2)
        } else if (legend.pos %in% c("right", "left")) {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + (legend.inset[2] / 2 - .1) * f.inset.y)
        } else {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - (legend.inset[2] / 2 - .1) * f.inset.y)
        }
      } else {
        
        # x position conditional on legend placement side
        if (legend.pos %in% c("bottomright", "right", "topright")) {
          lx <- pxlim[2] - signif(diff(bbx[1, ]) / 4, 0) - legend.inset[1] * diff(pxlim)
        } else {
          lx <- pxlim[1] + (legend.inset[1] + 0.02) * diff(pxlim)
        }
        
        # y position conditional legend placement position
        if (legend.pos %in% c("bottomright", "bottomleft")) {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2] / 2) * diff(pylim) + pylim[1]
        } else if (legend.pos %in% c("right", "left")) {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h + legend.inset[2] / 2 - .1) * diff(pylim) + pylim[1]
        } else {
          ly <- (leg.fr.pos$rect$top - leg.fr.pos$rect$h - legend.inset[2] / 2 - .1) * diff(pylim) + pylim[1]
        }
      }
      
      if (plot.scale) {
        if (sf::st_is_longlat(map)) {
          warning("Scale bar meaningless with un-projected maps. Set 'plot.scale = FALSE' to remove it.")
        }
        if (!add) {
          ldistance <- signif(diff(bbx[1, ]) / 4, 0)
        } else {
          ldistance <- signif(diff(par("usr")[1:2]) / 4, 0)
        }
        .Scalebar(
          x = lx,
          y = ly,
          distance = ldistance,
          scale = 0.001, t.cex = 0.8
        )
      }
      
      if (plot.arrow) {
        if (add) {
          nlen <- diff(par("usr")[1:2]) / 70
          # north arrow x position conditional on side where legend is plotted
          if (legend.pos %in% c("bottomright", "right", "topright")) {
            nx <- lx - 0.02 * diff(par("usr")[1:2])
          } else {
            nx <- lx + signif(diff(par("usr")[1:2]) / 4, 0) + 0.055 * diff(par("usr")[1:2])
          }
        } else {
          nlen <- diff(bbx[1, ]) / 70
          # north arrow x position conditional on side where legend is plotted
          if (legend.pos %in% c("bottomright", "right", "topright")) {
            nx <- lx - 0.02 * diff(pxlim)
          } else {
            nx <- lx + signif(diff(bbx[1, ]) / 4, 0) + 0.055 * diff(pxlim)
          }
        }
        
        .NorthArrow(
          xb = nx,
          yb = ly,
          len = nlen, cex.lab = .8
        )
      }
      
      # invisible unless assigned: return map with added data and color codes
      invisible(map)
    } else if (map.type == "leaflet") {
      
      # Reproject if not a lat/long CRS
      if(sf::st_is_longlat(x)==FALSE){
        x <- x %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
      }
      
      # Remove any empty geometries (these prevent labels from working)
      x <- x %>%
        dplyr::filter(!sf::st_is_empty(.))
      
      # Create legend labels, change NA color to selected NA color
      if (any(is.na(x[[2]]))) {
        l.label <- c(unlist(lapply(1:(length(cbrks) - 1), function(X) {
          paste(signif(cbrks[X], legend.signif), "to", signif(cbrks[X + 1], legend.signif))
        })), "NA")
        lcol[which(lcol %in% unique(x$color[which(is.na(x[[2]]))]))] <- na.color
        x[which(is.na(x[[2]])), "color"] <- na.color
      } else {
        l.label <- unlist(lapply(1:(length(cbrks) - 1), function(X) {
          paste(signif(cbrks[X], legend.signif), "-", signif(cbrks[X + 1], legend.signif))
        }))
      }
      
      # Create Leaflet Map
      message("Generating Map")
      leafmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addTiles() %>%
        leaflet::addLayersControl(
          baseGroups = c("Map", "Street", "Topo", "Satellite"),
          overlayGroups = c("Subbasins"),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoIndex = TRUE)
        ) %>%
        leaflet.extras::addResetMapButton()
      
      if (plot.label == TRUE) { # Create polygons with labels
        
        # Create labels
        x <- x %>%
          mutate(label = paste0("SUBID: ", .[[1]], " --- ", ifelse(var.name == "", "Value", var.name), ": ", .[[2]]))
        
        leafmap <- leafmap %>%
          leaflet::addPolygons(
            group = "Subbasins",
            data = x,
            color = "black",
            weight = weight,
            opacity = opacity,
            fillColor = ~color,
            fillOpacity = fillOpacity,
            label = ~label
          )
      }
      else { # Create polygons without labels
        leafmap <- leafmap %>%
          leaflet::addPolygons(
            group = "Subbasins",
            data = x,
            color = "black",
            weight = weight,
            opacity = opacity,
            fillColor = x$color,
            fillOpacity = fillOpacity
          )
      }
      
      # Add searchbar to map
      if (plot.searchbar == TRUE) {
        leafmap <- leafmap %>%
          leaflet.extras::addSearchFeatures(
            targetGroups = "Subbasins",
            options = leaflet.extras::searchFeaturesOptions(zoom = 10, hideMarkerOnCollapse = TRUE)
          )
      }
      
      # Add scalebar to map
      if (plot.scale == TRUE) {
        leafmap <- leafmap %>%
          leaflet::addScaleBar(position = "bottomright")
      }
      
      # Add legend to map
      if (plot.legend == TRUE) {
        leafmap <- leafmap %>%
          leaflet::addLegend(
            group = "Subbasins",
            position = legend.pos,
            title = ifelse(legend.title == "", "Legend", legend.title),
            colors = lcol,
            labels = l.label,
            values = x[[2]],
            opacity = 1
          )
      }
      
      # Add various basemaps
      leafmap <- leafmap %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "Map") %>%
        leaflet::addTiles(group = "Street") %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite")
      
      # Save Image
      if (!file == "") {
        message("Saving Image")
        mapview::mapshot(leafmap, file = file, vwidth = vwidth, vheight = vheight, remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"), selfcontained = FALSE)
      }
      
      # Save HTML
      if (!html.name == "") {
        message("Saving HTML")
        temp <- file.path(tempdir(), basename(html.name))
        htmlwidgets::saveWidget(leafmap, file = temp, title = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(html.name)), selfcontained = TRUE) # Save HTML file to temp directory so selfcontained=T works
        file.rename(temp, html.name) # Rename/Move HTML file to desired file
      }
      
      return(leafmap)
    }
  }
  
}

