#'
#' Plot function for mapped point information
#'
#' Plot mapped point information, e.g. model performances at observation sites.
#'
#' @param x Information to plot, typically model performances from imported HYPE 'subassX.txt' files. Data frame object
#' with two columns, first column containing SUBIDs and second column containing model results to plot. See details.
#' @param sites A \code{SpatialPointsDataFrame} or \code{sf} object. Typically an imported outlet point vector point file. Import of vector points
#' requires additional packages, e.g. [sf::st_read()].
#' @param sites.subid.column Integer, column index in the \code{sites} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param bg A \code{SpatialPolygonsDataFrame} or \code{sf} object to plot in the background. Typically an imported sub-basin vector polygon file.
#' For default maps with several background layers, use \code{add = TRUE} and plot background layer(s) first.
#' @param bg.label.column Integer, column index in the \code{bg} 'data' \code{\link{slot}} holding labels (e.g. SUBIDs) to use for plotting.
#' @param map.type Map type keyword string. Choose either \code{"default"} for the default static plots or \code{"leaflet"} for interactive Leaflet maps.
#' @param map.adj Numeric, map adjustment in direction where it is smaller than the plot window. A value of \code{0} means left-justified
#' or bottom-justified, \code{0.5} (the default) means centered, and \code{1} means right-justified or top-justified. Only used for default maps.
#' @param plot.legend Logical, plot a legend along with the map. Uses function \code{\link{legend}} for default maps.
#' @param legend.pos Legend, scale, and north arrow position, keyword string. For static plots, one of: \code{"left"}, \code{"topleft"}, \code{"topright"},
#' \code{"right"}, \code{"bottomright"}, \code{"bottomleft"}. For interactive Leaflet maps, one of: \code{"topleft"}, \code{"topright"}, \code{"bottomright"}, \code{"bottomleft"}.
#' @param legend.title Character string or mathematical expression. An optional title for the legend. If none is provided here, the name of the second column in \code{x}
#' is used as legend title string.
#' @param legend.outer Logical. If \code{TRUE}, outer break point values will be plotted in legend. Only used for default maps.
#' @param legend.inset Numeric, inset distance(s) from the margins as a fraction of the plot region for legend, scale and north arrow.
#' See \code{\link{legend}} and details below. Only used for default maps.
#' @param legend.signif Integer, number of significant digits to display in legend labels.
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
#' @param plot.arrow Logical, plot a North arrow below legend for default maps (i.e. position defined by legend position).
#' @param pt.cex Numeric, plot point size expansion factor, works on top of \code{par.cex}.
#' @param par.cex Numeric, character expansion factor. See description of \code{cex} in \code{\link{par}}. Only used for default maps.
#' @param par.mar Plot margins as in \code{\link{par}} argument \code{mar}. Defaults to a nearly margin-less plot.
#' In standard use cases of this function, plot margins do not need to be changed. Only used for default maps.
#' @param pch,lwd Integer, plotting symbol and line width. See \code{\link{points}}. Only used for default maps.
#' @param add Logical, default \code{FALSE}. If \code{TRUE}, add to existing plot. In that case \code{map.adj} has no effect. Only used for default maps.
#' @param graphics.off Logical, default \code{TRUE}. If \code{TRUE}, HYPEtools will turn off any existing plotting devices before generating a map. Set this to \code{FALSE} when 
#' adding default maps to a plotting device. See \code{\link{graphics.off}}.
#' @param radius Numeric, radius of markers in Leaflet maps. See [leaflet::addCircleMarkers()].
#' @param weight Numeric, weight of marker outlines in Leaflet maps. See [leaflet::addCircleMarkers()].
#' @param opacity Numeric, opacity of marker outlines in Leaflet maps. See [leaflet::addCircleMarkers()].
#' @param fillOpacity Numeric, opacity of markers in Leaflet maps. See [leaflet::addCircleMarkers()].
#' @param na.color Character string of color to use to symbolize markers in Leaflet maps which correspond to \code{NA} values.
#' @param bg.weight Numeric, weight of \code{bg} subbasin outlines in Leaflet maps. See [leaflet::addPolygons()].
#' @param bg.opacity Numeric, opacity of \code{bg} subbasin outlines in Leaflet maps. See [leaflet::addPolygons()].
#' @param bg.fillColor Character string of color to use to symbolize \code{bg} subbasin polygons in Leaflet maps. See [leaflet::addPolygons()].
#' @param bg.fillOpacity Numeric, opacity of \code{bg} subbasin polygons in Leaflet maps. See [leaflet::addPolygons()].
#' @param plot.label Logical, if \code{TRUE}, then labels will be displayed in Leaflet maps when the cursor hovers over markers. See [leaflet::addCircleMarkers()].
#' @param noHide Logical, set to \code{TRUE} to always display marker labels in Leaflet maps. See [leaflet::labelOptions()].
#' @param textOnly Logical, set to \code{TRUE} to hide marker label background in Leaflet maps. See [leaflet::labelOptions()].
#' @param font.size Numeric, font size (px) for marker labels in Leaflet maps.
#' @param plot.bg.label String, if \code{hover}, then labels will be displayed in Leaflet maps for \code{bg} when the cursor hovers over polygons. If \code{static}, then static
#' labels for \code{bg} will be displayed in Leaflet maps.
#' @param file Save Leaflet map to an image file by specifying the path to the desired output file using this argument. File extension must be specified. See [mapview::mapshot()].
#' You may need to run \code{webshot::install_phantomjs()} the first time you save a Leaflet map to an image file. See [webshot::install_phantomjs()].
#' @param vwidth Numeric, width of the exported Leaflet map image in pixels. See [webshot::webshot()].
#' @param vheight Numeric, height of the exported Leaflet map image in pixels. See [webshot::webshot()].
#' @param html.name Save Leaflet map to an interactive HTML file by specifying the path to the desired output file using this argument. File extension must be specified. 
#' See [htmlwidgets::saveWidget()].
#'
#' @details
#' \code{PlotMapPoints} can be used to print point information on a mapped surface. The primary target are model performance
#' measures as written to
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:subassx.txt}{HYPE 'subassX.txt' files}, but
#' color scale and break point arguments are flexible enough to also be used with e.g. HYPE output variables or other data.
#'
#' \code{PlotMapOutput} can return static plots or interactive Leaflet maps depending on value provided for the argument \code{map.type}.
#' By default, \code{PlotMapPoints} generates a margin-less figure and positions map and legend items close to the plot boundaries.
#' In order to move map and legend closer to each other, change the plot device width.
#'
#' Legends are positioned by keyword through argument \code{legend.pos}, defaulting to the bottom right side of the map. For default maps, \code{legend.pos} and
#' \code{map.adj} should be chosen so that legend and map do not overlap, and the legend position can be fine-tuned using
#' argument \code{legend.inset}. This is particularly useful for legend titles with more than one line. For details on inset
#' specification for the default maps, see \code{inset} in \code{\link{legend}}.
#'
#' @return
#' For default static maps, \code{PlotMapPoints} returns a plot to the currently active plot device, and invisibly an object of class
#' \code{SpatialPointsDataFrame} as provided in argument \code{sites}, with plotted values and color codes added as columns
#' in the data slot. For interactive Leaflet maps, \code{PlotMapOutput} returns
#' an object of class \code{leaflet}.
#'
#' @seealso
#' \code{\link{ReadSubass}} for HYPE result import; \code{\link{ReadMapOutput}} for a similar plot function

#' @examples
#' \donttest{
#' # Import plot data and subbasin polygons
#' require(sf)
#' te1 <- ReadSubass(filename = system.file("demo_model",
#' "results", "subass1.txt", package = "HYPEtools"))
#' te2 <- st_read(dsn = system.file("demo_model",
#' "gis", "Nytorp_station.gpkg", package = "HYPEtools"))
#' te2$SUBID <- 3587 # add station SUBID to point
#' te3 <- st_read(dsn = system.file("demo_model",
#' "gis", "Nytorp_map.gpkg", package = "HYPEtools"))
#' # plot NSE performance for discharge
#' PlotMapPoints(x = te1[, 1:2], sites = te2, sites.subid.column = 3, bg = te3)
#' }
#' 
#' @importFrom dplyr right_join %>% mutate filter across
#' @importFrom grDevices dev.list colorRampPalette
#' @importFrom graphics par frame legend strwidth text plot.new
#' @importFrom stats setNames
#' @importFrom rlang .data
#' @export
#' 


PlotMapPoints <- function(x, sites, sites.subid.column = 1, bg = NULL, bg.label.column = 1, map.type = "default", map.adj = 0, plot.legend = TRUE,
                          legend.pos = "bottomright", legend.title = NULL, legend.outer = FALSE, legend.inset = c(0, 0), legend.signif = 2,
                          col = NULL, col.breaks = NULL, plot.scale = TRUE, plot.arrow = TRUE, pt.cex = 1,
                          par.cex = 1, par.mar = rep(0, 4) + .1, pch = 21, lwd = .8, add = FALSE, graphics.off = TRUE,
                          radius = 5, weight = 0.15, opacity = 0.75, fillOpacity = 0.5, na.color = "#808080",
                          bg.weight = 0.15, bg.opacity = 0.75, bg.fillColor = "#e5e5e5", bg.fillOpacity = 0.75,
                          # plot.searchbar = FALSE, # leaflet.extras searchbar currently doesn't work for CircleMarkers
                          plot.label = FALSE, noHide = FALSE, textOnly = FALSE, font.size = 10, plot.bg.label = NULL, file = "", vwidth = 1424,
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
    stopifnot(
      is.data.frame(x), dim(x)[2] == 2,
      ("sf" %in% class(sites) | "SpatialPointsDataFrame" %in% class(sites)),
      ("sf" %in% class(bg) | "SpatialPolygonsDataFrame" %in% class(bg) | is.null(bg)),
      is.null(col.breaks) || is.numeric(col.breaks)
    )
    if (map.type == "default") {
      if ("sf" %in% class(sites)) {
        sites <- sf::as_Spatial(sites)
      }
      if ("sf" %in% class(bg) & !is.null(bg)) {
        bg <- sf::as_Spatial(bg)
      }
    } else if (map.type == "leaflet") {
      if ("SpatialPointsDataFrame" %in% class(sites)) {
        sites <- sf::st_as_sf(sites)
      }
      if ("SpatialPolygonsDataFrame" %in% class(bg) & !is.null(bg)) {
        bg <- sf::st_as_sf(bg)
      }
    }
    stopifnot(map.adj %in% c(0, .5, 1))
    if (map.type == "default") {
      stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
    } else if (map.type == "leaflet") {
      stopifnot(legend.pos %in% c("bottomright", "topright", "topleft", "bottomleft"))
    }
    # if (length(col.breaks) == 1) {
    #   col.breaks <- range(x[, 2], na.rm = TRUE)
    #   warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
    # }
    if (!is.null(col.breaks) && (min(col.breaks, na.rm = TRUE) > min(x[, 2], na.rm = TRUE) || max(col.breaks, na.rm = TRUE) < max(x[, 2], na.rm = TRUE))) {
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
        if (mnx < 0) {
          col.class <- c("purple", crfun(length(cbrks) - 2))
        } else {
          col.class <- crfun(length(cbrks) - 1)
        }
      }
    } else if (is.vector(col)) {
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
    x[, 3] <- cut(x[, 2], breaks = cbrks, include.lowest = TRUE)
    
    # For leaflet mapping add NA Factor Level if any MapOutput data is NA
    if (map.type == "leaflet" & any(is.na(x[[2]]))) {
      x[, 3] <- addNA(x[, 3])
    }
    
    # replace the factor levels with color codes using the color ramp function assigned above
    if (map.type == "leaflet" & any(is.na(x[[2]]))) {
      levels(x[, 3]) <- c(col.class, na.color) # Add extra color for NA in leaflet maps
    } else {
      levels(x[, 3]) <- col.class
    }
    
    # Leaflet Legend Colors
    lcol <- levels(x[, 3])
    
    # convert to character to make it conform to plotting requirements below
    x[, 3] <- as.character(x[, 3])
    # give it a name
    names(x)[3] <- "color"
    
    if (map.type == "default") {
      # number of columns in original sites map
      nc.sites <- ncol(sites@data)
      # add x to subid map table (in data slot, indicated by @), merge by SUBID
      sites@data <- data.frame(sites@data, x[match(sites@data[, sites.subid.column], x[, 1]), ])
      # select sites for which x exists and sort in order of x so that user-supplied vectors of pch, lwd, etc. are assigned to the right map point
      sts <- sites[!is.na(sites@data[, nc.sites + 1]), ]
      sts <- sts[match(x[, 1], sts@data[, sites.subid.column]), ]
      
    } else if (map.type == "leaflet") {
      message(paste0('Joining "', colnames(sites)[sites.subid.column], '" from GIS Data (sites) To "', colnames(x)[1], '" from subass (x)'))
      
      # Check for duplicate SUBIDS
      if(any(duplicated(sites[, sites.subid.column]%>%sf::st_drop_geometry()))){message(paste(" - Duplicate SUBIDS exist in GIS Data (sites)!"))}
      if(any(duplicated(x[,1]))){message(" - Duplicate SUBIDS exist in subass (x)!")}
      
      x <- right_join(sites[, sites.subid.column] %>% mutate(across(1, ~ as.character(.x))), x %>% mutate(across(1, ~ as.character(.x))), by = setNames(nm = colnames(sites)[sites.subid.column], colnames(x)[1])) # Join GIS Data with subass in a manner in which column names don't have to be identical (e.g. "SUBID" and "subid" is okay, character and integer is okay)
    }
    
    # update legend title if none was provided by user or "auto" selection
    if (is.null(legend.title)) {
      legend.title <- toupper(names(x)[2])
    }
    
    if (map.type == "default") {
      ## plot settings
      if (!add) {
        plot.new()
        par(mar = par.mar, xaxs = "i", yaxs = "i", lend = 1, xpd = TRUE, cex = par.cex)
        frame()
      } else {
        par(xpd = TRUE, cex = par.cex, lend = 1)
      }
      
      
      ## the positioning of all plot elements works with three scales for the device's plot region:
      ## inches, fraction, and map coordinates
      
      # plot width (inches)
      p.in.wd <- par("pin")[1]
      
      # legend position (fraction if 'add' is FALSE, otherwise already in map coordinates)
      leg.fr.pos <- legend(legend.pos,
                           legend = rep(NA, length(cbrks) - 1),
                           col = col.class, lty = 1, lwd = 14, bty = "n", title = legend.title, plot = FALSE
      )
      # legend width (fraction if 'add' is FALSE, otherwise already in map coordinates)
      leg.fr.wd <- leg.fr.pos$rect$w
      # legend box element height (fraction), with workaround for single-class maps
      if (length(leg.fr.pos$text$y) == 1) {
        te <- legend(legend.pos,
                     legend = rep(NA, length(cbrks)),
                     col = crfun(length(cbrks)), lty = 1, lwd = 14, bty = "n", title = legend.title, plot = FALSE
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
      if (!is.null(bg)) {
        if (sf::st_is_longlat(bg) == FALSE) {
          bbx <- matrix(sf::st_bbox(bg),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
          # map side ratio (h/w)
          msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
          # plot area side ratio (h/w)
          psr <- par("pin")[2] / par("pin")[1]
        } else {
          bbx <- matrix(sf::st_bbox(bg),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
          # set user coordinates using a dummy plot (no fast way with Spatial polygons plot, therefore construct with SpatialPoints map)
          par(new = TRUE)
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
        if (sf::st_is_longlat(sites) == FALSE) {
          bbx <- matrix(sf::st_bbox(sites),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
          # map side ratio (h/w)
          msr <- apply(bbx, 1, diff)[2] / apply(bbx, 1, diff)[1]
          # plot area side ratio (h/w)
          psr <- par("pin")[2] / par("pin")[1]
        } else {
          bbx <- matrix(sf::st_bbox(sites),nrow=2, ncol=2, dimnames = list(c("x","y"),c("min", "max")))
          # set user coordinates using a dummy plot
          par(new = TRUE)
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
      
      # map
      if (!is.null(bg)) {
        # plot(bg, col = "grey90", border = "grey70", ylim = pylim, xlim = pxlim, add = add)
        plot(sf::st_geometry(sf::st_as_sf(bg)), col = bg.fillColor, border = "grey70", ylim = pylim, xlim = pxlim, add = add)
        plot(sf::st_geometry(sf::st_as_sf(sts)), bg = sts$color, border = 1, pch = pch, lwd = lwd, cex = 1.2 * pt.cex, add = TRUE)
      } else {
        plot(sf::st_geometry(sf::st_as_sf(sts)), bg = sts$color, col = 1, pch = pch, lwd = lwd, cex = 1.2 * pt.cex, ylim = pylim, xlim = pxlim, add = add)
      }
      # legend
      if (plot.legend) {
        legend(legend.pos,
               legend = rep(NA, length(cbrks) - 1), inset = legend.inset,
               col = rev(col.class), lty = 1, lwd = 14, bty = "n", title = legend.title, pt.cex = pt.cex
        )
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
        if (sf::st_is_longlat(sites)) {
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
      # invisible(sites)
      invisible(sts)
    } else if (map.type == "leaflet") {
      
      # Reproject if not a lat/long CRS
      if(sf::st_is_longlat(x)==FALSE){
        x <- x %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
      }
      if(!is.null(bg)){
        if(sf::st_is_longlat(bg)==FALSE){
          bg <- bg %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
        }
      }
      
      # Remove any empty geometries (these prevent labels from working)
      x <- x %>%
        dplyr::filter(!sf::st_is_empty(.))
      if(!is.null(bg)){
        bg <- bg %>%
          dplyr::filter(!sf::st_is_empty(.))
      }
      
      # Create legend labels, change NA color to selected NA color
      if (any(is.na(x[[2]]))) {
        l.label <- c(unlist(lapply(1:(length(cbrks) - 1), function(X) {
          paste(signif(cbrks[X], legend.signif), "to", signif(cbrks[X + 1], legend.signif))
        })), "NA")
      } else {
        l.label <- unlist(lapply(1:(length(cbrks) - 1), function(X) {
          paste(signif(cbrks[X], legend.signif), "to", signif(cbrks[X + 1], legend.signif))
        }))
      }
      
      # Create Leaflet Map
      message("Generating Map")
      leafmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addTiles() %>%
        leaflet::addLayersControl(
          baseGroups = c("Map", "Street", "Topo", "Satellite"),
          overlayGroups = c("Points", "Subbasins"),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoIndex = TRUE)
        ) %>%
        leaflet.extras::addResetMapButton()
      
      # Add Subbasins
      if (!is.null(bg)) {
        # Do Not Plot Labels
        if(is.null(plot.bg.label)){
          leafmap <- leafmap %>%
            leaflet::addPolygons(
              group = "Subbasins",
              data = bg,
              color = "black",
              weight = bg.weight,
              opacity = bg.opacity,
              fillColor = bg.fillColor,
              fillOpacity = bg.fillOpacity
            )
          # Plot Labels
        } else{
          if (plot.bg.label == "hover"){
            leafmap <- leafmap %>%
              leaflet::addPolygons(
                group = "Subbasins",
                data = bg,
                label = bg[[bg.label.column]],
                color = "black",
                weight = bg.weight,
                opacity = bg.opacity,
                fillColor = bg.fillColor,
                fillOpacity = bg.fillOpacity
              )
          } else if (plot.bg.label == "static"){
            leafmap <- leafmap %>%
              leaflet::addPolygons(
                group = "Subbasins",
                data = bg,
                color = "black",
                weight = bg.weight,
                opacity = bg.opacity,
                fillColor = bg.fillColor,
                fillOpacity = bg.fillOpacity
              )%>%
              leaflet::addLabelOnlyMarkers(
                group = "Subbasins",
                data = suppressWarnings(sf::st_point_on_surface(bg)),
                label = bg[[bg.label.column]],
                labelOptions = leaflet::labelOptions(noHide = TRUE, direction = 'auto', textOnly = TRUE)
              )
          } else{ # Do not plot labels
            leafmap <- leafmap %>%
              leaflet::addPolygons(
                group = "Subbasins",
                data = bg,
                color = "black",
                weight = bg.weight,
                opacity = bg.opacity,
                fillColor = bg.fillColor,
                fillOpacity = bg.fillOpacity
              )
          }
        }
      }
      
      if (plot.label == TRUE) { # Create points with labels
        
        # Create labels
        x <- x %>%
          mutate(label = paste0("SUBID: ", .[[1]], " --- Value: ", .[[2]]))
        
        leafmap <- leafmap %>%
          leaflet::addCircleMarkers(
            group = "Points",
            data = x,
            color = "black",
            radius = radius,
            weight = weight,
            opacity = opacity,
            fillColor = x$color,
            fillOpacity = fillOpacity,
            label = ~label,
            labelOptions = leaflet::labelOptions(noHide = noHide, direction = "auto", textOnly = textOnly, style = list("font-size" = paste0(font.size, "px")))
          )
      } else { # Create points without labels
        leafmap <- leafmap %>%
          leaflet::addCircleMarkers(
            group = "Points",
            data = x,
            color = "black",
            radius = radius,
            weight = weight,
            opacity = opacity,
            fillColor = x$color,
            fillOpacity = fillOpacity
          )
      }
      
      # # Add searchbar to map
      # if (plot.searchbar == TRUE) {
      #   leafmap <- leafmap %>%
      #     leaflet.extras::addSearchFeatures(
      #       targetGroups = c("Points"),
      #       options = leaflet.extras::searchFeaturesOptions(zoom = 10, hideMarkerOnCollapse = TRUE)
      #     )
      # }
      
      # Add scalebar to map
      if (plot.scale == TRUE) {
        leafmap <- leafmap %>%
          leaflet::addScaleBar(position = "bottomright")
      }
      
      # Add legend to map
      if (plot.legend == TRUE) {
        leafmap <- leafmap %>%
          leaflet::addLegend(
            group = "Points",
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

# # DEBUG
# x <- ReadSubass("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_miracle/res_wq_baseline/subass2.txt")[, c(1, 3)]
# x <- ReadGeoData("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_miracle/GeoData.txt")[, c("SUBID", "PARREG")]
# x <- read.table(file = "//winfs-proj/data/proj/Fouh/Europe/E-HYPE/EHYPEv3.xDev/New Data/Xobs_WQ/Xobsar/RDir/subid_xobs.txt", sep = "\t", header = TRUE)
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
