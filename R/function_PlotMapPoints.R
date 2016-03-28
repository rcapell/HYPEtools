#'
#' Plot function for mapped point information
#'
#' Plot mapped point information, e.g. model performances at observation sites.
#'
#' 

PlotMapPoints <- function(x, sites, x.column = 2, sites.subid.column = 1, bg = NULL, add = FALSE, map.adj = 0, plot.legend = T, 
                          legend.pos = "right", legend.title = NULL, legend.inset = c(0, 0), 
                          col.ramp.fun = NULL, col.breaks = NULL) {
  
  # input argument checks
  stopifnot(is.data.frame(x), dim(x)[2] == 2, class(sites)=="SpatialPointsDataFrame", 
            is.null(col.breaks) || is.numeric(col.breaks))
  stopifnot(map.adj %in% c(0, .5, 1))
  stopifnot(legend.pos %in% c("bottomright", "right", "topright", "topleft", "left", "bottomleft"))
  if (length(col.breaks) == 1) {
    col.breaks <- range(x[, 2], na.rm = T)
    warning("Just one value in user-provided argument 'col.breaks', set to range of 'x[, 2]'.")
  }
  if (!is.null(col.breaks) && (min(col.breaks > min(x[, 2], na.rm = T)) || max(col.breaks < max(x[, 2], na.rm = T)))) {
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
  on.exit(par(mar = par.mar0, xaxs = par.xaxs, yaxs = par.yaxs, lend = par.lend, xpd = par.xpd, cex = par.cex0))
  
  
  
  # data preparation and conditional assignment of color ramp functions and break point vectors 
  # to internal variables crfun and cbrks
  
  if (is.function(col.ramp.fun)) {
    # a color ramp palette function is supplied
    crfun <- col.ramp.fun
  } else if (is.null(col.ramp.fun)) {
    # no color ramp function supplied, create default
    crfun <- colorRampPalette(c("#e81515", "#EEEE00", "#2892c7"))
  } else {
    # Error treatment for all other types of user input
    stop("Invalid 'col.ramp.fun' argument.")
  }
  
  if (!is.null(col.breaks)) {
    cbrks <- col.breaks
  } else {
    te <- min(x[, x.column], na.rm = TRUE)
    if (te < 0) {
      cbrks <- c(te, seq(0, 1, by = 0.1))
    }
    
  }
  
  
  if (!is.null(bg)) {
    plot(bg, col = "grey90", border = "grey70")
    plot(, add = T)
  }
}

# # DEBUG
# x <- ReadSubass("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/model_helgean_miracle/res_wq_baseline/subass2.txt")
# sites <- readOGR("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/gis", layer = "helgean_outlet_points")
# bg <- readOGR("//winfs-proj/data/proj/Fouh/Europe/Projekt/MIRACLE/WP2/gis/helgean/subbasin", layer = "helgean_shype_aro_y")
# add <- F
# map.adj <- 0
# plot.legend <- T
# legend.pos <- "right"
# legend.title <- "test"
# legend.inset <- 0
# col.breaks <- NULL
# x.column <- 3
