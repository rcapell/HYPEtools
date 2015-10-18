#' @export
#' 
#' @import hydroGOF

#' @title
#' Plot a suite of evaluation plots from a HYPE basin output file
#'
#' @description
#' Plot a standard suite of evaluation plots from a basin output file, typically used for model performance inspection and/or 
#' during manual calibration
#'
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param timestep  Character string, timestep of \code{x}, one of \code{"month"}, \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours). If not provided, an attribute \code{timestep} is required in \code{x}.
#' @param log.q
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year. For runoff regime plot, see also 
#' \code{\link{AnnualRegime}}.
#' @param from
#' @param to
#' @param name
#' @param area Numeric, upstream area of sub-basin in km^2. Required for calculation of accumulated volume error. Optional argument, 
#' either this or arguments \code{subid}, \code{gd}, and \code{bd} are required.
#' @param subid HYPE SUBID of a target sub-catchment (must exist in \code{gd}). Mandatory in combination with \code{gd} and 
#' optionally \code{bd} if argument \code{area} is not defined. Used to calculate upstream area internally with function 
#' \code{\link{SumUpstreamArea}}.
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory with argument 
#' \code{subid}, details see there. 
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional with argument 
#' \code{subid}, details see there. 
#' 
#' @details
#' date, uprf, upsf, temp, uppe, upev, cout, rout, soim, sm13, upfp, snow, uppr, ccin, rein, ccon, reon, cctn, retn, ccsp, resp, ccpp, repp, cctp, retp, wcom, wstr
#' \url{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}
#' 
#' 
#' @return 
#' Returns a multi-panel plot in a new graphics device.
#' 
#' @examples
#' PlotBasinOutput(x = mybasin, area = 56.67)

PlotBasinOutput <- function(x, timestep = attr(x, "timestep"), log.q = F, start.mon = 1, from = 1, to = nrow(x), name = "", area = NULL, subid = NULL, gd = NULL, bd = NULL) {
  
  ## Preliminaries
  
  # conditional: argument area given or able to calculate with arguments gd, bd, subid?,  incl. error handling
  if (is.null(area) && is.null(gd)) {
    stop("Provide either argument 'area' or argument 'gd'.")
  } else {
    if (is.null(area)) {
      if (is.null(subid)) {
        stop("Argument 'subid' is mandatory with argument 'gd'.")
      }
      uarea <- SumUpstreamArea(subid = subid, gd = gd, bd = bd)[, 2]
    } else {
      uarea <- area * 10^6
    }
  }
  
  # identify rows to plot, time window
  if (is.numeric(from)) {
    fw <- from
  } else if (is.character(from)) {
    fw <- which(format(x[, 1], format = "%F") == from)[1]
    if (length(fw) == 0) {
      stop("Argument 'from': Unknown date string or date outside model time period.")
    }
  } else {
    stop("Argument 'from': Wrong type.")
  }
  if (is.numeric(to)) {
    tw <- to
  } else if (is.character(to)) {
    tw <- which(format(x[, 1], format = "%F") == to)[1]
    if (length(tw) == 0) {
      stop("Argument 'to': Unknown date string or date outside model time period.")
    }
  } else {
    stop("Argument 'to': Wrong type.")
  }
  if (fw >= tw) {
    stop("'from' later than 'to'.")
  }
  
  # select time window from indata for plotting
  xw <- x[fw:tw, ]
  
  # save current state of par() variables which are altered below, for restoring on function exit
  par.mar <- par("mar")
  par.xaxs <- par("xaxs")
  par.mgp <- par("mgp")
  par.tcl <- par("tcl")
  on.exit(par(mar = par.mar, xaxs = par.xaxs, mgp = par.mgp, tcl = par.tcl))
  
  
  ## identify column indices of target variables and total number of variables to plot
  
  # force lower case for names in basin output file, for selecting target variables below
  names(xw) <- tolower(names(xw))
  
  # create vector over all target output variable names which are potentially used in the plot panels
  nm.t <- c("date", "uprf", "upsf", "temp", "uppe", "upev", "cout", "rout", "soim", "sm13", "upfp", "snow", "uppr", 
            "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", "repp", "cctp", "retp", "wcom", "wstr")
  # initialise logical vector to indicate existence of target variables
  exi.t <- logical()
  names(exi.t) <- nm.t
  
  # identify existing and non-empty variables for plotting in user-provided basin output table
  # and save existing variables to vectors
  for (i in 1:length(nm.t)) {
    te <- tryCatch(with(xw, get(nm.t[i])), error = function (e) NULL)
    if(!(is.null(te) || all(suppressWarnings(is.na(te))))) {
      assign(x = nm.t[i], value = te)
      exi.t[i] <- TRUE
    } else {
      exi.t[i] <- FALSE
    }
  }
  
  ## define plot dimensions and which panels are to be plotted based on existing variables
  
  # plot dimensions, height iteratively updated below
  wdth <- 23
  hght <- 0
  
  # split-screen screen matrix and screen height initialisation. Height will be cumulatively updated below
  # for each new screen row
  split.figs <- matrix(ncol = 4, nrow = 0)
  split.height <- 0
  # screen identity vector and counter initialisation, used to relate plot expressions to a screen
  cscr <- 0
  iscr <- integer()
  # plot expression list and counter initialisation. Expression evaluated after plot and screen dimensions are defined
  list.plotexpr <- list()
  cp <- 0
  
  # first row, three panels with FDC, GoFs, and regime
  if ((exi.t["rout"] || exi.t["cout"]) || (
    (exi.t["rein"] && exi.t["ccin"]) || 
      (exi.t["reon"] && exi.t["ccon"]) || 
      (exi.t["retn"] && exi.t["cctn"]) ||
      (exi.t["resp"] && exi.t["ccsp"]) ||
      (exi.t["repp"] && exi.t["ccpp"]) ||
      (exi.t["retp"] && exi.t["cctp"])
    )
    ) {
    
    # define screen for FDC
    split.figs <- rbind(split.figs, c(0, 1/3, split.height,  split.height + 1))
    # conditional: prepare FDC plot call depending on data availability
    cscr <- cscr + 1
    cp <- cp + 1
    iscr[cp] <- cscr
    if (exi.t["rout"] && exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(rout, cout)), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = rout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "Qobs", col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = cout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "Qsim", col = c("red"), mar = c(3.1, 3.1, .5, .5))')
    } else {
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    # define screen for text information
    split.figs <- rbind(split.figs, c(1/3, 2/3, split.height,  split.height + 1))
    # plot information texts
    cscr <- cscr + 1
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'title(name, line = -1)')
    # compute and plot GoFs for discharge, TN, and TP, if variables are available
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- gof(sim = cout, obs = rout, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      iscr[cp] <- cscr
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), " obs.)")), bty = "n", title = "Q, goodness of fit")')
    }
    if (exi.t["retn"] && exi.t["cctn"]){
      gof.tn <- gof(sim = cctn, obs = retn, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      iscr[cp] <- cscr
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness of fit")')
    }
    if (exi.t["retp"] && exi.t["cctp"]){
      gof.tp <- gof(sim = cctp, obs = retp, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      iscr[cp] <- cscr
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tp), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness of fit")')
    }
    
    # define screen for flow regime
    split.figs <- rbind(split.figs, c(2/3, 1, split.height,  split.height + 1))
    # update split height, so that next screen goes into next row
    split.height <- split.height + 1
    # conditional: prepare regime plot call depending on data availability
    cscr <- cscr + 1
    cp <- cp + 1
    iscr[cp] <- cscr
    if (exi.t["rout"] && exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs"), col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c(Qsim"), col = c(red"), mar = c(3.1, 3.1, .5, .5))')
    } else {
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
  }
  
  # precipitation and snowfall panel
  if (exi.t["uppr"]) {
    
    # define screen for precip chart
    split.figs <- rbind(split.figs, c(0, 1, split.height,  split.height + 1))
    
    cscr <- cscr + 1
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'plot(date, uppr, ylim = c(max(uppr), -2), col = NA, axes = F, ylab = "")')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'par(new = TRUE)')
    
    # conditional: if rainfall and snow variables available, plot stacked bars based on these, otherwise plot precip bars
    if (exi.t["uprf"] && exi.t["upsf"]) {
      cp <- cp + 1
      iscr[cp] <- cscr
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = t(as.matrix(data.frame(uprf, upsf))), border = NA, ylim = c(max(uppr), -2), xlab = "", col = c("darkblue", "forestgreen"), names.arg = rep("", length(uppr)), legend.text = c("Rain", "Snow"), args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2), ylab = "mm", space = 0, cex.axis = 1.1, cex.lab = 1.2)')
    } else {
      cp <- cp + 1
      iscr[cp] <- cscr
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = uppr, border = NA, ylim = c(max(uppr), -2), xlab = "", col = "darkblue", names.arg = rep("", length(uppr)), legend.text = "Precipitation", args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2), ylab = "mm", space = 0, cex.axis = 1.1, cex.lab = 1.2)')
    }
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    iscr[cp] <- cscr
    list.plotexpr[[cp]] <- parse(text = 'box()')
  }
  
  
  
  
  cscr <- cscr + 1
  cp <- cp + 1
  iscr[cp] <- cscr
  list.plotexpr[[cp]] <- parse(text = '')
  
  
  
  
  ## plot all results to split.screen device
  # update screen matrix with relative heights
  split.figs[, 3:4] <- split.figs[, 3:4] / split.figs[nrow(split.figs), 4]
  
  # open new device
  x11(width=15, height = 14)
  
  # define screens
  split.screen(figs = split.figs)
  
  # layout definition
  nf <- layout(matrix(c(1:3, rep(4, 3), rep(0, 3)), byrow= T, ncol = 3), widths =c(1, 1, 1), heights = c(rep(1, 2), .2))
  layout.show(nf)
  
  # plot
  for (i in 1:length(list.plotexpr)) {
    #screen(n = iscr[i], new = F)
    eval(list.plotexpr[[i]])
  }
  
  
  
  # make multi-panel layout
  nf <- layout(matrix(c(0,0, 1:3,0, 4,0, 5,0, 6:10,0, 11,12, 0,0), byrow= T, nrow=10, ncol = 2), widths =c(1, .08), heights = c(.15, rep(1, 8), .2))
  layout.show(nf)
  
  # panel 1: precip/snow
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  plot(xw[, col.date], xw[, col.uppr], ylim = c(max(xw[, col.uppr]), -2), col = NA, axes = F, ylab = "")
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  par(new = TRUE)
  barplot(height = t(as.matrix(xw[, c(col.uprf, col.upsf)])), border = NA, ylim = c(max(xw[, col.uppr]), -2), xlab = "", 
          col = c("darkblue", "dodgerblue"), names.arg = rep("", nrow(xw)), legend.text = c("Rain", "Snow"), 
          args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2), ylab = "mm", space = 0, cex.axis = 1.1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  box()
  mtext(paste(" ", river, " at ", gauge, ", ", round(uarea * 10^-6, 0), " km2", sep = ""), side = 3, line = 0)
  par(mar = c(0, 0, 0, 0))
  plot(NA, ann = F, axes = F, ylim = c(0, 1), xlim = c(0, 1))
  
  col.date <- which(nm == "date")
  col.uprf <- which(nm == "uprf")
  col.upsf <- which(nm == "upsf")
  col.temp <- which(nm == "temp")
  col.uppe <- which(nm == "uppe")
  col.upev <- which(nm == "upev")
  col.cout <- which(nm == "cout")
  col.rout <- which(nm == "rout")
  col.soim <- which(nm == "soim")
  col.sm13 <- which(nm == "sm13")
  col.upfp <- which(nm == "upfp")
  col.snow <- which(nm == "snow")
  col.uppr <- which(nm == "uppr")
  col.ccin <- which(nm == "ccin")
  col.rein <- which(nm == "rein")
  col.ccon <- which(nm == "ccon")
  col.reon <- which(nm == "reon")
  col.cctn <- which(nm == "cctn")
  col.retn <- which(nm == "retn")
  col.ccsp <- which(nm == "ccsp")
  col.resp <- which(nm == "resp")
  col.ccpp <- which(nm == "ccpp")
  col.repp <- which(nm == "repp")
  col.cctp <- which(nm == "cctp")
  col.retp <- which(nm == "retp")
  col.wcom <- which(nm == "wcom")
  col.wstr <- which(nm == "wstr")
  
  
  # conditional incl. error handling: argument area given or able to calculate with arguments gd, bd, subid?
  if (is.null(area) & is.null(gd)) {
    stop("Provide either argument 'area' or argument 'gd'.")
  } else {
    if (is.null(area)) {
      if (is.null(subid)) {
        stop("Argument 'subid' is mandatory with argument 'gd'.")
      }
      uarea <- SumUpstreamArea(subid = subid, gd = gd, bd = bd)[, 2]
    } else {
      uarea <- area * 10^6
    }
  }
  
}

# # debug
# x <- ReadBasinOutput("//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/cal_wbalance_coarse_open/res1/9548212.txt")
# x <- ReadBasinOutput("../9548212.txt")
# names(x)
# x <- x[, -c(20:25)]
# from <- 1
# from <- "2005-01-01"
# to <- nrow(x)
# name <- "Weaver EHYPE3 default"
# area <- NULL
# gd <- ReadGeoData("//winfs-proj/data/proj/Fouh/Europe/Projekt/SWITCH-ON/WP3 experiments/experiment_wq_weaver/Analyses/cal_wbalance_coarse_open/GeoData.txt")
# log.q <- F
# filename <- NA
# subid <- 9548212
# bd <- NULL
# timestep <- "day"
# start.mon <- 10
