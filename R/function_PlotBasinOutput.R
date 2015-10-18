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
#' @param filename String, file name for plotting to \code{\link{png}} device, defaults to name provided in argument \code{x}
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

PlotBasinOutput <- function(x, filename = deparse(substitute(x)), timestep = attr(x, "timestep"), log.q = F, start.mon = 1, from = 1, to = nrow(x), name = "", area = NULL, subid = NULL, gd = NULL, bd = NULL) {
  
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
  
  ## parse plot commands based on existing HYPE variables to a list
  ## create layout() arguments based on existinng HYPE variables
  
  # create list to hold all plot commands, and plot counter
  list.plotexpr <- list(NULL)
  cp <- 0
  
  # layout() matrix initialisation
  lay.mat <- matrix(ncol = 3, nrow = 0)
  # layout() panel widths (hard-coded for now)
  lay.widths <- c(1.5, 1, 1.5)
  # layout() panel heights initialisation
  lay.heights <- NULL
  
  
  # conditional: three panels with FDC, GoFs, and regime. If GoF variables exist
  if ((exi.t["rout"] || exi.t["cout"]) || (
    (exi.t["rein"] && exi.t["ccin"]) || 
      (exi.t["reon"] && exi.t["ccon"]) || 
      (exi.t["retn"] && exi.t["cctn"]) ||
      (exi.t["resp"] && exi.t["ccsp"]) ||
      (exi.t["repp"] && exi.t["ccpp"]) ||
      (exi.t["retp"] && exi.t["cctp"])
    )
    ) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, 1:3)
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    # conditional: prepare FDC plot call depending on data availability
    if (exi.t["rout"] && exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(rout, cout)), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = rout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "Qobs", col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = cout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "Qsim", col = c("red"), mar = c(3.1, 3.1, .5, .5))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    ## plot information texts
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    # plot name
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'title(main = name, line = -1)')
    # compute and plot GoFs for discharge, TN, and TP, if variables are available
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- gof(sim = cout, obs = rout, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), " obs.)")), bty = "n", title = "Q, goodness of fit", cex = .8)')
    }
    if (exi.t["retn"] && exi.t["cctn"]){
      gof.tn <- gof(sim = cctn, obs = retn, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness of fit", cex = .8)')
    }
    if (exi.t["retp"] && exi.t["cctp"]){
      gof.tp <- gof(sim = cctp, obs = retp, na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ]
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tp), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness of fit", cex = .8)')
    }
    
    # conditional: prepare regime plot call depending on data availability
    if (exi.t["rout"] && exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs"), col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c(Qsim"), col = c(red"), mar = c(3.1, 3.1, .5, .5))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
  }
  
  # precipitation and snowfall panel
  if (exi.t["uppr"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, uppr, ylim = c(max(uppr), -2), col = NA, axes = F, ylab = "")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(new = TRUE)')
    
    # conditional: if rainfall and snow variables available, plot stacked bars based on these, otherwise plot precip bars
    if (exi.t["uprf"] && exi.t["upsf"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = t(as.matrix(data.frame(uprf, upsf))), border = NA, ylim = c(max(uppr), -2), xlab = "", col = c("darkblue", "forestgreen"), names.arg = rep("", length(uppr)), legend.text = c("Rain", "Snow"), args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2), ylab = "mm", space = 0, cex.axis = 1.1, cex.lab = 1.2)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = uppr, border = NA, ylim = c(max(uppr), -2), xlab = "", col = "darkblue", names.arg = rep("", length(uppr)), legend.text = "Precipitation", args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2), ylab = "mm", space = 0, cex.axis = 1.1, cex.lab = 1.2)')
    }
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
  }
  
  # temperature panel
  if (exi.t["temp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, temp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(""*degree, "C")), cex.axis = 1.1, cex.lab = 1.2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'lines(date, temp, col = "red")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(" Air temp. at outlet", side=3, adj= 0, line=-1.1, cex = .8)')
    
  }
  
  # Qsim, Qobs panel
  if (exi.t["cout"] || exi.t["rout"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)')
    
    cp <- cp + 1
    if (!exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, rout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), ylim = c(ifelse(log.q, 0.001, 0), max(rout, na.rm=T)), log = ifelse(log.q, "y", ""), cex.axis = 1.1, cex.lab = 1.2)')  
    } else if (!exi.t["rout"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), ylim = c(ifelse(log.q, 0.001, 0), max(cout, na.rm=T)), log = ifelse(log.q, "y", ""), cex.axis = 1.1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), ylim = c(ifelse(log.q, 0.001, 0), max(c(cout, rout), na.rm=T)), log = ifelse(log.q, "y", ""), cex.axis = 1.1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, rout, col = "royalblue4")')
    }
    
    if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, cout, col = "orangered3")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", c("Qobs", "Qsim"), lty = 1, col = c("royalblue4", "orangered3"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = '')
  
  
  # panel 3: ET
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  plot(xw[, col.date], xw[, col.uppe], type = "l", col = NA, xaxt = "n", ylab = "mm", ylim = c(0, max(xw[, col.uppe])), cex.axis = 1.1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  lines(xw[, col.date], xw[, col.uppe], col = "green3", lty = 3)
  lines(xw[, col.date], xw[, col.upev], col = "green4")
  legend("topleft", c("ETp", "ETa"), lty = c(3, 1), col = c("green3", "green4"), bty = "n", cex = 1.2, horiz = TRUE)
  
  # panel 4: snow water equivalent
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  plot(xw[, col.date], xw[, col.snow], type = "l", col = NA, xaxt = "n", ylab = "mm", cex.axis = 1.1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  lines(xw[, col.date], xw[, col.snow], col = "deepskyblue3")
  mtext(" Snow water equivalent", side=3, adj= 0, line=-1.1, cex = .8)
  
  
  # panel 6: soil moisture in local subid
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  plot(xw[, col.date], xw[, col.soim], type = "l", col = NA, xaxt = "n", ylab = "mm", 
       ylim = c(min(xw[, col.soim], na.rm = TRUE), max(xw[, col.soim], na.rm = TRUE)), cex.axis = 1.1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  lines(xw[, col.date], xw[, col.soim], col = "firebrick3")
  lines(xw[, col.date], xw[, col.sm13], col = "springgreen4")
  legend("topleft", c("soil moisture", "surface water"), lty = 1, col = c("springgreen4", "firebrick3"), 
         bty = "n", cex = 1.2, horiz = TRUE)
  
  # panel 7: accumulated volume error
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  sqsim <- ConvertDischarge(q = xw[, col.cout], area = uarea, from = "m3s", to = "mmd")
  sqobs <- ConvertDischarge(q = xw[, col.rout], area = uarea, from = "m3s", to = "mmd")
  accvolerr <- cumsum(sqsim - ifelse(is.na(sqobs), sqsim, sqobs))
  plot(xw[, col.date], accvolerr, type = "l", col = NA, xaxt = "n", ylab = "mm", cex.axis = 1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  lines(xw[, col.date], accvolerr, col = "seagreen")
  mtext(" Accumulated volume error", side=3, adj= 0, line=-1.1, cex = .8)
  
  # panel 8: soil moisture
  par(mar = c(0, 2.5, 0, 0), xaxs = "i", mgp = c(1.2, .2, 0), tcl = .2)
  plot(xw[, col.date], xw[, col.upfp], type = "l", col = NA, xaxt = "s", ylab = "(-)", xlab = "", cex.axis = 1.1, cex.lab = 1.2)
  abline(h = 0, col = "grey", lwd = .5)
  abline(v = xw[, col.date][which(format(xw[, col.date], format = "%m%d") == "0101")], , col = "grey", lwd = .5)
  lines(xw[, col.date], xw[, col.upfp], col = "chocolate3")
  mtext(" Rel. soil moisture", side=3, adj= 0, line=-1.1, cex = .8)
  
  
  
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = '')
  
  # add empty row at the figure bottom in layout, as space for x-axis annotation
  lay.mat <- rbind(lay.mat, rep(0, 3))
  # add layout height for this row
  lay.heights <- c(lay.heights, .2)
  
  ## set up plot device with layout and call all plot commands 
  
  # define device width (hard-coded for now)
  wdth <- 20
  # set device height, based on layout rows
  hght <- sum(lay.heights) * 1.5
  
  # create plot device, conditional on filename
  if (is.null(filename)) {
    x11(width=wdth, height = hght)
  } else {
    png(filename = paste0(filename, ".png"), width=wdth*.8, height = hght*.8, units = "in", res = 300)
  }
  
  # layout definition
  nf <- layout(mat = lay.mat, widths = lay.widths, heights = lay.heights)
  layout.show(nf)
  
  # plot all commands in list
  for (i in 1:length(list.plotexpr)) {
    eval(list.plotexpr[[i]])
  }
  
  # close the file device, if any
  if (!is.null(filename)) {
    dev.off()
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
# filename <- NULL
