#' Plot a summary of model results for a single sub-basin
#' 
#' Plot a standard suite of plots summarising properties of a sub-basin including upstream area and model performance 
#' for discharge and nutrients.
#' 
#' @param x Data frame, with column-wise daily time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param filename String, file name for plotting to \code{\link{png}} device. \code{NULL} triggers a plot on a new
#' screen device. \emph{Device dimensions are currently hard-coded.}
#' @param timestep  Character string, timestep of \code{x}, one of \code{"month"}, \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours). If not provided, an attribute \code{timestep} is required in \code{x}.
#' 
#' @details 
#' 
#' @export


PlotBasinSummary <- function(x, filename = "plot_basin", gd = gd, bd = NULL, gcl = gcl, psd = NULL, desc = NULL, 
                             from = 1, to = nrow(x)) {
  
  ## Preliminaries
  
  
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
    if (tw > nrow(x)) {
      date.plot <- seq(x[1, 1], by = "day", length.out = tw)
    }
  } else if (is.character(to)) {
    tw.d <- strptime(to, format = "%F", tz = "GMT")
    if (tw.d > x[nrow(x), 1]) {
      date.plot <- seq(x[1, 1], tw.d, by = "day")
      tw <- length(date.plot)
    } else {
      tw <- which(x[, 1] == tw.d)
    }
    if (length(tw) == 0) {
      stop("Argument 'to': Unknown date string or other date matching error.")
    }
  } else {
    stop("Argument 'to': Wrong type.")
  }
  if (fw >= tw) {
    stop("'from' later than 'to'.")
  }
  
  # select time window from indata for plotting
  xw <- x[fw:tw, ]
  # save data extent for regime plot label
  xlab.regime <- paste(format(range(xw[, 1], na.rm = T), "%Y"), collapse = " to ")
  # if selected time window is longer than time series in x, extend date column
  if (exists("date.plot")) {
    xw[, 1] <- date.plot[fw:length(date.plot)]
  }
  
  
  
  ## identify column indices of target variables and total number of variables to plot
  
  # force lower case for names in basin output file, for selecting target variables below
  names(xw) <- tolower(names(xw))
  
  # create vector over all target output variable names which are potentially used in the plot panels
  nm.t <- c("date", "cout", "rout", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", 
            "repp", "cctp", "retp", "totn", "totp")
  # initialise logical vector to indicate existence of target variables
  exi.t <- logical(length = length(nm.t))
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
  
  # select from existing variables based on user request, default is to use all available
  if (hype.vars[1] != "all") {
    if (hype.vars[1] == "hydro") {
      nm.hydro <- c("date", "cout", "rout")
      exi.t[!(nm.t %in% nm.hydro)] <- FALSE
    } else if (hype.vars[1] == "nutrients") {
      nm.nutri <- c("date", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", "repp", "cctp", "retp", "totn", "totp", "cout")
      exi.t[!(nm.t %in% nm.nutri)] <- FALSE
    } else if (is.character(hype.vars)) {
      nm.manu <- c("date", tolower(hype.vars))
      exi.t[!(nm.t %in% nm.manu)] <- FALSE
      # warn if an unknown variable was specified
      if (any(!(nm.manu %in% nm.t))) {
        warning(paste("Unknown variable(s) specified in argument 'hype.vars':", paste(nm.manu[!(nm.manu %in% nm.t)], collapse = ",")))
      }
    } else {
      stop("Wrong specification of argument 'hype.vars'.")
    }
  }
  
  
  ## calculate upstream point sources if nutrients requested
  if (hype.vars[1] %in% c("all", "nutrients") && !is.null(psd)) {
    upsd <- UpstreamPointSources(subid = subid, gd = gd, psd = psd, bd = bd, progbar = F)
    # calculate load in ton/year, proceed only if upstream point sources exist
    if (nrow(upsd) > 0) {
      load.psn <- weighted.mean(x = upsd$up_ps_tnconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .000365
      load.psp <- weighted.mean(x = upsd$up_ps_tpconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .000365
    } else {
      load.psn <- NA
      load.psd <- NA
    }
  }
  
  ## calculate upstream rural household emissions if nutrients requested
  if (hype.vars[1] %in% c("all", "nutrients")) {
    # calculate upstream loads in ton/year, proceed only if all necessary variables exist in gd
    if (all(c("loc_vol", "loc_tn", "loc_tp") %in% tolower(names(gd)))) {
      ugd <- suppressWarnings(UpstreamGeoData(subid = subid, gd = gd, bd = bd, progbar = F))
      pos.lvol <- which(tolower(names(ugd)) == "up_loc_vol")
      pos.ltn <- which(tolower(names(ugd)) == "up_loc_tn")
      pos.ltp <- which(tolower(names(ugd)) == "up_loc_tp")
      load.rurn <- ugd[, pos.ltn] * ugd[, pos.lvol] * .00036525
      load.rurp <- ugd[, pos.ltp] * ugd[, pos.lvol] * .00036525
    } else {
      load.rurn <- NA
      load.rurp <- NA
    }
  }
  
  ## calculate mean annual N and P loads at outlet in ton/year if they exist in x
  if (hype.vars[1] %in% c("all", "nutrients") && exi.t["totn"] && exi.t["cout"]) {
    load.outn <- mean(totn * cout * 0.36525)
  } else {
    load.outn <- NA
  }
  if (exi.t["totp"] && exi.t["cout"]) {
    load.outp <- mean(totp * cout * 0.031536)
  } else {
    load.outp <- NA
  }
  
  # combine all loads
  if (hype.vars[1] %in% c("all", "nutrients")) {
    loads.tn <- c(load.outn, load.psn, load.rurn)
    loads.tp <- c(load.outp, load.psp, load.rurp)
  }
  
  ## parse plot commands based on existing or requested HYPE variables to a list
  ## create layout() arguments based on existinng HYPE variables
  
  # create list to hold all plot commands, and plot counter
  list.plotexpr <- list(NULL)
  cp <- 0
  
  # layout() matrix initialisation
  lay.mat <- matrix(ncol = 6, nrow = 0)
  # layout() panel widths (hard-coded for now)
  lay.widths <- c(.25, rep(1, 3), rep(.5, 2))
  # layout() panel heights initialisation
  lay.heights <- NULL
  
  
  
  #### First row: 5 panels with land use, soil, crop barplots, and load bars for tn and tp. 
  
  # fill layout matrix with panel IDs
  lay.mat <- rbind(lay.mat, 0:5)
  # add layout height for this row
  lay.heights <- c(lay.heights, 3)
  
  ## panel 1: upstream land use bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(subid, gd = gd, bd = bd, gc = gcl, type = "l", progbar = F), type = "l", desc = desc)')
  
  ## panel 2: upstream soil bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(subid, gd = gd, bd = bd, gc = gcl, type = "s", progbar = F), type = "s", desc = desc)')
  
  ## panel 3: upstream crop bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(subid, gd = gd, bd = bd, gc = gcl, type = "c", progbar = F), type = "c", desc = desc)')
  
  ## panels 4 and 5: upstream TN and TP loads in ton/year, conditional on if nutrients requested, alternatively plot empty frame
  if (hype.vars[1] %in% c("all", "nutrients")) {
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tn / 1000, col = ColNitr(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, max(loads.tn / 1000) * 1.5))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ."), side = 3, at = c(.7, 1.9, 3.1), line = -.2, padj = .3, cex = .9, las = 3, adj = 1)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TN loads", side = 1, line = .5, cex = .9)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tp / 1000, col = ColPhos(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, max(loads.tp / 1000) * 1.5))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ."), side = 3, at = c(.7, 1.9, 3.1), line = -.2, padj = .3, cex = .9, las = 3, adj = 1)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TP loads", side = 1, line = .5, cex = .9)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
  } else {
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
  }
  
  
  #### 2nd row: 5 panels (one unused atm) with GOFs, sim-obs, FDC, and regime for Q , conditional on if Q is requested
  
  if (hype.vars[1] %in% c("all", "hydro")) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 3)
    
    
    ## panel 1: compute and plot GoFs for discharge, TN, and TP, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- tryCatch(gof(sim = get("cout"), obs = get("rout"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                        error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.8, legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), " obs.)")), bty = "n", title = "Q, goodness of fit", cex = 1)')
    }
    
    ## panel 2: unused
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    ## panel 3: sim-obs comparison dotty plot
    if (exi.t["rout"] && exi.t["cout"]){
      
    }
  }
  
  # fill layout matrix with panel IDs
  lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
  # add layout height for this row
  lay.heights <- c(lay.heights, 3)
  
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = '')
  
  
  
  
  {
  ## plot information texts
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    # plot name
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'title(main = name, line = -length(strsplit(x = name, split = "\n")[[1]])*1.1)')
    # compute and plot GoFs for discharge, TN, and TP, if variables are available
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- tryCatch(gof(sim = get("cout"), obs = get("rout"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                        error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), " obs.)")), bty = "n", title = "Q, goodness of fit", cex = .8)')
    }
    if (exi.t["retn"] && exi.t["cctn"]){
      gof.tn <- tryCatch(gof(sim = get("cctn"), obs = get("retn"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 1/3, y = 0.9, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness of fit", cex = .8)')
    }
    if (exi.t["retp"] && exi.t["cctp"]){
      gof.tp <- tryCatch(gof(sim = get("cctp"), obs = get("retp"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 2/3, y = 0.9, legend = c(paste(names(gof.tp), gof.tp, sep = ": "),"",paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness of fit", cex = .8)')
    }
    
    # conditional: prepare regime plot call depending on data availability
    if (exi.t["rout"] && exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qobs"), col = c("blue"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotAnnualRegime(x = AnnualRegime(data.frame(date, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), type = "mean", add.legend = T, l.legend = c("Qsim"), col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
  }
  
  
  ## set up plot device with layout and call all plot commands 
  
  # define device width in inches (hard-coded for now)
  wdth <- 15
  # set device height in inches, based on layout rows
  hght <- sum(lay.heights)
  
  # create plot device, conditional on filename
  if (is.null(filename)) {
    dev.new(width=wdth, height = hght, noRStudioGD = T)
  } else {
    png(filename = paste0(filename, ".png"), width=wdth, height = hght, units = "in", res = 300, pointsize = 20)
    # close the file device on exit
    on.exit(dev.off(), add = T)
  }
  
  # layout definition
  nf <- layout(mat = lay.mat, widths = lay.widths, heights = lay.heights)
  #layout.show(nf)
  
  # plot all commands in list
  for (i in 1:length(list.plotexpr)) {
    eval(list.plotexpr[[i]])
  }
}

