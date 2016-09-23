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
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file imported with \code{\link{ReadGeoData}}.
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param gcl Data frame containing columns with SLCs and corresponding landuse and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}.
#' @param psd A data frame with HYPE point source specifications, typically a 'PointSourceData.txt' file imported with \code{\link{PointSourceData}}.
#' @param subid Integer, SUBID of sub-basin for which results are plotted. If \code{NULL} (default), a \code{subid} attribute is 
#' required in \code{x}.
#' @param desc List for use with \code{type}. Class description labels imported from a 'description.txt' file, for bar labeling.
#' See \code{\link{ReadDescription}} for formatting details.
#' @param timestep Character string, timestep of \code{x}, one of \code{"month"}, \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours). If not provided, an attribute \code{timestep} is required in \code{x}.
#' @param hype.vars Either a keyword string or a character vector of HYPE output variables. User-scpecified selection of HYPE variables 
#' to plot. Default (\code{"all"}) is to plot all variables which the function knows and which are available in \code{x}. See details 
#' for a list of known variables. Other possible keywords are \code{"hydro"} and \code{"nutrients"}, for which a pre-selected range of 
#' (available) result variables is plotted. Alternatively, a character vector holding HYPE output variables to be plotted. Variables unknown 
#' to the function will be ignored with a warning.
#' @param from,to Integer or date string of format \%F, see \code{\link{strptime}}. Time period bounds for plotting . Integers are 
#' interpreted as row indices of \code{x}.
#' @param log.q Logical, y-axis scaling for flow duration curve and discharge time series, set to \code{TRUE} for log-scaling.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year. For regime plots, see also 
#' \code{\link{AnnualRegime}}.
#' 
#' @details
#' \code{PlotBasinSummary} plots a multi-panel plot with a number of plots to evaluate model properties and performances for a 
#' chosen sub-basin. Performance plots include discharge and HYPE-modelled nutrient species for nitrogen (total, inorganic, organic) 
#' and phosphorus (total, particulate, soluble).
#' 
#' The generated plot provides information about: 
#' \itemize{
#' \item{Summarised catchment characteristics as bar charts}
#' \item{Goodness-of-fit measures for discharge and nutrients}
#' \item{Simulation-observation relationships for discharge and nutrients}
#' \item{Duration curves for flow and nutrient concentrations}
#' \item{Annual regimes for flow and nutrient concentrations}
#' \item{Concentration-discharge relationships for nutrients}
#' \item{Corresponding plots for IN/TN and SP/TP ratios}
#' }
#' 
#' Per default, the function plots from available model variables in an imported HYPE basin output file, and missing variables will be 
#' automatically omitted. Variable selection can be additionally fine-tuned using argument \code{hype.vars}.
#' 
#' Below a complete list of HYPE variables known to the function in HYPE info.txt format, ready to copy-paste into an info.txt file. 
#' For a detailed description of the variables, see the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE online documentation}.
#' 
#' \code{basinoutput variable cout rout ccin rein ccon reon cctn retn ccsp resp ccpp repp cctp retp totn totp}
#' 
#' @return 
#' Returns a multi-panel plot in a new graphics device.
#' 
#' @seealso
#' \code{\link{PlotUpstreamClasses}}, \code{\link{PlotSimObsRegime}}, \code{\link{PlotAnnualRegime}}, 
#' \code{\link{PlotDurationCurve}}, \code{\link{ReadBasinOutput}}
#' 
#' @examples
#' \dontrun{PlotBasinSummary(x = mybasin, gd = mygd, gcl = mygcl, psd = mypsd, desc = mydesc)}
#' 
#' @importFrom hydroGOF gof gof.default
#' @export


PlotBasinSummary <- function(x, filename = "plot_basin", gd = gd, bd = NULL, gcl = gcl, psd = NULL, 
                             subid = NULL, desc = NULL, timestep = attr(x, "timestep"), hype.vars = "all", 
                             from = 1, to = nrow(x), log.q = FALSE, start.mon = 10) {
  
  ## Preliminaries
  
  # get subid for which to plot summary from attribute of x if not user-provided
  if (is.null(subid)) {
    sbd <- attr(x, "subid")
    if (is.null(sbd)) {
      stop("No 'subid' attribute in 'x', please provide argument 'subid'.")
    }
  }
  # check type
  if (!is.numeric(sbd)) {
    stop("Non-numeric 'subid'.")
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
  
  
  ## calculate upstream point sources if psd supplied
  if (!is.null(psd)) {
    upsd <- UpstreamPointSources(subid = sbd, gd = gd, psd = psd, bd = bd, progbar = F)
    # calculate load in ton/year, proceed only if upstream point sources exist
    if (nrow(upsd) > 0) {
      load.psn <- weighted.mean(x = upsd$up_ps_tnconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .000365
      load.psp <- weighted.mean(x = upsd$up_ps_tpconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .000365
    } else {
      load.psn <- NA
      load.psd <- NA
    }
  } else {
    load.psn <- NA
    load.psd <- NA
  }
  
  ## calculate upstream rural household emissions if possible
  # calculate upstream loads in ton/year, proceed only if all necessary variables exist in gd
  if (all(c("loc_vol", "loc_tn", "loc_tp") %in% tolower(names(gd)))) {
    ugd <- suppressWarnings(UpstreamGeoData(subid = sbd, gd = gd, bd = bd, progbar = F))
    pos.lvol <- which(tolower(names(ugd)) == "up_loc_vol")
    pos.ltn <- which(tolower(names(ugd)) == "up_loc_tn")
    pos.ltp <- which(tolower(names(ugd)) == "up_loc_tp")
    load.rurn <- ugd[, pos.ltn] * ugd[, pos.lvol] * .00036525
    load.rurp <- ugd[, pos.ltp] * ugd[, pos.lvol] * .00036525
  } else {
    load.rurn <- NA
    load.rurp <- NA
  }
  
  ## calculate mean annual N and P loads at outlet in ton/year if they exist in x
  if (exi.t["totn"] && exi.t["cout"]) {
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
  loads.tn <- c(load.outn, load.psn, load.rurn)
  loads.tp <- c(load.outp, load.psp, load.rurp)
  
  
  ## Q axis limits for conc-Q plots
  if(exi.t["cout"]) {
    lim.q <- range(cout, na.rm = T)
  }
  
  ## parse plot commands based on existing or requested HYPE variables to a list
  ## create layout() arguments based on existinng HYPE variables
  
  # create list to hold all plot commands, and plot counter
  list.plotexpr <- list(NULL)
  cp <- 0
  
  # layout() matrix initialisation
  lay.mat <- matrix(ncol = 6, nrow = 0)
  # layout() panel widths (hard-coded for now)
  lay.widths <- c(.4, rep(1, 3), rep(.5, 2))
  # layout() panel heights initialisation
  lay.heights <- NULL
  
  
  
  #### First row: 5 panels with land use, soil, crop barplots, and load bars for tn and tp. 
  
  # fill layout matrix with panel IDs
  lay.mat <- rbind(lay.mat, 0:5)
  # add layout height for this row
  lay.heights <- c(lay.heights, 2)
  
  ## panel 1: upstream land use bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "l", progbar = F), type = "l", desc = desc, cex.names = .8)')
  
  ## panel 2: upstream soil bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "s", progbar = F), type = "s", desc = desc, cex.names = .8)')
  
  ## panel 3: upstream crop bars
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "c", progbar = F), type = "c", desc = desc, cex.names = .8)')
  
  ## panels 4 and 5: upstream TN and TP loads in ton/year
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tn / 1000, col = ColNitr(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, ifelse(all(is.na(loads.tn)), 1, max(loads.tn / 1000, na.rm = T) * 1.5)))')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ."), side = 3, at = c(.7, 1.9, 3.1), line = -.2, padj = .3, cex = .8, las = 3, adj = 1)')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TN loads", side = 1, line = .5, cex = .8)')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'box()')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tp / 1000, col = ColPhos(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, ifelse(all(is.na(loads.tp)), 1, max(loads.tp / 1000, na.rm = T) * 1.5)))')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ."), side = 3, at = c(.7, 1.9, 3.1), line = -.2, padj = .3, cex = .8, las = 3, adj = 1)')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TP loads", side = 1, line = .5, cex = .8)')
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'box()')
  
  
  
  #### Q row: 5 panels (one unused atm) with GOFs, sim-obs, FDC, and regime for Q, conditional on if Q is requested
  
  if (exi.t["rout"] || exi.t["cout"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for discharge, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- tryCatch(gof(sim = get("cout"), obs = get("rout"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                        error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), " obs.)")), bty = "n", title = "Q, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: unused
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    
    ## panel 3: sim-obs comparison dotty plot for Q
    if (exi.t["rout"] && exi.t["cout"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(rout, cout, col = "#8873FF80", pch = 16, xlab = expression(paste("observed Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 4: FDC for Q
    if (exi.t["rout"] && exi.t["cout"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(rout, cout)), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = c("obs. Q", "sim. Q"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = rout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "obs. Q", col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = cout), xscale = "gauss", yscale = ifelse(log.q, "log", "lin"), add.legend = T, l.legend = "sim. Q", col = c("red"), mar = c(3.1, 3.1, .5, .5))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    # panel 5: regimeplot for Q
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
  
  
  
  #### TN row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for TN, conditional on if TN is requested/available
  
  if (exi.t["retn"] || exi.t["cctn"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for TN, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["retn"] && exi.t["cctn"]){
      gof.tn <- tryCatch(gof(sim = get("cctn"), obs = get("retn"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                        error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["retn"] || exi.t["cctn"])) {
      if (exi.t["retn"] && exi.t["cctn"]) {
        
        # calculate y axis limits
        lim.tn <- range(c(retn, cctn), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctn, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, retn, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["retn"]) {
        
        # calculate y axis limits
        lim.tn <- range(retn, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, retn, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["cctn"]) {
        
        # calculate y axis limits
        lim.tn <- range(cctn, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctn, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for TN
    if (exi.t["cctn"] && exi.t["retn"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(retn, cctn, col = "#B57D5480", pch = 16, xlab = expression(paste("observed TN (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated TN (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for TN
    if (exi.t["retn"] && exi.t["cctn"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(retn, cctn)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. TN", "sim. TN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["retn"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = retn), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. TN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["cctn"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = cctn), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. TN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for TN
    if (exi.t["retn"] && exi.t["cctn"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "cctn", obs = "retn", start.mon = start.mon, l.legend = c("sim. TN", "obs. TN"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### IN row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for IN, conditional on if IN is requested/available
  
  if (exi.t["rein"] || exi.t["ccin"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for IN, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["rein"] && exi.t["ccin"]){
      gof.in <- tryCatch(gof(sim = get("ccin"), obs = get("rein"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.in), gof.in, sep = ": "),"",paste0("(", length(na.omit(rein)), " obs.)")), bty = "n", title = "IN, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["rein"] || exi.t["ccin"])) {
      if (exi.t["rein"] && exi.t["ccin"]) {
        
        # calculate y axis limits
        lim.in <- range(c(rein, ccin), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccin, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, rein, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["rein"]) {
        
        # calculate y axis limits
        lim.in <- range(rein, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, rein, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccin"]) {
        
        # calculate y axis limits
        lim.in <- range(ccin, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccin, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for IN
    if (exi.t["ccin"] && exi.t["rein"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(rein, ccin, col = "#B57D5480", pch = 16, xlab = expression(paste("observed IN (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated IN (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for IN
    if (exi.t["rein"] && exi.t["ccin"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(rein, ccin)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. IN", "sim. IN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["rein"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = rein), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. IN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["ccin"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccin), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. IN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for IN
    if (exi.t["rein"] && exi.t["ccin"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "ccin", obs = "rein", start.mon = start.mon, l.legend = c("sim. IN", "obs. IN"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### ON row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for ON, conditional on if ON is requested/available
  
  if (exi.t["reon"] || exi.t["ccon"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for ON, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["reon"] && exi.t["ccon"]){
      gof.on <- tryCatch(gof(sim = get("ccon"), obs = get("reon"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.on), gof.on, sep = ": "),"",paste0("(", length(na.omit(reon)), " obs.)")), bty = "n", title = "ON, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["reon"] || exi.t["ccon"])) {
      if (exi.t["reon"] && exi.t["ccon"]) {
        
        # calculate y axis limits
        lim.on <- range(c(reon, ccon), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccon, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, reon, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["reon"]) {
        
        # calculate y axis limits
        lim.on <- range(reon, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, reon, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccon"]) {
        
        # calculate y axis limits
        lim.on <- range(ccon, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccon, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for ON
    if (exi.t["ccon"] && exi.t["reon"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(reon, ccon, col = "#B57D5480", pch = 16, xlab = expression(paste("observed ON (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated ON (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for ON
    if (exi.t["reon"] && exi.t["ccon"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(reon, ccon)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. ON", "sim. ON"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["reon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = reon), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. ON", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["ccon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccon), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. ON", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for ON
    if (exi.t["reon"] && exi.t["ccon"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "ccon", obs = "reon", start.mon = start.mon, l.legend = c("sim. ON", "obs. ON"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### IN/TN row: 5 panels (first unused) with Conc-Q relationships, sim-obs, FDC, and regime for IN/TN ratio
  
  if ((exi.t["rein"] && exi.t["retn"]) || (exi.t["ccin"] && exi.t["cctn"])) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    # calculate IN/TN ratio in percent
    if (exi.t["rein"]) {
      reintn <- rein * 100 / retn
    }
    if (exi.t["ccin"]) {
      ccintn <- ccin * 100 / cctn
    }
    
    
    ## panel 1: unused atm
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    
    ## panel 2: Conc-Q plots, depending on variable availability, for IN/TN ratio
    if (exi.t["cout"] && (exists("reintn") || exists("ccintn"))) {
      if (exists("reintn") && exists("ccintn")) {
        
        # calculate y axis limits
        lim.intn <- range(c(reintn, ccintn), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccintn, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, reintn, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exists("reintn")) {
        
        # calculate y axis limits
        lim.intn <- range(reintn, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, reintn, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccon"]) {
        
        # calculate y axis limits
        lim.intn <- range(ccintn, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccintn, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for IN/TN ratio
    if (exists("reintn") && exists("ccintn")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(reintn, ccintn, col = "#B57D5480", pch = 16, xlab = "observed IN/TN ratio (%)", ylab = "simulated IN/TN ratio (%)")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for IN/TN ratio
    if (exists("reintn") && exists("ccintn")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(reintn, ccintn)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. IN/TN", "sim. IN/TN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
    } else if (exi.t["reon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = reon), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. IN/TN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
    } else if (exi.t["ccon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccon), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. IN/TN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for IN/TN ratio
    if (exists("reintn") && exists("ccintn")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = data.frame(xw[, 1], reintn, ccintn), sim = "ccintn", obs = "reintn", ts.in = timestep, start.mon = start.mon, l.legend = c("sim. IN/TN", "obs. IN/TN"), mar = c(3.1, 3.1, .5, .5), ylab = "IN/TN ratio (%)")')
    }
  }
  
  
  
  #### TP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for TP, conditional on if TP is requested/available
  
  if (exi.t["retp"] || exi.t["cctp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for TP, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["retp"] && exi.t["cctp"]){
      gof.tp <- tryCatch(gof(sim = get("cctp"), obs = get("retp"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tp), gof.tp, sep = ": "),"",paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["retp"] || exi.t["cctp"])) {
      if (exi.t["retp"] && exi.t["cctp"]) {
        
        # calculate y axis limits
        lim.tp <- range(c(retp, cctp), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, retp, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["retp"]) {
        
        # calculate y axis limits
        lim.tp <- range(retp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, retp, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["cctp"]) {
        
        # calculate y axis limits
        lim.tp <- range(cctp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for TP
    if (exi.t["cctp"] && exi.t["retp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(retp, cctp, col = "#7FAD8E80", pch = 16, xlab = expression(paste("observed TP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated TP (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for TP
    if (exi.t["retp"] && exi.t["cctp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(retp, cctp)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. TP", "sim. TP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["retp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = retp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. TP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["cctp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = cctp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. TP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for TP
    if (exi.t["retp"] && exi.t["cctp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "cctp", obs = "retp", start.mon = start.mon, l.legend = c("sim. TP", "obs. TP"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### SP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for SP, conditional on if SP is requested/available
  
  if (exi.t["resp"] || exi.t["ccsp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for SP, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["resp"] && exi.t["ccsp"]){
      gof.sp <- tryCatch(gof(sim = get("ccsp"), obs = get("resp"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.sp), gof.sp, sep = ": "),"",paste0("(", length(na.omit(resp)), " obs.)")), bty = "n", title = "SP, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["resp"] || exi.t["ccsp"])) {
      if (exi.t["resp"] && exi.t["ccsp"]) {
        
        # calculate y axis limits
        lim.sp <- range(c(resp, ccsp), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, resp, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["resp"]) {
        
        # calculate y axis limits
        lim.sp <- range(resp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, resp, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccsp"]) {
        
        # calculate y axis limits
        lim.sp <- range(ccsp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for SP
    if (exi.t["ccsp"] && exi.t["resp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(resp, ccsp, col = "#7FAD8E80", pch = 16, xlab = expression(paste("observed SP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated SP (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for SP
    if (exi.t["resp"] && exi.t["ccsp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(resp, ccsp)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. SP", "sim. SP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["resp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = resp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. SP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["ccsp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccsp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. SP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for SP
    if (exi.t["resp"] && exi.t["ccsp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "ccsp", obs = "resp", start.mon = start.mon, l.legend = c("sim. SP", "obs. SP"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### PP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for PP, conditional on if PP is requested/available
  
  if (exi.t["repp"] || exi.t["ccpp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    
    ## panel 1: compute and plot GoFs for PP, if variables are available
    # empty frame first, then GoFs as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    if (exi.t["repp"] && exi.t["ccpp"]){
      gof.pp <- tryCatch(gof(sim = get("ccpp"), obs = get("repp"), na.rm = T)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.pp), gof.pp, sep = ": "),"",paste0("(", length(na.omit(repp)), " obs.)")), bty = "n", title = "PP, goodness\nof fit", cex = 1)')
    }
    
    
    ## panel 2: Conc-Q plots, depending on variable availability
    if (exi.t["cout"] && (exi.t["repp"] || exi.t["ccpp"])) {
      if (exi.t["repp"] && exi.t["ccpp"]) {
        
        # calculate y axis limits
        lim.pp <- range(c(repp, ccpp), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccpp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, repp, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exi.t["repp"]) {
        
        # calculate y axis limits
        lim.pp <- range(repp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, repp, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccpp"]) {
        
        # calculate y axis limits
        lim.pp <- range(ccpp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccpp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for PP
    if (exi.t["ccpp"] && exi.t["repp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(repp, ccpp, col = "#7FAD8E80", pch = 16, xlab = expression(paste("observed PP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated PP (",mu,"g l"^"-1", ")")))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for PP
    if (exi.t["repp"] && exi.t["ccpp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(repp, ccpp)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. PP", "sim. PP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["repp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = repp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. PP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
    } else if (exi.t["ccpp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccpp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. PP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for PP
    if (exi.t["repp"] && exi.t["ccpp"]){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = xw, sim = "ccpp", obs = "repp", start.mon = start.mon, l.legend = c("sim. PP", "obs. PP"), mar = c(3.1, 3.1, .5, .5))')
    }
  }
  
  
  
  #### SP/TP row: 5 panels (first unused) with Conc-Q relationships, sim-obs, FDC, and regime for SP/TP ratio
  
  if ((exi.t["resp"] && exi.t["retp"]) || (exi.t["ccsp"] && exi.t["cctp"])) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    # calculate SP/TP ratio in percent
    if (exi.t["resp"]) {
      resptp <- resp * 100 / retp
    }
    if (exi.t["ccsp"]) {
      ccsptp <- ccsp * 100 / cctp
    }
    
    
    ## panel 1: unused atm
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    
    
    ## panel 2: Conc-Q plots, depending on variable availability, for SP/TP ratio
    if (exi.t["cout"] && (exists("resptp") || exists("ccsptp"))) {
      if (exists("resptp") && exists("ccsptp")) {
        
        # calculate y axis limits
        lim.sptp <- range(c(resptp, ccsptp), na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsptp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'points(cout, resptp, col = "#0000003C", pch = 16)')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }  else if (exists("resptp")) {
        
        # calculate y axis limits
        lim.sptp <- range(resptp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, resptp, col = "#0000003C", pch = 16, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      } else if (exi.t["ccpp"]) {
        
        # calculate y axis limits
        lim.sptp <- range(ccsptp, na.rm = T)
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsptp, col = "#FF00003C", pch = 16, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
        
      }
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 3: sim-obs comparison dotty plot for SP/TP ratio
    if (exists("resptp") && exists("ccsptp")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(resptp, ccsptp, col = "#7FAD8E80", pch = 16, xlab = "observed SP/TP ratio (%)", ylab = "simulated SP/TP ratio (%)")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
    }
    
    
    ## panel 4: CDC for SP/TP ratio
    if (exists("resptp") && exists("ccsptp")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = data.frame(resptp, ccsptp)), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = c("obs. SP/TP", "sim. SP/TP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
    } else if (exi.t["repp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = repp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "obs. SP/TP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
    } else if (exi.t["ccpp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotDurationCurve(ExtractFreq(data = ccpp), xscale = "gauss", yscale = "lin", add.legend = T, l.legend = "sim. SP/TP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
    
    ## panel 5: sim-obs regime plot for SP/TP ratio
    if (exists("resptp") && exists("ccsptp")){
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'PlotSimObsRegime(x = data.frame(xw[, 1], resptp, ccsptp), sim = "ccsptp", obs = "resptp", ts.in = timestep, start.mon = start.mon, l.legend = c("sim. SP/TP", "obs. SP/TP"), mar = c(3.1, 3.1, .5, .5), ylab = "SP/TP ratio (%)")')
    }
  }
  
  
  ## set up plot device with layout and call all plot commands 
  
  # define device width in inches (hard-coded for now)
  wdth <- 13
  # set device height in inches, based on layout rows
  hght <- sum(lay.heights)
  
  # create plot device, conditional on filename
  if (is.null(filename)) {
    dev.new(width=wdth, height = hght, noRStudioGD = T)
  } else {
    png(filename = paste0(filename, ".png"), width=wdth * 1.5, height = hght * 1.5, units = "in", res = 300, pointsize = 15)
    # close the file device on exit
    on.exit(dev.off(), add = T)
  }
  
  # layout definition
  nf <- layout(mat = lay.mat, widths = lay.widths, heights = lay.heights)
  # layout.show(nf)
  
  # plot all commands in list
  for (i in 1:length(list.plotexpr)) {
    eval(list.plotexpr[[i]])
  }
}


