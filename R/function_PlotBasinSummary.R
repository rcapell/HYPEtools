#' Plot a summary of model results for a single sub-basin
#' 
#' Plot a standard suite of plots summarising properties of a sub-basin including upstream area and model performance 
#' for discharge and concentrations of nutrients, sediment, and tracers.
#' 
#' @param x Data frame, with column-wise daily time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param filename String, file name for plotting to file device, see argument \code{driver}. \emph{No file extension!} Ignored with plotting 
#' to screen device. \emph{Device dimensions are currently hard-coded, see Details.}
#' @param driver String, device driver name, one of \code{default}, \code{pdf}, \code{png}, or \code{screen}.
#' Defaults to \code{default}, which plots using default plotting device \code{getOption("device")}.
#' @param panels Integer, either \code{1}, \code{2}, or \code{3}, indicating which panels to plot. See Details.
#' @param gd A data frame, containing 'SUBID', 'MAINDOWN', and 'AREA' columns, e.g. an imported 'GeoData.txt' file. 
#' Only needed with bar chart panels, see Details. 
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. 
#' Optional argument. Only needed with bar chart panels, see Details. 
#' @param gcl Data frame containing columns with SLCs and corresponding land use and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}. Only needed with bar chart panels, see Details. 
#' @param psd A data frame with HYPE point source specifications, typically a 'PointSourceData.txt' file imported with 
#' \code{\link{ReadPointSourceData}}. Only needed with bar chart panels, see Details. 
#' @param subid Integer, SUBID of sub-basin for which results are plotted. If \code{NULL} (default), a \code{subid} attribute is 
#' required in \code{x}. Only needed with bar chart panels, see Details. 
#' @param desc List for use with \code{type}. Class description labels imported from a 'description.txt' file, for bar labeling.
#' See \code{\link{ReadDescription}} for formatting details. Only needed with bar chart panels, see Details. 
#' @param timestep Character string, timestep of \code{x}, one of \code{"month"}, \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours). If not provided, an attribute \code{timestep} is required in \code{x}.
#' @param hype.vars Either a keyword string or a character vector of HYPE output variables. User-specified selection of HYPE variables 
#' to plot. Default (\code{"all"}) is to plot all variables which the function knows and which are available in \code{x}. See details 
#' for a list of known variables. Other possible keywords are \code{"hydro"} and \code{"wq"} (water quality), for which a pre-selected range of 
#' (available) result variables is plotted. Alternatively, a character vector holding HYPE output variable IDs to be plotted. Variables unknown 
#' to the function will be ignored with a warning.
#' @param from,to Integer or date string of format \%F, see \code{\link{strptime}}. Time period bounds for plotting . Integers are 
#' interpreted as row indices of \code{x}.
#' @param log Logical, log scaling discharge and concentrations.
#' @param xscale Character string, keyword for x-axis scaling. Either \code{"lin"} for linear scaling or \code{"gauss"} for gaussian scaling. 
#' See description in \code{\link{PlotDurationCurve}}.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year. For regime plots, see also 
#' \code{\link{AnnualRegime}}. 
#' @param name Character or expression string. Site name to plot besides bar chart panels. Only relevant with \code{panels} \code{1} or \code{3}.
#' @param ylab.t1 String or \code{\link{plotmath}} expression, y axis label for T1 tracer time series panel (tracer concentration units 
#' are not prescribed in HYPE).
#' 
#' @details
#' \code{PlotBasinSummary} plots a multi-panel plot with a number of plots to evaluate model properties and performances for a 
#' chosen sub-basin. Performance plots include discharge, HYPE-modelled nutrient species for nitrogen (total, inorganic, organic) 
#' and phosphorus (total, particulate, soluble), and HYPE modelled suspended and total sediment concentrations.
#' 
#' Plotted panels show: 
#' \itemize{
#' \item{\emph{Summarised catchment characteristics as bar charts}: Upstream-averaged land use, soil, and crop group fractions; modelled nutrient 
#' loads in sub-basin outlet, and summed upstream gross loads from point sources and rural households (if necessary variables available, omitted 
#' otherwise).}
#' \item{\emph{Goodness-of-fit measures for discharge and concentrations}: KGE (Kling-Gupta Efficiency), NSE (Nash-Sutcliffe Efficiency), PBIAS 
#' (Percentage Bias, aka relative error), MAE (Mean Absolute Error), r (Pearson product-moment correlation coefficient), VE (Volumetric Efficiency). 
#' See package \code{\link{hydroGOF}} for details.}
#' \item{\emph{Simulation-observation relationships for discharge and concentrations}: Simulated and observed concentration-discharge relationships, 
#' relationship between observed and simulated nutrient, sediment, and tracer concentrations.}
#' \item{\emph{Duration curves for flow and concentrations}: Pairwise simulated and observed curves.}
#' \item{\emph{Annual regimes for flow and concentrations}: Pairwise simulated and observed regime plots at monthly aggregation, with 
#' number of observations for concentration regimes.}
#' \item{\emph{Corresponding plots for IN/TN and SP/TP ratios}.}
#' }
#' 
#' Per default, the function plots from available model variables in an imported HYPE basin output file, and missing variables will be 
#' automatically omitted. Variable selection can be additionally fine-tuned using argument \code{hype.vars}.
#' 
#' Argument \code{panels} allows to choose if bar chart panels should be plotted. This can be time-consuming for sites with many upstream 
#' sub-basins and might not necessary e.g. during calibration. If \code{1} (default), all panels are plotted. If set to \code{2}, bar 
#' charts will be excluded. If \code{3}, only bar charts will be plotted. Arguments \code{gd}, \code{bd}, \code{gcl}, \code{psd}, \code{subid}, 
#' and \code{desc} are only needed for bar chart plotting.
#' 
#' Below a complete list of HYPE variables known to the function in HYPE info.txt format, ready to copy-paste into an info.txt file. 
#' For a detailed description of the variables, see the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE online documentation}.
#' 
#' \code{basinoutput variable cout rout ccin rein ccon reon cctn retn ccsp resp ccpp repp cctp retp ctnl ctpl ccss ress ccts rets cct1 ret1}
#' 
#' #' \emph{Device dimensions} are hard-coded to a width of 13 inches and height depending on the number of plotted time series. When plotting 
#' to a screen device, a maximum height of 10 inches is enforced in order to prevent automatic resizing with slow redrawing. 
#' \code{PlotBasinOutput} throws a warning if the plot height exceeds 10 inches, which can lead to overlapping plot elements. On screens with 
#' less than 10 inch screen height, redrawing is inhibited, which can lead to an empty plot. The recommended solution for both effects 
#' is to plot to pdf or png file devices instead.
#' 
#' @return 
#' Returns a multi-panel plot in a new graphics device.
#' 
#' @seealso
#' \code{\link{PlotBasinOutput}}, \code{\link{BarplotUpstreamClasses}}, \code{\link{PlotSimObsRegime}}, \code{\link{PlotAnnualRegime}}, 
#' \code{\link{PlotDurationCurve}}, \code{\link{ReadBasinOutput}}
#' 
#' @examples
#' # Source data, HYPE basin output with a number of result variables
#' te1 <- ReadBasinOutput(filename = system.file("demo_model", "results", "0003587.txt", 
#'                        package = "HYPEtools"))
#' te2 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' 
# Screen devices should not be used in examples
#' \dontrun{
#' # Plot basin summary for discharge on screen device
#' PlotBasinSummary(x = te1, gd = te2, driver = "screen", panels = 2)
#' }
#' 
#' @importFrom hydroGOF gof gof.default
#' @importFrom stats weighted.mean
#' @importFrom grDevices dev.new png dev.off cairo_pdf 
#' @export


PlotBasinSummary <- function(x, filename, driver = c("default", "pdf", "png", "screen"), panels = 1, gd = NULL, bd = NULL, gcl = NULL, psd = NULL, 
                             subid = NULL, desc = NULL, timestep = attr(x, "timestep"), hype.vars = "all", 
                             from = 1, to = nrow(x), log = FALSE, xscale = "gauss", start.mon = 10, name = "", ylab.t1 = "Conc.") {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  ## Preliminaries
  
  # Assign NULL value to all potential data variables extracted from 'x' further below
  # This to get rid of 'no visible binding for global variable'-notes with 'Check Package'
  # See Option Two here: https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  date <- cout <- rout <- ccin <- rein <- ccon <- reon <- cctn <- retn <- ccsp <- resp <- 
    ccpp <- repp <- cctp <- retp <- ctnl <- ctpl <- ccss <- ress <- ccts <- rets <- cct1 <- ret1 <- NULL
  
  # check and choose device driver
  driver <- match.arg(driver)
  if (driver %in% c("pdf", "png")) {
    filename <- paste(filename, driver, sep = ".")
  } else {
    filename <- NULL
  }
  
  # check if panels argument is ok
  if (length(panels) != 1 && !(panels %in% 1:3)) {
    stop("Wrong 'panels' specification.")
  }
  # check if gcl is specified if bar plots are requested
  if (is.null(gcl) && (panels %in% c(1, 3))) {
    stop("Argument 'gcl' required for bar plot panels.")
  }
  # check if gd is specified if bar plots are requested
  if (is.null(gd) && (panels %in% c(1, 3))) {
    stop("Argument 'gd' required for bar plot panels.")
  }
  
  
  # set axis scaling parameters, conditional on arguments
  if (log) {
    # for duration curve plots
    yscale = "log"
    # for conc-q and sim-obs dotty plots
    log.cq <- "xy"
    # for regime plots
    log.r <- log
  } else {
    # for duration curve plots
    yscale = "lin"
    # for conc-q and sim-obs dotty pplots
    log.cq <- ""
    # for regime plots
    log.r <- log
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
    tw.d <- strptime(to, format = "%F", tz = "UTC")
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
  xlab.regime <- paste(format(range(xw[, 1], na.rm = TRUE), "%Y"), collapse = " to ")
  # if selected time window is longer than time series in x, extend date column
  if (exists("date.plot")) {
    xw[, 1] <- date.plot[fw:length(date.plot)]
  }
  
  
  
  ## identify column indices of target variables and total number of variables to plot
  
  # force lower case for names in basin output file, for selecting target variables below
  names(xw) <- tolower(names(xw))
  
  # create vector over all target output variable names which are potentially used in the plot panels
  nm.t <- c("date", "cout", "rout", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", 
            "repp", "cctp", "retp", "ctnl", "ctpl", "ccss", "ress", "ccts", "rets", "cct1", "ret1")
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
    } else if (hype.vars[1] == "wq") {
      nm.wq <- c("date", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", "repp", 
                    "cctp", "retp", "ctnl", "ctpl", "cout", "ccss", "ress", "ccts", "rets", "cct1", "ret1")
      exi.t[!(nm.t %in% nm.wq)] <- FALSE
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
  
  
  ## calculate upstream info for bar charts and sub-basin info, conditional on panels
  
  if (panels %in% c(1, 3)) {
    # get subid for which to plot summary from attribute of x if not user-provided
    if (is.null(subid)) {
      sbd <- attr(x, "subid")
      if (is.null(sbd)) {
        stop("No 'subid' attribute in 'x', please provide argument 'subid'.")
      }
    } else {
      sbd <- subid
    }
    # check type
    if (!is.numeric(sbd)) {
      stop("Non-numeric 'subid'.")
    }
    
    ## calculate upstream point sources if psd supplied
    if (!is.null(psd)) {
      upsd <- UpstreamPointSources(subid = sbd, gd = gd, psd = psd, bd = bd, progbar = FALSE)
      # calculate load in ton/year, proceed only if upstream point sources exist
      if (nrow(upsd) > 0) {
        # conc mg/l * vol m3/d * 10^3 l/m3 * 365.25 d/y * 10^-9 ton/mg
        load.psn <- weighted.mean(x = upsd$up_ps_tnconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .00036525
        load.psp <- weighted.mean(x = upsd$up_ps_tpconc, w = upsd$up_ps_vol) * sum(upsd$up_ps_vol) * .00036525
      } else {
        load.psn <- 0
        load.psp <- 0
      }
    } else {
      load.psn <- NA
      load.psp <- NA
    }
    
    ## calculate upstream rural household emissions if possible
    # calculate upstream loads in ton/year, proceed only if all necessary variables exist in gd
    if (all(c("loc_vol", "loc_tn", "loc_tp") %in% tolower(names(gd)))) {
      ugd <- suppressWarnings(UpstreamGeoData(subid = sbd, gd = gd, bd = bd, progbar = FALSE))
      pos.lvol <- which(tolower(names(ugd)) == "up_loc_vol")
      pos.ltn <- which(tolower(names(ugd)) == "up_loc_tn")
      pos.ltp <- which(tolower(names(ugd)) == "up_loc_tp")
      # conc mg/l * vol m3/d * 10^3 l/m3 * 365.25 d/y * 10^-9 ton/mg
      load.rurn <- ugd[, pos.ltn] * ugd[, pos.lvol] * .00036525
      load.rurp <- ugd[, pos.ltp] * ugd[, pos.lvol] * .00036525
    } else {
      load.rurn <- NA
      load.rurp <- NA
    }
    
    ## calculate mean annual N and P loads at outlet in ton/year if they exist in x
    if (exi.t["ctnl"]) {
      # mean(load kg/d) * 365.25 d/y * 10^-3 ton/kg
      load.outn <- mean(ctnl) * 0.36525
    } else {
      load.outn <- NA
    }
    if (exi.t["ctpl"]) {
      load.outp <- mean(ctpl) * 0.36525
    } else {
      load.outp <- NA
    }
    
    # combine all loads
    loads.tn <- c(load.outn, load.psn, load.rurn)
    loads.tp <- c(load.outp, load.psp, load.rurp)
    
    # upstream area in km2 and number of subids
    uarea <- signif(SumUpstreamArea(subid = sbd, gd = gd, bd = bd)[1, 2] / 10^6, digits = 2)
    nsbd <- length(AllUpstreamSubids(subid = sbd, gd = gd, bd = bd))
  }
 
  
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  # Axis limits
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  # Q axis limits for conc-Q plots
  if(exi.t["cout"]) {
    lim.q <- range(cout, na.rm = TRUE)
    # change lower limit to >0 if log-scale
    if (log && lim.q[1] <= 0) {
      lim.q[1] <- min(cout[cout > 0])
    }
  }
  

  
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  # Parse plot commands based on existing or requested HYPE variables to a list
  # Create layout() arguments based on existing HYPE variables
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  # create list to hold all plot commands, and plot counter
  list.plotexpr <- list(NULL)
  cp <- 0
  
  # layout() matrix initialisation, with dummy row which is removed before plotting below
  lay.mat <- matrix(ncol = 6, nrow = 1, 0)
  # layout() panel widths (hard-coded for now)
  lay.widths <- c(.4, rep(1, 3), rep(.5, 2))
  # layout() panel heights initialisation
  lay.heights <- NULL
  
  
  
  #### First row: 5 panels with land use, soil, crop barplots, and load bars for tn and tp
  #### Conditional on panels value
  
  if (panels %in% c(1, 3)) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, 1:6)
    # add layout height for this row
    lay.heights <- c(lay.heights, 2)
    
    ## panel 1: plot basin infos
    # empty frame first, then infos as legend
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'frame()')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(as.expression(bquote("Area (km"^2*"):"~.(uarea))), as.expression(bquote("Sub-basins:"~.(nsbd)))), bty = "n", title = name, cex = 1)')
    
    ## panel 2: upstream land use bars
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = '.BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "l", progbar = F), type = "l", desc = desc, cex.names = .8, col = .makeTransparent("red", 150))')
    
    ## panel 3: upstream soil bars
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = '.BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "s", progbar = F), type = "s", desc = desc, cex.names = .8, col = .makeTransparent("red", 150))')
    
    ## panel 4: upstream crop bars
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = '.BarplotUpstreamClasses(x = UpstreamGroupSLCClasses(sbd, gd = gd, bd = bd, gc = gcl, type = "cr", progbar = F), type = "cr", desc = desc, cex.names = .8, col = .makeTransparent("red", 150))')
    
    ## panels 5 and 6: upstream TN and TP loads in ton/year
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tn / 1000, col = ColNitr(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, ifelse(all(is.na(loads.tn)), 1, max(loads.tn / 1000, na.rm = T) * 1.5)))')
    if (!all(is.na(loads.tn))) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ.")[!is.na(loads.tn)], side = 3, at = c(.7, 1.9, 3.1)[!is.na(loads.tn)], line = -.2, padj = .3, cex = .8, las = 3, adj = 1)')
    }
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TN loads", side = 1, line = .5, cex = .8)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(1.5, 3, .5, .5) + .1, mgp = c(1.5, .3, 0),  tcl = NA, xaxs = "i")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'barplot(loads.tp / 1000, col = ColPhos(3), border = NA, ylab = expression(paste("kiloton y"^"-1")), ylim = c(0, ifelse(all(is.na(loads.tp)), 1, max(loads.tp / 1000, na.rm = T) * 1.5)))')
    if (!all(is.na(loads.tp))) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'mtext(text = c("River", "Point srces.", "Rural househ.")[!is.na(loads.tp)], side = 3, at = c(.7, 1.9, 3.1)[!is.na(loads.tp)], line = -.2, padj = .3, cex = .8, las = 3, adj = 1)')
    }
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext("Modeled TP loads", side = 1, line = .5, cex = .8)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
    
  }
  
  
  #### remaining panels, conditional on panels value
  
  if(panels %in% 1:2) {
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # Q row: 5 panels (one unused atm) with GOFs, sim-obs, FDC, and regime for Q, conditional on if Q is requested
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.q <- tryCatch(gof(sim = get("cout"), obs = get("rout"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
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
        list.plotexpr[[cp]] <- parse(text = 'plot(rout, cout, col = "#8873FF80", pch = 16, log = log.cq, xlab = expression(paste("observed Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 4: FDC for Q
      if (exi.t["rout"] && exi.t["cout"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(rout, cout)), xscale = xscale, yscale =yscale, add.legend = T, l.legend = c("obs. Q", "sim. Q"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["rout"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = rout), xscale = xscale, yscale =yscale, add.legend = T, l.legend = "obs. Q", col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["cout"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = cout), xscale = xscale, yscale =yscale, add.legend = T, l.legend = "sim. Q", col = c("red"), mar = c(3.1, 3.1, .5, .5))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      # panel 5: regimeplot for Q
      if (exi.t["rout"] && exi.t["cout"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", log = log.r, add.legend = T, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
      } else if (exi.t["rout"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout), ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", log = log.r, add.legend = T, l.legend = c("Qobs"), col = c("blue"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
      } else if (exi.t["cout"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, cout), ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", log = log.r, add.legend = T, l.legend = c("Qsim"), col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # TN row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for TN, conditional on if TN is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.tn <- tryCatch(gof(sim = get("cctn"), obs = get("retn"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["retn"] || exi.t["cctn"])) {
        if (exi.t["retn"] && exi.t["cctn"]) {
          
          # calculate y axis limits
          lim.tn <- range(c(retn, cctn), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tn[1] <= 0) {
            lim.tn[1] <- min(c(retn, cctn)[c(retn, cctn) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tn))) {
              lim.tn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctn, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, retn, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["retn"]) {
          
          # calculate y axis limits
          lim.tn <- range(retn, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tn[1] <= 0) {
            lim.tn[1] <- min(retn[retn > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tn))) {
              lim.tn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, retn, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["cctn"]) {
          
          # calculate y axis limits
          lim.tn <- range(cctn, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tn[1] <= 0) {
            lim.tn[1] <- min(cctn[cctn > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tn))) {
              lim.tn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctn, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.tn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(retn, cctn, col = "#B57D54E6", pch = 16, log = log.cq, xlab = expression(paste("observed TN (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated TN (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for TN
      if (exi.t["retn"] && exi.t["cctn"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(retn, cctn)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. TN", "sim. TN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["retn"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = retn), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. TN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["cctn"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = cctn), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. TN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TN conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for TN
      if (exi.t["retn"] && exi.t["cctn"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cctn", obs = "retn", start.mon = start.mon, log = log.r, l.legend = c("sim. TN", "obs. TN"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["retn"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "retn", start.mon = start.mon, log = log.r, l.legend = c("obs. TN"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["cctn"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cctn", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. TN"), mar = c(3.1, 3.1, .5, .5))')
      } 
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # IN row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for IN, conditional on if IN is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.in <- tryCatch(gof(sim = get("ccin"), obs = get("rein"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.in), gof.in, sep = ": "),"",paste0("(", length(na.omit(rein)), " obs.)")), bty = "n", title = "IN, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots for IN, depending on variable availability
      if (exi.t["cout"] && (exi.t["rein"] || exi.t["ccin"])) {
        if (exi.t["rein"] && exi.t["ccin"]) {
          
          # calculate y axis limits
          lim.in <- range(c(rein, ccin), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.in[1] <= 0) {
            lim.in[1] <- min(c(rein, ccin)[c(rein, ccin) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.in))) {
              lim.in <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccin, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, rein, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["rein"]) {
          
          # calculate y axis limits
          lim.in <- range(rein, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.in[1] <= 0) {
            lim.in[1] <- min(rein[rein > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.in))) {
              lim.in <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, rein, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccin"]) {
          
          # calculate y axis limits
          lim.in <- range(ccin, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.in[1] <= 0) {
            lim.in[1] <- min(ccin[ccin > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.in))) {
              lim.in <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccin, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.in, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(rein, ccin, col = "#B57D54E6", pch = 16, log = log.cq, xlab = expression(paste("observed IN (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated IN (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for IN
      if (exi.t["rein"] && exi.t["ccin"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(rein, ccin)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. IN", "sim. IN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["rein"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = rein), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. IN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["ccin"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccin), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. IN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("IN conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for IN
      if (exi.t["rein"] && exi.t["ccin"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccin", obs = "rein", start.mon = start.mon, log = log.r, l.legend = c("sim. IN", "obs. IN"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["rein"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "rein", start.mon = start.mon, log = log.r, l.legend = c("obs. IN"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["ccin"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccin", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. IN"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # ON row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for ON, conditional on if ON is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.on <- tryCatch(gof(sim = get("ccon"), obs = get("reon"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.on), gof.on, sep = ": "),"",paste0("(", length(na.omit(reon)), " obs.)")), bty = "n", title = "ON, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["reon"] || exi.t["ccon"])) {
        if (exi.t["reon"] && exi.t["ccon"]) {
          
          # calculate y axis limits
          lim.on <- range(c(reon, ccon), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.on[1] <= 0) {
            lim.on[1] <- min(c(reon, ccon)[c(reon, ccon) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.on))) {
              lim.on <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccon, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, reon, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["reon"]) {
          
          # calculate y axis limits
          lim.on <- range(reon, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.on[1] <= 0) {
            lim.on[1] <- min(reon[reon > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.on))) {
              lim.on <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, reon, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccon"]) {
          
          # calculate y axis limits
          lim.on <- range(ccon, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.on[1] <= 0) {
            lim.on[1] <- min(ccon[ccon > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.on))) {
              lim.on <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccon, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.on, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(reon, ccon, col = "#B57D54E6", pch = 16, log = log.cq, xlab = expression(paste("observed ON (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated ON (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for ON
      if (exi.t["reon"] && exi.t["ccon"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(reon, ccon)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. ON", "sim. ON"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["reon"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = reon), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. ON", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["ccon"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccon), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. ON", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("ON conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for ON
      if (exi.t["reon"] && exi.t["ccon"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccon", obs = "reon", start.mon = start.mon, log = log.r, l.legend = c("sim. ON", "obs. ON"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["reon"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "reon", start.mon = start.mon, log = log.r, l.legend = c("obs. ON"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["ccon"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccon", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. ON"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # IN/TN row: 5 panels (first unused) with Conc-Q relationships, sim-obs, FDC, and regime for IN/TN ratio
    #--------------------------------------------------------------------------------------------------------------------------------------
    
    if ((exi.t["rein"] && exi.t["retn"]) || (exi.t["ccin"] && exi.t["cctn"])) {
      
      # fill layout matrix with panel IDs
      lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
      # add layout height for this row
      lay.heights <- c(lay.heights, 2)
      
      # calculate IN/TN ratio in percent
      if (exi.t["rein"] && exi.t["retn"]) {
        reintn <- rein * 100 / retn
        # remove vector if it does not contain any data, i.e. no days with concurrent observations
        if (!any(!is.na(reintn))) {
          rm(reintn)
        }
      }
      if (exi.t["ccin"] && exi.t["cctn"]) {
        ccintn <- ccin * 100 / cctn
      }
      
      
      ## panel 1: row title
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(1, 4))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'mtext(text = "IN/TN ratio", line = -1, adj = 0, cex = .7)')
      
      
      
      ## panel 2: Conc-Q plots, depending on variable availability, for IN/TN ratio
      if (exi.t["cout"] && (exists("reintn") || exists("ccintn"))) {
        if (exists("reintn") && exists("ccintn")) {
          
          # calculate y axis limits
          lim.intn <- range(c(reintn, ccintn), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.intn[1] <= 0) {
            lim.intn[1] <- min(c(reintn, ccintn)[c(reintn, ccintn) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.intn))) {
              lim.intn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccintn, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, reintn, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exists("reintn")) {
          
          # calculate y axis limits
          lim.intn <- range(reintn, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.intn[1] <= 0) {
            lim.intn[1] <- min(reintn[reintn > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.intn))) {
              lim.intn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, reintn, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccon"]) {
          
          # calculate y axis limits
          lim.intn <- range(ccintn, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.intn[1] <= 0) {
            lim.intn[1] <- min(ccintn[ccintn > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.intn))) {
              lim.intn <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccintn, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.intn, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "IN/TN ratio (%)")')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(reintn, ccintn, col = "#6B0601C6", pch = 16, log = log.cq, xlab = "observed IN/TN ratio (%)", ylab = "simulated IN/TN ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for IN/TN ratio
      if (exists("reintn") && exists("ccintn")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(reintn, ccintn)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. IN/TN", "sim. IN/TN"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
      } else if (exi.t["reon"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = reon), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. IN/TN", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
      } else if (exi.t["ccon"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccon), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. IN/TN", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "IN/TN ratio (%)")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for IN/TN ratio
      if (exists("reintn") && exists("ccintn")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], reintn, ccintn), sim = "ccintn", obs = "reintn", ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("sim. IN/TN", "obs. IN/TN"), mar = c(3.1, 3.1, .5, .5), ylab = "IN/TN ratio (%)")')
      } else if (exists("reintn")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], reintn), sim = NULL, obs = "reintn", ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("obs. IN/TN"), mar = c(3.1, 3.1, .5, .5), ylab = "IN/TN ratio (%)")')
      } else if (exists("ccintn")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], ccintn), sim = "ccintn", obs = NULL, ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("sim. IN/TN"), mar = c(3.1, 3.1, .5, .5), ylab = "IN/TN ratio (%)")')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # TP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for TP, conditional on if TP is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.tp <- tryCatch(gof(sim = get("cctp"), obs = get("retp"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.tp), gof.tp, sep = ": "),"",paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["retp"] || exi.t["cctp"])) {
        if (exi.t["retp"] && exi.t["cctp"]) {
          
          # calculate y axis limits
          lim.tp <- range(c(retp, cctp), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tp[1] <= 0) {
            lim.tp[1] <- min(c(retp, cctp)[c(retp, cctp) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tp))) {
              lim.tp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, retp, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["retp"]) {
          
          # calculate y axis limits
          lim.tp <- range(retp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tp[1] <= 0) {
            lim.tp[1] <- min(retp[retp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tp))) {
              lim.tp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, retp, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["cctp"]) {
          
          # calculate y axis limits
          lim.tp <- range(cctp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.tp[1] <= 0) {
            lim.tp[1] <- min(cctp[cctp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.tp))) {
              lim.tp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cctp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.tp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(retp, cctp, col = "#7FAD8EE6", pch = 16, log = log.cq, xlab = expression(paste("observed TP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated TP (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for TP
      if (exi.t["retp"] && exi.t["cctp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(retp, cctp)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. TP", "sim. TP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["retp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = retp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. TP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["cctp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = cctp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. TP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("TP conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for TP
      if (exi.t["retp"] && exi.t["cctp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cctp", obs = "retp", start.mon = start.mon, log = log.r, l.legend = c("sim. TP", "obs. TP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["retp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "retp", start.mon = start.mon, log = log.r, l.legend = c("obs. TP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["cctp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cctp", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. TP"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # SP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for SP, conditional on if SP is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.sp <- tryCatch(gof(sim = get("ccsp"), obs = get("resp"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.sp), gof.sp, sep = ": "),"",paste0("(", length(na.omit(resp)), " obs.)")), bty = "n", title = "SP, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["resp"] || exi.t["ccsp"])) {
        if (exi.t["resp"] && exi.t["ccsp"]) {
          
          # calculate y axis limits
          lim.sp <- range(c(resp, ccsp), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sp[1] <= 0) {
            lim.sp[1] <- min(c(resp, ccsp)[c(resp, ccsp) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sp))) {
              lim.sp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, resp, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["resp"]) {
          
          # calculate y axis limits
          lim.sp <- range(resp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sp[1] <= 0) {
            lim.sp[1] <- min(resp[resp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sp))) {
              lim.sp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, resp, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccsp"]) {
          
          # calculate y axis limits
          lim.sp <- range(ccsp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sp[1] <= 0) {
            lim.sp[1] <- min(ccsp[ccsp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sp))) {
              lim.sp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.sp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(resp, ccsp, col = "#7FAD8EE6", pch = 16, log = log.cq, xlab = expression(paste("observed SP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated SP (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for SP
      if (exi.t["resp"] && exi.t["ccsp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(resp, ccsp)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. SP", "sim. SP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["resp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = resp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. SP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["ccsp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccsp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. SP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("SP conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for SP
      if (exi.t["resp"] && exi.t["ccsp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccsp", obs = "resp", start.mon = start.mon, log = log.r, l.legend = c("sim. SP", "obs. SP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["resp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "resp", start.mon = start.mon, log = log.r, l.legend = c("obs. SP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["ccsp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccsp", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. SP"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
    
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # PP row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for PP, conditional on if PP is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
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
        gof.pp <- tryCatch(gof(sim = get("ccpp"), obs = get("repp"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, legend = c(paste(names(gof.pp), gof.pp, sep = ": "),"",paste0("(", length(na.omit(repp)), " obs.)")), bty = "n", title = "PP, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["repp"] || exi.t["ccpp"])) {
        if (exi.t["repp"] && exi.t["ccpp"]) {
          
          # calculate y axis limits
          lim.pp <- range(c(repp, ccpp), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.pp[1] <= 0) {
            lim.pp[1] <- min(c(repp, ccpp)[c(repp, ccpp) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.pp))) {
              lim.pp <- rep(0, 2)
            }
          }
          
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccpp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, repp, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["repp"]) {
          
          # calculate y axis limits
          lim.pp <- range(repp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.pp[1] <= 0) {
            lim.pp[1] <- min(repp[repp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.pp))) {
              lim.pp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, repp, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccpp"]) {
          
          # calculate y axis limits
          lim.pp <- range(ccpp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.pp[1] <= 0) {
            lim.pp[1] <- min(ccpp[ccpp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.pp))) {
              lim.pp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccpp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.pp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(repp, ccpp, col = "#7FAD8EE6", pch = 16, log = log.cq, xlab = expression(paste("observed PP (",mu,"g l"^"-1", ")")), ylab = expression(paste("simulated PP (",mu,"g l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for PP
      if (exi.t["repp"] && exi.t["ccpp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(repp, ccpp)), xscale = xscale, yscale = yscale, add.legend = T, l.legend = c("obs. PP", "sim. PP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["repp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = repp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "obs. PP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
      } else if (exi.t["ccpp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccpp), xscale = xscale, yscale = yscale, add.legend = T, l.legend = "sim. PP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Concentration exceedance percentile", ylab = expression(paste("PP conc. (",mu,"g l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for PP
      if (exi.t["repp"] && exi.t["ccpp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccpp", obs = "repp", start.mon = start.mon, log = log.r, l.legend = c("sim. PP", "obs. PP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["repp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "repp", start.mon = start.mon, log = log.r, l.legend = c("obs. PP"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["ccpp"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccpp", obs = NULL, start.mon = start.mon, log = log.r, l.legend = c("sim. PP"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # SP/TP row: 5 panels (first unused) with Conc-Q relationships, sim-obs, FDC, and regime for SP/TP ratio
    #--------------------------------------------------------------------------------------------------------------------------------------
    
    if ((exi.t["resp"] && exi.t["retp"]) || (exi.t["ccsp"] && exi.t["cctp"])) {
      
      # fill layout matrix with panel IDs
      lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
      # add layout height for this row
      lay.heights <- c(lay.heights, 2)
      
      # calculate SP/TP ratio in percent
      if (exi.t["resp"] && exi.t["retp"]) {
        resptp <- resp * 100 / retp
        # remove vector if it does not contain any data, i.e. no days with concurrent observations
        if (!any(!is.na(resptp))) {
          rm(resptp)
        }
      }
      if (exi.t["ccsp"] && exi.t["cctp"]) {
        ccsptp <- ccsp * 100 / cctp
      }
      
      
      ## panel 1: row title
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(1, 4))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'mtext(text = "SP/TP ratio", line = -1, adj = 0, cex = .7)')
      
      
      ## panel 2: Conc-Q plots, depending on variable availability, for SP/TP ratio
      if (exi.t["cout"] && (exists("resptp") || exists("ccsptp"))) {
        if (exists("resptp") && exists("ccsptp")) {
          
          # calculate y axis limits
          lim.sptp <- range(c(resptp, ccsptp), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sptp[1] <= 0) {
            lim.sptp[1] <- min(c(resptp, ccsptp)[c(resptp, ccsptp) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sptp))) {
              lim.sptp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsptp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, resptp, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exists("resptp")) {
          
          # calculate y axis limits
          lim.sptp <- range(resptp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sptp[1] <= 0) {
            lim.sptp[1] <- min(resptp[resptp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sptp))) {
              lim.sptp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, resptp, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccpp"]) {
          
          # calculate y axis limits
          lim.sptp <- range(ccsptp, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.sptp[1] <= 0) {
            lim.sptp[1] <- min(ccsptp[ccsptp > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.sptp))) {
              lim.sptp <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccsptp, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.sptp, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = "SP/TP ratio (%)")')
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
        list.plotexpr[[cp]] <- parse(text = 'plot(resptp, ccsptp, col = "#226633E6", pch = 16, log = log.cq, xlab = "observed SP/TP ratio (%)", ylab = "simulated SP/TP ratio (%)")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for SP/TP ratio
      if (exists("resptp") && exists("ccsptp")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(resptp, ccsptp)), xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = c("obs. SP/TP", "sim. SP/TP"), col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
      } else if (exi.t["repp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = repp), xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = "obs. SP/TP", col = c("black"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
      } else if (exi.t["ccpp"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccpp), xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = "sim. SP/TP", col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = "Exceedance percentile", ylab = "SP/TP ratio (%)")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for SP/TP ratio
      if (exists("resptp") && exists("ccsptp")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], resptp, ccsptp), sim = "ccsptp", obs = "resptp", ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("sim. SP/TP", "obs. SP/TP"), mar = c(3.1, 3.1, .5, .5), ylab = "SP/TP ratio (%)")')
      } else if (exists("resptp")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], resptp), sim = NULL, obs = "resptp", ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("obs. SP/TP"), mar = c(3.1, 3.1, .5, .5), ylab = "SP/TP ratio (%)")')
      } else if (exists("ccsptp")){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = data.frame(xw[, 1], ccsptp), sim = "ccsptp", obs = NULL, ts.in = timestep, start.mon = start.mon, log = log.r, l.legend = c("sim. SP/TP"), mar = c(3.1, 3.1, .5, .5), ylab = "SP/TP ratio (%)")')
      }
    }
    
    
    
    #--------------------------------------------------------------------------------------------------------------------------------------
    # SS row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for SS, conditional on if SS is requested/available
    #--------------------------------------------------------------------------------------------------------------------------------------
    
    if (exi.t["ress"] || exi.t["ccss"]) {
      
      # fill layout matrix with panel IDs
      lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
      # add layout height for this row
      lay.heights <- c(lay.heights, 2)
      
      
      ## panel 1: compute and plot GoFs for SS, if variables are available
      # empty frame first, then GoFs as legend
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
      
      if (exi.t["ress"] && exi.t["ccss"]){
        gof.ss <- tryCatch(gof(sim = get("ccss"), obs = get("ress"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, 
                                     legend = c(paste(names(gof.ss), gof.ss, sep = ": "),"",
                                     paste0("(", length(na.omit(ress)), " obs.)")), bty = "n", 
                                     title = "SS, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["ress"] || exi.t["ccss"])) {
        if (exi.t["ress"] && exi.t["ccss"]) {
          
          # calculate y axis limits
          lim.ss <- range(c(ress, ccss), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ss[1] <= 0) {
            lim.ss[1] <- min(c(ress, ccss)[c(ress, ccss) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ss))) {
              lim.ss <- rep(0, 2)
            }
          }
          
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccss, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.ss, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, ress, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["ress"]) {
          
          # calculate y axis limits
          lim.ss <- range(ress, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ss[1] <= 0) {
            lim.ss[1] <- min(ress[ress > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ss))) {
              lim.ss <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ress, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.ss, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["ccss"]) {
          
          # calculate y axis limits
          lim.ss <- range(ccss, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ss[1] <= 0) {
            lim.ss[1] <- min(ccss[ccss > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ss))) {
              lim.ss <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccss, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, 
                                       xlim = lim.q, ylim = lim.ss, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), 
                                       ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, 
                                       col = c("#FF000080", "#00000080"), bty = "n")')
          
        }
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 3: sim-obs comparison dotty plot for SS
      if (exi.t["ccss"] && exi.t["ress"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(ress, ccss, col = "#7FAD8EE6", pch = 16, log = log.cq, 
                                     xlab = expression(paste("observed SS (mg l"^"-1", ")")), 
                                     ylab = expression(paste("simulated SS (mg l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for SS
      if (exi.t["ress"] && exi.t["ccss"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(ress, ccss)), 
                                     xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = c("obs. SS", "sim. SS"), 
                                     col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ress"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ress), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "obs. SS", col = c("black"), mar = c(3.1, 3.1, .5, .5), 
                                     ylim = lim.ss, 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ccss"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccss), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "sim. SS", col = c("red"), mar = c(3.1, 3.1, .5, .5), 
                                     ylim = lim.ss, 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for SS
      if (exi.t["ress"] && exi.t["ccss"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccss", obs = "ress", start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. SS", "obs. SS"), mar = c(3.1, 3.1, .5, .5),
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ress"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "ress", start.mon = start.mon, 
                                     log = log.r, l.legend = c("obs. SS"), mar = c(3.1, 3.1, .5, .5),
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ccss"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccss", obs = NULL, start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. SS"), mar = c(3.1, 3.1, .5, .5), ylim = lim.ss, 
                                     ylab = expression(paste("SS conc. (mg l"^"-1", ")")))')
      }
    }
    
    
    
    
    #### TS row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for TS, conditional on if TS is requested/available
    
    if (exi.t["rets"] || exi.t["ccts"]) {
      
      # fill layout matrix with panel IDs
      lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
      # add layout height for this row
      lay.heights <- c(lay.heights, 2)
      
      
      ## panel 1: compute and plot GoFs for TS, if variables are available
      # empty frame first, then GoFs as legend
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
      
      if (exi.t["rets"] && exi.t["ccts"]){
        gof.ts <- tryCatch(gof(sim = get("ccts"), obs = get("rets"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, 
                                     legend = c(paste(names(gof.ts), gof.ts, sep = ": "),"",
                                     paste0("(", length(na.omit(rets)), " obs.)")), bty = "n", 
                                     title = "TS, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if ((exi.t["rout"] && exi.t["rets"]) || (exi.t["cout"] && exi.t["ccts"])) {
        if ((exi.t["rout"] && exi.t["rets"]) && (exi.t["cout"] && exi.t["ccts"])) {
          
          # calculate y axis limits
          lim.ts <- range(c(rets, ccts), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ts[1] <= 0) {
            lim.ts[1] <- min(c(rets, ccts)[c(rets, ccts) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ts))) {
              lim.ts <- rep(0, 2)
            }
          }
          
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccts, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.ts, xlab = expression(paste("Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(rout, rets, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["rout"] && exi.t["rets"]) {
          
          # calculate y axis limits
          lim.ts <- range(rets, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ts[1] <= 0) {
            lim.ts[1] <- min(rets[rets > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ts))) {
              lim.ts <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(rout, rets, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.ts, xlab = expression(paste("Q (m"^3,"s"^"-1", ")")), ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["cout"] && exi.t["ccts"]) {
          
          # calculate y axis limits
          lim.ts <- range(ccts, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.ts[1] <= 0) {
            lim.ts[1] <- min(ccts[ccts > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.ts))) {
              lim.ts <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ccts, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, 
                                       xlim = lim.q, ylim = lim.ts, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), 
                                       ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, 
                                       col = c("#FF000080", "#00000080"), bty = "n")')
          
        }
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 3: sim-obs comparison dotty plot for TS
      if (exi.t["ccts"] && exi.t["rets"]){
        
        # calculate axis limits (used on both axes)
        lim.ts <- range(c(ccts, rets), na.rm = TRUE)
        # change lower limit to half the observed !0-minimum if log-scale
        if (log && lim.ts[1] <= 0) {
          lim.ts[1] <- min(c(ccts, rets)[c(ccts, rets) > 0], na.rm = TRUE) * .5
          # treat case where there are no non-0 values, and Inf values are created
          if (any(is.infinite(lim.ts))) {
            lim.ts <- rep(0, 2)
          }
        }
        
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(rets, ccts, col = "#7FAD8EE6", pch = 16, log = log.cq, 
                                     ylim = lim.ts, xlim = lim.ts, 
                                     xlab = expression(paste("observed TS (mg l"^"-1", ")")), 
                                     ylab = expression(paste("simulated TS (mg l"^"-1", ")")))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 4: CDC for TS
      if (exi.t["rets"] && exi.t["ccts"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(rets, ccts)), 
                                     xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = c("obs. TS", "sim. TS"), 
                                     col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["rets"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = rets), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "obs. TS", col = c("black"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ccts"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ccts), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "sim. TS", col = c("red"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for TS
      if (exi.t["rets"] && exi.t["ccts"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccts", obs = "rets", start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. TS", "obs. TS"), mar = c(3.1, 3.1, .5, .5),
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["rets"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "rets", start.mon = start.mon, 
                                     log = log.r, l.legend = c("obs. TS"), mar = c(3.1, 3.1, .5, .5),
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      } else if (exi.t["ccts"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "ccts", obs = NULL, start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. TS"), mar = c(3.1, 3.1, .5, .5),
                                     ylab = expression(paste("TS conc. (mg l"^"-1", ")")))')
      }
    }
    
    
    
    
    #### T1 row: 5 panels with GOFs, Conc-Q relationships, sim-obs, FDC, and regime for tracer T1, conditional on if T1 is requested/available
    
    if (exi.t["ret1"] || exi.t["cct1"]) {
      
      # fill layout matrix with panel IDs
      lay.mat <- rbind(lay.mat, c(seq(max(lay.mat) + 1, by = 1, length.out = 5), max(lay.mat) + 5)) 
      # add layout height for this row
      lay.heights <- c(lay.heights, 2)
      
      
      ## panel 1: compute and plot GoFs for T1, if variables are available
      # empty frame first, then GoFs as legend
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'par(mar = rep(0, 4))')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
      
      if (exi.t["ret1"] && exi.t["cct1"]){
        gof.t1 <- tryCatch(gof(sim = get("cct1"), obs = get("ret1"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                           error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, 
                                     legend = c(paste(names(gof.t1), gof.t1, sep = ": "),"",
                                     paste0("(", length(na.omit(ret1)), " obs.)")), bty = "n", 
                                     title = "T1, goodness\nof fit", cex = 1)')
      }
      
      
      ## panel 2: Conc-Q plots, depending on variable availability
      if (exi.t["cout"] && (exi.t["ret1"] || exi.t["cct1"])) {
        if (exi.t["ret1"] && exi.t["cct1"]) {
          
          # calculate y axis limits
          lim.t1 <- range(c(ret1, cct1), na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.t1[1] <= 0) {
            lim.t1[1] <- min(c(ret1, cct1)[c(ret1, cct1) > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.t1))) {
              lim.t1 <- rep(0, 2)
            }
          }
          
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cct1, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, xlim = lim.q, ylim = lim.t1, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = ylab.t1)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'points(cout, ret1, col = "#00000080", pch = 16)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        }  else if (exi.t["ret1"]) {
          
          # calculate y axis limits
          lim.t1 <- range(ret1, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.t1[1] <= 0) {
            lim.t1[1] <- min(ret1[ret1 > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.t1))) {
              lim.t1 <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, ret1, col = "#00000080", pch = 16, log = log.cq, xlim = lim.q, ylim = lim.t1, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), ylab = ylab.t1)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, col = c("#FF000080", "#00000080"), bty = "n")')
          
        } else if (exi.t["cct1"]) {
          
          # calculate y axis limits
          lim.t1 <- range(cct1, na.rm = TRUE)
          # change lower limit to >0 if log-scale
          if (log && lim.t1[1] <= 0) {
            lim.t1[1] <- min(cct1[cct1 > 0], na.rm = TRUE)
            # treat case where there are no non-0 values, and Inf values are created
            if (any(is.infinite(lim.t1))) {
              lim.t1 <- rep(0, 2)
            }
          }
          
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'plot(cout, cct1, col = "#FF00003C", pch = 16, log = log.cq, cex = .7, 
                                       xlim = lim.q, ylim = lim.t1, xlab = expression(paste("simulated Q (m"^3,"s"^"-1", ")")), 
                                       ylab = ylab.t1)')
          cp <- cp + 1
          list.plotexpr[[cp]] <- parse(text = 'legend("topright", legend = c("sim.", "obs."), pch = 16, 
                                       col = c("#FF000080", "#00000080"), bty = "n")')
          
        }
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 3: sim-obs comparison dotty plot for T1
      if (exi.t["cct1"] && exi.t["ret1"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'par(mar = c(3.1, 3.1, .5, .5), tcl = -0.2, mgp = c(1.8, 0.3, 0))')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'plot(ret1, cct1, col = "#7FAD8EE6", pch = 16, log = log.cq, 
                                     xlab = "observed T1", 
                                     ylab = "simulated T1")')
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'abline(a = 0, b = 1, col = "#00000080")')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      
      ## panel 4: CDC for T1
      if (exi.t["ret1"] && exi.t["cct1"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(ret1, cct1)), 
                                     xscale = xscale, yscale = yscale, add.legend = TRUE, l.legend = c("obs. T1", "sim. T1"), 
                                     col = c("black", "red"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = ylab.t1)')
      } else if (exi.t["ret1"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = ret1), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "obs. T1", col = c("black"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = ylab.t1)')
      } else if (exi.t["cct1"]) {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = cct1), xscale = xscale, yscale = yscale, 
                                     add.legend = TRUE, l.legend = "sim. T1", col = c("red"), mar = c(3.1, 3.1, .5, .5), 
                                     xlab = "Concentration exceedance percentile", 
                                     ylab = ylab.t1)')
      } else {
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = 'frame()')
      }
      
      
      ## panel 5: sim-obs regime plot for T1
      if (exi.t["ret1"] && exi.t["cct1"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cct1", obs = "ret1", start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. T1", "obs. T1"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["ret1"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = NULL, obs = "ret1", start.mon = start.mon, 
                                     log = log.r, l.legend = c("obs. T1"), mar = c(3.1, 3.1, .5, .5))')
      } else if (exi.t["cct1"]){
        cp <- cp + 1
        list.plotexpr[[cp]] <- parse(text = '.PlotSimObsRegime(x = xw, sim = "cct1", obs = NULL, start.mon = start.mon, 
                                     log = log.r, l.legend = c("sim. T1"), mar = c(3.1, 3.1, .5, .5))')
      }
    }
  }
  
  
  
  ## set up plot device with layout and call all plot commands 
  
  # define device width in inches (hard-coded for now)
  wdth <- 13
  # set device height in inches, based on layout rows
  hght <- sum(lay.heights)
  
  # create plot device, conditional on filename
  if (is.null(filename)) {
    
    # automatic device resizing to prevent slow re-draw on screen device
    if (hght > 10) {
      hght <- 10
      warning("Computed plot device height overridden to fit screen height which might result in rendering overlaps. 
              Change argument 'driver' to plot to file or use argument 'hype.vars' to reduce number of variables to plot.")
    }
    
    if(driver == "default"){
      dev.new(width=wdth, height = hght, noRStudioGD = TRUE)
    } else if(driver == "screen"){
      if (Sys.info()['sysname'] == "Windows") {
        grDevices::windows(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device rezising
        dev.control("inhibit")
      } else if (Sys.info()['sysname'] == "Linux") {
        grDevices::X11(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device rezising
        dev.control("inhibit")
      } else if (Sys.info()['sysname'] == "Darwin") {
        grDevices::quartz(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device rezising
        dev.control("inhibit")
      } else {
        # try x11, not very likely to occur..
        grDevices::X11(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device rezising
        dev.control("inhibit")
      }
    }
    
  } else if (driver == "png") {
    png(filename = filename, width=wdth, height = hght, units = "in", res = 450, pointsize = 12)
    # close the file device on exit
    on.exit(dev.off(), add = TRUE)
  } else {
    cairo_pdf(filename = filename, width = wdth, height = hght, pointsize = 12)
    # close the file device on exit
    on.exit(dev.off(), add = TRUE)
  }
  
  # layout definition
  nf <- graphics::layout(mat = lay.mat[-1, , drop = FALSE], widths = lay.widths, heights = lay.heights)
  # layout.show(nf)
  
  # plot all commands in list
  for (i in 1:length(list.plotexpr)) {
    eval(list.plotexpr[[i]])
  }
}


