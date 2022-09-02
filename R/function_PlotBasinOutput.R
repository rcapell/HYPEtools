#' Plot a suite of time series plots from a HYPE basin output file
#'
#' Plot a standard suite of time series plots from a basin output file, typically used for model performance inspection and/or 
#' during manual calibration
#'
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' @param filename String, file name for plotting to file device, see argument \code{driver}. \emph{No file extension!} Ignored with plotting 
#' to screen device. \emph{Device dimensions are currently hard-coded, see Details.}
#' @param driver String, device driver name, one of \code{default}, \code{pdf}, \code{png}, or \code{screen}.
#' Defaults to \code{default}, which plots using default plotting device \code{getOption("device")}.
#' @param timestep  Character string, timestep of \code{x}, one of \code{"month"}, \code{"week"}, \code{"day"}, or 
#' \code{"nhour"} (n = number of hours). If not provided, an attribute \code{timestep} is required in \code{x}.
#' @param hype.vars Either a keyword string or a character vector of HYPE output variables. User-specified selection of HYPE variables 
#' to plot. Default (\code{"all"}) is to plot all variables which the function knows and which are available in \code{x}. See details 
#' for a list of known variables. Other possible keywords are \code{"hydro"} and \code{"wq"} (water quality), for which a pre-selected range of 
#' (available) result variables is plotted. Alternatively, a character vector holding HYPE output variables to be plotted. Variables unknown 
#' to the function will be ignored with a warning.
#' @param vol.err Logical, if \code{TRUE} and both observed and simulated discharge are available in \code{x}, the accumulated volume error 
#' will be plotted.
#' @param log.q Logical, y-axis scaling for flow duration curve and discharge time series, set to \code{TRUE} for log-scaling.
#' @param start.mon Integer between 1 and 12, starting month of the hydrological year. For runoff regime plot, see also 
#' \code{\link{AnnualRegime}}.
#' @param from,to Integer or date string of format \%F, see \code{\link{strptime}}. Time period bounds for plotting . Integers are 
#' interpreted as row indices of \code{x}.
#' @param name Character string, name to be printed on the plot.
#' @param area Numeric, upstream area of sub-basin in m^2. Required for calculation of accumulated volume error. Optional argument, 
#' either this or arguments \code{subid}, \code{gd}, and \code{bd} are required.
#' @param subid Integer, HYPE SUBID of a target sub-catchment (must exist in \code{gd}). Mandatory in combination with \code{gd} and 
#' optionally \code{bd} if argument \code{area} is not defined.  If not provided, an attribute \code{subid} is required in \code{x}. 
#' Used to calculate upstream area internally with function \code{\link{SumUpstreamArea}}. For repeated calls to \code{PlotBasinOutput} 
#' providing \code{area} in combination with a one-off separate call to \code{\link{SumUpstreamArea}} saves computation time, 
#' especially in basins with many upstream sub-basins.
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory with argument 
#' \code{subid}, details see there. 
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional with argument 
#' \code{subid}, details see there. 
#' @param ylab.t1 String or \code{\link{plotmath}} expression, y axis label for T1 tracer time series panel (tracer concentration units 
#' are not prescribed in HYPE).
#' 
#' @details
#' \code{PlotBasinOutput} plots a suite of time series along with a flow duration curve, a flow regime plot, and a selection of 
#' goodness-of-fit measures from an imported HYPE basin output file. The function selects from a range of "known" variables, and plots 
#' those which are available in the user-supplied basin output. It is mostly meant as a support tool during calibration, manual or 
#' automatic, providing a quick and comprehensive overview of model dynamics at in a sub-basin of interest.
#' 
#' HYPE outputs which are known to \code{PlotBasinOutput} include:
#' 
#' \itemize{
#' \item{precipitation}
#' \item{air temperature}
#' \item{discharge}
#' \item{lake water level}
#' \item{water temperature}
#' \item{evapotranspiration}
#' \item{snow water equivalent}
#' \item{sub-surface storage components}
#' \item{nitrogen concentrations}
#' \item{phosphorus concentrations}
#' \item{suspended sediment concentrations}
#' \item{total sediment concentrations}
#' \item{tracer concentration}
#' }
#' 
#' Below a complete list of HYPE variables known to the function in HYPE info.txt format, ready to copy-paste into an info.txt file. 
#' For a detailed description of the variables, see the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE online documentation}.
#' 
#' \code{basinoutput variable upcprf upcpsf temp upepot upevap cout rout soim sm13 upsmfp snow upcprc cct2 ret2 ccin rein ccon reon cctn retn 
#' ccsp resp ccpp repp cctp retp wcom wstr ccss ress ccts rets cct1 ret1}
#' 
#' \emph{Device dimensions} are hard-coded to a width of 15 inches and height depending on the number of plotted time series. When plotting 
#' to a screen device, a maximum height of 10 inches is enforced in order to prevent automatic resizing with slow redrawing. 
#' \code{PlotBasinOutput} throws a warning if the plot height exceeds 10 inches, which can lead to overlapping plot elements. On screens with 
#' less than 10 inch screen, redrawing is inhibited, which can lead to an empty plot. The recommended solution for both effects 
#' is to plot to pdf or png file devices instead.
#' 
#' @return 
#' Returns a multi-panel plot in a new graphics device.
#' 
#' @seealso
#' \code{\link{PlotBasinSummary}}, \code{\link{PlotAnnualRegime}}, \code{\link{PlotDurationCurve}}, \code{\link{ReadBasinOutput}}
#' 
#' @examples
#' # Source data, HYPE basin output with a number of result variables
#' te1 <- ReadBasinOutput(filename = system.file("demo_model",
#' "results","0003587.txt", package = "HYPEtools"))
#' te2 <- ReadGeoData(filename = system.file("demo_model",
#' "GeoData.txt", package = "HYPEtools"))
#'
# Screen devices should not be used in examples
#' \dontrun{ 
#' # Plot selected water variables on screen device
#' PlotBasinOutput(x = te1, gd = te2, driver = "screen",hype.vars = c("cout", "rout", 
#' "snow", "upcprf", "upcpsf"))
#' }
#' 
#' @importFrom hydroGOF gof gof.default
#' @importFrom grDevices dev.new dev.control dev.off cairo_pdf png
#' @export

PlotBasinOutput <- function(x, filename, driver = c("default", "pdf", "png", "screen"), timestep = attr(x, "timestep"), 
                            hype.vars = "all", vol.err = TRUE, log.q = FALSE, start.mon = 1, from = 1, to = nrow(x), name = "", area = NULL, 
                            subid = attr(x, "subid"), gd = NULL, bd = NULL, ylab.t1 = "Conc.") {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  ## Preliminaries
  
  # check and choose device driver
  driver <- match.arg(driver)
  if (driver %in% c("pdf", "png")) {
    filename <- paste(filename, driver, sep = ".")
  } else {
    filename <- NULL
  }
  
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
      uarea <- area
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
    if (tw > nrow(x)) {
      date.plot <- seq(x[1, 1], by = timestep, length.out = tw)
    }
  } else if (is.character(to)) {
    tw.d <- strptime(to, format = "%F", tz = "UTC")
    if (tw.d > x[nrow(x), 1]) {
      date.plot <- seq(x[1, 1], tw.d, by = timestep)
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
  nm.t <- c("date", "upcprf", "upcpsf", "temp", "upepot", "upevap", "cout", "rout", "soim", "sm13", "upsmfp", "snow", "upcprc", 
            "cct2", "ret2", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", "resp", "ccpp", "repp", "cctp", "retp", "wcom", 
            "wstr", "ccss", "ress", "ccts", "rets", "cct1", "ret1")
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
      nm.hydro <- c("date", "upcprf", "upcpsf", "temp", "upepot", "upevap", "cout", "rout", "snow", "upcprc", "wcom", "wstr", "cct2", "ret2")
      exi.t[!(nm.t %in% nm.hydro)] <- FALSE
    } else if (hype.vars[1] == "wq") {
      nm.wq <- c("date", "upcprf", "upcpsf", "cout", "rout", "upcprc", "ccin", "rein", "ccon", "reon", "cctn", "retn", "ccsp", 
                    "resp", "ccpp", "repp", "cctp", "retp", "ccss", "ress", "ccts", "rets", "cct1", "ret1")
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
  
  
  
  ## parse plot commands based on existing or requested HYPE variables to a list
  ## create layout() arguments based on existinng HYPE variables
  
  # create list to hold all plot commands, and plot counter
  list.plotexpr <- list(NULL)
  cp <- 0
  
  # layout() matrix initialisation
  lay.mat <- matrix(ncol = 3, nrow = 0)
  # layout() panel widths (hard-coded for now)
  lay.widths <- c(1, 1.5, 1)
  # layout() panel heights initialisation
  lay.heights <- NULL
  
  
  # conditional: three panels with FDC, GoFs, and regime. If GoF variables exist
  if ((exi.t["rout"] || exi.t["cout"]) || (
    (exi.t["rein"] && exi.t["ccin"]) || 
      (exi.t["reon"] && exi.t["ccon"]) || 
      (exi.t["retn"] && exi.t["cctn"]) ||
      (exi.t["resp"] && exi.t["ccsp"]) ||
      (exi.t["repp"] && exi.t["ccpp"]) ||
      (exi.t["retp"] && exi.t["cctp"]) ||
      (exi.t["ress"] && exi.t["ccss"])
    )
    ) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, 1:3)
    # add layout height for this row
    lay.heights <- c(lay.heights, 3)
    
    # conditional: prepare FDC plot call depending on data availability
    if (exi.t["rout"] && exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = data.frame(rout, cout)), xscale = "gauss", 
                                   yscale = ifelse(log.q, "log", "lin"), add.legend = TRUE, l.legend = c("Qobs", "Qsim"), 
                                   col = c("blue", "red"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = rout), xscale = "gauss", 
                                   yscale = ifelse(log.q, "log", "lin"), add.legend = TRUE, l.legend = "Qobs", 
                                   col = c("blue"), mar = c(3.1, 3.1, .5, .5))')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotDurationCurve(ExtractFreq(data = cout), xscale = "gauss", 
                                   yscale = ifelse(log.q, "log", "lin"), add.legend = TRUE, l.legend = "Qsim", 
                                   col = c("red"), mar = c(3.1, 3.1, .5, .5))')
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
    list.plotexpr[[cp]] <- parse(text = 'title(main = name, line = -length(strsplit(x = as.character(name), split = "\n")[[1]])*1.1)')
    # compute and plot GoFs for discharge, TN, TP, and suspended solids if variables are available
    if (exi.t["rout"] && exi.t["cout"]){
      gof.q <- tryCatch(gof(sim = get("cout"), obs = get("rout"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                        error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 0, y = 0.9, 
                                   legend = c(paste(names(gof.q), gof.q, sep = ": "),"",paste0("(", length(na.omit(rout)), 
                                   " obs.)")), bty = "n", title = "Q, goodness of fit", cex = .8)')
    }
    if (exi.t["retn"] && exi.t["cctn"]){
      gof.tn <- tryCatch(gof(sim = get("cctn"), obs = get("retn"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 1/4, y = 0.95, legend = c(paste(names(gof.tn), gof.tn, sep = ": "),"",
                                   paste0("(", length(na.omit(retn)), " obs.)")), bty = "n", title = "TN, goodness of fit", cex = .8)')
    }
    if (exi.t["retp"] && exi.t["cctp"]){
      gof.tp <- tryCatch(gof(sim = get("cctp"), obs = get("retp"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 2/4, y = 0.95, 
                                   legend = c(paste(names(gof.tp), gof.tp, sep = ": "),"",
                                   paste0("(", length(na.omit(retp)), " obs.)")), bty = "n", title = "TP, goodness of fit", cex = .8)')
    }
    if (exi.t["ress"] && exi.t["ccss"]){
      gof.ss <- tryCatch(gof(sim = get("ccss"), obs = get("ress"), na.rm = TRUE)[c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"), ], 
                         error = function(e){te <- rep(NA, 6); names(te) <- c("KGE", "NSE", "PBIAS %", "MAE", "r", "VE"); te})
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend(x = 3/4, y = 0.95, 
                                   legend = c(paste(names(gof.ss), gof.ss, sep = ": "),"",
                                   paste0("(", length(na.omit(ress)), " obs.)")), bty = "n", title = "SS, goodness of fit", cex = .8)')
    }
    
    # conditional: prepare regime plot call depending on data availability
    if (exi.t["rout"] && exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout, cout), 
                                   ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", 
                                   add.legend = TRUE, l.legend = c("Qobs", "Qsim"), col = c("blue", "red"), 
                                   mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else if (exi.t["rout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, rout), 
                                   ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", 
                                   add.legend = TRUE, l.legend = c("Qobs"), col = c("blue"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else if (exi.t["cout"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = '.PlotAnnualRegime(x = AnnualRegime(data.frame(date, cout), 
                                   ts.in = timestep, ts.out = "month", start.mon = start.mon), line = "mean", 
                                   add.legend = TRUE, l.legend = c("Qsim"), col = c("red"), mar = c(3.1, 3.1, .5, .5), xlab = xlab.regime)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'frame()')
    }
    
  }
  
  # precipitation and snowfall panel
  if (exi.t["upcprc"] || (exi.t["upcprf"] && exi.t["upcpsf"])) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    cp <- cp + 1
    if (exi.t["upcprc"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, upcprc, ylim = c(max(upcprc, na.rm = TRUE), -2), col = NA, axes = F, ylab = "")')
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, upcprf + upcpsf, ylim = c(max(upcprc, na.rm = TRUE), -2), col = NA, axes = F, ylab = "")')
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(new = TRUE)')
    
    # conditional: if rainfall and snow variables available, plot stacked bars based of these, otherwise plot precip bars
    if (exi.t["upcprf"] && exi.t["upcpsf"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = t(as.matrix(data.frame(upcprf, upcpsf))), border = c("darkblue", "forestgreen"), 
                                   ylim = c(max(c(upcprf, upcpsf), na.rm = TRUE), -2), xlab = "", col = c("darkblue", "forestgreen"), 
                                   names.arg = rep("", length(upcprf)), legend.text = c("Rain", "Snow"), 
                                   args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2, horiz = TRUE), 
                                   ylab = "mm", space = 0, cex.axis = 1, cex.lab = 1.2)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'barplot(height = upcprc, border = "darkblue", ylim = c(max(upcprc, na.rm = TRUE), -2), xlab = "", 
                                   col = "darkblue", names.arg = rep("", length(upcprc)), legend.text = "Precipitation", 
                                   args.legend = list(x = "bottomleft", bty = "n", border = NA, cex = 1.2, horiz = TRUE), 
                                   ylab = "mm", space = 0, cex.axis = 1, cex.lab = 1.2)')
    }
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'box()')
  }
  
  # air temperature panel
  if (exi.t["temp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, temp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(""*degree, "C")), 
                                 cex.axis = 1, cex.lab = 1.2)')
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
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["cout"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, rout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), 
                                   ylim = c(ifelse(log.q, 0.001, 0), max(rout, na.rm=T)), log = ifelse(log.q, "y", ""), 
                                   cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["rout"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), 
                                   ylim = c(ifelse(log.q, 0.001, 0), max(cout, na.rm=T)), log = ifelse(log.q, "y", ""), cex.axis = 1, 
                                   cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cout, type = "l", col = NA, xaxt = "n", ylab = expression(paste("m"^3, "s"^"-1")), 
                                   ylim = c(ifelse(log.q, 0.001, 0), max(c(cout, rout), na.rm=T)), log = ifelse(log.q, "y", ""), 
                                   cex.axis = 1, cex.lab = 1.2)')  
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
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("Qobs", "Qsim"), lty = 1, 
                                 col = c("royalblue4", "orangered3"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # lake water level panel
  if (exi.t["wcom"] || exi.t["wstr"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["wcom"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, wstr, type = "l", col = NA, xaxt = "n", ylab = "m", ylim = c(0, max(wstr, na.rm=T)), 
                                   cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["wstr"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, wcom, type = "l", col = NA, xaxt = "n", ylab = "m", ylim = c(0, max(wcom, na.rm=T)), 
                                   cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, wcom, type = "l", col = NA, xaxt = "n", ylab = "m", ylim = c(0, max(c(wcom, wstr), 
                                   na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["wstr"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, wstr, col = "black")')
    }
    
    if (exi.t["wcom"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, wcom, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("Obs. water level", "Sim. water level"), 
                                 lty = 1, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # water temperature panel
  if (exi.t["cct2"] || exi.t["ret2"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["cct2"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ret2, type = "l", col = NA, xaxt = "n", ylab = expression(paste(""*degree, "C")), 
                                   ylim = c(min(ret2, na.rm=T), max(ret2, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["ret2"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cct2, type = "l", col = NA, xaxt = "n", ylab = expression(paste(""*degree, "C")), 
                                   ylim = c(min(cct2, na.rm=T), max(cct2, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cct2, type = "l", col = NA, xaxt = "n", ylab = expression(paste(""*degree, "C")), 
                                   ylim = c(min(c(cct2, ret2), na.rm=T), max(c(cct2, ret2), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["ret2"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ret2, col = "steelblue4")')
    }
    
    if (exi.t["cct2"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, cct2, col = "violetred")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("Obs. water temp.", "Sim. water temp."), lty = c(1, 1), 
                                 col = c("steelblue4", "violetred"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  
  
  # ET panel
  if (exi.t["upepot"] || exi.t["upevap"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    cp <- cp + 1
    if (!exi.t["upevap"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, upepot, type = "l", col = NA, xaxt = "n", ylab = "mm", 
                                   ylim = c(0, max(upepot, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')
    } else if (!exi.t["upepot"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, upevap, type = "l", col = NA, xaxt = "n", ylab = "mm", 
                                   ylim = c(0, max(upevap, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, upevap, type = "l", col = NA, xaxt = "n", ylab = "mm", 
                                   ylim = c(0, max(c(upevap, upepot), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["upepot"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, upepot, col = "green3", lty = 3)')  
    }
    if (exi.t["upevap"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, upevap, col = "green4")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("ETp", "ETa"), lty = c(3, 1), 
                                 col = c("green3", "green4"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  } 
  
  # snow water equiv panel
  if (exi.t["snow"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, snow, type = "l", col = NA, xaxt = "n", ylab = "mm", cex.axis = 1, cex.lab = 1.2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'lines(date, snow, col = "deepskyblue3")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(" Snow water equivalent", side=3, adj= 0, line=-1.1, cex = .8)')
    
  }
  
  # accumulated vol err panel
  if (exi.t["cout"] && exi.t["rout"] && vol.err) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    sqsim <- ConvertDischarge(q = get("cout"), area = uarea, from = "m3s", to = "mmd")
    sqobs <- ConvertDischarge(q = get("rout"), area = uarea, from = "m3s", to = "mmd")
    accvolerr <- cumsum(sqsim - ifelse(is.na(sqobs), sqsim, sqobs))
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, accvolerr, type = "l", col = NA, xaxt = "n", ylab = "mm", cex.axis = 1, cex.lab = 1.2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'lines(date, accvolerr, col = "seagreen")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(" Accumulated volume error", side=3, adj= 0, line=-1.1, cex = .8)')
    
  }
  
  # soil moisture and surface water panel
  if (exi.t["soim"] || exi.t["sm13"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    if (exi.t["soim"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(date, soim, type = "l", col = NA, xaxt = "n", ylab = "mm", 
                                   ylim = c(min(soim, na.rm = TRUE), max(soim, na.rm = TRUE)), cex.axis = 1, cex.lab = 1.2)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'plot(date, sm13, type = "l", col = NA, xaxt = "n", ylab = "mm", 
                                   ylim = c(min(sm13, na.rm = TRUE), max(sm13, na.rm = TRUE)), cex.axis = 1, cex.lab = 1.2)')
    }
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    if (!exi.t["soim"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, sm13, col = "springgreen4")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), "pore water", lty = 1, 
                                   col = "springgreen4", bty = "n", cex = 1.2, horiz = TRUE)')
    } else if (!exi.t["sm13"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, soim, col = "springgreen4")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), " pore and surface water", 
                                   lty = 1, col = "springgreen4", bty = "n", cex = 1.2, horiz = TRUE)')
    } else {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, soim, col = "firebrick3")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, sm13, col = "springgreen4")')
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("pore water", "surface water"), 
                                   lty = 1, col = c("springgreen4", "firebrick3"), bty = "n", cex = 1.2, horiz = TRUE)')
    }
    
  }
  
  # upstream average relative soil moisture panel
  if (exi.t["upsmfp"]) {
    
    # fill layout matrix with panel IDs
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'plot(date, upsmfp, type = "l", col = NA, xaxt = "n", ylab = "(-)", xlab = "", 
                                 cex.axis = 1, cex.lab = 1.2)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'lines(date, upsmfp, col = "chocolate3")')
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'mtext(" Rel. soil moisture", side=3, adj= 0, line=-1.1, cex = .8)')
    
  }
  
  # TNsim, TNobs panel
  if (exi.t["cctn"] || exi.t["retn"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["cctn"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, retn, type = "l", col = NA, xaxt = "n", 
                                   ylab = expression(paste(mu,"g ", "l"^"-1")), ylim = c(0, max(retn, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["retn"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cctn, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(cctn, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cctn, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(cctn, retn), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["retn"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, retn, pch = 16, cex = .7)')
    }
    
    if (exi.t["cctn"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, cctn, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("TNobs", "TNsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # INsim, INobs panel
  if (exi.t["ccin"] || exi.t["rein"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccin"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, rein, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(rein, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["rein"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccin, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(ccin, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccin, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(ccin, rein), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["rein"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, rein, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccin"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccin, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("INobs", "INsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # ONsim, ONobs panel
  if (exi.t["ccon"] || exi.t["reon"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccon"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, reon, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(reon, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["reon"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccon, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(ccon, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccon, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(ccon, reon), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["reon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, reon, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccon"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccon, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("ONobs", "ONsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # TPsim, TPobs panel
  if (exi.t["cctp"] || exi.t["retp"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["cctp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, retp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(retp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["retp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cctp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(cctp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cctp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(cctp, retp), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["retp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, retp, pch = 16, cex = .7)')
    }
    
    if (exi.t["cctp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, cctp, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("TPobs", "TPsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # PPsim, PPobs panel
  if (exi.t["ccpp"] || exi.t["repp"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccpp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, repp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(repp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["repp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccpp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(ccpp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccpp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(ccpp, repp), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["repp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, repp, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccpp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccpp, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("PPobs", "PPsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # SPsim, SPobs panel
  if (exi.t["ccsp"] || exi.t["resp"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccsp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, resp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(resp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["resp"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccsp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(ccsp, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccsp, type = "l", col = NA, xaxt = "n", ylab = expression(paste(mu,"g ", "l"^"-1")), 
                                   ylim = c(0, max(c(ccsp, resp), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["resp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, resp, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccsp"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccsp, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("SPobs", "SPsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # SSsim, SSobs panel
  if (exi.t["ccss"] || exi.t["ress"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccss"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ress, type = "l", col = NA, xaxt = "n", 
                                  ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(ress, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["ress"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccss, type = "l", col = NA, xaxt = "n", 
                                   ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(ccss, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccss, type = "l", col = NA, xaxt = "n", 
                                   ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(c(ccss, ress), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["ress"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, ress, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccss"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccss, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("SSobs", "SSsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  # TSsim, TSobs panel
  if (exi.t["ccts"] || exi.t["rets"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["ccts"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, rets, type = "l", col = NA, xaxt = "n", 
                                  ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(rets, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["rets"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccts, type = "l", col = NA, xaxt = "n", 
                                   ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(ccts, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ccts, type = "l", col = NA, xaxt = "n", 
                                   ylab = expression(paste("mg l"^"-1")), 
                                   ylim = c(0, max(c(ccts, rets), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["rets"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, rets, pch = 16, cex = .7)')
    }
    
    if (exi.t["ccts"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, ccts, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("TSobs", "TSsim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  
  # sim tracer, obs tracer panel
  if (exi.t["cct1"] || exi.t["ret1"]) {
    
    lay.mat <- rbind(lay.mat, rep(if (suppressWarnings(expr = max(lay.mat)) == -Inf) {1} else {max(lay.mat) + 1}, 3)) 
    # add layout height for this row
    lay.heights <- c(lay.heights, 1.5)
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'par(mar = c(0, 3.6, 0, 0.5), xaxs = "i", mgp = c(2.2, .2, 0), tcl = .2, las = 1)')
    
    cp <- cp + 1
    if (!exi.t["cct1"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, ret1, type = "l", col = NA, xaxt = "n", 
                                  ylab = ylab.t1, 
                                   ylim = c(0, max(ret1, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else if (!exi.t["ret1"]) {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cct1, type = "l", col = NA, xaxt = "n", 
                                   ylab = ylab.t1, 
                                   ylim = c(0, max(cct1, na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    } else {
      list.plotexpr[[cp]] <- parse(text = 'plot(date, cct1, type = "l", col = NA, xaxt = "n", 
                                   ylab = ylab.t1, 
                                   ylim = c(0, max(c(cct1, ret1), na.rm=T)), cex.axis = 1, cex.lab = 1.2)')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(h = 0, col = "grey", lwd = .5)')
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'abline(v = date[which(format(date, format = "%m%d") == "0101")], , col = "grey", lwd = .5)')
    
    if (exi.t["ret1"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'points(date, ret1, pch = 16, cex = .7)')
    }
    
    if (exi.t["cct1"]) {
      cp <- cp + 1
      list.plotexpr[[cp]] <- parse(text = 'lines(date, cct1, col = "red")')  
    }
    
    cp <- cp + 1
    list.plotexpr[[cp]] <- parse(text = 'legend("topleft", inset = c(-.01, -.05), c("T1obs", "T1sim"), lty = c(NA, 1), pch = c(16, NA), 
                                 pt.cex = .7, col = c("black", "red"), bty = "n", cex = 1.2, horiz = TRUE)')
    
  }
  
  
  # add empty row at the figure bottom in layout, as space for x-axis annotation
  lay.mat <- rbind(lay.mat, rep(0, 3))
  # add layout height for this row
  lay.heights <- c(lay.heights, .3)
  
  # add axis annotation to plot list, conditional on daily or sub-daily time steps
  cp <- cp + 1
  list.plotexpr[[cp]] <- parse(text = 'axis.POSIXct(side = 1, x = date, cex.axis = 1)')

  
  ## set up plot device with layout and call all plot commands 
  
  # define device width in inches (hard-coded for now)
  wdth <- 15
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
    } else if (driver == "screen"){
      if(Sys.info()['sysname'] == "Windows") {
        grDevices::windows(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device resizing
        dev.control("inhibit")
      } else if (Sys.info()['sysname'] == "Linux") {
        grDevices::X11(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device resizing
        dev.control("inhibit")
      } else if (Sys.info()['sysname'] == "Darwin") {
        grDevices::quartz(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device resizing
        dev.control("inhibit")
      } else {
        # try x11, not very likely to occur..
        grDevices::X11(width=wdth, height = hght)
        # suppress slow redraw on automatic screen device resizing
        dev.control("inhibit")
      }
    }
    
  } else if (driver == "png") {
    png(filename = filename, width = wdth, height = hght, units = "in", res = 450, pointsize = 12)
    # close the file device on exit
    on.exit(dev.off(), add = TRUE)
  } else {
    cairo_pdf(filename = filename, width = wdth, height = hght, pointsize = 12)
    # close the file device on exit
    on.exit(dev.off(), add = TRUE)
  }
  
  # layout definition
  nf <- graphics::layout(mat = lay.mat, widths = lay.widths, heights = lay.heights)
  #layout.show(nf)
  
  # plot all commands in list
  for (i in 1:length(list.plotexpr)) {
    eval(list.plotexpr[[i]])
  }
}

