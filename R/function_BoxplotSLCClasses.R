#' Box plots of SLC distributions
#'
#' \code{BoxplotSLCClasses} plots SLC class distributions for all SUBIDs in a GeoData data frame as boxplots. Boxes can represent distributions 
#' of area fractions
#' 
#' @param gd Data frame containing columns with SLC fractions, typically a 'GeoData.txt' file imported with \code{\link{ReadGeoData}}.
#' @param gcl Data frame containing columns with SLCs and corresponding land use and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}.
#' @param col.landuse Specification of colors for box outlines, to represent land use classes. Either a keyword character string, or a vector of 
#' colors with one element for each land use class as given in argument \code{gcl} in ascending order. Possible keywords are \code{'rainbow'} (default) 
#' or \code{'auto'}. \code{'rainbow'} triggers a generation of land use class colors using the \code{\link{rainbow}} palette. \code{'auto'} triggers 
#' generation of a pretty color palette with similar colors for land use groups. This option requires specification of land use groups in argument 
#' \code{col.group}.
#' @param col.group Integer vector of the same length as the number of land use classes given in \code{gcl}. Specifies a land use group ID for 
#' each land use class ID, in ascending order. Groups and group IDs to use (in parentheses):
#' \itemize{
#' \item Water, snow, and ice (1)
#' \item Urban (2)
#' \item Forests (3)
#' \item Agriculture and pastures (4)
#' \item Natural non-forested (5)
#' }
#' @param lab.legend Character string giving optional land use and soil class names to label the legend. Land use classes first, then soil classes. 
#' Both following class IDs as given in \code{gcl} in ascending order.
#' @param pos.legend Numeric, legend position in x direction. Given as position on the right hand outside of the plot area in x-axis units.
#' @param abs.area Logical, if \code{TRUE}, boxes will be plotted for absolute areas instead of area fractions.
#' @param log Character string, passed to \code{\link{boxplot}}. Empty string for linearly scaled axes, \code{'y'} for log scaled y-axis 
#' (particularly in combination with \code{abs.area = TRUE}).
#' @param ylim Numeric vector of length 2, y-axis minimum and maximum. Set automatically if not specified.
#' @param range Argument to \code{\link{boxplot}} with changed default. See documentation in there.
#' @param mar,mgp,tcl,xaxs,xpd Arguments passed to \code{\link{par}}. See documentation in there.
#' 
#' @details
#' \code{BoxplotSLCClasses} allows to analyse the occurrence of individual SLCs in a given model set-up. both in terms of area fractions (SLC values) 
#' and absolute areas. The function uses \code{\link{boxplot}} to plot distributions of SLCs of all SUBIDs in a GeoData data frame. Land use classes 
#' are color-coded, and soil classes marked by a point symbol below each box. Box whiskers extend to the data extremes.
#' 
#' @return
#' \code{BoxplotSLCClasses} returns a plot to the currently active plot device, and invisibly a data frame of SLC class fractions with \code{0} 
#' values replaced by \code{NA}s. If absolute areas are plotted, these are returned in the data frame.
#' 
#' @note
#' There is a maximum of 26 symbols available for marking soil classes. \code{BoxplotSLCClasses} can be quite crowded, depending on the number of SLCs 
#' in a model set-up. Tested and recommended plot device dimensions are 14 x 7 inches (width x height), e.g.:
#'
#' \kbd{> x11(width = 14, height = 7)}
#' 
#' \kbd{> png("mySLCdistri.png", width = 14, height = 7, units = "in", res = 600)}
#'  
#' 
#' @examples
#' # Import source data
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te2 <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' BoxplotSLCClasses(gd = te1, gcl = te2)
#' 
#' @importFrom grDevices rainbow
#' @importFrom graphics par boxplot mtext points legend
#' @export

BoxplotSLCClasses <- function(gd, gcl, col.landuse = "rainbow", col.group = NULL, lab.legend = NULL, pos.legend = 1, abs.area = FALSE, log = "", ylim = NULL, 
                             range = 0, mar = c(3,3,1,7)+.1, mgp = c(1.5, .2, 0), tcl = .1, xaxs = "i", xpd = TRUE) {
  
  # Backup par and restore on function exit
  userpar <- par(no.readonly = TRUE) # Backup par
  on.exit(suppressWarnings(par(userpar))) # Restore par on function exit
  
  # if lab.legend was specified, check for consistency with number of land use and soil classes
  if (!is.null(lab.legend) && length(lab.legend) != length(c(unique(gcl[, 2]), unique(gcl[,3])))) {
    stop("Length of 'lab.legend' does not match the combined number of land use and soil classes in 'gcl'. Exiting.")
  }
  
  # if lab.legend was not specified, fill with land use and soil class IDs from gcl
  if (is.null(lab.legend)) {
    lab.legend <- c(paste("landuse_", sort(unique(gcl[, 2])), sep = ""), paste("soil_", sort(unique(gcl[, 3])), sep = ""))
  }
  
  # identify SLC columns in gd
  gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  # stop if none were found
  if (length(gdcols.slc) == 0) {
    stop("No SLC classes found in 'gd'. Exiting.")
  }
  
  # get SLCs and replace 0 fractions with NAs, so that they do not bias the area distributions
  # conditional: calculate absolute areas in km2
  slc <- gd[, gdcols.slc]
  if (abs.area) {
    slc <- as.data.frame(apply(slc, 2, function(x, y) {x * y}, y = gd[which(toupper(names(gd)) == "AREA")] / 1000000))
  }
  if (nrow(slc) == 1) {
    slc[which(slc == 0)] <- NA
  } else {
    slc <- as.data.frame(apply(slc, 2, function(x){x[which(x == 0)] <- NA; x}))
  }
  
  # input data check: search negative area fractions and throw error if found
  if (min(slc, na.rm = TRUE) < 0) {
    stop("Negative SLC class fraction(s) in 'gd'.")
  }
   
  # number of slc classes in GeoData
  nslc <- ncol(slc)
  
  ## Colors for box outlines
  
  # Conditional on argument col.landuse, keyword or vector
  if (length(col.landuse) == 1) {
    if (col.landuse == "rainbow") {
      
      col.lu <- as.factor(gcl[, 2])
      levels(col.lu) <- rainbow(length(levels(col.lu)), s = 0.8)
      legend.lu <- levels(col.lu)
      col.lu <- as.character(col.lu)
      
    } else if (col.landuse == "auto") {
      
      col.lu <- as.factor(gcl[, 2])
      
      # check if required argument col.group exists has the same length as the number of land use classes in gcl, and is vector within 1 and 5
      if (is.null(col.group)) {
        stop("'col.landuse = \"auto\"' requires non-NULL 'col.group'. Exiting.")
      }
      if (length(col.group) != length(levels(col.lu))) {
        stop("'col.group' length and number of land use classes in 'gcl' do not match. Exiting.")
      }
      if (min(col.group) < 1 || max(col.group) > 5) {
        stop("'col.group' not within a range from 1 to 5. Exiting.")
      }
      
      # get a pretty color for each SLC class using internal color ramp functions for each land use group
      col.gr <- col.group
      col.gr[col.gr == 1] <- ColBlues(length(col.gr[col.gr == 1]))
      col.gr[col.gr == 2] <- ColReds(length(col.gr[col.gr == 2]))
      col.gr[col.gr == 3] <- ColGreens(length(col.gr[col.gr == 3]))
      col.gr[col.gr == 4] <- ColYOB(length(col.gr[col.gr == 4]))
      col.gr[col.gr == 5] <- ColPurples(length(col.gr[col.gr == 5]))
      levels(col.lu) <- col.gr
      legend.lu <- col.gr
      col.lu <- as.character(col.lu)
    } else {
      # catch the rest, col.group input error
      stop("'col.landuse' keyword unknown. Exiting.")
    }
  # vector specified for col.landuse
  } else {
    
    col.lu <- as.factor(gcl[, 2])
    
    # check of number of colors matches number of land use classes
    if (length(col.landuse) != length(levels(col.lu))) {
      stop("'col.landuse' length and number of land use classes in 'gcl' do not match. Exiting.")
    }
    
    # assign colors to SLCs
    levels(col.lu) <- col.landuse
    legend.lu <- col.landuse
    col.lu <- as.character(col.lu)
  }
  
  # conditional: y-axis limits
  if (is.null(ylim)) {
    if (log == "y"){
      ylimit <- c(10^floor(log10(min(slc, na.rm = TRUE))), 10^ceiling(log10(max(slc, na.rm = TRUE))))
    } else {
      # find pretty max limit for non-logscaled y-axis
      mx <- round(max(slc, na.rm = TRUE))
      mx <- ceiling(mx * 10^-(nchar(mx) - 2)) * 10^(nchar(mx) - 2)
      ylimit <- c(0, mx)
    }
  } else {
    ylimit <- ylim
  }
  
  # conditional: y-axis label string
  if (abs.area) {
    ylabel <- expression(paste("Area (km"^2, ")"))
  } else {
    ylabel <- "Area fraction (-)"
  }
  
  # x11(width = 14, height = 7)
  par(mar = mar, mgp = mgp, tcl = tcl, xaxs = xaxs, xpd = xpd)
  boxplot(slc, border = col.lu, range = range, names = rep("", nslc), xlab= "SLC class", ylab = ylabel, xlim = c(0, nslc + 1), ylim = ylimit, 
          log = log, pars = list(boxlwd = 1.6, whisklwd = 1.6, staplelwd = 1.6, whisklty = 1))
  mtext(seq(1, nslc, by = 2), 1, line = .05, at = seq(1, nslc, by = 2), cex=.8)
  mtext(seq(2, nslc - 1, by = 2), 1, line = .6, at = seq(2, nslc - 1, by = 2), cex=.8)
  
  ## add identifiers for soil types
  # get plot region coordinates
  usr.cur <- par("usr")
  # derive y-position for soil symbols, conditional on log-scaling of y-axis
  if (log == "y") {
    soil.y <- 10^(usr.cur[3] + ((log10(ylimit[1]) - usr.cur[3])/2))
  } else {
    soil.y <- usr.cur[3] + (ylimit[1] - usr.cur[3])/2
  }
  points(x = 1:nslc, y = rep(soil.y, nslc), pch = gcl[, 3] - 1)
  
  ## add legend
  # number of soil classes, number of land use classes
  nsoil <- length(unique(gcl[, 3]))
  # legend y position, conditional on axis scaling
  if (log == "y") {
    legend.y <- 10^(mean(usr.cur[3:4]))
  } else {
    legend.y <- mean(usr.cur[3:4])
  }
  # legend plot
  legend(x = nslc + pos.legend, y = legend.y, legend = lab.legend, col = c(legend.lu, rep("black", nsoil)), 
         pch = c(rep(15, length(legend.lu)), 0:(nsoil - 1)), 
         bty = "n", pt.bg = "grey", cex = .9, pt.cex = 1, yjust = .5)
  
  # return (abs area) slcs invisibly
  invisible(slc)
}
