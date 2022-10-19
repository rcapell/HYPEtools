#'
#' Plot model performance by SUBID attributes
#'
#' Create scatterplots of model performance by SUBID attributes.
#'
#' @param subass Information to plot, typically model performances from imported HYPE 'subassX.txt' files. Data frame object
#' with first column containing SUBIDs and additional columns containing model results to plot. See details.
#' @param subass.column Column index of information in \code{subass} to plot on the y-axis of the output plots.
#' @param groups Optional data frame object to specify groups of SUBIDs to plot separately. First column should contain SUBIDs and second column should contain group IDs.
#' @param attributes Data frame object containing the subbasin attribute information to plot on the x-axis of the output plots. Typically a data frame created by \code{\link{SubidAttributeSummary}}
#' @param join.type Specify how to join \code{subass} to \code{attributes}. Default "join" will perform a \code{\link{left_join}} in which the order of the SUBIDs does not need to match. Additional option "cbind"
#' will perform a \code{\link{cbind}} in which the order of the SUBIDs needs to match; this can be helpful if you want to create plots where \code{subass} performance data is calculated according to a 
#' grouping variable (e.g. month).
#' @param groups.color.pal Vector containing colors to use when plotting groups. Only used if groups is not \code{NULL}.
#' @param drop Logical, should unused factor levels be omitted from the legend. See \code{\link{scale_color_manual}} and \code{link{scale_fill_manual}}.
#' @param alpha Numeric value to set transparency of dots in output plots. Should be in the range 0-1.
#' @param trendline Logical, if \code{TRUE}, then trendlines will be added to the output plots. Set to \code{FALSE} to hide trendlines. See \code{\link{geom_smooth}}.
#' @param trendline.method Specify method used to create trendlines. See \code{\link{geom_smooth}}.
#' @param trendline.formula Specify formula used to create trendlines. See \code{\link{geom_smooth}}.
#' @param trendline.alpha Numeric value to set transparency of trendlines in output plots. Should be in the range 0-1.
#' @param trendline.darken Numeric value to make the trendlines darker color shades of their corresponding scatterplot points. Should be in the range 1-100.
#' @param density.plot Logical, if \code{TRUE}, then density plots will be addded to the output plots. Set to \code{FALSE} to hide density plots. See \code{\link{geom_density}}.
#' @param scale.x.log Vector describing if output plots should use a log scale on the x-axis. If length of vector == 1, then the value will be used for all output plots. Vector values should be either \code{TRUE} or \code{FALSE}. See \code{\link{scale_x_log10}}.
#' @param scale.y.log Vector describing if output plots should use a log scale on the y-axis. If length of vector == 1, then the value will be used for all output plots. Vector values should be either \code{TRUE} or \code{FALSE}. See \code{\link{scale_y_log10}}.
#' @param xlimits Vector containing minimum and maximum values for the x-axis of the output plots. See \code{\link{scale_x_continuous}}.
#' @param xbreaks Vector containing the break values used for the x-axis of the output plots. See \code{\link{scale_x_continuous}}.
#' @param xlabels Vector containing the labels for each break value used for the x-axis of the output plots. See \code{\link{scale_x_continuous}}.
#' @param ylimits Vector containing minimum and maximum values for the y-axis of the output plots. See \code{\link{scale_y_continuous}}.
#' @param ybreaks Vector containing the break values used for the y-axis of the output plots. See \code{\link{scale_y_continuous}}.
#' @param ylabels Vector containing the labels for each break value used for the y-axis of the output plots. See \code{\link{scale_y_continuous}}.
#' @param xlab String containing the text to use for the x-axis title of the output plots. See \code{\link{xlab}}.
#' @param ylab String containing the text to use for the y-axis title of the output plots. See \code{\link{ylab}}.
#' @param ncol Integer, number of columns to use in the output arranged plot. See \code{\link{ggarrange}}.
#' @param nrow Integer, number of rows to use in the output arranged plot. See \code{\link{ggarrange}}.
#' @param align Specify how output plots should be arranged. See \code{\link{ggarrange}}.
#' @param common.legend Specify if arranged plot should use a common legend. See \code{\link{ggarrange}}.
#' @param legend.position Specify position of common legend for arragned plot. See \code{\link{ggarrange}}.
#' @param summary.table Logical, if \code{TRUE}, then a table providing summary statistics will be included at the bottom of the output plot.
#' @param filename String, filename used to save plot. File extension must be specified. See \code{\link{ggsave}}.
#' @param width Numeric, specify width of output plot. See \code{\link{ggsave}}.
#' @param height Numeric, specify height of output plot. See \code{\link{ggsave}}.
#' @param units Specify units of \code{width} and \code{height}. See \code{\link{ggsave}}.
#' @param dpi Specify resolution of output plot. See \code{\link{ggsave}}.
#'
#' @details
#' \code{PlotPerformanceByAttribute} can be used to analyze model performance according to subbasin attributes. The function requires two primary inputs; Model performance
#' information is contained in the \code{subass} input, and subbasin attribute information is contained in the \code{attributes} input. The \code{subass.column} argument controls
#' which column of the \code{subass} data frame will be used as the y-coordinate of points. Plots will be generated for each column in the \code{attributes} data frame
#' (except for the column named "SUBID") using the column values as the x-coordinate of the points.
#' 
#' A subbasin attribute summary table can be generated using \code{\link{SubidAttributeSummary}}, and additional columns can be joined to the data frame to add additional output plots.
#'
#' @return
#' \code{PlotPerformanceByAttribute} returns a plot to the currently active plot device.
#'
#' @seealso
#' \code{\link{ReadSubass}} for HYPE result import; \code{\link{SubidAttributeSummary}} for subbasin attribute summary

#' @examples
#' \donttest{
#' subass <- ReadSubass(filename = system.file("demo_model", "results",
#'   "subass1.txt",
#'   package = "HYPEtools"
#' ), check.names = TRUE)
#' gd <- ReadGeoData(filename = system.file("demo_model",
#'   "GeoData.txt",
#'   package = "HYPEtools"
#' ))
#' gc <- ReadGeoClass(filename = system.file("demo_model",
#'   "GeoClass.txt",
#'   package = "HYPEtools"
#' ))
#' 
#' attributes <- SubidAttributeSummary(subids <- subass$SUBID,
#'   gd = gd, gc = gc,
#'   mapoutputs = c(system.file("demo_model", "results", "mapCOUT.txt", package = "HYPEtools")),
#'   upstream.gd.cols = c("SLOPE_MEAN")
#' )
#' 
#' PlotPerformanceByAttribute(
#'   subass = subass,
#'   attributes = attributes[, c("SUBID", "landuse_1", "landuse_2", "landuse_3")],
#'   xlimits = c(0, 1)
#' )
#' }
#' 
#' @importFrom dplyr group_by sym left_join n rename select summarize
#' @importFrom ggplot2 aes aes_string coord_flip element_text geom_density geom_point geom_smooth ggplot ggsave guide_legend guides scale_color_manual scale_fill_discrete scale_fill_manual scale_x_continuous
#' scale_y_continuous theme theme_void unit waiver xlab ylab scale_x_log10 scale_y_log10
#' @importFrom ggpubr colnames_style get_legend ggarrange ggtexttable tab_add_title tbody_style ttheme
#' @importFrom grDevices colorRampPalette hcl
#' @importFrom patchwork plot_layout plot_spacer
#' @importFrom stats median
#' @importFrom rlang .data
#' @export

PlotPerformanceByAttribute <- function(subass, subass.column = 2, groups = NULL, attributes, join.type = c("join", "cbind"), groups.color.pal = NULL, drop = TRUE, alpha = 0.4,
                                       trendline = TRUE, trendline.method = "lm", trendline.formula = NULL, trendline.alpha = 0.5, trendline.darken = 15, density.plot = FALSE,
                                       scale.x.log = FALSE, scale.y.log = FALSE, xlimits = c(NA, NA), ylimits = c(NA, NA), xbreaks = waiver(), ybreaks = waiver(), xlabels = waiver(), ylabels = waiver(),
                                       xlab = NULL, ylab = NULL, ncol = NULL, nrow = NULL, align = "hv", common.legend = TRUE, legend.position = "bottom", summary.table = FALSE,
                                       filename = NULL, width = NA, height = NA, units = c("in", "cm", "mm", "px"), dpi = 300) {

  # Check join type
  join.type <- match.arg(join.type)

  # Check trendline.darken value
  if (trendline.darken < 1) {
    warning("trendline.darken set must be in range 1-100. Setting to 1")
  } else if (trendline.darken > 100) {
    warning("trendline.darken set must be in range 1-100. Setting to 100")
  }

  # Create dataframe to store plot data
  plotdata <- subass

  # Join subass data to groups if they are given
  if (!is.null(groups)) {
    plotdata <- left_join(plotdata, groups, by = "SUBID") %>% rename("Group" = colnames(groups)[2])
  }

  # Join subass data to attribute data
  if (join.type == "join") {
    plotdata <- left_join(plotdata, attributes, by = "SUBID")
  } else if (join.type == "cbind") {
    if (!nrow(plotdata) == nrow(attributes)) {
      stop("ERROR: number of rows in subass does not match number of rows in attributes")
    }
    plotdata <- cbind(plotdata, attributes %>% select(-"SUBID"))
  }

  # Create vector to store plots
  plots <- vector("list")
  plotcols <- colnames(attributes)[which(!colnames(attributes) == "SUBID")]
  
  # Check scale.x.log
  if(length(scale.x.log == 1)){
    scale.x.log = rep(scale.x.log, length(plotcols))
  } else if(!length(scale.x.log) == length(plotcols)){
    stop("ERROR: length of scale.x.log does not match number of output plots")
  }
  
  # Check scale.y.log
  if(length(scale.y.log == 1)){
    scale.y.log = rep(scale.y.log, length(plotcols))
  } else if(!length(scale.y.log) == length(plotcols)){
    stop("ERROR: length of scale.y.log does not match number of output plots")
  }

  # Generate plots
  for (col in plotcols) {

    # Create plot
    if (!is.null(groups)) {
      plot <- ggplot(data = plotdata, aes(x = !!sym(col), y = !!sym(colnames(subass)[subass.column]))) +
        geom_point(aes_string(fill = "Group"), alpha = alpha, shape = 21, color = "transparent")
    } else {
      plot <- ggplot(data = plotdata, aes(x = !!sym(col), y = !!sym(colnames(subass)[subass.column]))) +
        geom_point(alpha = alpha)
    }
    
    # Add trendlines
    if (trendline == TRUE) {
      if (!is.null(groups)) {
        plot <- plot + geom_smooth(aes_string(color = "Group"), method = trendline.method, formula = trendline.formula)
      } else {
        plot <- plot + geom_smooth(method = trendline.method, formula = trendline.formula)
      }
    }

    # Add x-axis label
    if (!is.null(xlab)) {
      plot <- plot + xlab(xlab)
    }

    # Add y-axis label
    if (!is.null(ylab)) {
      plot <- plot + ylab(ylab)
    }

    # Format colors if color palette specified
    if (!is.null(groups.color.pal)) {
      plot <- plot +
        scale_fill_manual(values = groups.color.pal, name = "Group", drop = drop) +
        scale_color_manual(values = unlist(lapply(groups.color.pal, function(X) {
          colorRampPalette(c(X, "black"))(100)[trendline.darken] # Add darker colors for trendlines
        })), name = "Group", drop = drop) + 
        guides(color = guide_legend(override.aes = list(color = groups.color.pal))) # Override colors in legend to be the original colors

      # Format colors if no color palette specified
    } else {

      # Function to get ggplot colors
      gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }

      # Get colors for ggplot
      gg_colors <- gg_color_hue(length(unique(groups[[2]])))

      # Adjust colors
      plot <- plot +
        scale_fill_discrete(name = "Group", drop = drop) + # Assign name to pallette for points
        scale_color_manual(values = unlist(lapply(gg_colors, function(X) {
          colorRampPalette(c(X, "black"))(100)[trendline.darken] # Add darker colors for trendlines
        })), name = "Group", drop = drop) + 
        guides(color = guide_legend(override.aes = list(color = gg_colors))) # Override colors in legend to be the original colors
    }
    
    # Scale x axis
    if(scale.x.log[which(plotcols == col)] == TRUE){ # Log scale
      plot <- plot + scale_x_log10(limits = xlimits, breaks = xbreaks, labels = xlabels)
    } else{ # Normal scale
      plot <- plot + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels)
    }
    
    # Scale y axis
    if(scale.y.log[which(plotcols == col)] == TRUE){ # Log scale
      plot <- plot + scale_y_log10(limits = ylimits, breaks = ybreaks, labels = ylabels)
    } else{ # Normal scale
      plot <- plot + scale_y_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels)
    }

    # Format plot
    plot <- plot +
      theme(axis.title = element_text(face = "bold"),
            legend.position = "bottom")
    
    # Add density plots
    if(density.plot == TRUE){
      
      if(!is.null(groups)){
        if (!is.null(groups.color.pal)) { # If custom colors exist
          # Create density plot for x-axis
          densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
            geom_density(size = 0.2, alpha = 0.4) +
            scale_fill_manual(values = groups.color.pal, name = "Group") +
            theme_void()+
            theme(legend.position = "none")
          
          # Create density plot for y-a.xis
          densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
            geom_density(size = 0.2, alpha = 0.4) +
            scale_fill_manual(values = groups.color.pal, name = "Group") +
            theme_void()+
            theme(legend.position = "none") +
            coord_flip()
        } else{ # Use default colors
          # Create density plot for x-axis
          densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
            geom_density(size = 0.2, alpha = 0.4) +
            theme_void()+
            theme(legend.position = "none")
          
          # Create density plot for y-axis
          densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
            geom_density(size = 0.2, alpha = 0.4) +
            theme_void()+
            theme(legend.position = "none") +
            coord_flip()
        }
      } else{
        # Create density plot for x-axis
        densx <- ggplot(plotdata, aes(x = !!sym(col))) +
          geom_density(fill = "#619CFF", size = 0.2, alpha = 1) +
          theme_void()
        
        # Create density plot for y-axis
        densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]))) +
          geom_density(fill = "#619CFF", size = 0.2, alpha = 1) +
          theme_void() +
          coord_flip()
      }
      
      # Scale x axis
      if(scale.x.log[which(plotcols == col)] == TRUE){ # Log scale
        densx <- densx + scale_x_log10(limits = xlimits, breaks = xbreaks, labels = xlabels)
      } else{ # Normal scale
        densx <- densx + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels)
      }
      
      # Scale y axis
      if(scale.y.log[which(plotcols == col)] == TRUE){ # Log scale
        densy <- densy + scale_x_log10(limits = ylimits, breaks = ybreaks, labels = ylabels)
      } else{ # Normal scale
        densy <- densy + scale_x_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels)
      }
      
      # Backup legend
      plot_legend <- plot
      
      # Remove legend from plot
      plot <- plot + theme(legend.position = "none")
      
      # Create arranged plot
      plot <- densx + 
        plot_spacer() + 
        plot + 
        densy + 
        plot_layout(
          ncol = 2, 
          nrow = 2, 
          widths = c(4, 1),
          heights = c(1, 4)
        ) 
    }

    # Store plot in list
    plots[[col]] <- plot
  }

  # Determine if ncol and nrow should be automatically calculated
  if (is.null(ncol) & is.null(nrow)) {
    override <- TRUE
  } else if ((ncol * nrow) < length(plots)) {
    warning("ncol * nrow is less than the number of generated plots. Overriding ncol and nrow values.", call. = FALSE)
    override <- TRUE
  } else {
    override <- FALSE
  }

  # Calculate ncol and nrow automatically if not specified
  if (override == TRUE) {
    # For 4 or fewer plots then just use one row
    if (length(plots) <= 4) {
      nrow <- 1
      ncol <- length(plots)

      # Otherwise use a square layout
    } else {
      ncol <- ceiling(length(plots)^0.5)
      nrow <- ceiling(length(plots)^0.5)
    }
  }

  # Arrange plots
  if(density.plot == TRUE){
    arrangeplot <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow, align = align, common.legend = common.legend, legend = legend.position, legend.grob = get_legend(plot_legend))
  } else{
    arrangeplot <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow, align = align, common.legend = common.legend, legend = legend.position)
  }

  # Add summary stats table
  if (summary.table == TRUE) {

    # Calculate Summary Stats
    if (!is.null(groups)) {
      table <- plotdata %>%
        group_by(.data$Group) %>%
        summarize(
          n = n(),
          Mean = round(mean(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2),
          Median = round(median(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2),
          Min = round(min(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2),
          Max = round(max(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2)
        )
    } else {
      table <- plotdata %>%
        summarize(
          n = n(),
          Mean = round(mean(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2),
          Median = round(median(!!sym(colnames(subass)[subass.column]), na.rm = TRUE), 2)
        )
    }

    # Create Colored Table
    if (is.null(groups)) { # No groups
      table.p <- ggtexttable(table,
        rows = NULL,
        theme = ttheme(
          colnames.style = colnames_style(color = "Black", fill = "grey"),
          tbody.style = tbody_style(size = 8, color = "black", fill = "lightgray")
        )
      )
    } else if (!is.null(groups.color.pal)) { # Custom colors
      table.p <- ggtexttable(table,
        rows = NULL,
        theme = ttheme(
          colnames.style = colnames_style(color = "Black", fill = "grey"),
          padding = unit(c(2, 2), "mm"),
          tbody.style = tbody_style(size = 8, color = "black", fill = groups.color.pal)
        )
      )
    } else { # No color ramp specified
      table.p <- ggtexttable(table,
        rows = NULL,
        theme = ttheme(
          colnames.style = colnames_style(color = "Black", fill = "grey"),
          tbody.style = tbody_style(size = 8, color = "black", fill = gg_colors)
        )
      )
    }

    # Add title to table
    if (!is.null(ylab)) {
      table.p <- table.p %>%
        tab_add_title(text = ylab, face = "bold", padding = unit(c(1, -2), "mm"))
    } else {
      table.p <- table.p %>%
        tab_add_title(text = colnames(subass)[subass.column], face = "bold", padding = unit(c(1, -2), "mm"))
    }

    # Arrange plots
    arrangeplot <- ggarrange(arrangeplot, table.p, nrow = 2, heights = c(1, 0.4))
  }

  # Save plot
  if (!is.null(filename)) {
    ggsave(filename = filename, plot = arrangeplot, width = width, height = height, units = units, dpi = dpi, bg = "white")
  }

  # Return plot
  return(arrangeplot)
}

# Alias Easter Egg
#' @rdname PlotPerformanceByAttribute
#' @export
PlotJohan <- PlotPerformanceByAttribute
