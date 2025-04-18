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
#' @param join.type Specify how to join \code{subass} to \code{attributes}. Default "join" will perform a [dplyr::left_join] in which the order of the SUBIDs does not need to match. Additional option "cbind"
#' will perform a \code{\link{cbind}} in which the order of the SUBIDs needs to match; this can be helpful if you want to create plots where \code{subass} performance data is calculated according to a 
#' grouping variable (e.g. month).
#' @param group.join.type Specify how to join \code{subass} to \code{groups}. Default "join" will perform a [dplyr::left_join] in which the order of the SUBIDs does not need to match. Additional option "cbind"
#' will perform a \code{\link{cbind}} in which the order of the SUBIDs needs to match; this can be helpful if you want to create plots where \code{subass} performance data is calculated according to a 
#' grouping variable (e.g. month).
#' @param groups.color.pal Vector containing colors to use when plotting groups. Only used if groups is not \code{NULL}.
#' @param drop Logical, should unused factor levels be omitted from the legend. See [ggplot2::scale_color_manual] and [ggplot2::scale_fill_manual].
#' @param alpha Numeric value to set transparency of dots in output plots. Should be in the range 0-1.
#' @param trendline Logical, if \code{TRUE}, then trendlines will be added to the output plots. Set to \code{FALSE} to hide trendlines. See [ggplot2::geom_smooth].
#' @param trendline.method Specify method used to create trendlines. See [ggplot2::geom_smooth].
#' @param trendline.formula Specify formula used to create trendlines. See [ggplot2::geom_smooth].
#' @param trendline.alpha Numeric value to set transparency of trendlines in output plots. Should be in the range 0-1.
#' @param trendline.darken Numeric value to make the trendlines darker color shades of their corresponding scatterplot points. Should be in the range 1-100.
#' @param density.plot Logical, if \code{TRUE}, then density plots will be added to the output plots. Set to \code{FALSE} to hide density plots.
#' @param density.plot.type String, type of plot geometry to use for density plots: \code{"density"} for [ggplot2::geom_density] or \code{"boxplot"} for [ggplot2::geom_boxplot]. Outliers are hidden from the boxplots.
#' @param scale.x.log Vector describing if output plots should use a log scale on the x-axis. A pseudo-log scale will be used if any zero or negative values are present. If length of vector == 1, then the value will be used for all output plots. Vector values should be either \code{TRUE} or \code{FALSE}. See [ggplot2::scale_x_log10].
#' @param scale.y.log Vector describing if output plots should use a log scale on the y-axis. A pseudo-log scale will be used if any zero or negative values are present. If length of vector == 1, then the value will be used for all output plots. Vector values should be either \code{TRUE} or \code{FALSE}. See [ggplot2::scale_y_log10].
#' @param xsigma Numeric, scaling factor for the linear part of psuedo-long transformation of x axis. Used if \code{scale.x.log} is \code{TRUE} and zero or negative values are present. See [scales::pseudo_log_trans].
#' @param ysigma Numeric, scaling factor for the linear part of psuedo-long transformation of y axis. Used if \code{scale.y.log} is \code{TRUE} and zero or negative values are present. See [scales::pseudo_log_trans].
#' @param xlimits Vector containing minimum and maximum values for the x-axis of the output plots. See [ggplot2::scale_x_continuous].
#' @param xbreaks Vector containing the break values used for the x-axis of the output plots. See [ggplot2::scale_x_continuous].
#' @param xlabels Vector containing the labels for each break value used for the x-axis of the output plots. See [ggplot2::scale_x_continuous].
#' @param ylimits Vector containing minimum and maximum values for the y-axis of the output plots. See [ggplot2::scale_y_continuous].
#' @param ybreaks Vector containing the break values used for the y-axis of the output plots. See [ggplot2::scale_y_continuous].
#' @param ylabels Vector containing the labels for each break value used for the y-axis of the output plots. See [ggplot2::scale_y_continuous].
#' @param xlab String containing the text to use for the x-axis title of the output plots. See [ggplot2::xlab].
#' @param ylab String containing the text to use for the y-axis title of the output plots. See [ggplot2::ylab].
#' @param ncol Integer, number of columns to use in the output arranged plot. See [ggpubr::ggarrange].
#' @param nrow Integer, number of rows to use in the output arranged plot. See [ggpubr::ggarrange].
#' @param align Specify how output plots should be arranged. See [ggpubr::ggarrange].
#' @param common.legend Specify if arranged plot should use a common legend. See [ggpubr::ggarrange].
#' @param legend.position Specify position of common legend for arranged plot. See [ggpubr::ggarrange]. Use \code{"none"} to hide legend.
#' @param group.legend.title String, title for plot legend when generating plots with \code{groups}.
#' @param common.y.axis Logical, if \code{TRUE}, then only one y-axis label and marginal density plot will be provided. If \code{FALSE}, then separate y-axis labels and marginal density plots will be included for each subplot.
#' @param summary.table Logical, if \code{TRUE}, then a table providing summary statistics will be included at the bottom of the output plot.
#' @param table.margin Numeric, controls spacing between plots and summary table.
#' @param filename String, filename used to save plot. File extension must be specified. See [ggplot2::ggsave].
#' @param width Numeric, specify width of output plot. See [ggplot2::ggsave].
#' @param height Numeric, specify height of output plot. See [ggplot2::ggsave].
#' @param units Specify units of \code{width} and \code{height}. See [ggplot2::ggsave].
#' @param dpi Specify resolution of output plot. See [ggplot2::ggsave].
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
#' @importFrom dplyr group_by sym left_join n rename select summarize n_distinct
#' @importFrom ggplot2 aes coord_cartesian coord_flip element_text geom_density geom_boxplot geom_point geom_smooth ggplot ggsave guide_legend guides scale_color_manual scale_fill_discrete scale_fill_manual scale_x_continuous
#' scale_y_continuous theme theme_void unit waiver xlab ylab scale_x_log10 scale_y_log10
#' @importFrom ggpubr colnames_style get_legend ggarrange ggtexttable tab_add_title tbody_style ttheme
#' @importFrom grDevices colorRampPalette hcl
#' @importFrom patchwork plot_layout plot_spacer
#' @importFrom purrr possibly
#' @importFrom stats median
#' @importFrom rlang .data
#' @importFrom scales pseudo_log_trans
#' @importFrom stats complete.cases
#' @export

PlotPerformanceByAttribute <- function(subass, subass.column = 2, groups = NULL, attributes, join.type = c("join", "cbind"), group.join.type = c("join", "cbind"), groups.color.pal = NULL, drop = TRUE, alpha = 0.4,
                                       trendline = TRUE, trendline.method = "lm", trendline.formula = NULL, trendline.alpha = 0.5, trendline.darken = 15, density.plot = FALSE, density.plot.type = c("density", "boxplot"),
                                       scale.x.log = FALSE, scale.y.log = FALSE, xsigma = 1, ysigma = 1, xlimits = c(NA, NA), ylimits = c(NA, NA), xbreaks = waiver(), ybreaks = waiver(), xlabels = waiver(), ylabels = waiver(),
                                       xlab = NULL, ylab = NULL, ncol = NULL, nrow = NULL, align = "hv", common.legend = TRUE, legend.position = "bottom", group.legend.title = "Group", common.y.axis = FALSE, summary.table = FALSE,
                                       table.margin = 0.4, filename = NULL, width = NA, height = NA, units = c("in", "cm", "mm", "px"), dpi = 300) {

  # Check join type and density plot type
  join.type <- match.arg(join.type)
  group.join.type <- match.arg(group.join.type)
  density.plot.type <- match.arg(density.plot.type)

  # Check trendline.darken value
  if (trendline.darken < 1) {
    warning("trendline.darken set must be in range 1-100. Setting to 1")
  } else if (trendline.darken > 100) {
    warning("trendline.darken set must be in range 1-100. Setting to 100")
  }

  # Create dataframe to store plot data
  if (join.type == "join") {
    plotdata <- subass %>%
      filter(!is.na(!!sym(colnames(subass)[subass.column]))) # Remove NA values from y-axis plotting column
  } else{
    plotdata <- subass
  }
  
  # Format groups
  if (!is.null(groups)) {
    
    # Convert group IDs to string if numeric
    if(is.numeric(groups[[2]])){
      groups[[2]] <- as.character(groups[[2]])
    }
    
    # Rename grouping column if it exists already in plotdata to avoid conflict when joining
    if(colnames(groups)[2] %in% colnames(plotdata)){
      colnames(groups)[2] <- "Group"
    }
  }

  # Join subass data to groups if they are given
  if (!is.null(groups)) {
    if(group.join.type == "join"){
      plotdata <- left_join(plotdata, groups, by = "SUBID") %>% rename("Group" = colnames(groups)[2]) %>% arrange(.data[["Group"]])
    } else if (group.join.type == "cbind"){
      if (!nrow(plotdata) == nrow(groups)) {
        stop("Number of rows in subass does not match number of rows in groups")
      }
      plotdata <- cbind(plotdata, groups %>% select(-"SUBID")) %>% rename("Group" = colnames(groups)[2]) %>% arrange(.data[["Group"]])
    }
  }

  # Join subass data to attribute data
  if (join.type == "join") {
    # Check for columns present in both subass and attributes
    colnames_intersect <- intersect(colnames(plotdata), colnames(attributes))
    if(length(colnames_intersect) > 1){
      warning(paste("Multiple column names are present in both subass and attributes. Joining by:", paste(colnames_intersect, collapse = ", ")))
    }
    plotdata <- left_join(plotdata, attributes, by = colnames_intersect)
  } else if (join.type == "cbind") {
    if (!nrow(plotdata) == nrow(attributes)) {
      stop("Number of rows in subass does not match number of rows in attributes")
    }
    plotdata <- cbind(plotdata, attributes %>% select(-"SUBID"))
  }

  # Create vector to store plots
  plots <- vector("list")
  plot_legends <- vector("list")
  plotcols <- colnames(attributes)[which(!colnames(attributes) == "SUBID")]
  
  # Check scale.x.log
  if(length(scale.x.log == 1)){
    scale.x.log = rep(scale.x.log, length(plotcols))
  } else if(!length(scale.x.log) == length(plotcols)){
    stop("Length of scale.x.log does not match number of output plots")
  }
  
  # Check scale.y.log
  if(length(scale.y.log == 1)){
    scale.y.log = rep(scale.y.log, length(plotcols))
  } else if(!length(scale.y.log) == length(plotcols)){
    stop("Length of scale.y.log does not match number of output plots")
  }
  
  # Determine if ncol and nrow should be automatically calculated
  if (is.null(ncol) & is.null(nrow)) {
    override <- TRUE
  } else if(is.null(ncol)){
    override <- FALSE
    ncol <- ceiling(length(plotcols)/nrow)
  } else if(is.null(nrow)){
    override <- FALSE
    nrow <- ceiling(length(plotcols)/ncol)
  } else if ((ncol * nrow) < length(plotcols)) {
    warning("ncol * nrow is less than the number of generated plots. Overriding ncol and nrow values.", call. = FALSE)
    override <- TRUE
  } else {
    override <- FALSE
  }
  
  # Calculate ncol and nrow automatically if not specified
  if (override == TRUE) {
    # For 4 or fewer plots then just use one row
    if (length(plotcols) <= 4) {
      nrow <- 1
      ncol <- length(plotcols)
      
      # Otherwise use a square layout
    } else {
      ncol <- ceiling(length(plotcols)^0.5)
      nrow <- ceiling(length(plotcols)^0.5)
    }
  }

  # Generate plots
  for (col in plotcols) {

    # Create plot
    if (!is.null(groups)) {
      plot <- ggplot(data = plotdata, aes(x = !!sym(col), y = !!sym(colnames(subass)[subass.column]))) +
        geom_point(aes(fill = .data[["Group"]]), alpha = alpha, shape = 21, color = "transparent")
    } else {
      plot <- ggplot(data = plotdata, aes(x = !!sym(col), y = !!sym(colnames(subass)[subass.column]))) +
        geom_point(alpha = alpha)
    }
    
    # Identify groups with values for x-axis plots
    x_groups <- plotdata %>% filter(complete.cases(!!sym(col)))
    x_groups <- which(sort(unique(plotdata$Group)) %in% x_groups$Group)
    
    # Add trendlines
    if (trendline == TRUE) {
      if (!is.null(groups)) {
        plot <- plot + geom_smooth(aes(color = .data[["Group"]]), method = trendline.method, formula = trendline.formula)
        
        # Identify which groups have unique values and thus trendlines
          trendline_groups <- plotdata %>%
            group_by(.data[["Group"]]) %>%
            summarize(unique = n_distinct(!!sym(col))) %>%
            filter(unique > 1) %>%
            select(all_of("Group")) %>%
            unlist()
          
          trendline_groups <- which(sort(unique(groups[[2]])) %in% trendline_groups)
        
      } else {
        plot <- plot + geom_smooth(method = trendline.method, formula = trendline.formula)
        trendline_groups = 1 # Specify color group for trendline
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
      
      # Check that enough colors are provided
      if(length(unique(groups[[2]])) > length(groups.color.pal)){
        stop(paste(length(unique(groups[[2]])), "groups specified but only", length(groups.color.pal), "colors provided. Increase the number of colors in groups.color.pal"))
      }
      
      # Get colors
      if(drop == TRUE){
        manual_colors <- groups.color.pal
        legend_colors <- manual_colors
        trendline_groups <- x_groups
      } else if(drop == FALSE){
        manual_colors <- groups.color.pal[which(sort(unique(groups[[2]])) %in% unique(plotdata$Group))]
        legend_colors <- groups.color.pal
        if(trendline == FALSE){
          trendline_groups <- 1:length(legend_colors)
        }
      }

      plot <- plot +
        scale_fill_manual(values = manual_colors, name = group.legend.title, drop = drop) +
        scale_color_manual(values = unlist(lapply(legend_colors[trendline_groups], function(X) {
          colorRampPalette(c(X, "black"))(100)[trendline.darken] # Add darker colors for trendlines
        })), name = group.legend.title, drop = drop) + 
        guides(color = guide_legend(override.aes = list(color = legend_colors[trendline_groups]))) # Override colors in legend to be the original colors

      # Format colors if no color palette specified
    } else {

      # Function to get ggplot colors
      gg_color_hue <- function(n) {
        hues <- seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }

      # Get colors
      if(drop == TRUE){
        gg_colors <- gg_color_hue(length(which(sort(unique(groups[[2]])) %in% unique(plotdata$Group))))
        legend_colors <- gg_colors
        trendline_groups <- x_groups
      } else if(drop == FALSE){
        gg_colors <- gg_color_hue(length(unique(groups[[2]])))[which(sort(unique(groups[[2]])) %in% unique(plotdata$Group))]
        legend_colors <- gg_color_hue(length(unique(groups[[2]])))
        if(trendline == FALSE){
          trendline_groups <- 1:length(legend_colors)
        }
      }

      # Adjust colors
      plot <- plot +
        scale_fill_manual(values = gg_colors, name = group.legend.title, drop = drop) + # Assign name to palette for points
        scale_color_manual(values = unlist(lapply(legend_colors[trendline_groups], function(X) {
          colorRampPalette(c(X, "black"))(100)[trendline.darken] # Add darker colors for trendlines
        })), name = group.legend.title, drop = drop) + 
        guides(color = guide_legend(override.aes = list(color = legend_colors[trendline_groups]))) # Override colors in legend to be the original colors
    }
    
    # Scale x axis
    if(scale.x.log[which(plotcols == col)] == TRUE){ # Log scale
      if(any(plotdata[[col]] <= 0, na.rm = TRUE)){
        plot <- plot + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels, trans=pseudo_log_trans(base = 10, sigma = xsigma)) # Pseudo-log if 0 or negative values
      } else{
        plot <- plot + scale_x_log10(limits = xlimits, breaks = xbreaks, labels = xlabels)
      }
    } else{ # Normal scale
      plot <- plot + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels)
    }

    # Scale y axis
    if(scale.y.log[which(plotcols == col)] == TRUE){ # Log scale
      if(any(plotdata[[colnames(subass)[subass.column]]] <= 0, na.rm = TRUE)){
        plot <- plot + scale_y_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels, trans=pseudo_log_trans(base = 10, sigma = ysigma)) # Psuedo-log if 0 or negative values
      } else{
        plot <- plot + scale_y_log10(limits = ylimits, breaks = ybreaks, labels = ylabels)
      }
    } else{ # Normal scale
      plot <- plot + scale_y_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels)
    }
    
    # Get Cartesian coordinate limits so that main plot and density plots align
    if(is.na(xlimits[1])){
      cart_x_min <- min(plotdata[[col]], na.rm = TRUE)
    } else{
      cart_x_min <- xlimits[1]
    }
    
    if(is.na(xlimits[2])){
      cart_x_max <- max(plotdata[[col]], na.rm = TRUE)
    } else{
      cart_x_max <- xlimits[2]
    }
    
    if(is.na(ylimits[1])){
      cart_y_min <- min(plotdata[[colnames(subass)[subass.column]]], na.rm = TRUE)
    } else{
      cart_y_min <- ylimits[1]
    }
    
    if(is.na(ylimits[2])){
      cart_y_max <- max(plotdata[[colnames(subass)[subass.column]]], na.rm = TRUE)
    } else{
      cart_y_max <- ylimits[2]
    }

    # Format plot
    plot <- plot +
      coord_cartesian(xlim = c(cart_x_min, cart_x_max), ylim = c(cart_y_min, cart_y_max)) +
      theme(axis.title = element_text(face = "bold"),
            legend.position = "bottom")
    
    # Add density plots
    if(density.plot == TRUE){
      
      if(!is.null(groups)){
        if (!is.null(groups.color.pal)) { # If custom colors exist
          if(density.plot.type == "density"){
            # Create density plot for x-axis
            densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
              geom_density(size = 0.2, alpha = 0.4) +
              scale_fill_manual(values = manual_colors[x_groups], name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none")
            
            # Create density plot for y-axis
            densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
              geom_density(size = 0.2, alpha = 0.4) +
              scale_fill_manual(values = manual_colors, name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none") +
              coord_flip()
          } else if(density.plot.type == "boxplot"){
            # Create density plot for x-axis
            densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
              geom_boxplot(size = 0.2, alpha = 0.4, outlier.shape = NA) +
              scale_fill_manual(values = manual_colors[x_groups], name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none")
            
            # Create density plot for y-a.xis
            densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
              geom_boxplot(size = 0.2, alpha = 0.4, outlier.shape = NA) +
              scale_fill_manual(values = manual_colors, name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none") +
              coord_flip()
          }
        } else{ # Use default colors
          if(density.plot.type == "density"){
            # Create density plot for x-axis
            densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
              geom_density(size = 0.2, alpha = 0.4) +
              scale_fill_manual(values = gg_colors[x_groups], name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none")
            
            # Create density plot for y-axis
            densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
              geom_density(size = 0.2, alpha = 0.4) +
              scale_fill_manual(values = gg_colors, name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none") +
              coord_flip()
          } else if(density.plot.type == "boxplot"){
            # Create density plot for x-axis
            densx <- ggplot(plotdata, aes(x = !!sym(col), fill = !!sym("Group"))) +
              geom_boxplot(size = 0.2, alpha = 0.4, outlier.shape = NA) +
              scale_fill_manual(values = gg_colors[x_groups], name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none")
            
            # Create density plot for y-axis
            densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]), fill = !!sym("Group"))) +
              geom_boxplot(size = 0.2, alpha = 0.4, outlier.shape = NA) +
              scale_fill_manual(values = gg_colors, name = group.legend.title) +
              theme_void()+
              theme(legend.position = "none") +
              coord_flip()
          }
        }
      } else{
        if(density.plot.type == "density"){
          # Create density plot for x-axis
          densx <- ggplot(plotdata, aes(x = !!sym(col))) +
            geom_density(fill = "#619CFF", size = 0.2, alpha = 1) +
            theme_void()
          
          # Create density plot for y-axis
          densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]))) +
            geom_density(fill = "#619CFF", size = 0.2, alpha = 1) +
            theme_void() +
            coord_flip()
        } else if(density.plot.type == "boxplot"){
          # Create density plot for x-axis
          densx <- ggplot(plotdata, aes(x = !!sym(col))) +
            geom_boxplot(fill = "#619CFF", size = 0.2, alpha = 1, outlier.shape = NA) +
            theme_void()
          
          # Create density plot for y-axis
          densy <- ggplot(plotdata, aes(x = !!sym(colnames(subass)[subass.column]))) +
            geom_boxplot(fill = "#619CFF", size = 0.2, alpha = 1, outlier.shape = NA) +
            theme_void() +
            coord_flip()
        }
      }
      
      # Scale x axis
      if(scale.x.log[which(plotcols == col)] == TRUE){ # Log scale
        if(any(plotdata[[col]] <= 0, na.rm = TRUE)){
          densx <- densx + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels, trans=pseudo_log_trans(base = 10, sigma = xsigma)) # Psuedo-log if 0 or negative values
        } else{
          densx <- densx + scale_x_log10(limits = xlimits, breaks = xbreaks, labels = xlabels)
        }
      } else{ # Normal scale
        densx <- densx + scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels)
      }
      
      # Scale y axis
      if(scale.y.log[which(plotcols == col)] == TRUE){ # Log scale
        if(any(plotdata[[colnames(subass)[subass.column]]] <= 0, na.rm = TRUE)){
          densy <- densy + scale_x_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels, trans=pseudo_log_trans(base = 10, sigma = ysigma)) # Psuedo-log if 0 or negative values
        } else{
          densy <- densy + scale_x_log10(limits = ylimits, breaks = ybreaks, labels = ylabels)
        }
      } else{ # Normal scale
        densy <- densy + scale_x_continuous(limits = ylimits, breaks = ybreaks, labels = ylabels)
      }
      
      # Backup legend
      plot_legends[[col]] <- plot + theme(legend.title = element_text(face = "bold"))
      
      # Remove legend from plot
      plot <- plot + theme(legend.position = "none")
      
      # Create arranged plot - separate y axes, only one column, or plot is the only plot on a row
      if(common.y.axis == FALSE | ncol == 1 | (col %in% plotcols[1 + ncol * seq(0,nrow - 1)] & col %in% plotcols[c(ncol * seq(1, nrow), length(plotcols))])){
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
      } else{
        # First plot on a row
        if(col %in% plotcols[1 + ncol * seq(0,nrow - 1)]){
          plot <- densx +
            plot + 
            plot_layout(
              ncol = 1, 
              nrow = 2,
              widths = 4,
              heights = c(1, 4)
            ) 
        # Last plot on a row
        } else if(col %in% plotcols[c(ncol * seq(1, nrow), length(plotcols))]){
          plot <- plot + ylab("") # Remove y-axis title
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
        # Middle plots
        } else{
          plot <- plot + ylab("") # Remove y-axis title
          plot <- densx + 
            plot + 
            plot_layout(
              ncol = 1, 
              nrow = 2,
              widths = 4,
              heights = c(1, 4)
            ) 
        }
      }
    }

    # Store plot in list
    plots[[col]] <- plot
  }
  
  # Specify arranged plot widths
  # - Currently, if there are fewer plots on the last row, density.plot == TRUE, and common.y.axis == TRUE, then the width of the last plot in the last row is narrower compared to the others
  #   A potential fix to this would be to add plot spacers to all of the plots in that column and then adjust the arrange_widths to be 5 for that column instead of 4, but then there will be 
  #   unequal amounts of white space between the different columns
  if(density.plot == TRUE & common.y.axis == TRUE){
    arrange_width = c(rep(4,(ncol-1)), 5) # Need to have wider last plot because it contains the density plot
  } else{
    arrange_width = 1
  }

  # Arrange plots
  if(density.plot == TRUE){
    
    # Try grabbing legends until one works - sometimes needed when drop == TRUE
    pass <-  FALSE # Logical for if legend worked
    for(i in 1:length(plot_legends)){ # Loop through legends
      if(pass == FALSE){
        try_legend <- possibly(~get_legend(plot_legends[i]), otherwise = NA)
        legend.grob <- try_legend()
        if(any(!is.na(legend.grob))){
          pass <- TRUE
        }
      }
    }
    
    # Arrange plot and suppress warning about alignment
    if(legend.position == "none"){
      arrangeplot <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow, align = align, widths = arrange_width, common.legend = common.legend, legend = legend.position)
    } else{
      arrangeplot <- suppressWarnings(ggarrange(plotlist = plots, ncol = ncol, nrow = nrow, align = align, widths = arrange_width, common.legend = common.legend, legend = legend.position, legend.grob = legend.grob))
    }
  } else{
    arrangeplot <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow, align = align, widths = arrange_width, common.legend = common.legend, legend = legend.position)
  }

  # Add summary stats table
  if (summary.table == TRUE) {

    # Calculate Summary Stats
    if (!is.null(groups)) {
      table <- plotdata %>%
        group_by(.data[["Group"]]) %>%
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
          tbody.style = tbody_style(size = 8, color = "black", fill = manual_colors)
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
    arrangeplot <- ggarrange(arrangeplot, table.p, nrow = 2, heights = c(1, table.margin))
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
