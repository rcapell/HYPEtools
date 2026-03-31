#' Plot HYPE model parameter values.
#'
#' Plot parameter values for HYPE par.txt files.
#'
#' @param par A HYPE par.txt file read in with \code{\link{ReadPar}} or a named list of par.txt files read in with \code{\link{ReadPar}}. E.g. \code{par <- list("par_1" = par1, "par2" = par2)}.
#' @param ignore_parameters A list containing HYPE parameter names that should be ignored while plotting. E.g. \code{ignore_pars <- c("par1", "par2")}.
#' @param n_plots Integer, if greater than 1, then the HYPE parameters will be split among *n* plots. Useful if plotting many parameters.
#' @param col.values A list containing color values to set custom colors. Length of \code{col.values} should match length of \code{par}. See [ggplot2::scale_color_manual].
#' @param file Optional filename used to save plot(s) to file. See [ggplot2::ggsave].
#' @param width Width in inches for output plot. See [ggplot2::ggsave].
#' @param height Height in inches for output plot. See [ggplot2::ggsave].
#'
#' @details
#' \code{PlotParValues} generates a set of faceted boxplots to show the parameter values for a given HYPE par.txt file or list of par.txt files. The plots can be used to
#' compare parameter values between models.
#'
#' @return
#' Returns an list of plot objects
#'
#' @examples
#' par <- ReadPar(
#'   system.file(
#'     "demo_model", "par.txt",
#'     package = "HYPEtools"
#'   )
#' )
#' PlotParValues(par)
#'
#' @importFrom dplyr %>% filter arrange
#' @importFrom ggplot2 facet_wrap ggplot ggsave geom_boxplot xlab ylab theme
#' @importFrom stringr str_starts
#' @importFrom tidyr pivot_longer unnest
#' @importFrom tools file_path_sans_ext file_ext
#' @export

PlotParValues <- function(par, ignore_parameters = NULL, n_plots = 1, col.values = NULL, file = NULL, width = NULL, height = NULL){
  
  # Get parameter names
  if(is_nested_list(par)){
    par_names <- sapply(par, function(X){names(X)}) %>% unlist()
  } else{
    par_names <- names(par)
  }
  
  # Create dataframe to store parameter data
  par_data <- data.frame(parameter = unique(par_names)) %>% # List all parameters to plot
    filter(!str_starts(parameter, "!")) %>%
    arrange(parameter)
  
  # Remove ignored parameters
  if(!is.null(ignore_parameters)){
    par_data <- par_data %>% filter(!parameter %in% ignore_parameters)
  }

  # Add parameter data
  if(is_nested_list(par)){
    for(name in names(par)){
      par_data[[name]] <- sapply(par_data$parameter, function(X){par[[name]][which(names(par[[name]]) == X)] %>% unique() %>% unlist()})
    }
  } else{
    par_data$par <- sapply(par_data$parameter, function(X){par[which(names(par) == X)] %>% unique() %>% unlist()})
  }

  # Reshape data
  par_data <- par_data %>%
    pivot_longer(cols = !matches("parameter"), names_to = "Model", values_to = "value") %>%
    unnest(value)

  # Divide parameters into chunks
  par_chunks <- chunk_list(unique(par_data$parameter), n_plots)

  # Create list to save plots
  plot_list <- vector("list")

  # Generate Plots
  for(i in 1:length(par_chunks)){

    # Create plot
    plot <- ggplot(par_data %>% filter(parameter %in% par_chunks[[i]])) +
      geom_boxplot(aes(x = .data$parameter, y = .data$value, color = .data$Model))

    # Apply custom colors
    if(!is.null(col.values)){
      plot <- plot + scale_color_manual(values = col.values)
    }

    # Format Plot
    plot <- plot +
      xlab("Parameter") +
      ylab("Parameter Value") +
      theme(
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold")
      ) +
      facet_wrap(~parameter, scales = "free")

    # Save plot to list
    plot_list[[paste("Plot", i)]] <- plot

  }

  # Save plots
  if(!is.null(file)){
    for(i in 1:length(plot_list)){

      # Append suffix to filename if multiple plots
      if(n_plots > 1){
        filename = append_suffix(file, i)
      } else{
        filename = file
      }
      ggsave(filename = filename, plot = plot_list[i], width = width, height = height)
    }
  }
  
  # Return plots as a list
  return(plot_list)
  
}

# Helper Functions --------------------------------------------------------------------------------------------------------------------

# Function to determine if object is a nested list
#' @noRd
is_nested_list <- function(x) {
  is.list(x) && any(vapply(x, is.list, logical(1)))
}

# Function to divide list into n chunks with equal size
#' @noRd
chunk_list <- function(x, n) {
  n <- max(1L, as.integer(n))
  len <- length(x)
  # group indices from 1..len into n groups as evenly as possible
  idx <- ceiling(seq_len(len) * n / len)
  split(x, idx)
}

# Function to append suffix to filename
#' @noRd
append_suffix <- function(filename, suffix) {
  file_path_sans_ext(filename) |>
    paste0("_", toString(suffix), ".", file_ext(filename))
}
