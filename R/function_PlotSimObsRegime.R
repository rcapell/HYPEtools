#' Plot annual regimes of simulated and observed variables
#' 
#' A combined plot for annual regimes with box plot elements for observed and ribbon elements 
#' for simulated variables. Particularly designed for comparisons of sparse observations with
#' model high-density model results, e.g. for in-stream nutrients.
#' 
#' @param x Data frame, with column-wise equally-spaced time series of HYPE variables. Date-times in 
#' \code{\link{POSIXct}} format in first column. Typically an imported basin output file from HYPE using \code{\link{ReadBasinOutput}}. 
#' See details for HYPE output variables required for plotting.
#' 
#' 
#' @seealso 
#' \code{\link{PlotAnnualRegime}} for a more generic annual regime plot, \code{\link{AnnualRegime}} to compute annual regimes only.


PlotSimObsRegime <- function() {
  
}

