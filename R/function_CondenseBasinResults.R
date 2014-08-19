
# Condense basin result time series to longer time steps

# x Data frame, typically a
# in.step Character string keyword, giving the time step width in the input data,
# start.month Integer, giving the starting month of the hydrological year as a number between 1 and 12.
# \code{CondenseBasinResults} does not check for incomplete periods, users are expected to pass meaninful time periods to the functions.

CondenseBasinResults <- function(x, in.step = "day", start.month = 10) {
  
  # check validity of outformat argument
  if (outformat != "df" & outformat != "m" & outformat != "dataframe" & outformat != "matrix") {
    stop("Argument 'outformat' invalid.")
  }
  
  
}