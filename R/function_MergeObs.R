#' Merge two HYPE observation data frames
#'
#' Function to merge two HYPE observation data frames, with handling of overlapping time periods and time periods gaps 
#' as well as merging of common columns.
#'
#' @param x,y Data frames containing observation timeseries data. Typically imported using \code{\link{ReadObs}}.
#' 
#' @details
#' \code{MergeObs} handles time steps of different lengths (e.g. daily, hourly), but requires identical time 
#' step lengths from both inputs data frames.
#' 
#' In case of common columns (identical date and SUBID combinations in \code{x} and \code{y}), 
#' values from columns in \code{x} will take precedence, and values from \code{y} will only be added if 
#' \code{x} values are missing.
#' 
#' @return
#' \code{MergeObs} returns a data frame with merged Obs data.
#' 
#' 
#' @examples
#' # Import dummy data, add new observations to second Obs table, and merge
#' te1 <- ReadObs(filename = system.file("demo_model", "Tobs.txt", package = "HYPEtools"))
#' te2 <- ReadObs(filename = system.file("demo_model", "Tobs.txt", package = "HYPEtools"))
#' te2$X0000[1:365] <- runif(n = 365, -20, 25)
#' MergeObs(x = te1, y = te2)
#' 
#' @importFrom dplyr select %>%
#' @importFrom stats na.omit
#' @export


MergeObs <- function(x, y) {
  
  # check time step with in both inputs and if they are identical
  # requires equidistant time steps
  x.tstep <- difftime(x[2, 1], x[1, 1])
  y.tstep <- difftime(x[2, 1], x[1, 1])
  
  if (x.tstep != y.tstep) {
    stop("Time step lengths in 'x' and 'y' differ.")
  }
  
  # check if there are any duplicated dates in x or y
  if (anyDuplicated(x[, 1]) || anyDuplicated(y[, 1])) {
    stop("Duplicated dates in either 'x' or 'y'.")
  }
  
  # Create data frame with common time period column for both input Obs in appropriate time steps 
  date.min <- min(min(x[, 1]), min(y[, 1]))
  date.max <- max(max(x[, 1]), max(y[, 1]))
  res <- data.frame(date = seq(date.min, date.max, by = x.tstep))
  
  # merge x and y with new time axis individually
  res1 <- merge(res, x, by = 1, all = TRUE)
  res2 <- merge(res, y, by = 1, all = TRUE)
  
  # extract variable names from the merged data and match common column where observations have to be merged
  common.cols <- match(colnames(res1)[2:ncol(res1)], colnames(res2)[2:ncol(res2)])
  
  # conditional: common columns exist, merge them
  if(length(na.omit(common.cols)) > 0) {
    cat("Common columns found, merging.\n")
    cat(paste0("Common column indices in 'x': ", paste(which(!is.na(common.cols)) + 1, collapse = " "), "\n"))
    cat(paste0("Common column indices in 'y': ", paste(as.integer(na.omit(common.cols)) + 1, collapse = " "), "\n"))
    # columns to merge, res1
    te1 <- res1[, c(TRUE, !is.na(common.cols))]
    # columns to merge, res2
    te2 <- res2[, c(1, as.integer(na.omit(common.cols)) + 1)]
    
    # fill observations from Obs without precedence into the one with precedence, if no Obs exist there
    # mapply this to all identified columns (te1 and te2 are ALWAYS of the same length, therefore mapply is safe)
    te3 <- as.data.frame(mapply(function(x, y) {ifelse(!is.na(x), x, y)}, te1, te2))
    
    # update columns in data source with precedence
    res1[, c(FALSE, !is.na(common.cols))] <- te3[, -1]
    
    # remove columns from data source without precedence
    res2 <- res2[, -(as.integer(na.omit(common.cols)) + 1)]
    
  }
  
  # combine the results, catch special case where all columns are common and only a date vector is left in res2
  if(is.data.frame(res2)) {
    res <- suppressWarnings(cbind(res, res1 %>% select(-1), res2 %>% select(-1)))
  } else {
    res <- suppressWarnings(cbind(res, res1[, -1]))
  }
  
  return(res)
}
