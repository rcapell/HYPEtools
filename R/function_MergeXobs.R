#' @export

#' @title
#' Merge two Xobs data frames
#'
#' @description
#' Function to merge two Xobs data frames, with handling of overlapping time periods and time periods gaps 
#' as well as merging of common columns.
#'
#' @param x,y Data frames with additional attributes \code{comment}, \code{variable}, and \code{subid}, 
#' typically imported using \code{\link{ReadXobs}}. For details on attribute format, see there.
#' 
#' @param x.first Logical, indicating which input data frame will be given precedence in case of common 
#' columns (identical observation variable and SUBID combinations in \code{x} and \code{y}). If \code{TRUE}, 
#' values from columns in \code{x} will take precedence, and values from \code{y} will only be added if 
#' \code{x} values are missing. Vice versa if \code{FALSE}.
#' 
#' @param comment Character string, will be added to the result as attribute \code{comment}. If empty, 
#' comment attributes from \code{x} and \code{y} will be merged to new comment string.
#' 
#' @details
#' \code{MergeXobs} handles time steps of different lengths (e.g. daily, hourly), but requires identical time 
#' step lengths from both input data frames.
#' 
#' @return
#' \code{MergeXobs} returns a data frame with attributes for Xobs data.
#' 
#' 
#' @examples
#' \dontrun{MergeXobs(x = myxobs1, y = myxobs2)}


MergeXobs <- function(x, y, x.first = TRUE, comment = "") {
  
  # check time step with in both inputs and if they are identical
  x.tstep <- difftime(x[2, 1], x[1, 1])
  y.tstep <- difftime(x[2, 1], x[1, 1])
  if (x.tstep != y.tstep) {
    stop("Time step lengths in 'x' and 'y' differ.")
  }
  
  # Create data frame with common time period column for both input xobs in appropriate time steps 
  date.min <- min(min(x[, 1]), min(y[, 1]))
  date.max <- max(max(x[, 1]), max(y[, 1]))
  res <- data.frame(date = seq(date.min, date.max, by = x.tstep))
  

  # merge x and y with new time axis individually
  if(x.first) {
    res1 <- merge(res, x, by = 1, all = TRUE)
    res2 <- merge(res, y, by = 1, all = TRUE)
  } else {
    res1 <- merge(res, y, by = 1, all = TRUE)
    res2 <- merge(res, x, by = 1, all = TRUE)
  }
  
  # extract variable names from the merged data and match common column where observations have to be merged
  names1 <- names(res1)[-1]
  names2 <- names(res2)[-1]
  common.cols <- c(NA, match(names1, names2))
  
  # conditional: common columns exist, merge them
  if(length(na.omit(common.cols)) > 0) {
    
    # merge candidates from res1
    te1 <- res1[, !is.na(common.cols)]
    # merge candidates from res2
    te2 <- res2[, as.integer(na.omit(common.cols)) + 1]
    
    # fill observations from xobs without precedence into the one with precedence, if no obs exist there
    # mapply this to all identified columns (te1 and te2 are ALWAYS of the same length, therefore mapply is safe)
    te3 <- as.data.frame(mapply(function(x, y) {ifelse(!is.na(x), x, y)}, te1, te2))
    
    # update columns in data source with precedence
    res1[, !is.na(common.cols)] <- te3
    
    # remove columns from data source without precedence
    res2 <- res2[, -(as.integer(na.omit(common.cols)) + 1)]
  }
  
  # combine the results
  res <- cbind(res, res1[, -1], res2[, -1])
  
  # reconstruct xobs attributes variable and subid from header
  attr(xobsMerge1,"variable") <- sapply(strsplit(names(res), "_"), function(x){x[1]})[-1]
  attr(xobsMerge1,"subid") <- as.integer(sapply(strsplit(names(res), "_"), function(x){x[2]})[-1])
  
  # Comment attribute, conditional on function argument value
  if (comment == "") {
    if (x.first) {
      attr(xobsMerge1,"comment") <- paste(paste("!Created by MergeXobs from [","] and [",sep=attr(x,"comment")),"]",sep = attr(y,"comment"))
    } else {
      attr(xobsMerge1,"comment") <- paste(paste("!Created by MergeXobs from [","] and [",sep=attr(y,"comment")),"]",sep=attr(x,"comment"))
    }
  } else {
    attr(xobsMerge1,"comment") <- comment
  }
  
  return(res)
}