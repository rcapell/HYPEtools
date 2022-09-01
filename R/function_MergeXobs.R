#' Merge two Xobs data frames
#'
#' Function to merge two Xobs data frames, with handling of overlapping time periods and time periods gaps 
#' as well as merging of common columns.
#'
#' @param x,y Data frames of class \code{\link{HypeXobs}}, including additional attributes \code{comment}, 
#' \code{variable}, \code{subid}, and \code{timestep}, typically imported using \code{\link{ReadXobs}}. 
#' For details on attribute format, see the class description. Class attribute not formally necessary.
#' @param comment Character string, will be added to the result as attribute \code{comment}. If empty, 
#' comment attributes from \code{x} and \code{y} will be merged to new comment string.
#' 
#' @details
#' \code{MergeXobs} handles time steps of different lengths (e.g. daily, hourly), but requires identical time 
#' step lengths from both inputs data frames. The functions expects data frames of class \code{\link{HypeXobs}} 
#' or data frames with comparable structure and will throw a warning if the class attribute is missing.
#' 
#' In case of common columns (identical observation variable and SUBID combinations in \code{x} and \code{y}), 
#' values from columns in \code{x} will take precedence, and values from \code{y} will only be added if 
#' \code{x} values are missing
#' 
#' @return
#' \code{MergeXobs} returns a data frame with attributes for Xobs data.
#' 
#' 
#' @examples
#' # Import dummy data, add new observations to second Xobs table
#' te1 <- ReadXobs(filename = system.file("demo_model", "Xobs.txt", package = "HYPEtools"))
#' te2 <- ReadXobs(filename = system.file("demo_model", "Xobs.txt", package = "HYPEtools"))
#' te2$WSTR_40541[1:10] <- runif(n = 10, 50, 100)
#' MergeXobs(x = te1, y = te2)
#' 
#' @importFrom stats na.omit
#' @export


MergeXobs <- function(x, y, comment = "") {
  
  # check time step with in both inputs and if they are identical
  # requires equidistant time steps
  if (!any(class(x) == "HypeXobs")) {
    warning("'x' not of class HypeXobs.")
    x.tstep <- difftime(x[2, 1], x[1, 1])
  } else {
    x.tstep <- attr(x, "timestep")
  }
  if (!any(class(y) == "HypeXobs")) {
    warning("'y' not of class HypeXobs.")
    y.tstep <- difftime(x[2, 1], x[1, 1])
  } else {
    y.tstep <- attr(y, "timestep")
  }
  
  if (x.tstep != y.tstep) {
    stop("Time step lengths in 'x' and 'y' differ.")
  }
  
  # check if there are any duplicated dates in x or y
  if (anyDuplicated(x[, 1]) || anyDuplicated(y[, 1])) {
    stop("Duplicated dates in either 'x' or 'y'.")
  }
  
  # Create data frame with common time period column for both input xobs in appropriate time steps 
  date.min <- min(min(x[, 1]), min(y[, 1]))
  date.max <- max(max(x[, 1]), max(y[, 1]))
  res <- data.frame(date = seq(date.min, date.max, by = x.tstep))
  

  # merge x and y with new time axis individually
  res1 <- merge(res, x, by = 1, all = TRUE)
  attr(res1, "comment") <- attr(x, "comment")
  attr(res1, "variable") <- attr(x, "variable")
  attr(res1, "subid") <- attr(x, "subid")
  attr(res1, "class") <- attr(x, "class")
  attr(res1, "timestep") <- attr(x, "timestep")
  
  res2 <- merge(res, y, by = 1, all = TRUE)
  attr(res2, "comment") <- attr(y, "comment")
  attr(res2, "variable") <- attr(y, "variable")
  attr(res2, "subid") <- attr(y, "subid")
  attr(res2, "class") <- attr(y, "class")
  attr(res2, "timestep") <- attr(y, "timestep")
  
  
  # extract variable names from the merged data and match common column where observations have to be merged
  names1 <- paste0(attr(res1, "variable"), "_", attr(res1, "subid"))
  names2 <- paste0(attr(res2, "variable"), "_", attr(res2, "subid"))
  common.cols <- match(names1, names2)
  
  # conditional: common columns exist, merge them
  if(length(na.omit(common.cols)) > 0) {
    cat("Common columns found, merging.\n")
    cat(paste0("Common column indices in 'x': ", paste(which(!is.na(common.cols)) + 1, collapse = " "), "\n"))
    cat(paste0("Common column indices in 'y': ", paste(as.integer(na.omit(common.cols)) + 1, collapse = " "), "\n"))
    # columns to merge, res1
    te1 <- res1[, c(TRUE, !is.na(common.cols))]
    # columns to merge, res2
    te2 <- res2[, c(1, as.integer(na.omit(common.cols)) + 1)]
    
    # fill observations from xobs without precedence into the one with precedence, if no obs exist there
    # mapply this to all identified columns (te1 and te2 are ALWAYS of the same length, therefore mapply is safe)
    te3 <- as.data.frame(mapply(function(x, y) {ifelse(!is.na(x), x, y)}, te1, te2))
    
    # update columns in data source with precedence
    res1[, c(FALSE, !is.na(common.cols))] <- te3[, -1]
    
    # remove columns from data source without precedence
    res2 <- res2[, -(as.integer(na.omit(common.cols)) + 1)]
    
  }
  
  # combine the results, catch special case where all columns are common and only a date vector is left in res2
  if(is.data.frame(res2)) {
    res <- suppressWarnings(cbind(res, res1[, -1], res2[, -1]))
  } else {
    res <- suppressWarnings(cbind(res, res1[, -1]))
  }
  
  # update comment attribute, conditional on function argument value
  if (comment == "") {
    comment <- paste0("!Created by MergeXobs. Original comments: ", 
                                   attr(x,"comment"), " (x); ", attr(y,"comment"), " (y)")
  }
  
  # reconstruct other HypeXobs attributes from res1 and res2
  res <- HypeXobs(x = res, comment = comment, 
                  variable = c(attr(res1, "variable"), attr(res2, "variable")), 
                  subid = c(attr(res1, "subid"), attr(res2, "subid")))
  
  return(res)
}