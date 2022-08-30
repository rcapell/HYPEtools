
#--------------------------------------------------------------------------------------------------------------------------------------
#   Collection of internal helper functions, which are not exported from the package's namespace. Herein:
#
#     - .CheckcharLengthDf()
#     - .FindUpstrSbd()
#     - .CreateLabelsFromBreaks()
#     - .Scalebar()
#     - .NorthArrow()
#     - .FillWeek()
#     - .makeTransparent()
#     - .ExtractHeader()
#     - 
#
#--------------------------------------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------
# .CheckCharLengthDf
#--------------------------------------------------------------------------------------------------------------------------------------

## internal function to test if string columns elements are longer than a number of characters
# x: a dataframe to be tested
# maxChar: maximum number of characters
.CheckCharLengthDf <- function (x, maxChar) {
  
  # which columns are of type factor or character (strings)
  facts <- sapply(x, function(z) {is.factor(z)})
  chars <- sapply(x, function(z) {is.character(z)})
  lf <- length(which(facts))
  lc <- length(which(chars))
  
  # select and convert factor columns to a character matrix, if factors exist
  if (lf > 0) {
    facmat <- apply(as.matrix(x[, facts]), 2, as.character)
    # test if the longest string in any column is longer than maxChar characters, return with warning
    if (max(apply(facmat, 2, function (z) max(nchar(encodeString(z), allowNA = TRUE)))) > maxChar) {
      warning(paste("String with more than", maxChar, "characters in exported data detected. This will lead to an error in HYPE."))
    }
  }
  # select and convert character columns to a character matrix, if characters exist
  if (lc > 0) {
    chamat <- as.matrix(x[, chars])
    # test if the longest string in any column is longer than maxChar characters, return with warning
    te <- apply(chamat, 2, function (z) {max(nchar(encodeString(z), allowNA = TRUE), na.rm = TRUE)})
    if (any(te > maxChar)) {
      warning(paste0("String with more than ", maxChar, " characters in detected in column(s): ", paste(names(te[te > maxChar]), collapse = ","), ". This is not HYPE-comform."))
    }  
  }
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .FindUpstrSbd
#--------------------------------------------------------------------------------------------------------------------------------------


# internal function to create upstream subid list, used in DirectUpstreamSubids with 'lapply' on all subids in gd
# sbd: subid for which upstreams are searched
# dtf: df data frame which contains columns subid, maindown, branchid, mainpart, maxqmain, minqmain, maxqbranch
.FindUpstrSbd <- function(sbd, dtf) {
  # look for subid in maindown and branchid columns, concatenate positions
  mup <- dtf[which(dtf[,2] == sbd), c(1, 4, 5, 6)]
  bup <- dtf[which(dtf[,3] == sbd), c(1, 4, 7)]
  
  ## calculations of a main or branch upstream characteristics conditional on that upstreams exist
  if (nrow(mup) == 0 & nrow(bup) == 0) {
    # no upstreams exist, return an integer of length 0
    return(list(subid = sbd, upstr.df = integer()))
  }
  else {
    # if either or both main and branch upstreams exist, update to fraction of flow coming down, and the optional maximum flow
    if (nrow(mup) != 0) {
      mup <- data.frame(upstream = mup[,1], is.main = TRUE, fraction = ifelse(is.na(mup[,2]), 1, mup[,2]), llim = ifelse(is.na(mup[,3]), 0, mup[,3]), ulim = ifelse(is.na(mup[,3]), Inf, mup[,3]))
    }
    if (nrow(bup) != 0) {
      bup <- data.frame(upstream = bup[,1], is.main = FALSE, fraction = 1 - bup[,2], llim = 0, ulim = ifelse(is.na(bup[,3]), Inf, bup[,3]))
    }
    # combine the two and return result
    res <- rbind(mup, bup)
    return(list(subid = sbd, upstr.df = res))
  }
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .CreateLabelsFromBreaks
#--------------------------------------------------------------------------------------------------------------------------------------


# Internal function to make pretty label expressions from a given vector of breakpoints, which is used to convert 
# a continuous scale to a discrete one. Used for map plot function legends => not any longer, but function kept for future ref.
# breaks: vector of breakpoints
.CreateLabelsFromBreaks <- function(breaks) {
  # create first element as an expression which uses the value given in breaks
  lab.legend <- as.expression(bquote("" < .(round(breaks[2], 2))))
  # create following elements analoguously
  for (i in 2:(length(breaks)-2)) {
    lab.legend[i] <- as.expression(bquote("" >= .(round(breaks[i], 2)) - .(round(breaks[i+1], 2))))
  }
  # create the last element
  lab.legend[length(breaks)-1] <- as.expression(bquote("" >= .(round(breaks[length(breaks)-1], 2))))
  
  return(lab.legend)
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .Scalebar
#--------------------------------------------------------------------------------------------------------------------------------------


# Internal function to add a distance scalebar to a projected map.
# Code adapted from function Scalebar() in package SDMTools by Jeremy VanDerWal jjvanderwal@gmail.com. 
# x:        the x-axis position for the lower left corner of the bar
# y:        the y-axis position for the lower left corner of the bar
# distance: the distance for which the scale bar should represent
# unit:     the units to report as the scaling
# scale:    the scaling factor to rescale the distance to a different unit.
#           e.g., if your map is in m and want the scalebar to be in km, use a scale of 0.01
#t.cex:     the scaling of the font size to be used for the scalebar

#' @importFrom graphics rect text segments

.Scalebar <- function (x, y, distance, unit = "km", scale = 1, t.cex = 0.8) {
  xvals <- distance * c(0, 0.25, 0.5, 0.75, 1) + x
  yvals <- c(0, distance/c(30, 20, 10)) + y
  cols <- c("black", "white", "black", "white")
  for (i in 1:4) rect(xvals[i], yvals[1], xvals[i + 1], yvals[2], col = cols[i])
  for (i in 1:5) segments(xvals[i], yvals[2], xvals[i], yvals[3])
  labels <- c((xvals[c(1, 3)] - xvals[1]) * scale, paste((xvals[5] - xvals[1]) * scale, unit))
  labels <- c((xvals[c(1, 3, 5)] - xvals[1]) * scale, unit)
  text(c(xvals[c(1, 3, 5)], xvals[5] + diff(xvals[1:2])*.8), yvals[4], labels = labels, adj = c(0.5, 0.2), cex = t.cex)
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .NorthArrow
#--------------------------------------------------------------------------------------------------------------------------------------


# Internal function to add a North arrow to a map plot.
# Code adapted from function north.arrow in package GISTools Chris Brunsdon <christopher.brunsdon@nuim.ie>.
# xb:      The x-centre (in map units) of the arrow base.
# yb:      The y-centre (in map units) of the arrow base.
# len:     A scaling length (in map units) for arrow dimensioning.
# lab:     Label to appear under the arrow
# cex.lab: Scale factor for the label for the arrow.
# tcol:    The colour of the label text.
# ...:     Other graphical parameters passed to the drawing of the arrow.

#' @importFrom graphics polygon text strheight

.NorthArrow <- function (xb, yb, len, lab = "N", cex.lab = 1, tcol = "black", ...) {
  sx <- len * .5
  sy <- len
  arrow.x <- c(-1, 1, 1, 2, 0, -2, -1, -1)
  arrow.y <- c(0, 0, 2, 2, 4, 2, 2, 0)
  polygon(xb + arrow.x * sx, yb + arrow.y * sy, ...)
  text(xb, yb - strheight(lab, cex = cex.lab) * .9, lab, cex = cex.lab, adj = 0.4, 
       col = tcol)
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .FillWeek
#--------------------------------------------------------------------------------------------------------------------------------------


# Internal function to fill weekly averages written on last day of the week in a daily time series into preceeding NAs
# used in AnnualRegime()
.FillWeek <- function(y) {
  # reverse y
  y <- rev(y)
  # positions of non-NA values
  ind <- which(!is.na(y))
  # repeat the original values each 7 times, except for the last
  y <- rep(y[ind], times = c(rep(7, times = length(ind) - 1), 1))
  # reverse to original order again
  y <- rev(y)
  return(y)
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .makeTransparent
#--------------------------------------------------------------------------------------------------------------------------------------


# internal function to calculate transparent colors for variation polygon
# from: http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color

#' @importFrom grDevices col2rgb rgb

.makeTransparent <- function(someColor, alpha=60) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], alpha = alpha, maxColorValue = 255)})
}




#--------------------------------------------------------------------------------------------------------------------------------------
# .ExtractHeader
#--------------------------------------------------------------------------------------------------------------------------------------


# internal function to extract key-value pairs from metadata header row of HYPE result files
.ExtractHeader <- function(x) {
  
  # clip comment characters and separate key-value pairs
  te <- strsplit(substr(x, 4, nchar(x) - 1), split = "; ")[[1]]
  
  # split key-value pairs into named list
  te <- lapply(te, function(x) strsplit(x, split = "=")[[1]])
  res <- lapply(te, function(x) x[2])
  names(res) <- tolower(lapply(te, function(x) x[1]) )
}
