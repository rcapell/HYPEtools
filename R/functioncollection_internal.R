#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of internal helper functions, which are not exported from the package's namespace. Herein:
#
#     - .CheckcharLengthDf()
#     - .FindUpstrSbd()
#     - .
#     - .ReadBasinOutput() => SEEMS OUT OF DATE, EXISTS AS ReadBasinOutput() in functioncollection_export.R
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.CheckCharLengthDf~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



## internal function to test if string columns elements are longer than a number of characters
# x: a dataframe to be tested
# maxChar: maximum number of characters
.CheckCharLengthDf <- function (x, maxChar) {
  ## test if string columns elements are longer than 50 characters, which is the maximum accepted for BranchData by HYPE
  
  # which columns are of type factor or character (strings)
  facts <- sapply(x, function(z) {is.factor(z)})
  chars <- sapply(x, function(z) {is.character(z)})
  lf <- length(which(facts))
  lc <- length(which(chars))
  
  # select and convert factor columns to a character matrix, if factors exist
  if (lf > 0) {
    facmat <- apply(as.matrix(x[, facts]), 2, as.character)
    # test if the longest string in any column is longer than 50 characters, return with warning
    if (max(apply(facmat, 2, function (z) max(nchar(z)))) > maxChar) {
      warning("String with more than 50 characters in exported data detected. This will lead to an error in HYPE.")
    }
  }
  # select and convert character columns to a character matrix, if characters exist
  if (lc > 0) {
    chamat <- as.matrix(x[, chars])
    # test if the longest string in any column is longer than 50 characters, return with warning
    if (max(apply(chamat, 2, function (z) max(nchar(z)))) > maxChar) {
      warning("String with more than 50 characters in exported data detected. This will lead to an error in HYPE.")
    }  
  }
}







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.FindUpstrSbd~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# internal function to create upstream subid list, used in find.upstream.subids with 'lapply' on all subids in gd
# sbd: subid for which upstreams are searched
# dtf: df data frame above
.FindUpstrSbd <- function(sbd, dtf) {
  # look for subid in maindown and branchid columns, concatenate positions
  mup <- dtf[which(dtf[,2] == sbd), c(1, 4, 5)]
  bup <- dtf[which(dtf[,3] == sbd), c(1, 4, 5)]
  
  ## calculations of a main or branch upstream characteristics conditional on that upstreams exist
  if (nrow(mup) == 0 & nrow(bup) == 0) {
    # no upstreams exist, return an integer of length 0
    return(list(subid = sbd, upstr.df = integer()))
  }
  else {
    # if either or both main and branch upstreams exist, update to fraction of flow coming down, and the optional maximum flow
    if (nrow(mup) != 0) {
      mup <- data.frame(upstream = mup[,1], is.main = TRUE, fraction = ifelse(is.na(mup[,2]), 1, mup[,2]), limit = ifelse(is.na(mup[,3]), Inf, mup[,3]))
    }
    if (nrow(bup) != 0) {
      bup <- data.frame(upstream = bup[,1], is.main = FALSE, fraction = 1 - bup[,2], limit = ifelse(is.na(bup[,3]), Inf, bup[,3]))
    }
    # combine the two and return result
    res <- rbind(mup, bup)
    return(list(subid = sbd, upstr.df = res))
  }
}







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.CreateLabelsFromBreaks~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



# Internal function to make pretty label expressions from a given vector of breakpoints, which is used to convert 
# a continuous scale to a discrete one. Used for map plot function legends.
# breaks: vector of breakpoints
.CreateLabelsFromBreaks <- function(breaks) {
  # create first element as an expression which uses the value given in breaks
  lab.legend <- as.expression(bquote("" <= .(breaks[2])))
  # create following elements analoguously
  for (i in 2:length(breaks)-2) {
    lab.legend[i] <- as.expression(bquote("" >= .(breaks[i]) - .(breaks[i+1])))
  }
  # create the last element
  lab.legend[length(breaks)-1] <- as.expression(bquote("" >= .(breaks[length(breaks)-1])))
  
  return(lab.legend)
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.ReadBasinOutput~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# internal function to read a single basin output file, the exported import function wraps a multi-import from a directory
# path around it.
# filename: file location of basin file to import
# dt.format: format string as in strptime
# outformat: can be matrix or dataframe
.ReadBasinOutput <- function(filename, dt.format = "%Y-%m-%d", outformat = "df") {
  
  # check validity of outformat argument
  if (outformat != "df" & outformat != "m" & outformat != "dataframe" & outformat != "matrix") {
    stop("Argument 'outformat' invalid.")
  }
  
  x <- read.table(filename, header = F, na.strings = "-9999", skip = 2)      
  names(x) <- strsplit(readLines(filename, n = 1),split = "\t")[[1]]
  
  
  ## Date string handling, conditional on import format (HYPE allows for matlab or posix type, without or with hyphens),
  ## handles errors which might occur if the date string differs from the specified format, on error, strings are returned.
  
  # convert date column to character to avoid problems with factor levels in the date conversion
  x[, 1] <- as.character(x[, 1])
  
  # convert to posix string if possible, catch failed attempts with error condition and return string unchanged
  if (dt.format == "%Y-%m") {
    xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
    x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
  } else if (dt.format == "%Y%m") {
    xd <- as.POSIXct(strptime(paste(x[, 1], "-01", sep = ""), format = "%Y%m-%d"), tz = "GMT")
    x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
  } else if (dt.format == "%Y") {
    xd <- as.POSIXct(strptime(paste(x[, 1], "-01-01", sep = ""), format = "%Y-%m-%d"), tz = "GMT")
    x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
  } else {
    xd <- as.POSIXct(strptime(x[, 1], format = dt.format), tz = "GMT")
    x[, 1] <- tryCatch(na.fail(xd), error = function(e) {
      print("Date/time conversion attempt led to introduction of NAs, date/times returned as strings"); return(x[, 1])})
  }
  
  # update with new attribute to hold measurement units
  xattr <- readLines(filename, n = 2)
  attr(x, which = "unit") <- strsplit(xattr[2], split = "\t")
  
  # handling of argument 'outformat', 
  if(outformat == "matrix" | outformat == "m") {
    
    return(as.matrix(cbind(DATE = as.numeric(x[, 1]), x[, -1])))
  } else return(x)
}
