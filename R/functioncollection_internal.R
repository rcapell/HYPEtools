#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Collection of internal helper functions, which are not exported from the package's namespace. Herein:
#
#     - .CheckcharLengthDf()
#     - .FindUpstrSbd()
#     - 
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

