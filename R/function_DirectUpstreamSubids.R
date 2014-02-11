#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Internal function herein:
#
#     - .FindUpstrSbd()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DirectUpstreamSubids~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' @export
#' @title
#' Find Direct Upstream SUBIDs, with Flow Fractions
#'
#' @description
#' Function to find all SUBIDs of \bold{direct} upstream sub-catchments for one or all 
#' sub-catchments in a GeoData file, connected through 'maindown' and 'branchid' 
#' SUBIDs. Includes extraction of the appropriate flow fractions for each connection.
#' 
#'
#' @param subid SUBID of a target sub-catchment (must exist in \code{gd}), defaults to 
#' \code{NULL}. If non-\code{NULL}, \code{DirectUpstreamSubids} 
#' returns only the upstream SUBIDs for this sub-catchment, otherwise for all in \code{gd}.
#' 
#' @param gd A data frame, an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' 
#' @param bd A data frame, an imported 'BranchData.txt' file. Optional argument, defaults to 
#' an empty placeholder. See 'Details'.
#' 
#' @details
#' \code{DirectUpstreamSubids} matches \bold{direct} upstream SUBIDs to a target SUBID, or 
#' all SUBIDs in an imported GeoData file. Strictly speaking, it only requires a data frame 
#' with tow columns 'SUBID' and 'MAINDOWN' as \code{gd}, and optionally a data frame with four 
#' columns 'BRANCHID', 'SOURCEID', 'MAINPART', and 'MAXQMAIN' as input, butwill select these 
#' columns from imported files automatically.
#' 
#' 
#' @return
#' \code{DirectUpstreamSubids} always returns a \link{list}:
#' \itemize{
#'  \item If argument \code{subid} is non-\code{NULL}, returns a list with two elements: 
#'  \code{subid} contains the target SUBID, \code{upstr.df} contains a data frame with columns 
#'  \code{upstream} (upstream SUBID), \code{is.main} (logical, \code{TRUE} if it is a MAINDOWN connection), 
#'  \code{fraction} (fraction of flow going into the target SUBID), and \code{limit} (maximum flow 
#'  contribution to target SUBID).
#'  \item Otherwise returns a list with list elements as detailed above.
#' }
#' 
#' 
#' @examples
#' \dontrun{DirectUpstreamSubids(subid = 1, gd = mygeodata)}

DirectUpstreamSubids <- function(subid = NULL, 
                               gd, 
                               bd = data.frame("BRANCHID" = integer(), "SOURCEID" = integer(), 
                                               "MAINPART" = numeric(), "MAXQMAIN" = numeric())
                               ) {
  
  geocol.md <- which(colnames(gd)=="maindown" | colnames(gd) =="MAINDOWN")
  geocol.sub <- which(colnames(gd)=="subid" | colnames(gd) =="SUBID")
  brcol.br <- which(colnames(bd)=="branchid" | colnames(bd) =="BRANCHID")
  brcol.sr <- which(colnames(bd)=="sourceid" | colnames(bd) =="SOURCEID")   
  brcol.mp <- which(colnames(bd)=="mainpart" | colnames(bd) =="MAINPART")   
  brcol.mx <- which(colnames(bd)=="maxQmain" | colnames(bd) =="MAXQMAIN")
  
  
  # merge subid, maindown, branchid, mainpart, maxqmain to working data frame
  df <- merge(gd[, c(geocol.sub, geocol.md)], bd[, c(brcol.sr, brcol.br, brcol.mp, brcol.mx)], by.x = 1, by.y = 1, all = T, sort = F)
  
  
  # if no target subid is given as argument, apply the internal function on all subids in gd
  if (is.null(subid)){
    res <- lapply(df[, 1], .FindUpstrSbd, dtf = df)
    # name entries in the list with 'X' + subid
    names(res) <- paste("X", df[, 1], sep = "")
    return(res)
  }
  # otherwise just run the function for the one subid
  else {
    res <- .FindUpstrSbd(sbd = subid, dtf = df)
    return(res)
  }
}

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
