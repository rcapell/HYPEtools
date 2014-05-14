#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   Internal function herein:
#
#     - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DirectUpstreamSubids~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#' @export
#' @title
#' Find Direct Upstream SUBIDs, with Flow Fractions
#'
#' @description
#' Function to find \bold{direct} upstream SUBIDs including flow fractions for MAINDOWN/BRANCHDOWN splits for a single
#' sub-catchment or all sub-catchments in a GeoData-like data frame. Can be used for a
#' single sub-catchmentone or all 
#' sub-catchments in a GeoData file, connected through 'maindown' and 'branchid' 
#' SUBIDs. Includes extraction of the appropriate flow fractions for each connection.
#' 
#'
#' @param subid Integer, SUBID of a target sub-catchment (must exist in \code{gd}), defaults to 
#' \code{NULL}. If non-\code{NULL}, \code{DirectUpstreamSubids} 
#' returns the direct upstream SUBIDs for this sub-catchment, otherwise for all sub-catchments in \code{gd}.
#' 
#' @param gd Data frame, typically an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' 
#' @param bd Data frame, typically an imported 'BranchData.txt' file. Optional argument, defaults to 
#' an empty placeholder. See 'Details'.
#' 
#' @details
#' \code{DirectUpstreamSubids} identifies \bold{direct} upstream SUBIDs for a user-provided target SUBID or 
#' for all SUBIDs given in a data frame \code{gd}, typically an imported GeoData file. 
#' 
#' A sub-catchment in HYPE can have several upstream sub-catchments. If there are more than one upstream sub-catchments, 
#' the downstream sub-catchment is a confluence. HYPE stores these connections in the GeoData file, in downstream direction, 
#' given as downstream SUBID in column 'MAINDOWN'. Bifurcations, i.e. splits in downstream direction, are also possible to model in HYPE.
#' These additional downstream connections are provided in the BranchData file, together with flow fractions to each downstream sUBID.
#' 
#' Formally, \code{gd} can be any data frame which contains columns 'SUBID' and 'MAINDOWN' (not case-sensitive), and \code{bd} any 
#' data frame which contains four columns 'BRANCHID', 'SOURCEID', 'MAINPART', and 'MAXQMAIN'. Typically, these are HYPE data files 
#' imported through \code{\link{ReadGeoData}} and \code{\link{ReadBranchData}}. See HYPE documentation for further details on connections
#' Between SUBIDs in the model.
#' 
#' 
#' @return
#' \code{DirectUpstreamSubids} always returns a \link{list}. If argument \code{subid} is non-\code{NULL}, a list with two elements is returned: 
#'  \code{subid} contains an integer giving the target SUBID and \code{upstr.df} contains a data frame with columns 
#'  \code{upstream} (upstream SUBID), \code{is.main} (logical, \code{TRUE} if it is a MAINDOWN connection), 
#'  \code{fraction} (fraction of flow going into the target SUBID), and \code{limit} (maximum flow contribution to target SUBID).
#'  
#'  If no specific SUBID was provided, \code{DirectUpstreamSubids} returns a list with elements each containing the list described above, i.e.
#'  with an integer element (SUBID) and a data frame element (upstream connections).
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}, which returns all upstream SUBIDs, i.e. the full upstream network up to the headwaters, for a given SUBID.
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
  
  
  # if no target subid is given as argument, apply internal function to find upstream subids on all subids in gd
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

## DEBUG
# te <- DirectUpstreamSubids(subid = NULL, gd = gd)
