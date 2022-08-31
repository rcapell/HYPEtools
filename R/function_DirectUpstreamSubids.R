#' @title
#' Find Direct Upstream SUBIDs, with Flow Fractions
#'
#' @description
#' Function to find \bold{direct} upstream SUBIDs including flow fractions for MAINDOWN/BRANCHDOWN splits for a single
#' sub-catchment or all sub-catchments in a GeoData-like data frame. 
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
#' These additional downstream connections are provided in the BranchData file, together with flow fractions to each downstream SUBID.
#' 
#' Formally, \code{gd} can be any data frame which contains columns 'SUBID' and 'MAINDOWN' (not case-sensitive), and \code{bd} any 
#' data frame which contains three columns: 'BRANCHID', 'SOURCEID', and 'MAINPART', and optionally columns 'MAXQMAIN', 'MINQMAIN', 'MAXQBRANCH'. 
#' Typically, these are HYPE data files 
#' imported through \code{\link{ReadGeoData}} and \code{\link{ReadBranchData}}. See HYPE documentation for further details on connections
#' Between SUBIDs in the model.
#' 
#' 
#' @return
#' \code{DirectUpstreamSubids} always returns a \link{list}. If argument \code{subid} is non-\code{NULL}, a list with two elements is returned: 
#'  \code{subid} contains an integer giving the target SUBID and \code{upstr.df} contains a data frame with columns 
#'  \code{upstream} (upstream SUBID), \code{is.main} (logical, \code{TRUE} if it is a MAINDOWN connection), 
#'  \code{fraction} (fraction of flow going into the target SUBID), and \code{llim} and \code{ulim} giving upper and lower flow boundaries which 
#'  optionally limit flow into the target SUBID.
#'  
#'  If no specific SUBID was provided, \code{DirectUpstreamSubids} returns a list with upstream information for all SUBIDs in argument 
#'  \code{gd}, each list element containing the list described above, i.e. with an integer element (SUBID) and a data frame element 
#'  (upstream connections).
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}, which returns all upstream SUBIDs, i.e. the full upstream network up to the headwaters, for a given SUBID.
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' DirectUpstreamSubids(subid = 3594, gd = te)
#' 
#' @export

DirectUpstreamSubids <- function(subid = NULL, gd, bd = NULL) {
  
  # create dummy branchdata if none was provided, needed for merging below
  if (is.null(bd)) {
    bd <- data.frame("BRANCHID" = integer(), "SOURCEID" = integer(), "MAINPART" = numeric(), "MAXQMAIN" = numeric(), 
                     "MINQMAIN" = numeric(), "MAXQBRANCH" = numeric())
  }
  
  # mandatory column positions
  geocol.md <- which(toupper(colnames(gd)) =="MAINDOWN")
  geocol.sub <- which(toupper(colnames(gd)) =="SUBID")
  brcol.br <- which(toupper(colnames(bd)) =="BRANCHID")
  brcol.sr <- which(toupper(colnames(bd)) =="SOURCEID")  
  # optional branchdata column positions
  brcol.mp <- which(toupper(colnames(bd)) =="MAINPART")   
  brcol.mx <- which(toupper(colnames(bd)) =="MAXQMAIN")
  brcol.mn <- which(toupper(colnames(bd)) =="MINQMAIN")
  brcol.mb <- which(toupper(colnames(bd)) =="MAXQBRANCH")
  # add optional columns if they do not exist, to create a well defined argument for internal function below
  if (length(brcol.mp) == 0) {
    brcol.mp <- ncol(bd) + 1
    bd <- data.frame(bd, MAINPART = NA)
  }
  if (length(brcol.mx) == 0) {
    brcol.mx <- ncol(bd) + 1
    bd <- data.frame(bd, MAXQMAIN = NA)
  }
  if (length(brcol.mn) == 0) {
    brcol.mn <- ncol(bd) + 1
    bd <- data.frame(bd, MINQMAIN = NA)
  }
  if (length(brcol.mb) == 0) {
    brcol.mb <- ncol(bd) + 1
    bd <- data.frame(bd, MAXQBRANCH = NA)
  }
  
  # merge subid, maindown, branchid, mainpart, maxqmain to working data frame
  df <- merge(gd[, c(geocol.sub, geocol.md)], bd[, c(brcol.sr, brcol.br, brcol.mp, brcol.mx, brcol.mn, brcol.mb)], by.x = 1, by.y = 1, all = TRUE, sort = FALSE)
  # sort as in gd, necessary to do separately because non-sorting in merge will still put all matches first (bifurcations) and then append all non-matches (no bif.)
  df <- df[match(x = gd[, geocol.sub], table = df[, 1]), ]
  
  # if no target subid is given as argument, apply internal function to find upstream subids of all subids in gd
  if (is.null(subid)){
    res <- lapply(df[, 1], .FindUpstrSbd, dtf = df)
    # name entries in the list with 'X' + subid
    names(res) <- paste("X", df[, 1], sep = "")
  } else {
    # otherwise just run the function for the one subid
    res <- .FindUpstrSbd(sbd = subid, dtf = df)
  }
  
  return(res)
}

## DEBUG
# te <- DirectUpstreamSubids(subid = NULL, gd = gd)
