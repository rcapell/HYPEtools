#' @export

#' @title
#' Find All Upstream SUBIDs
#'
#' @description
#' Function to find all SUBIDs of upstream sub-catchments for a single 
#' sub-catchment. 
#'
#' @param subid SUBID of a target sub-catchment (must exist in \code{gd}). 
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param write.arcgis Logical. If \code{TRUE}, a string containing an SQL expression suitable for ArcGIS's 
#' 'Select By Attributes' feature will be written to the clipboard. Works just for Windows.
#' 
#' @details
#' \code{AllUpstreamSubids} finds all upstream SUBIDs of a given SUBID (including itself but not 
#' including potential irrigation links or groundwater flows) using GeoData columns 'SUBID' and 'MAINDOWN', i.e the full upstream catchment. 
#' If a BranchData file is provided, the function will also include upstream areas which are connected through an upstream bifurcation. The 
#' results can be directly used as 'partial model setup file' ('pmsf.txt') using the export function \code{\link{WritePmsf}}.
#' 
#' 
#' @return
#' \code{AllUpstreamSubids} returns a vector of SUBIDs, ordered as downstream sequence.
#' 
#' 
#' @examples
#' \dontrun{AllUpstreamSubids(subid = 21, gd = mygeodata)}


AllUpstreamSubids <- function(subid, gd, bd = NULL, write.arcgis = FALSE) {
  
  # identify relevant column positions in geodata and branchdata
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sub <- which(tolower(colnames(gd)) == "subid")
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sub) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  if (!is.null(bd)) {
    brcol.br <- which(tolower(colnames(bd)) == "branchid")
    brcol.sr <- which(tolower(colnames(bd)) == "sourceid")
    # check existence
    if (length(brcol.br) != 1 || length(brcol.sr) != 1) {
      stop("BRANCHID and/or SOURCEID column not found in 'bd'.")
    }
  }
  
  # check if subid exists in gd
  if (length(which(gd[, geocol.sub] == subid)) != 1) {
    stop("'subid' not found in 'gd'.")
  }
  
  # internal helper function, used with sapply() in while loop below, finds direct upstream subids in geodata and branchdata
  findfun <- function(this.sub) {
    # case with branchdata
    if (!is.null(bd)) ff <- c(gd[which(gd[, geocol.md] == this.sub), geocol.sub], bd[which(bd[, brcol.br] == this.sub), brcol.sr])
    # case without branchdata
    if (is.null(bd)) ff <- c(gd[which(gd[, geocol.md] == this.sub), geocol.sub])
    return(ff)
  }
  
  # create start conditions for while loop: find direct upstream subids of the target subid
  us <- findfun(subid)
  this.us <- us
  us.exists <- length(us) > 0
  
  # loop through upstreams of the upstreams repeatedly, until none are found anymore
  while(us.exists) {
    this.us <- unlist(sapply(this.us, findfun))
    if (length(this.us) > 0) {
      us.exists <- TRUE
    } else {
      us.exists<-FALSE
    }
    us <- unique(c(us, this.us))
  }
  
  # add outlet SUBID to result vector
  us <- c(subid, us)
  
  # order in downstream sequence, for direct use as pmsf file
  us <- gd[, geocol.sub][sort(match(us, gd[, geocol.sub]))]
  
  # try to write arcgis select string to clipboard, with error recovery
  if (write.arcgis == T) {
    to.arc <- paste(paste("\"SUBID\" =", us, 'OR'), collapse=" ")
    to.arc <- substr(to.arc, 1, nchar(to.arc) - 3)
    tryCatch(writeClipboard(to.arc), error = function(e) {
      print("Writing to clipboard failed, this is probably not a Windows environment")})
  }
  
  return(us)
}

