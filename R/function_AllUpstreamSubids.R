


#' @export

#' @title
#' Find All Upstream SUBIDs
#'
#' @description
#' Function to find all SUBIDs of upstream sub-catchments for a single 
#' sub-catchment in a GeoData file, connected through 'maindown' and 'branchid' 
#' SUBIDs, i.e the full upstream catchment.
#'
#' @param subid SUBID of a target sub-catchment (must exist in \code{gd}). 
#' @param gd A data frame, an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' @param bd A data frame, an imported 'BranchData.txt' file. Optional argument.
#' @param write.arcgis Logical. If \code{TRUE}, a string containing an SQL expression suitable for ArcGIS's 
#' 'Select By Attributes' feature will be written to the clipboard. Works just for Windows.
#' 
#' @details
#' \code{AllUpstreamSubids} finds the upstream SUBIDs of a given SUBID (including itself but not 
#' including potential irrigation links or groundwater flows).
#' 
#' 
#' @return
#' \code{AllUpstreamSubids} returns a vector of SUBIDs.
#' 
#' 
#' @examples
#' \dontrun{AllUpstreamSubids(subid = 21, gd = mygeodata)}

# ------------------------------
# AllUpstreamSubids(subid,geodata,branchdata)
# 
# - Function to find the upstream SUBIDs of a given HYPE SUBID (including itself but not including potential irrigation links or groundwater flows)
#   -'subid' = the ID of the subbasin for which the value is desired
#   -'geodata' = dataframe with the GeoData.txt information
#   -'branchdata' = dataframe with the BranchData.txt information (if applicable)
#   - output is a list of upstream subbasin ids to the given subid
#   - Written for HYPE 4.3.1 (with separate BranchData file)

AllUpstreamSubids <- function(subid, gd, bd = NULL, write.arcgis = FALSE) {
  
  # identify relevant column positions in geodata and branchdata
  geocol.md <- which(colnames(gd) == "maindown" | colnames(gd) == "MAINDOWN")
  geocol.sub <- which(colnames(gd) == "subid" | colnames(gd) == "SUBID")
  if (!is.null(bd)) {
    brcol.br <- which(colnames(bd) == "branchid" | colnames(bd) == "BRANCHID")
    brcol.sr <- which(colnames(bd) == "sourceid" | colnames(bd) == "SOURCEID")   
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
  
  # try to write to clipboard, with error recovery
  if (write.arcgis == T){
    tryCatch(writeClipboard(paste(paste("\"SUBID\" =", us, 'OR'), collapse=" ")), error = function(e) {
      print("Writing to clipboard failed, this is probably not a Windows environment")})
  }
  
  return(us)
}

