
#' @export

#' @title
#' Find All Downstream SUBIDs
#'
#' @description
#' Function to find all SUBIDs of downstream sub-catchments along the main stem for a single 
#' sub-catchment.
#'
#' @param subid Integer, SUBID of a target sub-catchment (must exist in \code{gd}). 
#' @param gd Dataframe, an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' @param bd Dataframe, an imported 'BranchData.txt' file. Optional argument. See 'Details'.
#' @param write.arcgis Logical. If \code{TRUE}, a string containing an SQL expression suitable for ArcGIS's 
#' 'Select By Attributes' feature will be written to the clipboard.
#' 
#' @details
#' \code{AllDownstreamSubids} finds all downstream SUBIDs of a given SUBID along the main stem (including itself but not 
#' including potential irrigation links or groundwater flows) using GeoData columns 'SUBID' and 'MAINDOWN'. If a BranchData file 
#' is provided, the function will also include information on downstream bifurcations.
#' 
#' @return
#' \code{AllDownstreamSubids} returns a vector of downstream SUBIDs to the outlet if no BranchData is provided, otherwise a data frame with 
#' two columns \code{downstream} with downstream SUBIDs and \code{is.branch} with logical values indicating if a downstream SUBID contains a 
#' bifurcation ('branch' in HYPE terms). Downstream SUBIDs are ordered from source to final outlet SUBID.
#' 
#' @seealso 
#' \code{\link{AllUpstreamSubids}}, \code{\link{OutletSubids}}, \code{\link{OutletIds}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' AllDownstreamSubids(subid = 3344, gd = te)
#' 
#' @importFrom clipr write_clip clipr_available

AllDownstreamSubids <- function(subid, gd, bd = NULL, write.arcgis = FALSE) {
  
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
  
  ## find all downstreams on main stem
  
  # use function OutletIds() to find outlet ids (can be several)
  outlet.id <- OutletIds(gd = gd)
  # initialize loop variables
  sbd.cur <- subid
  md.cur <- gd[which(gd[, geocol.sub] == sbd.cur), geocol.md]
  ds <- sbd.cur
  
  # loop through downstream subid chain until outlet is found
  while (any(md.cur != outlet.id)) {
    # add current downstream to vector
    ds <- c(ds, md.cur)
    # update current downstream with downstream's downstream
    md.cur <- gd[which(gd[, geocol.sub] == md.cur), geocol.md]
  }
  # force integer
  ds <- as.integer(ds)
  
  # try to write arcgis select string to clipboard, with error recovery
  if (write.arcgis == TRUE) {
    to.arc <- paste(paste("\"SUBID\" =", ds, 'OR'), collapse=" ")
    to.arc <- substr(to.arc, 1, nchar(to.arc) - 3)
    if (clipr_available() == TRUE) {
      write_clip(to.arc)
    } else {
      message("Writing to clipboard failed. Try installing command line tool 'xclip' if you run Linux.")
    }
  }
  
  # conditional: add logical vector with TRUE for branched catchments if branchdata exists
  if (!is.null(bd)) {
    ds <- data.frame(downstream = ds, is.branch = ds %in% bd[, brcol.sr])
  }
  
  # return result
  return(ds)
}
