
#' Check HYPE SUBID properties
#' 
#' Quickly query vectors of HYPE sub-basin IDs (SUBID) for various properties.
#' 
#' @param subid Numeric, vector of SUBIDs to be queried
#' @param gd [`HypeGeoData`] or base data frame with columns `SUBID` and `MAINDOWN`, typically an imported 
#' [GeoData.txt](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:geodata.txt) file.
#' @param dd Data frame, typically an imported 
#' [DamData.txt](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:damdata.txt) file. Defaults 
#' to `NULL`. `dd` or `ld` has to be provided in `IsRegulated`.
#' @param ld Data frame, typically an imported 
#' [LakeData.txt](http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:lakedata.txt) file. Defaults 
#' to `NULL`. `dd` or `ld` has to be provided in `IsRegulated`.
#' 
#' @details 
#' These are convenience functions to query subbasin properties. Some functions can be inefficient if applied to many or all 
#' subbasins of a HYPE model setup and more efficient functions may exist in HYPEtools, see links in *See also* section below or browse 
#' the package index.
#' 
#' @return 
#' The functions return a logical vector of the same length as `subid`, with `NA` values for all SUBIDs which do not exist 
#' in `gd`.
#' 
#' @seealso 
#' [AllUpstreamSubids()]; [AllDownstreamSubids()]; [OutletSubids()]; [OutletIds()]
#' 
#' @examples 
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' IsHeadwater(subid = 40556, gd = te)
#' IsHeadwater(subid = te$SUBID, gd = te)
#' 
#' @name HypeSubidChecks

NULL

#' @rdname HypeSubidChecks
#' @export

IsHeadwater <- function(subid, gd) {
  
  # input checks
  stopifnot(is.numeric(subid))
  stopifnot(c("SUBID", "MAINDOWN") %in% names(gd))
  
  # check if headwater (does not catch non-existing subids)
  res <- !(subid %in% gd$MAINDOWN)
  
  # set non-existing SUBIDs to NA
  res[!(subid %in% gd$SUBID)] <- NA
  
  res
}


#' @rdname HypeSubidChecks
#' @export

IsOutlet <- function(subid, gd) {
  
  # input checks
  stopifnot(is.numeric(subid))
  stopifnot(c("SUBID", "MAINDOWN") %in% names(gd))
  
  # set non-existing SUBIDs to NA
  subid[!(subid %in% gd$SUBID)] <- NA
  
  # identify existing SUBID-MAINDOWN pairs and sort in 'subid' order
  te <- gd[gd$SUBID %in% subid, c("SUBID", "MAINDOWN")]
  te <- te[match(subid, te$SUBID), ]
  
  # check if outlet, with NA propagation for non-existing SUBIDs
  res <- ifelse(is.na(te$SUBID), NA, !(te$MAINDOWN %in% gd$SUBID))
  
  res
}


#' @rdname HypeSubidChecks
#' @export

IsRegulated <- function(subid, gd, dd = NULL, ld = NULL) {
  
  # input checks, either DamData or LakeData must be provided
  stopifnot(!(is.null(dd) && is.null(ld)))
  
  warning("Not yet implemented!")
}


