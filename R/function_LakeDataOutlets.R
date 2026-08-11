#' Identify LakeData Outlet SUBIDs
#'
#' Identifies the SUBID(s) corresponding to the outlet of each lake in a
#' LakeData data frame.
#'
#' A SUBID is identified as a lake outlet when all SUBIDs belonging to
#' the same lake occur in the upstream area of that SUBID. The function
#' uses [AllUpstreamSubids()] to determine the upstream SUBIDs.
#'
#' @param ld A data frame containing LakeData information (i.e. an imported 'LakeData.txt' file).
#' Must contain the columns `LAKEID` and `SUBID`.
#' @param gd A data frame containing GeoData information used to determine
#' the upstream connectivity between SUBIDs (i.e. an imported 'GeoData.txt' file).
#'
#' @return An integer vector containing the SUBIDs identified as lake
#'   outlets. One or more SUBIDs may be returned for a lake if the lake
#'   contains multiple downstream outlets.
#'
#' @seealso [AllUpstreamSubids()]
#'
#' @examples
#' ld <- ReadLakeData(
#'   filename = system.file("demo_model", "LakeData.txt", package = "HYPEtools")
#' )
#' gd <- ReadGeoData(
#'   filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools")
#' )
#' 
#' LakeDataOutlets(ld, gd)
#'
#' @export
#' 
LakeDataOutlets <- function(ld, gd) {
  
  # Argument checks
  stopifnot(
    is.data.frame(ld),
    is.data.frame(gd),
    all(c("LAKEID", "SUBID") %in% names(ld))
  )
  
  # Split LakeData by LAKEID
  lake_subids <- split(ld$SUBID, ld$LAKEID)
  
  # Vector to save if SUBID is a lake outlet
  is_outlet <- logical()
  
  # Loop through each lake
  for (subids in lake_subids) {
    
    # Check if SUBIDs are the outlet
    # (all SUBIDs in the lake are upstream of it)
    for (subid in subids) {
      is_outlet[as.character(subid)] <-
        all(subids %in% AllUpstreamSubids(subid, gd))
    }
  }
  
  # Return SUBIDs of lake outlets
  as.integer(names(is_outlet)[is_outlet])
}
