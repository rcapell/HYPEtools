#' Calculate River Distance Between Two SUBIDs
#'
#' Calculates the river distance between two SUBIDs along the routing network.
#' The distance is calculated by summing the `RIVLEN` values of the subbasins
#' between the two requested SUBIDs. The sign of the returned distance indicates
#' the direction of the routing: positive if `SUBID1` is upstream of `SUBID2`,
#' and negative if `SUBID1` is downstream of `SUBID2`.
#'
#' @param SUBID1 Numeric or character. The first SUBID.
#' @param SUBID2 Numeric or character. The second SUBID.
#' @param gd A data frame containing HYPE geographical data (i.e. an imported 'GeoData.txt' file). Must contain the
#'   columns `SUBID`, `MAINDOWN`, and `RIVLEN`.
#' @param bd Optional data frame containing HYPE branch data. Used by
#'   [AllDownstreamSubids()] to determine the routing network. Default is
#'   `NULL`.
#'
#' @details
#' The function first checks that both requested SUBIDs exist in `gd` and that
#' the `RIVLEN` column is available. It then uses [AllDownstreamSubids()] to
#' identify the downstream routing from the first SUBID.
#'
#' If `SUBID2` is downstream of `SUBID1`, the distance is returned as a positive
#' value. If `SUBID1` is downstream of `SUBID2`, the routing direction is
#' reversed and the distance is returned as a negative value.
#'
#' The distance is calculated by summing the `RIVLEN` values for the SUBIDs
#' between the two requested SUBIDs. The `RIVLEN` of the upstream SUBID itself
#' is not included in the calculation.
#'
#' If no routing connection exists between the two SUBIDs, the function issues
#' a warning and returns `NA`.
#'
#' @return
#' A numeric value giving the river distance between `SUBID1` and `SUBID2`.
#' The value is positive when `SUBID1` is upstream of `SUBID2` and negative when
#' `SUBID1` is downstream of `SUBID2`. Returns `NA` if no routing connection
#' exists between the two SUBIDs.
#'
#' @seealso
#' [AllDownstreamSubids()]
#' [AllUpstreamSubids()]
#'
#' @examples
#' # Read GeoData and BranchData files
#' gd <- ReadGeoData(
#'   filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools")
#' )
#' bd <- ReadGeoData(
#'   filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools")
#' )
#' 
#' # Calculate the river distance between two SUBIDs
#' CalculateRiverDistance(
#'   SUBID1 = 3344,
#'   SUBID2 = 3587,
#'   gd = gd
#' )
#'
#' @export

CalculateRiverDistance <- function(SUBID1, SUBID2, gd, bd = NULL){
  
  # Check that the requested SUBIDs exist and that two separate SUBIDs are requested
  if (!SUBID1 %in% gd$SUBID) {stop("SUBID1 not found in GeoData.")}
  if (!SUBID2 %in% gd$SUBID) {stop("SUBID2 not found in GeoData.")}
  
  # Check that required column exists in GeoData
  if (!"SUBID" %in% colnames(gd)) {stop("SUBID column not found in GeoData.")}
  if (!"MAINDOWN" %in% colnames(gd)) {stop("MAINDOWN column not found in GeoData.")}
  if (!"RIVLEN" %in% colnames(gd)) {stop("RIVLEN column not found in GeoData.")}
  
  # Find everything downstream of SUBID1
  downstream <- AllDownstreamSubids(
    subid = SUBID1,
    gd = gd,
    bd = bd
  )
  
  # Determine direction
  if (SUBID2 %in% downstream) {
    
    # SUBID1 is upstream of SUBID2
    upstreamSUBID <- SUBID1
    downstreamSUBID <- SUBID2
    sign <- 1
    
  } else {
    
    # Check the opposite direction
    upstreamSUBID <- SUBID2
    downstreamSUBID <- SUBID1
    sign <- -1
    
    downstream <- AllDownstreamSubids(
      subid = SUBID2,
      gd = gd,
      bd = bd
    )
    
    if (!downstreamSUBID %in% downstream) {
      warning("No routing connection found between SUBID1 and SUBID2.")
      return(NA)
    }
  }
  
  # Get the SUBIDs along the route
  route_ids <- downstream[
    match(upstreamSUBID, downstream):
      match(downstreamSUBID, downstream)
  ]
  
  # Remove upstreamSUBID from route because we want distance BETWEEN the SUBIDs
  route_ids <- route_ids[which(route_ids != upstreamSUBID)]
  
  # Calculate river distance
  distance <- sum(
    gd$RIVLEN[match(route_ids, gd$SUBID)],
    na.rm = TRUE
  )
  
  # Apply direction: positive if SUBID1 is upstream of SUBID2 and negative if SUBID1 is downstream of SUBID2
  return(sign * distance)
  
}
