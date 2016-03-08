#' @export
#' @import sp
#' @importFrom pbapply pblapply
#' 
#' @title
#' Map regional irrigation source connection as spatial lines
#'
#' @description
#' This function creates a \code{SpatialLinesDataFrame} object which contains regional irrigation connections between 
#' source and target HYPE sub-catchments.
#'
#' @param data Dataframe, containing a column \code{SUBID} and a column \code{REGSRCID} (not case-sensitive), which identify 
#' irrigation target and source sub-catchments, respectively. Typically a HYPE 'MgmtData.txt' file, imported with \code{\link{ReadMgmtData}}.
#' @param map A \code{SpatialPointsDataFrame} object providing sub-catchment locations as points. Typically an imported SUBID 
#' centre-point shape file, shape file import requires additional packages, e.g. \code{rgdal}.
#' @param map.subid.column Integer, index of the column in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs).
#' @param digits Integer, number of digits to which irrigation connection lengths are rounded to.
#' @param progbar Logical, display a progress bar while calculating.
#' 
#' @details
#' \code{MapRegionalSources} creates a \code{SpatialLinesDataFrame} object from HYPE SUBID centerpoints using a table of SUBID pairs. Regional 
#' irrigation sources in HYPE are transfers from outlet lakes or rivers in a source sub-catchment to the soil storage of irrigated SLC classes 
#' (Soil, Land use, Crop) in a targed sub-catchment.
#' 
#' @return 
#' \code{MapRegionalSources} returns a \code{SpatialLinesDataFrame} with a data slot containing columns \code{SUBID} (irrigation target 
#' sub-catchment),  \code{REGSRCID} (irrigation source sub-catchment), and  \code{Length_[unit]} (distance between sub-catchments) where 
#' '[unit]' is the actual length unit of the distances. The projection of the returned object is always identical to the projection of 
#' argument \code{map}.
#' 
#' @examples
#' \dontrun{MapRegionalSources(data = myMgmtData, map = mySUBIDCentrePoints)}

MapRegionalSources <- function (data, map, map.subid.column = 1, digits = 3, progbar = T) {
  
  # input argument checks
  stopifnot(is.data.frame(data), class(map)=="SpatialPointsDataFrame")
  
  # check if map is projected and assign lonlat value and unit string accordingly, both used below
  if (is.projected(map)) {
    longlat <- FALSE
    # extract map units from projstring
    munit <- gsub(" ", "", strsplit(proj4string(map), "+", fixed = T)[[1]])
    munit <- munit[grep("units", munit)]
    munit <- gsub("units=", "", munit)
  } else {
    longlat <- TRUE
    munit <- "km"
  }
  
  # column with target and regional source SUBIDs
  col.subid <- which(toupper(names(data)) == "SUBID")
  col.regsrcid <- which(toupper(names(data)) == "REGSRCID")
  
  # get row indices of regionally connected basins
  row.rcb <- which(data[, col.regsrcid] > 0)
  
  # select data for mapping
  rcb <- data[row.rcb, c(col.subid, col.regsrcid)]
  
  # update row names, necessary for connection to map data below
  rownames(rcb) <- 1:nrow(rcb)
  
  # add a column to hold connection lengths, 
  rcb <- data.frame(rcb, 0)
  names(rcb)[3] <- paste("Length", munit, sep = "_")
  
  
  
  ## create a SpatialLines object from centerpoint coordinate pairs of target and source SUBIDs
  
  # create dataframe of target-source connection to iterate through: 
  # row indices 'map' data slot of irrigation target and source SUBIDs rcb dataframe, and character IDs for use below
  condata <- data.frame(row.sbd = match(rcb[, col.subid], map@data[, map.subid.column]), 
                        row.rgsrc = match(rcb[, col.regsrcid], map@data[, map.subid.column]), id = as.character(1:nrow(rcb))
                        )
  
  
  # MAIN COMPUTATION: lapply an anonymous function over all source-target connections to create a list of Lines objects
  # Function arguments
  # x: numeric, row index over which to lapply
  # cnd: dataframe, condata above, with rows of source and target SUBIDs in map, and an ID
  # mp: a SpatialPointsDataframe map
  lineslist <- lapply(1:nrow(condata), 
                      FUN = function (x, cnd, mp) {Lines(list(Line(rbind(coordinates(mp)[cnd[x, 1], ], coordinates(map)[cnd[x, 2], ]))), ID = cnd[x, 3])}, 
                      cnd = condata, mp = map)
  # apply row-wise to slc data frame, THIS LINE IS A MAIN COMPUTATION
  if (progbar) {
    lineslist <- pblapply(1:nrow(condata), 
                        FUN = function (x, cnd, mp) {Lines(list(Line(rbind(coordinates(mp)[cnd[x, 1], ], coordinates(map)[cnd[x, 2], ]))), ID = cnd[x, 3])}, 
                        cnd = condata, mp = map)
  } else {
    lineslist <- lapply(1:nrow(condata), 
                        FUN = function (x, cnd, mp) {Lines(list(Line(rbind(coordinates(mp)[cnd[x, 1], ], coordinates(map)[cnd[x, 2], ]))), ID = cnd[x, 3])}, 
                        cnd = condata, mp = map)
  }
  
  # calculate vector of connection lengths for all Lines objects and add to rcb dataframe
  rcb[, 3] <- sapply(1:nrow(condata), function(x, y, ll) {LinesLength(Ls = y[[x]], longlat = ll)}, y = lineslist, ll = longlat)
  round(rcb[, 3], digits = digits)
  
  # create Spatial result object
  res <- SpatialLinesDataFrame(SpatialLines(lineslist, proj4string = map@proj4string), rcb)
  
  # return map
  return(res)
}
