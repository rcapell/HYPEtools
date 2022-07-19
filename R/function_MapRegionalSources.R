#' @export
#' @importFrom dplyr all_of left_join mutate rename_with select %>%
#' @importFrom pbapply pblapply
#' 
#' @title
#' Map regional irrigation source connection as spatial lines
#'
#' @description
#' This function creates an \code{sf} object which contains regional irrigation connections between 
#' source and target HYPE sub-catchments.
#'
#' @param data Dataframe, containing a column \code{SUBID} and a column \code{REGSRCID} (not case-sensitive), which identify 
#' irrigation target and source sub-catchments, respectively. Typically a HYPE 'MgmtData.txt' file, imported with \code{\link{ReadMgmtData}}.
#' @param map A \code{SpatialPointsDataFrame}, \code{SpatialPolygonsDataFrame}, or \code{sf} object providing sub-catchment locations as points or polygons. Typically an imported SUBID 
#' centre-point shape file or geopackage. If provided polygon data, then the polygon centroids will be calculated and used as the point locations (See \code{\link{st_centroid}}). Spatial data import requires additional packages, e.g. \code{sf}.
#' @param map.subid.column Integer, index of the column in the \code{map} column holding SUBIDs (sub-catchment IDs).
#' @param digits Integer, number of digits to which irrigation connection lengths are rounded to.
#' @param progbar Logical, display a progress bar while calculating.
#' @param map.type Map type keyword string. Choose \code{"default"} for the default static plots.
#' 
#' @details
#' \code{MapRegionalSources} creates an \code{sf} object from HYPE SUBID centerpoints using a table of SUBID pairs. Regional 
#' irrigation sources in HYPE are transfers from outlet lakes or rivers in a source sub-catchment to the soil storage of irrigated SLC classes 
#' (Soil, Land use, Crop) in a targed sub-catchment.
#' 
#' @return 
#' \code{MapRegionalSources} returns an \code{sf} object containing columns \code{SUBID} (irrigation target 
#' sub-catchment),  \code{REGSRCID} (irrigation source sub-catchment), and  \code{Length_[unit]} (distance between sub-catchments) where 
#' 'unit' is the actual length unit of the distances. The projection of the returned object is always identical to the projection of 
#' argument \code{map}.
#' 
#' @examples
#' \dontrun{MapRegionalSources(data = myMgmtData, map = mySUBIDCentrePoints)}

MapRegionalSources <- function(data, map, map.subid.column = 1, digits = 3, progbar = T, map.type = "default") {
  
  # Check/Load Dependencies for interactive mapping features - do this here so that these packages are not required for the base HYPEtools installation
  if (map.type == "default" & !all(
    requireNamespace("sf", quietly = T)
  )) {
    # Warn that a dependency is not installed
    stop("To use these mapping features, please ensure that the following packages are installed: sf", call.=F)
    
    # Perform function
  } else {
    
    # Input argument checks
    stopifnot(
      is.data.frame(data), # Check that data is a data frame
      ("sf" %in% class(map) | "SpatialPointsDataFrame" %in% class(map) | "SpatialPolygonsDataFrame" %in% class(map)) # Check that map is in a sp or sf spatial format
    )
    
    # Convert map to the sf spatial format if it is in the sp spatial format
    if ("SpatialPointsDataFrame" %in% class(map) | "SpatialPolygonsDataFrame" %in% class(map)) {
      map <- sf::st_as_sf(map)
    }
    
    # If not given a point type, then convert to points using centroid of object
    if (!all(sf::st_geometry_type(map) == "POINT")) {
      warning('The "map" input is not in a point format. Converting to point format using object centroids')
      map <- map %>% sf::st_centroid()
    }
    
    # Rename data columns to all uppercase
    colnames(data) <- toupper(colnames(data))
    
    # Get column indices fortarget and regional source SUBIDs
    col.subid <- which(names(data) == "SUBID")
    col.regsrcid <- which(names(data) == "REGSRCID")
    
    # Stop processing if no connected basins
    if (all(data[, col.regsrcid] == 0)) {
      stop('No connected basins; All REGSRCID in "data" are 0')
    }
    
    # Get row indices of regionally connected basins
    row.rcb <- which(data[, col.regsrcid] > 0)
    
    # Select data for mapping
    rcb <- data[row.rcb, c(col.subid, col.regsrcid)]
    
    # Update row names, necessary for connection to map data below
    rownames(rcb) <- 1:nrow(rcb)
    
    # Add a column to hold connection lengths,
    rcb <- data.frame(rcb, 0)
    length_column <- paste("Length", sf::st_crs(map, parameters = T)$units_gdal, sep = "_") # Get units of CRS
    names(rcb)[3] <- length_column
    
    # Create data frame to store point coordinates
    geometry <- map %>%
      select(all_of(map.subid.column)) %>%
      rename_with(.cols = 1, .fn = ~"SUBID") %>%
      mutate(SUBID_GEO = sf::st_geometry(.)) %>%
      mutate(REGSRC_GEO = sf::st_geometry(.)) %>%
      sf::st_drop_geometry()
    
    # Create dataframe of target-source connections
    condata <- left_join(rcb, geometry %>% select(SUBID, SUBID_GEO), by = "SUBID") %>% # Add coordinates for SUBID
      left_join(geometry %>% select(SUBID, REGSRC_GEO), by = c("REGSRCID" = "SUBID")) %>% # Add corrdinates for REGSRCID
      mutate(id = as.character(1:nrow(.)), .before = 1) # Add character ID
    
    # Apply function over all source-target connections to create line objects between connections
    if (progbar) {
      condata$LINE <- sf::st_sfc(pblapply(1:nrow(condata), function(X) {
        sf::st_linestring(matrix(data = c(sf::st_coordinates(condata$SUBID_GEO[X]), sf::st_coordinates(condata$REGSRC_GEO[X])), ncol = 2, byrow = 2))
      }))
    } else {
      condata$LINE <- sf::st_sfc(lapply(1:nrow(condata), function(X) {
        sf::st_linestring(matrix(data = c(sf::st_coordinates(condata$SUBID_GEO[X]), sf::st_coordinates(condata$REGSRC_GEO[X])), ncol = 2, byrow = 2))
      }))
    }
    
    # Set geometry of dataframe to be geometry in the LINE column
    sf::st_geometry(condata) <- "LINE"
    
    # Calculate connection lengths for all lines
    condata[, length_column] <- round(sf::st_length(condata$LINE), digits = digits)
    
    # Plot map and return a subset of data frame columns invisibly
    plot(condata$LINE)
    invisible(condata %>% select(SUBID, REGSRCID, all_of(length_column)))
  }
}
