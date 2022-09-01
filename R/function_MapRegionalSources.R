#' Map regional irrigation source connection as spatial lines
#'
#' By default, this function creates an \code{sf} object which contains regional irrigation connections between
#' source and target HYPE sub-catchments. However, this function can also be used to create interactive Leaflet maps.
#'
#' @param data Dataframe, containing a column \code{SUBID} and a column \code{REGSRCID} (not case-sensitive), which identify
#' irrigation target and source sub-catchments, respectively. Typically a HYPE 'MgmtData.txt' file, imported with \code{\link{ReadMgmtData}}.
#' @param map A \code{sf}, \code{SpatialPointsDataFrame}, or \code{SpatialPolygonsDataFrame} object providing sub-catchment locations as points or polygons. Typically an imported SUBID
#' centre-point shape file or geopackage. If provided polygon data, then the polygon centroids will be calculated and used as the point locations (See \code{\link{st_centroid}}). Spatial data import requires additional packages, e.g. \code{sf}.
#' @param map.subid.column Integer, index of the column in the \code{map} column holding SUBIDs (sub-catchment IDs).
#' @param digits Integer, number of digits to which irrigation connection lengths are rounded to.
#' @param progbar Logical, display a progress bar while calculating.
#' @param map.type Map type keyword string. Choose either \code{"default"} for the default static plots or \code{"leaflet"} for interactive Leaflet maps.
#' @param plot.scale Logical, include a scale bar on Leaflet maps.
#' @param plot.searchbar Logical, if \code{TRUE}, then a search bar will be included on Leaflet maps. See [leaflet.extras::addSearchFeatures()].
#' @param weight Numeric, weight of subbasin boundary lines in Leaflet maps. Used if \code{map} contains polygon data. See [leaflet::addPolygons()].
#' @param opacity Numeric, opacity of subbasin boundary lines in Leaflet maps. Used if \code{map} contains polygon data. See [leaflet::addPolygons()].
#' @param fillColor String, color of subbasin polygons in Leaflet maps. Used if \code{map} contains polygon data. See [leaflet::addPolygons()].
#' @param fillOpacity Numeric, opacity of subbasin polygons in Leaflet maps. Used if \code{map} contains polygon data. See [leaflet::addPolygons()].
#' @param line.weight Numeric, weight of connection lines in Leaflet maps. See [leaflet::addPolylines()].
#' @param line.opacity Numeric, opacity of connection lines in Leaflet maps. See [leaflet::addPolylines()].
#' @param font.size Numeric, font size (px) for subbasin labels in Leaflet maps.
#' @param file Save a Leaflet map to an image file by specifying the path to the desired output file using this argument. File extension must be specified.
#' See [mapview::mapshot()].
#' You may need to run [webshot::install_phantomjs()] the first time you save a map to an image file.
#' @param vwidth Numeric, width of the exported Leaflet map image in pixels. See [webshot::webshot()].
#' @param vheight Numeric, height of the exported Leaflet map image in pixels. See [webshot::webshot()].
#' @param html.name Save a Leaflet map to an interactive HTML file by specifying the path to the desired output file using this argument. File extension must be specified.
#' See [htmlwidgets::saveWidget()].
#'
#' @details
#' \code{MapRegionalSources} can return static plots or interactive Leaflet maps depending on value provided for the argument \code{map.type}.
#' By default, \code{MapRegionalSources} creates an \code{sf} object from HYPE SUBID centerpoints using a table of SUBID pairs. Regional
#' irrigation sources in HYPE are transfers from outlet lakes or rivers in a source sub-catchment to the soil storage of irrigated SLC classes
#' (Soil, Land use, Crop) in a target sub-catchment. If \code{map.type} is set to "leaflet", then  \code{MapRegionalSources} returns an object of class \code{leaflet}.
#'
#' @return
#' For default static maps, \code{MapRegionalSources} returns an \code{sf} object containing columns \code{SUBID} (irrigation target
#' sub-catchment),  \code{REGSRCID} (irrigation source sub-catchment), and  \code{Length_[unit]} (distance between sub-catchments) where
#' 'unit' is the actual length unit of the distances. The projection of the returned object is always identical to the projection of
#' argument \code{map}. For interactive Leaflet maps, \code{PlotMapOutput} returns an object of class \code{leaflet}. If \code{map} contains 
#' polygon data, then the interactive map will include the polygons as a background layer.
#'
#' @examples
#' # Import subbasin centroids and subbasin polygons (to use as background)
#' require(sf)
#' te1 <- st_read(dsn = system.file("demo_model", "gis",
#' "Nytorp_centroids.gpkg", package = "HYPEtools"))
#' te2 <- st_read(dsn = system.file("demo_model", "gis",
#' "Nytorp_map.gpkg", package = "HYPEtools"))
#' # Create dummy MgmtData file with irrigation links
#' te3 <- data.frame(SUBID = c(3594, 63794), REGSRCID = c(40556, 3486))
#' \donttest{
#' # Plot regional irrigation links between subbasins with subbasin outlines as background
#' MapRegionalSources(data = te3, map = te1, map.subid.column = 25)
#' plot(st_geometry(te2), add = TRUE, border = 2)
#' }
#'
#' @importFrom dplyr all_of left_join mutate rename_with select sym %>%
#' @importFrom pbapply pblapply
#' @importFrom rlang .data
#' @export

MapRegionalSources <- function(data, map, map.subid.column = 1, digits = 3, progbar = FALSE, map.type = "default",
                               plot.scale = TRUE, plot.searchbar = FALSE, weight = 0.5, opacity = 1, fillColor = "#4d4d4d",
                               fillOpacity = 0.25, line.weight = 5, line.opacity = 1, font.size = 10, file = "",
                               vwidth = 1424, vheight = 1000, html.name = "") {

  # Check/Load Dependencies for mapping features - do this here so that these packages are not required for the base HYPEtools installation
  if (map.type == "default" & !all(
    requireNamespace("sf", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop("To use this function, please ensure that the following packages are installed: sf", call. = FALSE)
  } else if (map.type == "leaflet" & !all(
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("leaflet", quietly = TRUE),
    requireNamespace("leaflet.extras", quietly = TRUE),
    requireNamespace("mapview", quietly = TRUE),
    requireNamespace("htmlwidgets", quietly = TRUE),
    requireNamespace("randomcoloR", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop("To use this function, please ensure that the following packages are installed: sf, leaflet, leaflet.extras, mapview, htmlwidgets, randomcoloR", call. = FALSE)

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
      map_original <- map
      map <- map %>% sf::st_centroid()
    }

    # Rename data columns to all uppercase
    colnames(data) <- toupper(colnames(data))

    # Get column indices for target and regional source SUBIDs
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
    length_column <- paste("Length", sf::st_crs(map, parameters = TRUE)$units_gdal, sep = "_") # Get units of CRS
    names(rcb)[3] <- length_column

    # Create data frame to store point coordinates
    geometry <- map %>%
      select(all_of(map.subid.column)) %>%
      rename_with(.cols = 1, .fn = ~"SUBID") %>%
      mutate(SUBID_GEO = sf::st_geometry(.)) %>%
      mutate(REGSRC_GEO = sf::st_geometry(.)) %>%
      sf::st_drop_geometry()

    # Create dataframe of target-source connections
    condata <- left_join(rcb, geometry %>% select(.data$SUBID, .data$SUBID_GEO), by = "SUBID") %>% # Add coordinates for SUBID
      left_join(geometry %>% select(.data$SUBID, .data$REGSRC_GEO), by = c("REGSRCID" = "SUBID")) %>% # Add coordinates for REGSRCID
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

    # Set CRS of dataframe back to CRS of map
    sf::st_crs(condata) <- sf::st_crs(map)

    # Calculate connection lengths for all lines
    condata[, length_column] <- round(sf::st_length(condata$LINE), digits = digits)

    # Return outputs
    if (map.type == "default") {

      # Plot map and Return a subset of data frame columns invisibly
      plot(condata$LINE)
      invisible(condata %>% select(.data$SUBID, .data$REGSRCID, all_of(length_column)))
    } else if (map.type == "leaflet") {

      # Create Leaflet Plot
      message("Creating Map")

      # Create map
      leafmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
        leaflet::addTiles() %>%
        leaflet::addLayersControl(
          baseGroups = c("Map", "Street", "Topo", "Satellite"),
          overlayGroups = c("Connections", "Subbasins", "Labels"),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoIndex = TRUE)
        ) %>%
        leaflet.extras::addResetMapButton()

      # Get name of map SUBID column
      map.subid.name <- colnames(map)[map.subid.column]

      # Add Polygons if they exist
      if (exists("map_original")) {

        # Reproject if not a lat/long CRS
        if (sf::st_is_longlat(map_original) == FALSE) {
          map_original <- map_original %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
        }

        # Add subbasins to map
        leafmap <- leafmap %>%
          leaflet::addPolygons(
            group = "Subbasins",
            data = map_original,
            color = "black",
            weight = weight,
            opacity = opacity,
            fillColor = fillColor,
            fillOpacity = fillOpacity,
            label = map_original[[map.subid.name]], # Add label so searchbar will work
            labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE, style = list("color" = fillColor, "font-size" = "0px")) # Set label color and size to 0 to hide labels
          )
      }

      # Reproject if not a lat/long CRS
      if (sf::st_is_longlat(map) == FALSE) {
        map <- map %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
      }
      if (sf::st_is_longlat(condata) == FALSE) {
        condata <- condata %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
      }

      # Add labels to map
      label_data <- map %>% filter(!!sym(map.subid.name) %in% c(condata$SUBID, condata$REGSRCID)) # Only add labels to subbasins with connections
      leafmap <- leafmap %>%
        leaflet::addLabelOnlyMarkers(
          group = "Labels",
          data = suppressWarnings(sf::st_point_on_surface(label_data)),
          label = label_data[[map.subid.name]],
          labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto", textOnly = TRUE, style = list("font-size" = paste0(font.size, "px")))
        )

      # Create function to get colors for polylines
      color_pal <- function(X) {
        tryCatch(randomcoloR::distinctColorPalette(X), # Try to get a distinct color for each line
          error = function(e) {
            rep_len(randomcoloR::distinctColorPalette(100), X) # If there is an error, then repeat palette of 100 colors as necessary
          }
        )
      }

      # Get colors for polylines
      colors <- color_pal(nrow(condata))

      # Add Lines
      message("Adding Connection Lines")
      progress <- 1
      for (i in 1:nrow(condata)) {

        # Add Progress Message for datasets with >=1000 polylines
        if (nrow(condata) >= 1000 & i == ceiling(nrow(condata) / 10) * progress) {
          message(paste0("Adding Routing Lines: ", progress * 10, "%"))
          progress <- progress + 1
        }

        leafmap <- leafmap %>%
          leaflet::addPolylines(
            group = "Connections",
            lat = sf::st_coordinates(condata$LINE[i])[, 2],
            lng = sf::st_coordinates(condata$LINE[i])[, 1],
            label = paste("REGSRCID ", condata$REGSRCID[i], "to SUBID", condata$SUBID[i]),
            color = colors[i],
            weight = line.weight,
            opacity = line.opacity
          )
      }

      # Add searchbar to map
      if (plot.searchbar == TRUE) {
        leafmap <- leafmap %>%
          leaflet.extras::addSearchFeatures(
            targetGroups = c("Subbasins", "Connections"),
            options = leaflet.extras::searchFeaturesOptions(zoom = 10, hideMarkerOnCollapse = TRUE)
          )
      }

      # Add scalebar to map
      if (plot.scale == TRUE) {
        leafmap <- leafmap %>%
          leaflet::addScaleBar(position = "bottomright")
      }

      # Add various basemaps
      leafmap <- leafmap %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "Map") %>%
        leaflet::addTiles(group = "Street") %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite")

      # Save Image
      if (!file == "") {
        message("Saving Image")
        mapview::mapshot(leafmap, file = file, vwidth = vwidth, vheight = vheight, remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"), selfcontained = FALSE)
      }

      # Save HTML
      if (!html.name == "") {
        message("Saving HTML")
        temp <- file.path(tempdir(), basename(html.name))
        htmlwidgets::saveWidget(leafmap, file = temp, title = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(html.name)), selfcontained = TRUE) # Save HTML file to temp directory so selfcontained=T works
        file.rename(temp, html.name) # Rename/Move HTML file to desired file
      }

      return(leafmap)
    }
  }
}
