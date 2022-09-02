#' Plot HYPE model subbasin routing.
#'
#' Plot routing of subbasins for a HYPE model on an interactive map.
#'
#' @param map Path to file containing subbasin polygon GIS data (e.g. shapefile or geopackage) or a \code{SpatialPolygonsDataFrame} 
#' or \code{sf} object. For large maps, a small/simplified polygon file should be used as larger files can take an excessive amount of time to render.
#' @param map.subid.column Integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs). 
#' Only required if providing GeoData information with \code{gd}.
#' @param gd Path to model GeoData.txt or a GeoData object from \code{\link{ReadGeoData}}. Only required if \code{map} does not contain SUBID and/or MAINDOWN fields.
#' @param bd Path to model BranchData.txt or a BranchData object from \code{\link{ReadBranchData}}. Only required if model has a BranchData.txt file.
#' @param plot.scale Logical, include a scale bar on the map.
#' @param plot.searchbar Logical, if \code{TRUE}, then a search bar will be included. See [leaflet.extras::addSearchFeatures()].
#' @param weight Numeric, weight of subbasin boundary lines. See [leaflet::addPolygons()].
#' @param opacity Numeric, opacity of subbasin boundary lines. See [leaflet::addPolygons()].
#' @param fillColor String, color of subbasin polygons. See [leaflet::addPolygons()].
#' @param fillOpacity Numeric, opacity of subbasin polygons. See [leaflet::addPolygons()].
#' @param line.weight Numeric, weight of routing lines. See [leaflet::addPolylines()].
#' @param line.opacity Numeric, opacity of routing lines. See [leaflet::addPolylines()].
#' @param font.size Numeric, font size (px) for map subbasin labels.
#' @param file Save map to an image file by specifying the path to the desired output file using this argument. File extension must be specified. 
#' See [mapview::mapshot()].
#' You may need to run [webshot::install_phantomjs()] the first time you save a map to an image file. 
#' @param vwidth Numeric, width of the exported map image in pixels. See [webshot::webshot()].
#' @param vheight Numeric, height of the exported map image in pixels. See [webshot::webshot()].
#' @param html.name Save map to an interactive HTML file by specifying the path to the desired output file using this argument. File extension must be specified. 
#' See [htmlwidgets::saveWidget()].
#'
#' @details
#' \code{PlotSubbasinRouting} generates an interactive Leaflet map with lines indicating the routing of flow between subbasins. GeoData information only needs 
#' to be provided if the \code{map} GIS data does not include SUBID and/or MAINDOWN fields. BranchData information only needs to be provided if model has a 
#' BranchData.txt file. Subbasin routing lines are randomly assigned a color using [randomcoloR::distinctColorPalette()].
#'
#' @return
#' Returns an interactive Leaflet map.
#'
#' @examples
#' \donttest{
#' # Import GeoData and subbasin polygons
#' require(sf);require(leaflet.extras);require(leaflet);require(mapview)
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te2 <- st_read(dsn = system.file("demo_model", "gis", "Nytorp_map.gpkg", package = "HYPEtools"))
#' PlotSubbasinRouting(map = system.file("demo_model",
#' "gis", "Nytorp_map.gpkg", package = "HYPEtools"), 
#'                     gd = system.file("demo_model", "GeoData.txt", package = "HYPEtools"),
#'                     map.subid.column = 25)
#' }
#'
#' @importFrom dplyr full_join %>% bind_rows filter across
#' @importFrom tidyselect matches
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @export

PlotSubbasinRouting <- function(map, map.subid.column = 1, gd = NULL, bd = NULL, plot.scale = TRUE, plot.searchbar = FALSE,
                                weight = 0.5, opacity = 1, fillColor = "#4d4d4d", fillOpacity = 0.25, line.weight = 5, line.opacity = 1,
                                font.size = 10, file = "", vwidth = 1424, vheight = 1000, html.name = "") {

  # Check/Load Dependencies - do this here so that these packages are not required for the base HYPEtools installation
  if (!all(
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("leaflet", quietly = TRUE),
    requireNamespace("leaflet.extras", quietly = TRUE),
    requireNamespace("mapview", quietly = TRUE),
    requireNamespace("htmlwidgets", quietly = TRUE),
    requireNamespace("randomcoloR", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop("To use this function, please ensure that the following packages are installed: sf, leaflet, leaflet.extras, mapview, htmlwidgets, randomcoloR", call.=FALSE)

    # Perform function
  } else {
    # Import GIS Data
    if ("character" %in% class(map)) {
      map <- sf::st_read(map)
    } else if ("SpatialPolygonsDataFrame" %in% class(map)) {
      map <- sf::st_as_sf(map)
    }

    # Reproject if not a lat/long CRS
    if (sf::st_is_longlat(map) == FALSE) {
      map <- map %>% sf::st_transform(sf::st_crs("+proj=longlat +datum=WGS84"))
    }

    # Rename columns to all uppercase except geometry column
    map <- map %>%
      rename_with(.fn = toupper, .cols = !matches(attr(map, "sf_column")))

    # Get name of map subid column
    map.subid.name <- colnames(map)[map.subid.column]

    # Check if GeoData is required
    if (is.null(gd) & !all(c("SUBID", "MAINDOWN") %in% colnames(map))) {
      stop("SUBID and/or MAINDOWN fields are missing in GIS data, so a GeoData.txt file/object must be supplied.")
    }

    # Read GeoData if supplied a path
    if (!is.null(gd) & typeof(gd) == "character") {
      gd <- ReadGeoData(gd)
    }

    # Read BranchData if supplied a path
    if (!is.null(bd) & typeof(bd) == "character") {
      bd <- suppressMessages(ReadBranchData(bd))
    }

    # Format BranchData
    if (!is.null(bd)) {
      bd <- bd %>%
        dplyr::filter(!is.na(.data$SOURCEID))
    }

    # Join GIS & GeoData if GeoData provided
    if (!is.null(gd)) {
      message(paste0('Joining "', colnames(map)[map.subid.column], '" from GIS Data (map) To "', "SUBID", '" from GeoData (gd)'))
      map <- full_join(map[, map.subid.column] %>% mutate(across(1, ~ as.character(.x))), gd %>% mutate(across("SUBID", ~ as.character(.x))), by = setNames(nm = colnames(map)[map.subid.column], "SUBID")) # Join GIS Data with GeoData in a manner in which column names don't have to be identical (e.g. "SUBID" and "subid" is okay, character and integer is okay)
    }

    # Create Subbasin Points and remove rows where downstream subbasins don't exist
    map_point <- suppressWarnings(sf::st_point_on_surface(map))

    # Get Downstream Subbasin Point
    message("Finding Downstream Subbasins")

    map_point$ds_geometry <- sf::st_sfc(unlist(lapply(1:nrow(map_point), function(X) {

      # Get Downstream SUBID
      ds <- map_point$MAINDOWN[X]

      # If Downstream SUBID Exists
      if (ds %in% unlist(map_point[, map.subid.name] %>% sf::st_drop_geometry())) {
        sf::st_geometry(map_point[which(unlist(map_point[, map.subid.name] %>% sf::st_drop_geometry()) == ds), attr(map_point, "sf_column")])
      } else {
        sf::st_sfc(sf::st_point(c(0, 0))) # Assign Point 0,0 and remove later
      }
    }), recursive = FALSE))

    # Get Downstream Subbasin Points for Branches
    if (!is.null(bd)) {
      message("Finding Branch Subbasins")
      for (i in 1:nrow(bd)) {

        # Get row of data for source subbasin and change MAINDOWN to Branch subbasin
        branch <- map_point[which(map_point[, map.subid.name] == bd$SOURCEID[i]), ] %>%
          mutate(MAINDOWN = bd$BRANCHID[i])

        # Get branch Geometry if branch SUBID Exists
        if (nrow(branch) > 0) {
          if (bd$BRANCHID[i] %in% map_point[, map.subid.name]) {
            branch$ds_geometry <- sf::st_sfc(sf::st_geometry(map_point[which(map_point[, map.subid.name] == bd$BRANCHID[i]), attr(map, "sf_column")]))
          } else {
            branch$ds_geometry <- sf::st_sfc(sf::st_point(c(0, 0))) # Assign Point 0,0 and remove later
          }

          # Add row to map_point
          map_point <- map_point %>%
            bind_rows(branch)
        }
      }
    }

    # Remove Subbasins where downstream subbasin doesn't exist
    map_point <- map_point %>%
      dplyr::filter(.data$MAINDOWN %in% unlist(map_point[, map.subid.name] %>% sf::st_drop_geometry()))

    # Create Leaflet Plot
    message("Creating Map")
    leafmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addTiles() %>%
      leaflet::addLayersControl(
        baseGroups = c("Map", "Street", "Topo", "Satellite"),
        overlayGroups = c("Routing", "Subbasins"),
        options = leaflet::layersControlOptions(collapsed = FALSE, autoIndex = TRUE)
      ) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet::addPolygons(
        group = "Subbasins",
        data = map,
        color = "black",
        weight = weight,
        opacity = opacity,
        fillColor = fillColor,
        fillOpacity = fillOpacity,
        label = paste(map[, map.subid.name]), # Add label so searchbar will work
        labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = TRUE, style = list("color" = fillColor, "font-size" = "0px")) # Set label color and size to 0 to hide labels
      ) %>%
      leaflet::addLabelOnlyMarkers(
        group = "Subbasins",
        data = suppressWarnings(sf::st_point_on_surface(map)),
        label = map[[map.subid.name]],
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
    colors <- color_pal(nrow(map_point))

    # Add Lines
    message("Adding Routing Lines")
    progress <- 1
    for (i in 1:nrow(map_point)) {

      # Add Progress Message for datasets with >=1000 polylines
      if (nrow(map_point) >= 1000 & i == ceiling(nrow(map_point) / 10) * progress) {
        message(paste0("Adding Routing Lines: ", progress * 10, "%"))
        progress <- progress + 1
      }

      leafmap <- leafmap %>%
        leaflet::addPolylines(
          group = "Routing",
          lat = c(sf::st_coordinates(map_point[attr(map, "sf_column")])[i, 2], sf::st_coordinates(map_point$ds_geometry)[i, 2]),
          lng = c(sf::st_coordinates(map_point[attr(map, "sf_column")])[i, 1], sf::st_coordinates(map_point$ds_geometry)[i, 1]),
          label = paste("SUBID", unlist(map_point[, map.subid.name] %>% sf::st_drop_geometry())[i], "to SUBID", map_point$MAINDOWN[i]),
          color = colors[i],
          weight = line.weight,
          opacity = line.opacity
        )
    }

    # Add searchbar to map
    if (plot.searchbar == TRUE) {
      leafmap <- leafmap %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = "Subbasins",
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

# DEBUG:
# library(dplyr)
# library(sf)
# library(leaflet)
# library(leaflet.extras)
# library(randomcoloR)
# map.subid.column <- 1
# gd <- NULL
# bd <- NULL
# plot.scale <- T
# plot.searchbar <- F
# weight <- 0.15
# opacity <- 0.75
# fillColor <- "#4d4d4d"
# fillOpacity <- 0.25
# line.weight <- 5
# line.opacity <- 1
# font.size <- 10
# file <- ""
# vwidth <- 1424
# vheight <- 1000
# html.name <- ""
# selfcontained <- FALSE
