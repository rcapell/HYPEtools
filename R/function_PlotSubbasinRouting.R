#' Plot HYPE model subbasin routing.
#'
#' Plot routing of subbasins for a HYPE model on an interactive map.
#'
#' @param map String, path to file containing subbasin polygon GIS data (e.g. shapefile or geopackage). For large maps, a small/simplified polygon file should be used as larger files can take an excessive amount of time to render.
#' @param map.subid.column Integer, column index in the \code{map} 'data' \code{\link{slot}} holding SUBIDs (sub-catchment IDs). Only required if providing GeoData information with \code{gd}.
#' @param gd Path to model GeoData.txt or a GeoData object from \code{\link{ReadGeoData}}. Only required if \code{map} does not contain SUBID and/or MAINDOWN fields.
#' @param bd Path to model BranchData.txt or a BranchData object from \code{\link{ReadBranchData}}. Only required if model has a BranchData.txt file.
#' @param plot.scale Logical, include a scale bar on the map.
#' @param plot.searchbar Logical, if \code{TRUE}, then a search bar will be included. See \code{\link{addSearchFeatures}}.
#' @param weight Numeric, weight of subbasin boundary lines. See \code{\link{addPolygons}}.
#' @param opacity Numeric, opacity of subbasin boundary lines. See \code{\link{addPolygons}}.
#' @param fillColor String, color of subbasin polygons. See \code{\link{addPolygons}}.
#' @param fillOpacity Numeric, opacity of subbasin polygons. See \code{\link{addPolygons}}.
#' @param line.weight Numeric, weight of routing lines. See \code{\link{addPolylines}}.
#' @param line.opacity Numeric, opacity of routing lines. See \code{\link{addPolylines}}.
#' @param font.size Numeric, font size (px) for map subbasin labels.
#' @param file Save map to an image file by specifying the path to the desired output file using this argument. File extension must be specified. See \code{\link{mapshot}}.
#' You may need to run \code{webshot::install_phantomjs()} the first time you save a map to an image file. See \code{\link{install_phantomjs}}.
#' @param vwidth Numeric, width of the exported map image in pixels. See \code{\link{webshot}}.
#' @param vheight Numeric, height of the exported map image in pixels. See \code{\link{webshot}}.
#' @param html.name Save map to an interactive HTML file by specifying the path to the desired output file using this argument. File extension must be specified. See \code{\link{saveWidget}}. If using \code{selfcontained = TRUE}, then the output file path must be within in the working directory and on a local device (i.e. not a network location).
#' @param selfcontained Logical, whether to save the HTML as a single self-contained file (with external resources base64 encoded) or a file with external resources placed in an adjacent directory. See \code{\link{saveWidget}}.
#' Users should set argument to \code{FALSE} for large Leaflet maps with lots of subbasins, when using a subbasin vector polygon files with unsimplified geometry, and/or when working on a network directory.
#'
#' @details
#' \code{PlotSubbasinRouting} generates an interactive Leaflet map with lines indicating the routing of flow between subbasins. GeoData information only needs to be provided if the \code{map} GIS
#' data does not include SUBID and/or MAINDOWN fields. BranchData information only needs to be provided if model has a BranchData.txt file. Subbasin routing lines are randomly assigned a color using \code{\link{distinctColorPalette}}.
#'
#' @return
#' Returns an interactive Leaflet map.
#'
#' @examples
#' \dontrun{
#' PlotSubbasinRouting(map = "subbasins.shp", gd = "GeoData.txt")
#' }
#'
#' @export
#' @importFrom dplyr full_join %>% bind_rows
#' @importFrom sf st_read st_sfc st_geometry st_coordinates st_point
#' @importFrom leaflet.extras addResetMapButton addSearchFeatures searchFeaturesOptions
#' @importFrom leaflet addLayersControl layersControlOptions addTiles leaflet leafletOptions addPolygons addPolylines addScaleBar addProviderTiles
#' @importFrom mapview mapshot
#' @importFrom htmlwidgets saveWidget
#' @importFrom randomcoloR distinctColorPalette

PlotSubbasinRouting <- function(map, map.subid.column = 1, gd = NULL, bd = NULL, plot.scale = TRUE, plot.searchbar = FALSE,
                                weight = 0.15, opacity = 0.75, fillColor = "#4d4d4d", fillOpacity = 0.25, line.weight = 5, line.opacity = 1,
                                font.size = 10, file = "", vwidth = 1424, vheight = 1000, html.name = "", selfcontained = FALSE) {

  # Import GIS Data and rename columns to all uppercase
  map <- st_read(map) %>%
    rename_with(.fn = toupper, .cols = !matches("geometry"))

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
  if(!is.null(bd)){
    bd <- bd%>%
      filter(!is.na(SOURCEID))
  }

  # Join GIS & GeoData if GeoData provided
  if (!is.null(gd)) {
    message(paste0('Joining "', colnames(map)[map.subid.column], '" from GIS Data (map) To "', "SUBID", '" from GeoData (gd)'))
    map <- full_join(map[, map.subid.column] %>% mutate(across(1, ~ as.character(.x))), gd %>% mutate(across("SUBID", ~ as.character(.x))), by = setNames(nm = colnames(map)[map.subid.column], "SUBID")) # Join GIS Data with GeoData in a manner in which column names don't have to be identical (e.g. "SUBID" and "subid" is okay, character and integer is okay)
  }

  # Create Subbasin Points
  map_point <- suppressWarnings(st_point_on_surface(map))

  # Get Downstream Subbasin Point
  map_point$ds_geometry <- st_sfc(unlist(lapply(1:nrow(map_point), function(X) {
    
    # Get Downstream SUBID
    ds <- map_point$MAINDOWN[X]

    # If Downstream SUBID Exists
    if (ds %in% map_point$SUBID) {
      st_geometry(map_point[which(map_point$SUBID == ds), "geometry"])
    } else {
      NA
    }
  }), recursive = F))
  
  # Get Downstream Subbasin Points for Branches
  if(!is.null(bd)){
    for(i in 1:nrow(bd)){
      
      # Get row of data for source subbasin and change MAINDOWN to Branch subbasin
      branch <- map_point[which(map_point$SUBID==bd$SOURCEID[i]),]%>%
        mutate(MAINDOWN = bd$BRANCHID[i])
      
      # Get branch Geometry if branch SUBID Exists
      if (bd$BRANCHID[i] %in% map_point$SUBID) {
        branch$ds_geometry <- st_sfc(st_geometry(map_point[which(map_point$SUBID == bd$BRANCHID[i]), "geometry"]))
      } else {
        branch$ds_geometry <- st_sfc(st_point(as.numeric(c(NA,NA))))
      }
      
      # Add row to map_point
      map_point <- map_point%>%
        bind_rows(branch)
    }
  }

  # Create Leaflet Plot
  message("Creating Map")
  leafmap <- leaflet(options = leafletOptions(preferCanvas = T)) %>%
    addTiles() %>%
    addLayersControl(
      baseGroups = c("Map", "Street", "Topo", "Satellite"),
      overlayGroups = c("Routing", "Subbasins"),
      options = layersControlOptions(collapsed = F, autoIndex = T)
    ) %>%
    addResetMapButton() %>%
    addPolygons(
      group = "Subbasins",
      data = map,
      color = "black",
      weight = weight,
      opacity = opacity,
      fillColor = fillColor,
      fillOpacity = fillOpacity,
      label = paste(map$SUBID), # Add label so searchbar will work
      labelOptions = labelOptions(noHide = T, textOnly = T, style = list("color" = fillColor, "font-size" = "0px")) # Set label color and size to 0 to hide labels
    ) %>%
    addLabelOnlyMarkers(
      group = "Subbasins",
      data = suppressWarnings(st_point_on_surface(map)),
      label = map[["SUBID"]],
      labelOptions = labelOptions(noHide = T, direction = "auto", textOnly = T, style = list("font-size" = paste0(font.size, "px")))
    )

  # Add Lines
  message("Adding Routing Lines")
  for (i in 1:nrow(map_point)) {
    leafmap <- leafmap %>%
      addPolylines(
        group = "Routing",
        lat = c(st_coordinates(map_point$geometry)[i, 2], st_coordinates(map_point$ds_geometry)[i, 2]),
        lng = c(st_coordinates(map_point$geometry)[i, 1], st_coordinates(map_point$ds_geometry)[i, 1]),
        label = paste("SUBID", map_point$SUBID[i], "to SUBID", map_point$MAINDOWN[i]),
        color = distinctColorPalette(nrow(map_point))[i],
        weight = line.weight,
        opacity = line.opacity
      )
  }

  # Add searchbar to map
  if (plot.searchbar == T) {
    leafmap <- leafmap %>%
      addSearchFeatures(
        targetGroups = "Subbasins",
        options = searchFeaturesOptions(zoom = 10, hideMarkerOnCollapse = T)
      )
  }

  # Add scalebar to map
  if (plot.scale == T) {
    leafmap <- leafmap %>%
      addScaleBar(position = "bottomright")
  }

  # Add various basemaps
  leafmap <- leafmap %>%
    addProviderTiles("CartoDB.Positron", group = "Map") %>%
    addTiles(group = "Street") %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels", group = "Satellite")

  # Save Image
  if (!file == "") {
    message("Saving Image")
    mapshot(leafmap, file = file, vwidth = vwidth, vheight = vheight, remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"), selfcontained = F)
  }

  # Save HTML
  if (!html.name == "") {
    message("Saving HTML")
    saveWidget(leafmap, file = html.name, title = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(html.name)), selfcontained = selfcontained)
  }

  return(leafmap)
}

# DEBUG:
# library(dplyr)
# library(sf)
# library(leaflet)
# library(leaflet.extras)
# library(randomcoloR)
# map <- "C:/Users/a002416/Desktop/sediment_plots/banja/gis/subbasins/banja_20210413.shp"
# map.subid.column <- 1
# gd <- NULL
# bd <- NULL
# plot.scale <- T
# plot.searchbar <- F
# weight <- 0.15
# opacity <- 0.75
# fillColor <- "#4d4d4d"
# fillOpacity <- 0.25
# line.weight = 5
# line.opacity = 1
# font.size <- 10
# file <- ""
# vwidth = 1424
# vheight = 1000
# html.name <- ""
# selfcontained = FALSE
