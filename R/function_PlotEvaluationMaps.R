#'
#' Plot function for up to two HYPE map results.
#'
#' Draw HYPE map results, with pretty scale discretizations and color ramp defaults for selected HYPE variables and performance metrics.
#'
#' @param dirTemp A character string for the directory to write geo-spatial objects. It is madatory and used for the figures if the figures directory is not set. The geo-spatial objects  are saved the first time the function is called and read during subsequent calls (e.g., to visualize different performance metrics) to save time. 
#' @param dirFigures A character string for the directory to write figures. If not set, figures are written with the other outputs in the outputs folder.
#' @param dird A character string specifying the path to the HYPE model subass file corresponding to the default run. The info.txt and GeoData.tx files should also be located there. See details.
#' The info.txt file provides the model run start/end dates and aggregation periods. 
#' The GeoData.txt file provides the sub-basin outlets for the gauges listed in the subass file. Uses [sf::st_as_sf] to convert the matrix of extracted co-ordinates to an \code{"sf"} object.
#' @param dirc A character string specifying the optional HYPE model subass file to be compared against the default run.


#' @param geo.summary An R list object that contains the items \code{"borders"} with the global political borders; \code{"subids"}, which are the model sub-basins polygons; \code{"rivers"}, the stream network; and \code{"gauges"} station point geospatial data constructed from the GeoData.tx file. This list is created by the function \code{"PrepareGeospatData"} the first time and saved for subsequent use.
#' @param var.eval The name of the variable e.g., "Discharge" being analysed. A character string.
#' @param sim.sum The simulation information extracted from the info and subass files.
#' @param run.names The names of the models being evaluated, written as titles in the maps. These could typically be two model versions. Character strings.
#' @param col.bar The histogram colours, also used for the map titles. Valid colour names or hex colour codes, optional.
#' @param gag.list List of gauges (must exist in the subass file), if not given, all gauges in the subass file are plotted.
#' @param criterion A valid name of a HYPE evaluation criterion. 'NSE', 'KGE', 'CC', 'MAE', 'RE(%)', and 'RMSE' are currently implemented.
#' @param data.presentation The way to present the data on the maps. Choices: "basin.outlets" (currently implemented),   "subbasin.centroids" or "subbasin.polygons". The first two use markers while the third uses filled polygons.
#' @param country.borders Logical choice to show country borders. Not reasonable for the global model.
#' @param marker.size Numerical value for the marker size, optional.
#' @param plot.river.network Logical choice to plot river network on the map. Not reasonable for the global model.
#' @param domain.name The name of the domain, a character string. The default is "wwhype" for the global model.
#' @param visualization The desired type of visualization. Choices are "relative.difference", "best.simulation" and "comparison". The first two options produce a single map while the third produces two side-by-side maps.
#' @param sequence The enumeration number of the subass file to be plotted according to the criteria defined in the info.txt file. A numeric, obligatory.
#' 
#' 
#' @importFrom sf st_as_sf st_crs st_transform as_Spatial st_geometry st_read st_is_valid st_make_valid st_union st_crop st_intersection st_is_empty st_geometry st_centroid 
#' @importFrom graphics par legend strwidth text mtext axis barplot
#' @importFrom terra fillHoles vect is.valid makeValid 
#' 
#' st_write st_buffer 
