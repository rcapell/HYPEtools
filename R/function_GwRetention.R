
#' Calculate groundwater retention of nutrients
#' 
#' Function to calculate nutrient load retention fractions in groundwater parts of HYPE, i.e. after root zone retention. See Details for 
#' exact definition.
#' 
#' @param nfrz Data frame with two-columns. Sub-basin IDs in first column, net loads from root zone in kg/year in second column. Typically an 
#' imported HYPE map output file, HYPE output variable SL06. See Details. 
#' @param gts3 Data frame with two-columns. Sub-basin IDs in first column, gross loads to soil layer 3 in kg/year in second column. 
#' Typically an imported HYPE map output file, HYPE output variable SL17. See Details. 
#' @param nfs3 Data frame with two-columns. Sub-basin IDs in first column, net loads from soil layer 3 in kg/year in second column. 
#' Typically an imported HYPE map output file, HYPE output variable SL18. See Details. 
#' @param gd Data frame, with columns containing sub-basin IDs and rural household emissions, e.g. an imported 'GeoData.txt' file. 
#' See details. 
#' @param unit.area Logical, set to \code{FALSE} to calculate incoming load (leaching rates) in kg/year instead of kg/(ha year).
#' @param par List, HYPE parameter list, typically an imported 'par.txt' file. Must contain parameter \emph{locsoil} (not case-sensitive). 
#' @param nutrient Character keyword, one of the HYPE-modelled nutrient groups, for which to calculate groundwater retention. Not 
#' case-sensitive. \emph{Currently, only \code{tn} {total nitrogen} is implemented.}
#' 
#' @details 
#' \code{GwRetention} calculates a groundwater nutrient retention as fractions of outgoing and incoming loads using HYPE soil load variables. Incoming loads  
#' include drainage into layer 3 from the root zone (defined as soil layer 1 and 2), rural load fractions into soil (dependent on parameter \emph{locsoil}), 
#' tile drainage, surface flow, and flow from layer 1 and 2. Outgoing loads include runoff from all soil layers, tile drain, and surface flow.
#' 
#' The retention fraction \emph{R} is calculated as (see also the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_model_description:hype_np_soil#diagnostic_output_variables_of_soil_nutrients}{variable description in the HYPE online documentation}):
#' 
#' \eqn{R = 1 - \frac{OUT}{IN} = 1 - \frac{nfrz - gts3 + nfs3 + locsoil * lr}{nfrz + locsoil * lr}}{R = 1 - OUT/IN = 1 - (nfrz - gts3 + nfs3 + locsoil * lr)/li} [-]
#'
#'  \eqn{li = nfrz + locsoil * lr} \[kg/y\]
#'
#'  \eqn{lr = LOC_VOL * LOC_TN * 0.365} \[kg/y\]
#' 
#' , where \emph{li} is incoming load to groundwater (leaching rates), \emph{lr} is rural load (total from GeoData converted to kg/yr; \emph{locsoil} in the formula converts it to rural load into soil layer 3), and 
#' \emph{nfrz}, \emph{gts3}, \emph{nfs3} are soil loads as in function arguments described above. See Examples for HYPE variable names for \code{TN} loads.
#' 
#' Columns \code{SUBID}, \code{LOC_VOL}, and code{LOC_TN} must be present in \code{gd}, for a description of column contents see the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:geodata.txt}{GeoData file description in the HYPE online documentation}. 
#' Column names are not case-sensitive. 
#' 
#' @return 
#' \code{GwRetention} returns a three-column data frame, containing SUBIDs, retention in groundwater as a fraction of incoming loads 
#' (if multiplied by 100, it becomes \%), and incoming loads to groundwater (leaching rates) in units depending on argument \code{unit.area}. 
#' . 
#' 
#' @examples
#' # Create dummy data
#' te1 <- ReadGeoData(filename = system.file("demo_model",
#' "GeoData.txt", package = "HYPEtools"))
#' te1$loc_tn <- runif(n = nrow(te1), min = 0, max = 100)
#' te1$loc_vol <- runif(n = nrow(te1), min = 0, max = 2)
#' te2 <- ReadPar(filename = system.file("demo_model",
#' "par.txt", package = "HYPEtools"))
#' te2$locsoil <- .3
#' # HYPE soil load (sl) variables for TN, dummy loads
#' GwRetention(nfrz = data.frame(SUBID = te1$SUBID, SL06 = runif(n = nrow(te1), 10, 50)), 
#'             gts3 = data.frame(SUBID = te1$SUBID, SL17 = runif(n = nrow(te1), 10, 50)), 
#'             nfs3 = data.frame(SUBID = te1$SUBID, SL18 = runif(n = nrow(te1), 10, 50)), 
#'             gd = te1, par = te2)
#' 
#' @export


GwRetention <- function(nfrz, nfs3, gts3, gd, par, unit.area = TRUE, nutrient = "tn") {
  
  # input checks
  if (tolower(nutrient) == "tn") {
    loc_c <- "loc_tn"
  } else {
    stop("Unknown 'substance' keyword.")
  }
  
  if(!all(nrow(gd) == c(nrow(nfrz), nrow(nfs3), nrow(gts3)))) {
    warning("Different number of SUBIDs in input arguments.")
  }
  
  
  # extract relevant columns from geodata
  rural <- tryCatch(gd[, match(c("subid", "loc_vol", loc_c, "area"), tolower(colnames(gd)))], error = function(e) {NULL})
  # check that all were found
  if (is.null(rural)) {
    stop("Missing column(s) in 'gd'.")
  }
  
  # calculate rural load in kg/y
  rural <- data.frame(rural, lr = rural[, 2] * rural[, 3] * 0.365)
  
  # get locsoil from par
  locsoil <- tryCatch(par[[which(tolower(names(par)) == "locsoil")]], error = function(e) {NULL})
  # check that it was found
  if (is.null(locsoil)) {
    stop("Missing parameter LOCSOIL in 'par'.")
  }
  
  # merge all soil load inputs with rural loads
  loads <- merge(rural[, -c(2:3)], nfrz, by = 1, all = FALSE, sort = FALSE)
  loads <- merge(loads, nfs3, by = 1, all = FALSE, sort = FALSE)
  loads <- merge(loads, gts3, by = 1, all = FALSE, sort = FALSE)
  names(loads)[4:6] <- c("nfrz", "nfs3", "gts3")
  
  # calculate leaching rate, conditional on user choice (unit area or not)
  leach.gw <- with(loads, nfrz + locsoil * lr)
  if (unit.area) {
    leach.gw <- leach.gw / loads[, 2] * 10000
  } 
  
  # calculate groundwater retention
  retention <- data.frame(SUBID = loads[, 1], ret.gw = with(loads, 1 - (nfrz - gts3 + nfs3 + locsoil * lr)/(nfrz + locsoil * lr)), leach.gw = leach.gw)
  
  return(retention)
}