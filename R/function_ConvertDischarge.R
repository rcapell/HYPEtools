
#' @export
#' @title
#' Calculate Specific Discharge
#'
#' @description
#' \code{ConvertDischarge} converts volumetric discharge to specific discharge from a unit area.
#' 
#' @param q An object of type \code{numeric}, containing volumetric discharge in \eqn{m^3 s^{-1}}, 
#' typically HYPE variables COUT or ROUT.
#' @param area An object of type \code{numeric}, containing a catchment area in \eqn{m^2}.
#' 
#' @details
#' \code{ConvertDischarge} is a simple conversion function, most likely to be used in combination with \code{\link{apply}} 
#' or related functions. It converts input discharge from \eqn{m^3 s^{-1}} to \eqn{mm y^{-1}}
#' 
#' @examples
#' \dontrun{ConvertDischarge(6, 400000000)
#' apply(data[, -c(1:2)], 2, ConvertDischarge, data$AREA)
#' }
#' 

ConvertDischarge <- function (q, area) {
  # q in m3/s, area in m2
  # factors: 1000 mm/m * 86400 s/d * 365 d/y
  q * 3.1536e+10/ area
}