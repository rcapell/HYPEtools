
#' @export
#' @title
#' Calculate Specific runoff from volumetric discharge and vice versa
#'
#' @description
#' \code{ConvertDischarge} converts volumetric discharge to specific discharge (unit area discharge) and vice versa.
#' 
#' @param q An object of type \code{numeric}, containing volumetric or specific discharge values, 
#' typically HYPE variables COUT or ROUT.
#' @param area An object of type \code{numeric}, containing a catchment area in \eqn{m^2}.
#' @param from Character string keyword, giving the current unit of \code{q}. Either a specific discharge, one of:
#' 
#' \code{"mmy"} (\eqn{mm y^{-1}})
#' 
#' \code{"mmd"} (\eqn{mm d^{-1}})
#' 
#' \code{"mmh"} (\eqn{mm h^{-1}}), 
#' 
#' or a volumetric discharge, one of:
#' 
#' \code{"m3s"} (\eqn{m^3 s^{-1}})
#' 
#' \code{"ls"} (\eqn{l s^{-1}}).
#' @param to Character string keyword, see \code{from}. Conversion will not work between units within volumetric or specific discharge groups.
#' 
#' @details
#' \code{ConvertDischarge} is a simple conversion function, most likely to be used in combination with \code{\link{apply}} 
#' or related functions.
#' 
#' @return 
#' \code{ConvertDischarge} returns a numeric object of the same type as provided in argument \code{q}.
#' 
#' @examples
#' ConvertDischarge(6, 400000000)
#' ConvertDischarge(c(1.1, 1.2, 1.9, 2.8, 2, 1.5, 1.3, 1.2, 1.15, 1.1), 
#'                  from = "mmd", to = "ls", area = 1.2e6)

ConvertDischarge <- function (q, area, from = "m3s", to = "mmd") {
  
  # input validity checks
  if (!(from %in% c("mmy", "mmd", "mmh", "m3s", "ls"))) {
    stop("Argument 'from': keyword unknown.")
  }
  if (!(to %in% c("mmy", "mmd", "mmh", "m3s", "ls"))) {
    stop("Argument 'to': keyword unknown.")
  }
  if (from == to) {
    stop("'from' and 'to' units identical, nothing to convert.")
  }
  
  # conditional: conversion cases
  if (from == "mmy" && to == "m3s") {
    # factor: 1 / (86400 * 365 s/y * 1000 mm/m)
    q.out <- q * area / 3.1536e+10
  } else if (from == "mmy" && to == "ls"){
    # factor: 1000 l/m3 / (86400 * 365 s/y * 1000 mm/m)
    q.out <- q * area / 31536000
  } else if (from == "mmd" && to == "m3s"){
    # factor: 1 / (86400 s/d * 1000 mm/m)
    q.out <- q * area / 86400000
  } else if (from == "mmd" && to == "ls"){
    # factor: 1000 l/m3 / (86400 s/d * 1000 mm/m)
    q.out <- q * area / 86400
  } else if (from == "mmh" && to == "m3s"){
    # factor: 1 / (3600 s/h * 1000 mm/m)
    q.out <- q * area / 3600000
  } else if (from == "mmh" && to == "ls"){
    # factor: 1000 l/m3 / (3600 s/h * 1000 mm/m)
    q.out <- q * area / 3600
    
  } else if (from == "m3s" && to == "mmy"){
    # factor: 1000 mm/m * 86400 s/d * 365 d/y
    q.out <- q * 3.1536e+10 / area
  } else if (from == "m3s" && to == "mmd"){
    # factor: 86400 s/d * 1000 mm/m
    q.out <- q * 86400000 / area
  } else if (from == "m3s" && to == "mmh"){
    # factor: 3600 s/h * 1000 mm/m
    q.out <- q * 3600000 / area
  } else if (from == "ls" && to == "mmy"){
    # factor: 86400 * 365 s/y * 1000 mm/m / 1000 l/m3
    q.out <- q * 31536000 / area
  } else if (from == "ls" && to == "mmd"){
    # factor: 86400 s/d * 1000 mm/m / 1000 l/m3
    q.out <- q * 86400 / area
  } else if (from == "ls" && to == "mmh"){
    # factor: 3600 s/h * 1000 mm/m / 1000 l/m3
    q.out <- q * 3600 / area
  } else {
    stop("Requested conversion is not supported.")
  }
  
  return(q.out)
}