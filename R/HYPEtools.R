#' HYPEtools: Tools to work with HYPE model set-ups and results
#'
#' This package contains functions to handle and analyse input and
#' output files from the conceptual rainfall-runoff model HYPE (HYdrological
#' Predictions for the Environment).
#'
#' @docType package
#' @name HYPEtools
#' 
#' @return 
#' No return value, package documentation.
#' 
#' @importFrom utils globalVariables

NULL
#> NULL

# Removes R Check Note on '.' not having a visible binding
# See https://stackoverflow.com/questions/66816638/no-visible-binding-for-global-variable
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

utils::globalVariables("where")