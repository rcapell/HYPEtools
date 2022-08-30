# this file contains any actions performed on load and unload

# .onUnload <- function(libpath)
#   library.dynam.unload("HYPEtools", libpath)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("HYPEtools version ", utils::packageVersion("HYPEtools"), ". Find the latest release at: https://github.com/rcapell/HYPEtools/releases\nPlease report bugs and feature requests at: https://github.com/rcapell/HYPEtools/issues \n",
                               '- To suppress this message use: "suppressPackageStartupMessages(library(HYPEtools))"'))
}

# Questions to ask when running devtools::release()
release_questions <- function() {
  c(
    "Have you updated the internal package data for the VariableLookup() function? Use the code at the bottom of function_VariableLookup.R",
    "Have you used TRUE/FALSE instead of T/F? Check with lintr::lint_package(linters=list(lintr::T_and_F_symbol_linter()))"
  )
}