# this file contains any actions performed on load and unload

# .onUnload <- function(libpath)
#   library.dynam.unload("HYPEtools", libpath)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("HYPEtools version ", utils::packageVersion("HYPEtools"), ". Find the latest release at: https://github.com/rcapell/HYPEtools/releases\nPlease report bugs and feature requests at: https://github.com/rcapell/HYPEtools/issues"))
}
