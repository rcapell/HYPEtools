# this file contains any actions performed on load

.onUnload <- function(libpath)
  library.dynam.unload("HYPEtools", libpath)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("HYPEtools version 0.4-3. Find the latest release at: https://github.com/rcapell/HYPEtools/releases\nPlease report bugs and feature requests at: https://github.com/rcapell/HYPEtools/issues")
}
