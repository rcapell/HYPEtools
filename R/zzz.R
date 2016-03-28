# this file contains any actions performed on load

.onUnload <- function(libpath)
  library.dynam.unload("RHYPE", libpath)

.onLoad <- function(...) {
  packageStartupMessage("This is RHYPE version 0.3-10. Find the latest release at: https://github.com/rcapell/RHYPE/releases\nPlease report bugs and feature requests at: https://github.com/rcapell/RHYPE/issues")
}
