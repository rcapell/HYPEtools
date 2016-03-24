# this file contains any actions performed on load

.onUnload <- function(libpath)
  library.dynam.unload("RHYPE", libpath)

.onLoad <- function(...) {
  packageStartupMessage("RHYPE is improved continuously, check for updates frequently. Please report bugs and feature requests here: https://github.com/rcapell/RHYPE/issues\nThis is version 0.3-10. Find the latest release here: https://github.com/rcapell/RHYPE/releases")
}
