.onUnload <- function(libpath)
  library.dynam.unload("RHYPE", libpath)

.onLoad <- function(...) {
  packageStartupMessage("RHYPE is improved continuously, consider updating frequently.\nThis is version 0.3-10.\nFind the latest release here: https://github.com/rcapell/RHYPE/releases")
}
