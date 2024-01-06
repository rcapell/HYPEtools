#' Generate optimally distinct color palettes
#' 
#' \code{distinctColorPalette} generates an attractive palette of random colors.

#' @param count Integer, number of colors (>= 1). May be ineffective for count > 40.
#' @param seed Integer, seed number to produce repeatable palettes.
#' @details
#' Adapted from the randomcoloR package \url{https://cran.r-project.org/package=randomcoloR}.
#' 
#' @return
#' \code{distinctColorPalette} returns a character vector of \code{count} optimally distinct colors in hexadecimal codes.

#' @examples
#' distinctColorPalette()

#' @importFrom colorspace hex RGB LAB
#' @importFrom stats kmeans
#' @export

distinctColorPalette <-function(count = 1, seed = NULL) {
  
  # Check count
  if (!count >= 1) {
    stop('"count" must be >= 1.',call. = FALSE)
  }

  # Set seed if specified
  if(!is.null(seed)){

    # Remove seed on exit
    on.exit({rm(.Random.seed, envir=.GlobalEnv)})
    
    # Set seed
    set.seed(seed)
  }

  # Compute a 2000 color spectrum and convert to LAB
  n <- 2e3
  currentColorSpace <- RGB(runif(n), runif(n), runif(n))
  currentColorSpace <- as(currentColorSpace, "LAB")
  currentColorSpace <- currentColorSpace@coords

  # Get random colors
  km <- kmeans(currentColorSpace, count, iter.max=20)
  colors <- unname(hex(LAB(km$centers)))

  return(colors)
}
