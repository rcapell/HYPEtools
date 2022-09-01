#' Re-scale SLC classes in a GeoData data frame
#' 
#' \code{RescaleSLCClasses} re-scales several or all SLC classes for each SUBID in a GeoData data frame 
#' to a new target sum for all classes.
#' 
#' @param gd A data frame containing columns 'SLC_n' (\eqn{n \ge 1}), typically an imported 'GeoData.txt' file.
#' @param slc.exclude Integer, SLC class numbers. Area fractions of classes listed here are kept fixed 
#' during re-scaling. If \code{NULL} (default), all classes are re-scaled.
#' @param target Numeric, target sum for SLC class fractions in each subbasin after re-scaling. Either a single 
#' number or a vector with one value for each row in \code{gd}. 
#' @param plot.box Logical, if \code{TRUE}, a box plot of SLC area sums is returned.
#' 
#' @details 
#' \code{RescaleSLCClasses} allows to rescale SLC classes, e.g. as part of a post-processing work flow during 
#' HYPE model setup. Individual SLC classes can be excluded to protect. This can be useful e.g. for lake areas which 
#' maybe must correspond to areas a LakeData file. The function will throw a warning if excluded SLC class fractions 
#' are greater than sums provided in \code{target}, but not if they are smaller.
#' 
#' @return 
#' \code{RescaleSLCClasses} returns the data frame provided in \code{gd}, with re-scaled SLC class fractions.
#' 
#' @seealso 
#' \code{\link{SumSLCClasses}} for inspection of SLC class fraction sums in each subbasin
#' \code{\link{CleanSLCClasses}} for pruning of small SLC fractions.
#' 
#' @examples 
#' # Import source data
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' # Re-scale SLC classes, protect the first two
#' RescaleSLCClasses(gd = te, slc.exclude = 1:2)
#' 
#' @export

RescaleSLCClasses <- function(gd, slc.exclude = NULL, target = 1, plot.box = TRUE) {
  
  # check user input
  if (!(length(target) %in% c(1, nrow(gd)))) {
    stop("Vector length of 'target' does not match number of rows in 'gd'.")
  }
  
  # blow up single-number targets to vectors (needed for exception treatment below)
  if (length(target) == 1) {
    target <- rep(target, nrow(gd))
  }
  
  ## SLC positions and their SLC numbers
  pos.s <- which(substr(names(gd), 1, 4) == "SLC_")
  # extract numbers
  suppressWarnings(n.s <- as.numeric(substr(names(gd)[pos.s], 5, 99)))
  # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
  pos.s <- pos.s[!is.na(n.s)]
  n.s <- n.s[!is.na(n.s)]
  
  # check that there are any SLCs in input
  if (length(pos.s) == 0) {
    stop("No SLC columns found in 'gd'.")
  }
  
  # exclude SLC columns if requested
  if (!is.null(slc.exclude)) {
    if (all(slc.exclude %in% n.s)) {
      pos.s.i <- pos.s[!(n.s %in% slc.exclude)]
      pos.s.e <- pos.s[n.s %in% slc.exclude]
      
      # fixed area fraction
      fa <- as.numeric(apply(gd[, pos.s.e], 1, sum))
      
      # check if any fixed fractions exceed the target sum
      if (any(fa > target)) {
        warning(paste0("Excluded SLC fractions in 'gd' rows ", paste0(which(fa > target), collapse = ", "), 
                       " exceed 'target' area sum. Offending values replaced with 'target' values for re-scaling but returned unchanged."))
        fa[fa > target] <- target[fa > target]
      }
      
      # target area without fixed fractions
      ta <- target - fa
      
      # current area sum of non-fixed fractions
      ca <- as.numeric(apply(gd[, pos.s.i], 1, sum))
      
      # scaling factor, with treatment of exceptions if subbasin area completely within fixed classes
      scalfac <- ifelse(is.na(ta / ca) | ca == 0, 0, ta / ca)
      
      # rescale
      gd[, pos.s.i] <- gd[, pos.s.i] * scalfac
      
    } else {
      stop("One or several classes provided in 'slc.exclude' not found in gd.")
    }
  } else {
    # all SLC columns included
    
    # current area sum of SLC fractions
    ca <- as.numeric(apply(gd[, pos.s], 1, sum))
    
    # scaling factor, with treatment of exception that subbasin fractions sum up to 0
    scalfac <- ifelse(ca == 0, 0, target / ca)
    gd[, pos.s] <- gd[, pos.s] * scalfac
  }
  
  if (plot.box) {
    SumSLCClasses(gd, plot.box = TRUE, silent = TRUE)
  }
  
  gd
}
