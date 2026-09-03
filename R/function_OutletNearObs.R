
#' Find outlet-near observations in HYPE observation data files.
#' 
#' Find observation stations close to specified outlet subbasins of a HYPE model set-up. Proximity threshold as upstream area fraction of target 
#' outlet subbasin(s). Currently, only upstream observations are identified.
#' 
#' @param gd Data frame with two columns `subid` and `maindown` (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using `[ReadGeoData]`. 
#' @param file.qobs,file.xobs Character string, file location of HYPE observation data file. *Only one of these needs to be 
#' supplied*, with `file.qobs` taking precedence if both are provided. Either an
#' [Xobs.txt](http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:xobs.txt) or a 
#' [Qobs.txt](http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:qobs.txt) file.
#' @param variable Character string, HYPE variable to use. Needed only with argument `file.xobs`. If `NULL` (default), 
#' a vector of available variables in `file.xobs` is returned. 
#' @param outlets Integer vector, HYPE SUBIDs of subbasins to be considered outlets. If `NULL` (default), all outlet 
#' subbasins in `gd` are used.
#' @param frac.drain Numeric, minimum fraction of drainage area at corresponding outlet to be covered by observation site.
#' @param nearest.only Logical, if `TRUE` (default), only the nearest observation site SUBID is returned. If `FALSE`, 
#' all observation site SUBIDs available within `frac.drain` are returned. 
#' @param verbose Logical, print status messages and progress bars during runtime.
#' 
#' @details 
#' `OutletNearObs` finds observation sites for observation variables in 
#' [HYPE 'Qobs.txt'](http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:qobs.txt) and 
#' [HYPE 'Xobs.txt'](http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:xobs.txt) files 
#' located upstream an outlet sub-basin. For `file.xobs` files, which can hold several observation variables, a single variable has 
#' to be selected (the function conveniently prints available variables in `file.xobs`, if no `variable` is provided). 
#' Any number of SUBIDs present in `gd` can be defined as outlet subbasins with argument `outlets`. The function handles nested
#' outlets, i.e. cases where user-provided subbasins in `outlets` are upstream basins of one another. Outlet proximity is 
#' defined by drainage area size compared to the respective outlet. The function returns either the nearest or all sites matching 
#' or exceeding fraction `frac.drain`, depending on argument `nearest.only`.
#' 
#' @return 
#' `OutletNearObs` returns a list with two elements containing data frames. 
#' 
#' Element `outlet.no.match` with three columns, containing row-wise outlet subbasins without 
#' upstream observations according to the chosen criteria: 
#' * `subid.outlet`: SUBID of outlet subbasin
#' * `area.outlet`: Drainage area of outlet subbasin, in km^2
#' * `drain.frac.max`: Area fraction of the nearest observation subbasin, if any. `NA` otherwise.
#' 
#' Element `upstream.obs` with five columns, containing row-wise all observation sites which match 
#' the search criteria:
#' * `subid.outlet`: SUBID of outlet subbasin
#' * `subid.obs`: SUBID of observation subbasin
#' * `area.fraction`: Area fraction of observation subbasin
#' * `area.outlet`: Drainage area of outlet subbasin, in km^2
#' * `area.obs`: Drainage area of observation site, in km^2
#' 
#' If `file.xobs` is provided without `variable`, the function prints available HYPE observation variables in `file.xobs` and silently 
#' returns the same information as character vector. 
#' 
#' @examples 
#' \donttest{
#' # Import source data
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' # Find observation near domain outlet
#' OutletNearObs(file.qobs = system.file("demo_model", "Qobs.txt", package = "HYPEtools"), gd = te,
#' verbose = FALSE)
#' # get vector of variables in an Xobs file
#' OutletNearObs(file.xobs = system.file("demo_model", "Xobs.txt", package = "HYPEtools"), gd = te,
#' verbose = FALSE)
#' }
#' 
#' @importFrom pbapply pbsapply timerProgressBar setTimerProgressBar
#' @export

OutletNearObs <- function(gd, file.qobs = NULL, file.xobs = NULL, variable = NULL, outlets = NULL, frac.drain = 0.8, 
                          nearest.only = TRUE, verbose = TRUE) {
  
  # argument checks and preps
  
  if (length(frac.drain) > 1 || (frac.drain <= 0 & frac.drain > 1)) {
    stop("Argument 'frac.drain' must have a value between 0 and 1.")
  }
  if (is.null(file.qobs) && is.null(file.xobs)) {
    stop("Please provide either 'file.qobs' or 'file.xobs'.")
  }
  if (!is.null(file.qobs) && !is.null(file.xobs)) {
    file.xobs <- NULL
    warning("Both 'file.qobs' and 'file.xobs' provided. 'file.qobs' takes precedence.")
  }
  
  
  # conditional: if file.xobs is provided without variable, list available variables in file.xobs and exit
  
  if (!is.null(file.xobs) && is.null(variable)) {
    
    res <- unique(attr(ReadXobs(filename = file.xobs, nrows = 0), "variable"))
    cat(paste0("Available variables in 'file.xobs':\n", paste(res, collapse = " "), "\n"))
    invisible(res)
    
  } else {
    
    # import obs info
    if (is.null(file.xobs)) {
      sbd.obs <- attr(ReadObs(filename = file.qobs, variable = "rout", nrows = 0), "obsid")
    } else {
      sbd.obs <- attr(ReadXobs(filename = file.xobs, nrows = 0, variable = variable), "subid")
    }
    
    # check if 'outlets' exist in gd, or get all from gd if none provided
    if (!is.null(outlets)) {
      
      outs <- outlets
      check.ogd <- outs %in% gd$SUBID
      
      if (!all(check.ogd)) {
        
        stop(paste0("SUBID(s) ", paste0(outlets[!check.ogd], collapse = ", "), " provided in argument 'outlets' not existing in 'gd'."))
        
      } else {
        
        # only sub-set of domain needs to be searched, reduce search data. Works even if duplicates in results from AllUpstreamSubids 
        # (happens if 'outlets' contains nested subbasins)
        if (verbose) {
          cat("\nExtracting 'gd' sub-set needed for upstream search of SUBIDs in 'outlets'.\n")
          outup <- pbsapply(outlets, FUN = AllUpstreamSubids, gd = gd)
        } else {
          outup <- sapply(outlets, FUN = AllUpstreamSubids, gd = gd)
        }
        
        gd.sel <- gd[gd$SUBID %in% unlist(outup), ]
        sbd.obs <- sbd.obs[sbd.obs %in% gd.sel$SUBID]
        
      }
      
    } else {
      
      outs <- OutletSubids(gd)
      gd.sel <- gd
      
    }
    
    
    # upstream areas and area fraction at obs
    
    if (verbose) {
      cat("\nCalculating upstream area of outlet subbasins.")
    }
    outs.area <- SumUpstreamArea(outs, gd = gd.sel, progbar = verbose)
    
    if (verbose) {
      cat("\nCalculating upstream area of observation site subbasins")
    }
    obs.area <- SumUpstreamArea(sbd.obs, gd = gd.sel, progbar = verbose)
    
    
    
    ## iterate through outlet subids and identify upstream observations
    
    # return table
    res <- list(outlet.no.match = data.frame(subid.outlet = numeric(), 
                                             area.outlet = numeric(), 
                                             drain.frac.max = numeric()
    ), 
    upstream.obs = data.frame(subid.outlet = numeric(), 
                              subid.obs = numeric(), 
                              area.fraction = numeric(), 
                              area.outlet = numeric(), 
                              area.obs = numeric()
    )
    )
    
    # initialise progress bar
    if (verbose) {
      cat("\nIdentifying upstream observations for all outlet subbasins.\n")
      pbar <- timerProgressBar(min = 0, max = length(outs), initial = 0, char = "+", width = 50, 
                               style = 6) 
    }
    
    for (i in 1:length(outs)) {
      
      # update progress bar
      if (verbose) {
        setTimerProgressBar(pbar, value = i)
      }
      
      # find all upstream subids of current outlet
      te.upstream <- AllUpstreamSubids(outs[i], gd = gd, sort = FALSE, get.weights = FALSE)
      
      # match upstream observations
      te.match <- sbd.obs %in% te.upstream
      
      if (!any(te.match)) {
        
        # no matches, return subid to no-match results
        res$outlet.no.match[nrow(res$outlet.no.match) + 1, 1] <- outs.area$SUBID[i]
        res$outlet.no.match[nrow(res$outlet.no.match), 2] <- outs.area$UPSTREAMAREA[i] * 10^-6
        res$outlet.no.match[nrow(res$outlet.no.match), 3] <- NA
        
      } else {
        
        # one or more matches, return a row for each 
        te.obs.area <- obs.area[te.match, ]
        te.res <- data.frame(subid.outlet = rep(outs[i], nrow(te.obs.area)), 
                             subid.obs = te.obs.area$SUBID, 
                             area.fraction = rep(NA, nrow(te.obs.area)), 
                             area.outlet = rep(outs.area[i, 2], nrow(te.obs.area)), 
                             area.obs = te.obs.area$UPSTREAMAREA)
        
        # calculate area fraction and convert area to km2 from m2
        te.res$area.fraction <- te.res$area.obs / te.res$area.outlet
        te.res$area.outlet <- te.res$area.outlet * 10^-6
        te.res$area.obs <- te.res$area.obs * 10^-6
        
        # identfy fractions under threshold 
        te.frac <- which(te.res$area.fraction < frac.drain)
        
        if (length(te.frac) == nrow(te.res)) {
          
          # if all matches are under threshold, add current outlet subid to no-match results
          res$outlet.no.match[nrow(res$outlet.no.match) + 1, 1] <- outs.area$SUBID[i]
          res$outlet.no.match[nrow(res$outlet.no.match), 2] <- outs.area$UPSTREAMAREA[i] * 10^-6
          res$outlet.no.match[nrow(res$outlet.no.match), 3] <- max(te.res$area.fraction)
          
          
        } else {
          
          # remove fractions under threshold
          te.res <- te.res[-te.frac, ]
          
          # if requested, keep only nearest observation subid
          if (nearest.only) {
            te.res <- te.res[which.max(te.res$area.fraction), ]
          }
          
          # return results over area fraction threshold to result table
          res$upstream.obs <- rbind(res$upstream.obs, te.res)
          
        }
      }
    }
    
    return(res)
    
  }
}
