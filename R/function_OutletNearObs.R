
#' Find outlet-near observations in HYPE observation data files
#' 
#' @param gd Data frame with two columns \code{subid} and \code{maindown} (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' @param qobs,xobs Character string, file location of HYPE observation data file. \emph{Only one of these needs to be 
#' supplied}, with \code{qobs} taking precedence if both are provided. Either an
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt} or a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:qobs.txt}{Qobs.txt} file.
#' @param variable Character string, HYPE variable to use. Needed only with argument \code{xobs}. If \code{NULL} (default), 
#' a vector of available variables in \code{xobs} is returned. 
#' outlets Integer, vector of HYPE SUBIDs to be considered outlets. If \code{NULL} (default), all outlets of the model domain 
#' are used.
#' @param frac.drain Numeric, minimum fraction of drainage area at corresponding outlet to be covered by observation site.
#' @param nearest.only Logical, if \code{TRUE} (default), only the nearest observation site SUBID is returned. If \code{FALSE}, 
#' all observation site SUBIDs available within \code{frac.drain} are returned. 
#' 
#' @details 
#' \code{OutletNearObs} identifies observation sites located near to an outlet sub-basin. Outlet proximity is defined 
#' by drainage area size compared to the respective outlet, with sites matching or exceeding fraction \code{frac.drain}
#' 
#' @return 
#' \code{OutletNearObs} silently returns a vector of available HYPE variables in \code{xobs}, if that is provided without 
#' \code{variable}. Otherwise, the functions returns a data frame with 2 columns
#' 
#' @examples 
#' \dontrun{OutletNearObs(qobs = "Qobs.txt", gd = mygd)
#' 
#' # get vector of variables in an Xobs file
#' OutletNearObs(xobs = "Xobs.txt", gd = mygd)
#' 
#' # get observation site SUBIDs for total nitrogen concentrations
#' OutletNearObs(xobs = "Xobs.txt", gd = gd, variable = "retn")
#' }
#' 
#' @export

OutletNearObs <- function(gd, qobs = NULL, xobs = NULL, variable = NULL, outlets = NULL, frac.drain = 0.8, 
                          nearest.only = TRUE) {
  
  # argument checks and preps
  
  if (length(frac.drain) > 1 || (frac.drain <= 0 & frac.drain > 1)) {
    stop("Argument 'frac.drain' must have a value between 0 and 1.")
  }
  if (is.null(qobs) && is.null(xobs)) {
    stop("Please provide either 'qobs' or 'xobs'.")
  }
  if (!is.null(qobs) && !is.null(xobs)) {
    xobs <- NULL
    warning("Both 'qobs' and 'xobs' provided. 'qobs' takes precedence.")
  }
  
  
  # conditional: if xobs is provided without variable, list available variables in xobs and exit
  
  if (!is.null(xobs) && is.null(variable)) {
    
    res <- unique(attr(ReadXobs(filename = xobs, nrows = 0), "variable"))
    cat(paste0("Available variables in 'xobs':\n", paste(res, collapse = " "), "\n"))
    invisible(res)
    
  } else {
    
    # import obs info
    if (is.null(xobs)) {
      sbd.obs <- attr(ReadPTQobs(filename = qobs, variable = "rout", nrows = 0), "obsid")
    } else {
      sbd.obs <- attr(ReadXobs(filename = xobs, nrows = 0, variable = variable), "subid")
    }
    
    # check if 'outlets' exist in gd, or get all from gd if none provided
    if (!is.null(outlets)) {
      
      check.ogd <- outlets %in% gd$SUBID
      
      if (!all(check.ogd)) {
        
        stop(paste0("SUBID(s) ", paste0(outlets[!check.ogd], collapse = ", "), " provided in argument 'outlets' not existing in 'gd'."))
        
      } else {
        
        # only sub-set of domain needs to be searched, reduce search data. Works even if duplicates in results from AllUpstreamSubids 
        # (happens if 'outlets' contains nested subbasins)
        outup <- sapply(outlets, FUN = AllUpstreamSubids, gd = gd)
        gd.sel <- gd[gd$SUBID %in% unlist(outup), ]
        sbd.obs <- sbd.obs[sbd.obs %in% gd.sel$SUBID]
      }
      
    } else {
      
      outlets <- OutletSubids(gd)
      gd.sel <- gd
    }
    
    # downstream SUBIDs of observations
    ## THIS FAILS WITH DUPLICATES IN outlets, WRITE NEW SOLUTION BASED ON outup!
    outdown <- sapply(sbd.obs, AllDownstreamSubids, gd = gd.sel) 
    
    # outlet basins, corresponding to sbd.obs
    sbd.out <- sapply(outdown, function(x) x[length(x) - 1])
    
    # upstream areas and area fraction at obs
    outarea <- SumUpstreamArea(sbd.out, gd = gd.sel, progbar = T)
    obsarea <- SumUpstreamArea(sbd.obs, gd = gd.sel, progbar = T)
    obsfrac <- obsarea[, 2] / outarea[, 2]
    
    # combine and select results according to frac.drain
    res <- data.frame(subid.outlet = sbd.out, subid.obs = sbd.obs, area.fraction = obsfrac, area.obs = obsarea[, 2])
    res <- res[res$area.fraction >= frac.drain, ]
    
    # conditional, keep only obs site nearest to outlet
    if (nearest.only) {
      
      tapply(res$area.fraction, list(res$subid.outlet, function(x, y) which.max)
      # check for river duplicates
      dupli <- unique(res$subid.outlet[duplicated(res$subid.outlet)])
      te <- res[res$subid.outlet == dupli[1], ]
      te[which.max(rank(te$area.fraction))
      for (i in 1:length(dupli)) {
        te <- sapply(outdown[sbd.out == dupli[i]], length)
        outdown[outletbasins == dupli[i]][rank(te) != 1] <- NA
      }
    }
  }
  
}
