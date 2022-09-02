
#' Find outlet-near observations in HYPE observation data files.
#' 
#' Find observation stations close to specified outlet subbasins of a HYPE model set-up. Proximity threshold as upstream area fraction of target 
#' outlet subbasin(s). Currently, only upstream observations are identified.
#' 
#' @param gd Data frame with two columns \code{subid} and \code{maindown} (not case-sensitive). 
#' Typically a 'GeoData.txt' file imported using \code{\link{ReadGeoData}}. 
#' @param file.qobs,file.xobs Character string, file location of HYPE observation data file. \emph{Only one of these needs to be 
#' supplied}, with \code{file.qobs} taking precedence if both are provided. Either an
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt} or a 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:qobs.txt}{Qobs.txt} file.
#' @param variable Character string, HYPE variable to use. Needed only with argument \code{file.xobs}. If \code{NULL} (default), 
#' a vector of available variables in \code{file.xobs} is returned. 
#' @param outlets Integer vector, HYPE SUBIDs of subbasins to be considered outlets. If \code{NULL} (default), all outlet 
#' subbasins in \code{gd} are used.
#' @param frac.drain Numeric, minimum fraction of drainage area at corresponding outlet to be covered by observation site.
#' @param nearest.only Logical, if \code{TRUE} (default), only the nearest observation site SUBID is returned. If \code{FALSE}, 
#' all observation site SUBIDs available within \code{frac.drain} are returned. 
#' @param verbose Logical, print status messages and progress bars during runtime.
#' 
#' @details 
#' \code{OutletNearObs} finds observation sites for observation variables in 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:qobs.txt}{HYPE 'Qobs.txt'} and 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{HYPE 'Xobs.txt'} files 
#' located upstream an outlet sub-basin. For \code{file.xobs} files, which can hold several observation variables, a single variable has 
#' to be selected (the function conveniently prints available variables in \code{file.xobs}, if no \code{variable} is provided). 
#' Any number of SUBIDs present in \code{gd} can be defined as outlet subbasins with argument \code{outlets}. The function handles nested
#' outlets, i.e. cases where user-provided subbasins in \code{outlets} are upstream basins of one another. Outlet proximity is 
#' defined by drainage area size compared to the respective outlet. The function returns either the nearest or all sites matching 
#' or exceeding fraction \code{frac.drain}, depending on argument \code{nearest.only}.
#' 
#' @return 
#' \code{OutletNearObs} returns a data frame with 4 columns, containing row-wise all observation sites which match the search 
#' criteria:
#' \describe{
#'   \item{subid.outlet}{SUBID of outlet subbasin}
#'   \item{subid.obs}{SUBID of observation site}
#'   \item{area.fraction}{Relative drainage area fraction of observation site, compared to corresponding outlet subbasin}
#'   \item{area.outlet}{Drainage area of outlet subbasin, in km^2}
#'   \item{area.obs}{Drainage area of observation site, in km^2}
#' }
#' 
#' If \code{file.xobs} is provided without \code{variable}, the function prints available HYPE observation variables in \code{file.xobs} and silently 
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
#' @importFrom pbapply pbsapply
#' @importFrom utils txtProgressBar setTxtProgressBar
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
      sbd.obs <- attr(ReadPTQobs(filename = file.qobs, variable = "rout", nrows = 0), "obsid")
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
          cat("Extracting 'gd' sub-set needed for upstream search of SUBIDs in 'outlets'.\n")
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
    
    # downstream subid sequences of observation sites
    if (verbose) {
      cat("Calculating downstream sequences of observation sites\n")
      outdown <- pbsapply(sbd.obs, AllDownstreamSubids, gd = gd.sel) 
    } else {
      outdown <- sapply(sbd.obs, AllDownstreamSubids, gd = gd.sel) 
    }
    
    # => this list does not include possible nested sites in user-defined 'outlets' vector (it includes only domain outlets)
    #    such cases are identified and appended below
    
    
    # if user-defined outlets are searched, find nested subids in 'outlets', ie check if any of subids in outlets are an upstream 
    # subbasin of another subid in outlets
    if (!is.null(outlets)) {
      
      te <- unlist(lapply(lapply(outup, function(x) x[-1]), function(x, y) y[y %in% x], y = outs))
      
      ## check if any nested subids exist in downstream sequences of observations, and add any existing to 'outdown'
      if (length(te) > 0) {
        
        # initialise list with downstream sequences of nested outlets
        outdown.nested <- list()
        
        if (verbose) {
          cat("Extract downstream sequences of nested outlet SUBIDs.")
          pbar <- txtProgressBar(min = 0, max = length(te), initial = 0) 
        }
        
        # iterate through identified nested subids
        for (i in 1:length(te)) {
          
          if (verbose) {
            setTxtProgressBar(pbar, i)
          }
          
          # only proceed if nested subid exists in downstream sequences of obs sites
          if (te[i] %in% unlist(outdown)) {
            
            # find position(s) in downstream sequences (duplicates possible if obs sites in tributaries of nested outlet subid)
            pos.nested <- sapply(outdown, function (x, y) which(x == y), y = te[i])
            # pick the first one (any duplicates above are identitical), and select downstream sequence from nested subid to outlet
            pos.outd <- which(sapply(pos.nested, length) == 1)
            outdown.nested <- c(outdown.nested, lapply(1:length(pos.outd), function (x, y, z) z[[x]][1:y[[x]]], y = pos.nested[pos.outd], z = outdown[pos.outd]))
            
          } else {
            next
          }
        }
        
        # if any were found, append new downstream sequences to the list
        if (length(outdown.nested) > 0) {
          outdown <- c(outdown, outdown.nested)
        }
      }
    }
    
    
    # outlet and observation basins (can contain duplicated obs basins if nested outlet basins exist)
    sbd.out <- sapply(outdown, function(x) x[length(x)])
    sbd.obs <- sapply(outdown, function(x) x[1])
    
    
    # upstream areas and area fraction at obs
    
    if (verbose) {
      cat("Calculating upstream area of outlet subbasins\n")
    }
    outarea <- SumUpstreamArea(sbd.out, gd = gd.sel, progbar = verbose)
    
    if (verbose) {
      cat("Calculating upstream area of observation site subbasins\n")
    }
    obsarea <- SumUpstreamArea(sbd.obs, gd = gd.sel, progbar = verbose)
    
    obsfrac <- obsarea[, 2] / outarea[, 2]
    
    # combine and select results according to frac.drain
    res <- data.frame(subid.outlet = sbd.out, subid.obs = sbd.obs, area.fraction = obsfrac, area.outlet = outarea[, 2] * 10^-6, area.obs = obsarea[, 2] * 10^-6)
    res <- res[res$area.fraction >= frac.drain, ]
    
    # order results by outlet subid and area fraction (decreasing)
    res <- res[order(res$subid.outlet, res$area.fraction, decreasing = TRUE), ]
    
    # conditional, keep only obs sites nearest to outlet
    if (nearest.only) {
      
      # remove outlet subid duplicates, retaining the first occurrence with the largest area fraction
      res <- res[!duplicated(res$subid.outlet), ]
    }
    
    # clean row names
    row.names(res) <- 1:nrow(res)
    
    return(res)
  }
}
