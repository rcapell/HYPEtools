#' 
#' Find All Upstream SUBIDs
#'
#' Function to find all SUBIDs of upstream sub-catchments for a single 
#' sub-catchment. 
#'
#' @param subid SUBID of a target sub-catchment (must exist in \code{gd}). 
#' @param gd A data frame, containing 'SUBID' and 'MAINDOWN' columns, e.g. an imported 'GeoData.txt' file. Mandatory argument. See 'Details'.
#' @param bd A data frame, containing 'BRANCHID' and 'SOURCEID' columns, and 'MAINPART' with argument \code{get.weights}, 
#' e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param sort Logical. If \code{TRUE}, the resulting upstream SUBID vector will be sorted according to order in argument \code{gd}, i.e. in 
#' downstream order for a working GeoData table.
#' @param get.weights Logical. If \code{TRUE}, flow weights are computed along the upstream SUBID sequence. See details.
#' @param write.arcgis Logical. If \code{TRUE}, a string containing an SQL expression suitable for ArcGIS's 
#' 'Select By Attributes' feature will be written to the clipboard.
#' 
#' @details
#' \code{AllUpstreamSubids} finds all upstream SUBIDs of a given SUBID (including itself but not 
#' including potential irrigation links or groundwater flows) using GeoData columns 'SUBID' and 'MAINDOWN', i.e the full upstream catchment. 
#' If a BranchData file is provided, the function will also include upstream areas which are connected through an upstream bifurcation. The 
#' results can be directly used as 'partial model setup file' ('pmsf.txt') using the export function \code{\link{WritePmsf}}.
#' 
#' If argument \code{get.weights} is set to \code{TRUE}, weighting fractions are returned along with upstream SUBIDs. The fractions are based 
#' on column 'MAINPART' in argument \code{bd}. The function considers fractions from bifurcation branches which flow into the basin, and 
#' fractions where bifurcation branches remove discharge from the basin. Fractions are incrementally updated, i.e. nested bifurcation fractions 
#' are multiplied.
#' 
#' For details on bifurcation handling in HYPE, see the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:branchdata.txt}{HYPE online documentation for BranchData.txt}.
#' 
#' @return
#' If \code{get.weights} is \code{FALSE}, \code{AllUpstreamSubids} returns a vector of SUBIDs, otherwise a two-column data frame with SUBIDs in 
#' the first, and flow weight fractions in the second column.
#' 
#' @seealso 
#' \code{\link{UpstreamGeoData}}, \code{\link{AllDownstreamSubids}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' AllUpstreamSubids(subid = 63794, gd = te)
#' 
#' @export
#' 
#' @importFrom clipr write_clip clipr_available


AllUpstreamSubids <- function(subid, gd, bd = NULL, sort = FALSE, get.weights = FALSE, write.arcgis = FALSE) {
  
  # identify relevant column positions in geodata and branchdata
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sbd <- which(tolower(colnames(gd)) == "subid")
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sbd) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  if (!is.null(bd)) {
    brcol.br <- which(tolower(colnames(bd)) == "branchid")
    brcol.sr <- which(tolower(colnames(bd)) == "sourceid")
    # check existence
    if (length(brcol.br) != 1 || length(brcol.sr) != 1) {
      stop("BRANCHID and/or SOURCEID column not found in 'bd'.")
    }
    if (get.weights) {
      brcol.mp <- which(tolower(colnames(bd)) == "mainpart")
      if (length(brcol.mp) != 1) {
        stop("Weights requested, but column 'MAINPART' not found in 'bd'.")
      }
    }
  }
  
  # warn if argument specification indicates user confusion
  if (is.null(bd) && get.weights) {
    warning("Bifurcation weights requested with 'get.weights' but 'bd' not specified. Ignoring request.")
    get.weights <- FALSE
  }
  
  # check if subid exists in gd
  if (length(which(gd[, geocol.sbd] == subid)) != 1) {
    stop("'subid' not found in 'gd'.")
  }
  
  # internal helper function for get.weights = F, used with sapply() in while loop below, 
  # finds direct upstream subids in geodata and branchdata
  # this.sub: integer, subid
  findfun <- function(this.sub) {
    # case with branchdata
    if (!is.null(bd)) ff <- c(bd[which(bd[, brcol.br] == this.sub), brcol.sr], gd[which(gd[, geocol.md] == this.sub), geocol.sbd])
    # case without branchdata
    if (is.null(bd)) ff <- c(gd[which(gd[, geocol.md] == this.sub), geocol.sbd])
    return(ff)
  }
  
  # RENE'S OLD VERSION OF THE WEIGHT HELPER FUNCTION, _MUCH_ SLOWER BUT LEFT FOR REFERENCE ATM
  # # internal helper function for get.weights = TRUE
  # # finds direct upstream subids and weight fractions
  # # this.sub.gw: vector with two values, subid and weight fraction
  # findfun.gw <- function(this.sub.gw) {
  #   # get upstream subids plus weight fraction
  #   ff <- DirectUpstreamSubids(this.sub.gw[1], gd, bd)[[2]]
  #   if (length(nrow(ff)) > 0) {
  #     ff <- ff[, c(1, 3)]
  #     # update weight fraction with weight of downstream subid
  #     ff[, 2] <- ff[, 2] * this.sub.gw[2]
  #     return(ff)
  #   } else {
  #     return(NULL)
  #   }
  # }
  
  # internal helper function for get.weights = TRUE, builds on David's version, github issue #37
  # finds direct upstream subids and weight fractions
  # (use the old trick adding the branchpart as decimal to the SUBID)
  # this.sub: numeric, subid + flow weight fraction
  findfun.gw <- function(this.sub) {
    # get current weight, saved as decimal fraction in current subid
    if (this.sub - floor(this.sub) > 0) {
      # weight > 0, thus we are in a branch, remove decimal fraction from subid
      cw <- this.sub %% 1
      this.sub <- floor(this.sub)
    } else {
      #weight==0, we are on the main river
      cw <- 0
    }
    #print(c(this.sub,cw))
    
    # find upstream subid(s) in geodata and add fraction of current subid
    us.gd <- gd[gd[, geocol.md] == this.sub, geocol.sbd] + cw
    
    # find upstream subid(s) and fraction in branchdata 1, include fraction of current subid: 
    # this subid as branchid, find sourceid(s)
    us.bd <- bd[bd[, brcol.br] == this.sub, brcol.sr] + (1 - bd[bd[, brcol.br] == this.sub, brcol.mp]) * ifelse(cw > 0, cw, 1)
    
    # find upstream subid(s) and fraction in branchdata 2: 
    # upstream subid(s) as sourceids (meaning they loose water to a branch), update current weight with mainpart fraction in branchdata
    us.gd.f <- floor(us.gd)
    # rows in branchdata where upstream subids loose water to branch
    us.bsrc <- match(us.gd.f, bd[, brcol.sr])
    # update if any exist
    if (any(!is.na(us.bsrc))) {
      # calculate new weight fraction
      te <- ifelse(us.gd %% 1 > 0, us.gd %% 1, 1) * ifelse(is.na(bd[us.bsrc, brcol.mp]), 1, bd[us.bsrc, brcol.mp])
      # update, replacing 1 with 0
      us.gd <- us.gd.f + ifelse(te == 1, 0, te)
    }
    
    # prepare result vector
    ff <- c(us.gd, us.bd)
    
    # remove any NA values (no upstream subbasins) from result vector
    ff <- ff[!is.na(ff)]
    
    return(ff)
    }
  
  
  ## conditional on get.weights: iterate though upstream chain and get subids (plus weights)
  if (!get.weights) {
    # create start conditions for while loop: find direct upstream subids of the target subid
    us <- findfun(subid)
    this.us <- us
    us.exists <- length(us) > 0
    
    # loop through upstreams of the upstreams repeatedly, until none are found anymore
    while(us.exists) {
      this.us <- unlist(sapply(this.us, findfun))
      if (length(this.us) > 0) {
        us.exists <- TRUE
      } else {
        us.exists <- FALSE
      }
      # append newly found upstreams to existing
      # ff duplicates found, keep the newest one, which is the most upstream (this only happens with branches)
      us <- unique(c(us, this.us), fromLast = TRUE)
    }
    
    # add outlet SUBID to result vector
    us <- c(subid, us)
    
  } else if (get.weights) {
    # create start conditions for while loop: find direct upstream subids of the target subid
    # us <- DirectUpstreamSubids(subid, gd = gd, bd = bd)[[2]][, c(1, 3)]
    # this.us <- us
    # us.exists <- length(nrow(us)) > 0
    us <- findfun.gw(subid)
    this.us <- us
    us.exists <- length(us) > 0
    
    while(us.exists) {
      this.us <- unlist(sapply(this.us, findfun.gw))
      if (length(this.us) > 0) {
        us.exists <- TRUE
      } else {
        us.exists <- FALSE
      }
      us <- c(us, this.us)
    }
    
    # RENE'S OLD VERSION, _MUCH_ SLOWER BUT LEFT FOR REFERENCE ATM
    # # loop through upstreams of the upstreams repeatedly, until none are found anymore
    # system.time(while(us.exists) {
    #   this.us <- tryCatch(do.call(rbind.data.frame, apply(this.us, 1, findfun.gw)), error = function(e) NULL)
    #   if (length(nrow(this.us)) > 0) {
    #     us.exists <- TRUE
    #     row.names(this.us) <- 1:nrow(this.us)
    #   } else {
    #     us.exists <- FALSE
    #   }
    #   us <- rbind(us, this.us)
    # })
    
    # add outlet SUBID to result vector
    us <- c(subid, us)
    
    # convert to dataframe of subids and weights
    us <- data.frame(subid = floor(us), weight = ifelse(us %% 1 > 0, us %% 1, 1))
    
    # merge (sum) duplicates, keeping us's order
    te <- tapply(us$weight, us$subid, sum)
    te <- data.frame(subid = as.integer(names(te)), weight = as.numeric(te))
    us <- te[match(x = unique(us$subid), table = te$subid), ]
  }
  
  
  # condional: order in downstream sequence, for direct use as pmsf file
  if (sort && !get.weights) {
    us <- gd[, geocol.sbd][sort(match(us, gd[, geocol.sbd]))]
  }
  
  if (sort && get.weights) {
    us <- data.frame(us, downstreamrank = rank(match(us[, 1], gd[, geocol.sbd])))
    us <- us[order(us$downstreamrank), -3]
  }
  
  # try to write arcgis select string to clipboard, with error recovery
  if (write.arcgis == TRUE && !get.weights) {
    to.arc <- paste0("\"SUBID\" IN (", paste(us, collapse = ","), ")")
    if (clipr_available() == TRUE) {
      write_clip(to.arc)
    } else {
      message("Writing to clipboard failed. Try installing command line tool 'xclip' if you run Linux.")
    }
  }
  
  if (write.arcgis == TRUE && get.weights) {
    to.arc <- paste0("\"SUBID\" IN (", paste(us[, 1], collapse = ","), ")")
    if (clipr_available() == TRUE) {
      write_clip(to.arc)
    } else {
      message("Writing to clipboard failed. Try installing command line tool 'xclip' if you run Linux.")
    }
  }
  
  return(us)
}

