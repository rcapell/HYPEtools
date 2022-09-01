
#' Sort a GeoData dataframe in downstream order
#'
#' Function to sort an imported GeoData.txt file in downstream order, so that all upstream sub-basins are listed in rows above downstream sub-basins.
#' 
#' @param gd A data frame containing a column with SUBIDs and a column with areas, e.g. an imported 'GeoData.txt' file.
#' @param bd A data frame with bifurcation connections, e.g. an imported 'BranchData.txt' file. Optional argument.
#' @param progbar Logical, display a progress bar while calculating SUBID sorting. 
#' 
#' @details
#' GeoData.txt files need to be sorted in downstream order for HYPE to run without errors. \code{SortGeoData} considers bifurcation connections, but not 
#' irrigation or groundwater flow links.
#' 
#' @return
#' \code{SortGeoData} returns a GeoData dataframe.
#' 
#' @seealso
#' \code{\link{AllUpstreamSubids}}
#' \code{\link{OutletSubids}}
#' 
#' @examples
#' te <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' SortGeoData(gd = te)
#' 
#' @importFrom pbapply pblapply
#' @export


SortGeoData <- function(gd, bd = NULL, progbar = TRUE) {
  
  # input data checks
  if (!is.null(bd)) {
    brcol.br <- which(tolower(colnames(bd)) == "branchid")
    brcol.sr <- which(tolower(colnames(bd)) == "sourceid")
    # check existence
    if (length(brcol.br) != 1 || length(brcol.sr) != 1) {
      stop("BRANCHID and/or SOURCEID column not found in 'bd'.")
    }
  }
  
  # column position of subids in gd
  geocol.md <- which(tolower(colnames(gd)) == "maindown")
  geocol.sbd <- which(tolower(colnames(gd)) == "subid")
  # check existence
  if (length(geocol.md) != 1 || length(geocol.sbd) != 1) {
    stop("SUBID and/or MAINDOWN column not found in 'gd'.")
  }
  
  # internal copies of relevant gd and bd columns
  igd <- gd[, c(geocol.sbd, geocol.md)]
  if (!is.null(bd)) {
    ibd <- bd[, c(brcol.sr, brcol.br)]
  }
  
  
  ## create dummy outlets for independent basins connected through BranchData
  ## this is needed to avoid upstream duplicates from subid sorting below
  
  if (!is.null(bd)) {
    
    ## find outlet subids for all subids in bd
    
    # initialise dataframe
    bd.outlets <- data.frame(osbd.main = rep(NA, nrow(ibd)), osbd.branch = rep(NA, nrow(ibd)))
    # get outlets for main stream in branchings
    bd.outlets$osbd.main <- sapply(ibd$SOURCEID, function(x, y) {rev(AllDownstreamSubids(subid = x, gd = y))[1]}, y = igd)
    # get outlets for branches, conditional on that they exist in gd (they can go out of the domain)
    bd.outlets$osbd.branch <- sapply(ibd$BRANCHID, function(x, y) {ifelse(x %in% y[, 1], rev(AllDownstreamSubids(subid = x, gd = y))[1], 0)}, y = igd)
    
    # identify and remove rows where maindown and branch outlets are the same (which means that the branch is a "shortcut" within a basin)
    bd.outlets <- bd.outlets[ifelse(bd.outlets$osbd.main == bd.outlets$osbd.branch, FALSE, TRUE), ]
    
    # identify and remove rows with branches to outside domain (e.g. karstic sinks)
    bd.outlets <- bd.outlets[which(bd.outlets$osbd.branch != 0), ]
    
    # identify and remove row duplicates (in case of several branch connections between different basins)
    bd.outlets <- bd.outlets[!duplicated(bd.outlets), ]
    
    # Find unique inter-basin connections, conditional on any cases being left (meaning there are inter-basin connections)
    if (nrow(bd.outlets) > 0) {
      
      
      # convert outlet data frame to list, which is then iteratively regrouped below if necessary
      le <- split(bd.outlets, 1:nrow(bd.outlets))
      
      # conditional on existence of duplicated outlet SUBIDs in branch-connected basins:
      # find and group all branch-connected outlets (this can theoretically be several in a chain)
      ve <- unlist(bd.outlets)
      if (length(ve[duplicated(ve)]) > 0) {
        ve <- unique(ve[duplicated(ve)])
        
        # loop through duplicated SUBIDs and regroup list
        for (i in 1:length(ve)) {
          # find which vectors to merge
          grp <- sapply(le, function(x, y) {any(x == y)}, y = ve[i])
          # merge
          mrg <- unique(unlist(le[grp]))
          # remove merged vectors from list
          le <- le[!grp]
          # add merge result to list
          le[[length(le) + 1]] <- mrg
        }
      }
      
      # dummy outlet subids, larger than max allowed subid number of digits (HYPE restriction)
      dsbd <- 1:length(le) + 10^8
      
      # add dummy outlets to internal gd
      for (i in 1:length(le)) {
        # add dummy subbasin as maindown to connected basin outlets
        igd[igd[, 1] %in% le[[i]], 2] <- dsbd[i]
        # add dummy subbasin as new basin outlet
        igd <- rbind(igd, c(dsbd[i], 0))
      }
    }
  }
  
  
  # vector of outlet subids in gd: for these, upstream chains will be calculated below
  osbd <- OutletSubids(gd = igd)
  
  
  # calculate subid sorting using function AllUpstreamSubids()
  if (progbar) {
    cat("Calculating SUBID order.\n")
    ssbd <- unlist(pblapply(osbd, function(x, y, z) {rev(AllUpstreamSubids(subid = x, gd = y, bd = z))}, y = igd, z = bd))
    # ssbd <- pblapply(osbd, function(x, y, z) {rev(AllUpstreamSubids(subid = x, gd = y, bd = z))}, y = igd, z = bd)
  } else {
    ssbd <- unlist(lapply(osbd, function(x, gd, bd) {rev(AllUpstreamSubids(subid = x, gd, bd))}, gd = igd, bd = bd))
  }
  
  # remove dummy subids from result
  ssbd <- ssbd[ssbd < 10^8]
  
  # sort gd
  gd <- gd[match(ssbd, gd[, geocol.sbd]), ]
  
  return(gd)
  
}
