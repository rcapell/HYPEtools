#' Clean Soil-Landuse classes (SLCs) from small fractions
#'
#' \code{CleanSLCClasses} attempts to clean small SLC fractions within each SUBID (sub-catchment) from an imported GeoData file using user-provided
#' area thresholds. Cleaning can be performed along class similarity rules or along SLC area alone. 
#' 
#' @param gd Data frame containing columns with SUBIDs, SUBID areas in m^2, and SLC fractions, typically a 'GeoData.txt' file 
#' imported with \code{\link{ReadGeoData}}.
#' @param gcl Data frame containing columns with SLCs and corresponding land use and soil class IDs, typically a 'GeoClass.txt' 
#' file imported with \code{\link{ReadGeoClass}}.
#' @param m1.file Character string, path and file name of the soil or land use class transfer table, a tab-separated text file. Format see details. 
#' A value of \code{NULL} (default) prevents method 1 cleaning.
#' @param m1.class Character string, either "soil" or "landuse", can be abbreviated. Gives the type of transfer class table for method 1 cleaning. 
#' See Details.
#' @param m1.clean A logical vector of length 2 which indicates if cleaning should be performed for area fraction thresholds (position 1) and/or 
#' absolute area thresholds (position 2).
#' @param m1.precedence A logical vector of length 2 which indicates if areas below cleaning threshold should be moved to similar areas according to 
#' precedence in the transfer table given in \code{m1.file} (\code{TRUE}) or to the largest area of available transfer classes (\code{FALSE}). Area 
#' fraction thresholds (position 1) and absolute area thresholds (position 2).
#' @param m2.frac Numeric, area fraction threshold for method 2 cleaning, i.e. moving of small SLC areas to largest SLC in each SUBID without considering
#' similarity between classes. Either a single value or a vector of the same length as the number of SLC classes in \code{gd}, giving area fraction 
#' thresholds for each SLC separately, with a value \code{0} for SLCs to omit from cleaning. A value of \code{NULL} (default) prevents method 2 area 
#' fraction cleaning.
#' @param m2.abs Numeric, see \code{m2.frac}. Threshold(s) for absolute areas in \eqn{m^{2}}{m^2}.
#' @param signif.digits Integer, number of significant digits to round cleaned SLCs to. See also \code{\link{signif}}. Set to \code{NULL} to prevent rounding. 
#' @param verbose Logical, print some information during runtime.
#' @param progbar Logical, display a progress bar while calculating SLC class fractions. Adds overhead to calculation time but useful when \code{subid} 
#' is \code{NULL} or contains many SUBIDs.
#' 
#' @details
#' \code{CleanSLCClasses} performs a clean-up of small SLC fractions in an imported GeoData file. Small SLCs are eliminated either by moving their 
#' area to similar classes according to rules which are passed to the function in a text file (\emph{Method 1}), or by simply moving their area to the 
#' largest SLC in the SUBID (\emph{Method 2}). Moving rules for the first method can be based on either soil classes or land use classes but these cannot 
#' be combined in one function call. Run the function two times to combine soil and land use based clean-up. Method 1 and 2, however, can be combined 
#' in one function call, in which case the rule-based classification will be executed first. Clean-up precedence in method 1: if 
#' clean-ups based on area fractions and absolute areas are combined (\code{m1.clean = rep(TRUE, 2)}), then area fractions will be cleaned first. In 
#' order to reverse precedence, call \code{CleanSLCClasses} two times with absolute area cleaning activated in first call and area fraction cleaning 
#' in second. In both methods, SLCs in each SUBID are cleaned iteratively in numerical order, starting with SLC_1. This implies a greater likelihood of 
#' eliminating SLCs with smaller indices.
#' 
#' \bold{Method 1}
#' 
#' For method one, small SLC fractions are moved to either similar land use classes within the same soil class, or vice versa. Similarities are 
#' defined by the user in a tab-separated text file, which is read by \code{CleanSLCClasses} during runtime. Soil and land use classes correspond to
#' the classes given in column two and three in the \code{GeoClass} file. The file must have the following format:
#' 
#' \tabular{cccccc}{
#' \emph{class.1}\tab\emph{thres.frac.1}\tab\emph{thres.abs.1}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.n}\cr
#' \emph{class.2}\tab\emph{thres.frac.2}\tab\emph{thres.abs.2}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.o}\cr
#' \emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\cr
#' \emph{class.m}\tab\emph{thres.frac.m}\tab\emph{thres.abs.m}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.p}\cr
#' }
#' 
#' Column 1 contains the source land use or soil classes subjected to clean-up, columns 2 and 3 contain threshold values for area fractions and 
#' absolute areas. The remaining columns contain classes to which areas below threshold will be transferred, in order of precedence. Each class can 
#' have one or several transfer classes. \code{CleanSLCClasses} will derive SLC classes to clean from the given soil or land use class using the 
#' GeoClass table given in argument \code{gcl}.
#' No header is allowed. At least one transfer class must exist, but classes can be omitted and will then be ignored by \code{CleanSLCClasses}. 
#' The order of transfer classes in the transfer file indicates transfer preference. \code{CleanSLCClasses} constructs a transfer list for each SLC 
#' class in the model set-up and per default uses the order to choose a preferred SLC to transfer to. However, if several SLCs exist for a given soil 
#' or land use class, one of them will be chosen without further sorting. If argument \code{m1.precedence} is set to \code{FALSE} for either area 
#' fractions or absolute areas, precedence will be ignored and the largest area available will be chosen to transfer small areas to. Area fraction 
#' thresholds are given as fractions of 1, absolute area thresholds as values in \eqn{m^{2}}{m^2}. If an area below threshold is identified but there 
#' are no fitting SLCs available to transfer to, the area will remain unchanged.
#' 
#' \bold{Method 2}
#' 
#' This method is more rigid than method one and can also be applied as a post-processor after clean-up using method 1 to force a removal of all SLCs 
#' below a given threshold from a GeoData file (method 1 cleaning can be be very selective, depending on how many transfer classes are provided in 
#' the transfer table). Cleaning thresholds for method 2 area fractions and absolute areas are given in arguments \code{m2.frac} and \code{m2.abs}. 
#' SLC areas below the given thresholds will be moved to the largest SLC in the given SUBID without considering any similarity between classes. 
#' 
#' @return
#' \code{CleanSLCClasses} returns the GeoData data frame passed to the function in argument \code{gd} with cleaned SLC class columns. 
#' 
#' @seealso 
#' \code{\link{RescaleSLCClasses}} for re-scaling of SLC area fraction sums.
#' 
#' @examples
#' # Import source data
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te2 <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' # Clean-up using method 2, 0.5 % area fraction threshold and 100 m^2 absolute area threshold
#' te3 <- CleanSLCClasses(gd = te1, gcl = te2, m2.frac = 0.005, m2.abs = 100)
#' # Detailed comparison with function CompareFiles
#' te4 <- CompareFiles(te1, te3, type = "GeoData")
#' te4
#' 
#' @importFrom pbapply pbapply
#' @importFrom utils read.table
#' @importFrom stats na.omit
#' @export

CleanSLCClasses <- function (gd, gcl, m1.file = NULL, m1.class = "s", m1.clean = rep(TRUE, 2), m1.precedence = rep(TRUE, 2), 
                            m2.frac = NULL, m2.abs = NULL, signif.digits = 3, verbose = TRUE, progbar = TRUE) {
  
  
  
  ## Conditional checks: is there anything to do for the function at all?
  if (is.null(m1.file) && is.null(m2.frac) && is.null(m2.abs) && is.null(signif.digits)) {
    stop("No arguments for calculations provided. Exiting.")
  }
  if (!is.null(m1.file) && m1.clean[1] == FALSE && m1.clean[2] == FALSE) {
    stop("Class transfer file provided for method 1, but no cleaning requested in 'm1.clean'")
  }
  
  # record start time, to print calculation time later on
  starttime <- Sys.time()
  
  ## Common data preparation operations
  
  # columns with SLCs in GeoData
  gdcols.slc <- which(toupper(substr(names(gd), 1, 3)) == "SLC")
  
  # extract slc classes as working data frame
  slc <- gd[, gdcols.slc]
  # number of slc classes in GeoData
  nslc <- ncol(slc)
  
  # error check: number of SLCs in gcl and gd must be identical
  if (nslc != nrow(gcl)) {
    stop("Number of SLCs in GeoData and Geoclass dataframes do not match.")
  }
  
  # print to screen if verbose
  if (verbose) {
    cat(paste("\nNumber of SLC classes in GeoData and GeoClass:", nslc, "\n"))
  }
  
  # for function efficiency statistic: total number of SLC instances in GeoData before cleaning
  n.before <- length(c(as.matrix(slc))[c(as.matrix(slc)) != 0])
  
  # extract area from gd, force conversion to numeric to avoid integer overflow errors
  area <- as.numeric(gd[, which(toupper(names(gd)) == "AREA")])
  
  
  ############################
  ## Conditional: Sophisticated cleaning with class transfer file if it exists
  if (!is.null(m1.file)) {
    
    # import transfer tables as data frame 
    transfer.classes <- tryCatch(read.table(m1.file, as.is = TRUE, header = FALSE, colClasses = rep("numeric", 100), fill = TRUE, col.names = paste0("V", 1:100)), 
                                 error = function(e) {print("Class transfer table import failed.")})
    transfer.classes <- transfer.classes[, !apply(transfer.classes, 2, function(x) all(is.na(x)))]
    
    # check if there are any transfer classes at all
    if (ncol(transfer.classes) < 4) {
      stop("No transfer classes given in transfer file. Exiting.")
    }
    
    # conditional check: file class type valid?
    if (!m1.class == "s" && !m1.class == "soil" && !m1.class == "l" && !m1.class == "landuse") {
      stop("'m1.class' argument invalid.")
    }
    
    # import error checks: no NAs allowed in threshold columns if they are used for cleaning
    if (m1.clean[1] && is.na(mean(transfer.classes[, 2]))) {
      stop("Class transfer table contains empty value(s) in area fraction threshold column.")
    }
    if (m1.clean[2] && is.na(mean(transfer.classes[, 3]))) {
      stop("Class transfer table contains empty value(s) in absolute area threshold column.")
    }
    
    # Table with indices over thresholds for cleanable slc classes
    # Conditional: depending on land use or soil transfer classification
    if (m1.class == "s" || m1.class == "soil") {
      ind.thr <- merge(gcl[, c(1:3)], transfer.classes[, 1:3], by.x = 3, by.y = 1, all.x = FALSE, all.y = FALSE, sort = FALSE)
      # sort on SLC column
      ind.thr <- ind.thr[order(ind.thr[,2]), ]
    }
    if (m1.class == "l" || m1.class == "landuse") {
      ind.thr <- merge(gcl[, c(1:3)], transfer.classes[, 1:3], by.x = 2, by.y = 1, all.x = FALSE, all.y = FALSE, sort = FALSE)
      # sort on SLC column
      ind.thr <- ind.thr[order(ind.thr[,2]), ]
    }
    
    # GeoClass columns 1 to 3, sorting conditional on classification
    if (m1.class == "s" || m1.class == "soil") {
      ind.gcl <- gcl[, c(3, 1, 2)]
    }
    if (m1.class == "l" || m1.class == "landuse") {
      ind.gcl <- gcl[, c(2, 1, 3)]
    }
    
    
    # create list with one element for each SLC which exists in transfer table
    # each element is a data frame with soil or land use types and SLCs in order of transfer preference
    list.transfer <- list()
    for (i in 1:nrow(ind.thr)) {
      # get current SLC and fix class from threshold index (not all SLCs might be in there if transfer rules only exist for a subset)
      # fix class is the soil/landuse class ID within which the current transfer is performed (e.g. a land use class if m1.class = "s")
      slc.cur <- ind.thr[i, 2]
      ## current transfer soil or land use classes, including the currently evaluated class which can reoccur in case of lakes and SLCs with crops,
      ## but omitting the current SLC class (it would not make sense to reassign the area to itself)
      # get current class and transfer class(es)
      trans.cur <- as.numeric(na.omit(as.numeric(transfer.classes[which(transfer.classes[, 1] == ind.thr[i, 1]), -c(2:3)])))
      # element in ind.gcl which is the current slc class
      gcslc.cur <- which(ind.gcl[, 2] == slc.cur)
      # get current transfer SLCs (soils within same land use class or vice versa, but not the current SLC itself)
      trslc.cur <- ind.gcl[-gcslc.cur, ][which(ind.gcl[-gcslc.cur, 3] == ind.gcl[gcslc.cur, 3]), 1:2]
      # conditional: if transfer slcs were found, merge them with actual user-defined transfer classes 
      if (nrow(trslc.cur) > 0) {
        
        # clean transfer SLCs from soil or land use classes which are not in current transfer class vector
        trslc.cur <- merge(trslc.cur, data.frame(trans.cur), by.x = 1, by.y = 1)
        # order transfer SLCs in order of transfer class vector (multiple occurrences unsorted)
        trslc.cur <- trslc.cur[order(trans.cur[trslc.cur[, 1]]), ]
      }
      
      list.transfer[[i]] <- trslc.cur
      names(list.transfer)[i] <- slc.cur
    }
    # remove zero-length data frames from list, where no transfer classes are available
    list.transfer <- list.transfer[sapply(list.transfer, nrow) > 0]
    
    # feedback during runtime
    if (verbose) {
      cat(paste("\nSLCs considered for cleaning:", paste(names(list.transfer), collapse = " "), "\n"))
    }
    
    
    # Conditional: cleaning for area fractions
    if (m1.clean[1] == TRUE) {
      
      # feedback during runtime
      if (verbose) {
        cat("\nStarting clean-up using method 1 with area fractions.\n")
      }
      

      # internal function to be applied row-wise on slc data frame for cleaning based on area fractions
      # iterates through SLCs in one SUBID (row in slc data frame) and transfers areas
      # x   :  row in slc
      # lt  :  transfer list as created above
      # thr :  threshold table, ind.thr above 
      # prec:  logical, should cleaned areas be added according to precedence (T) or according to largest area (F)
      IntTransferArea <- function(x, lt, thr, prec) {
        # SLCs with transfer rules
        slc.ids <- as.numeric(names(lt))
        # select rows with SLCs with transfer rules from threshold table
        thres <- thr[match(slc.ids, thr[,2]), ]
        # iterate over SLCs with rules
        for (l in 1:length(lt)) {
          # current SLC
          j <- slc.ids[l]
          # condition: current slc class below cleaning threshold for area fractions
          if (x[j] < thres[l, 4] && x[j] > 0) {
            # identify transfer SLCs which exist (do have an area > 0) in current SUBID. 
            slc.trans <- lt[[l]][which(x[lt[[l]][, 2]] > 0), 2]
            # condition: at least one transfer SLC exists
            if (length(slc.trans) > 0) {
              
              # condition: transfer to first SLC in vector, this is the preferred one according to the precedence order in the transfer table
              if (prec) {
                x[slc.trans[1]] <- x[slc.trans[1]] + x[j]
                x[j] <- 0
              }
              # condition: transfer to SLC with largest area fraction of the available transfer SLCs
              if (!prec) {
                x[slc.trans[which.max(x[slc.trans])]] <- x[slc.trans[which.max(x[slc.trans])]] + x[j]
                x[j] <- 0
              }
              
              # print(paste("SLC", j, "cleaned."))
            }
          }
        }
        return(x)
      }
      
      if (verbose) {
        cat("\nClean-up using method 1 with area fractions.\n")
      }
      
      # apply row-wise to slc data frame, THIS LINE IS A MAIN COMPUTATION
      if (progbar) {
        slc <- as.data.frame(t(pbapply(slc, 1, IntTransferArea, lt = list.transfer, thr = ind.thr, prec = m1.precedence[1])))
      } else {
        slc <- as.data.frame(t(apply(slc, 1, IntTransferArea, lt = list.transfer, thr = ind.thr, prec = m1.precedence[1])))
      }
      
      
      # for function efficiency statistic: total number of SLC instances in GeoData after cleaning
      n.m1.frac <- length(c(as.matrix(slc))[c(as.matrix(slc)) != 0])
    }
    
    
    # Conditional: cleaning for absolute areas
    if (m1.clean[2] == TRUE) {
      
      if (verbose) {
        cat("\nStarting clean-up using method 1 with absolute areas.\n")
      }
      
      # calculate absolute areas from fractions (potentially after clean-up with area fraction threshold)
      slc.abs <- as.data.frame(apply(slc, 2, function(x, y) {x * y}, y = area))
      
      
      # internal function to be applied row-wise on slc data frame for cleaning based on absolute areas
      # iterates through SLCs in one SUBID (row in slc data frame) and transfers areas
      # x   :  concatenated slc and slc.abs row
      # lt  :  transfer list as created above
      # thr :  threshold table, ind.thr above 
      # prec:  logical, should cleaned areas be added according to precedence (T) or according to largest area (F)
      IntTransferAreaAbs <- function(x, lt, thr, prec) {
        # SLCs with transfer rules
        slc.ids <- as.numeric(names(lt))
        # select rows with SLCs with transfer rules from threshold table
        thres <- thr[match(slc.ids, thr[,2]), ]
        # separate slc and slc.abs from input (needs to be done inside this function because I don't know how to iterate over an argument in apply())
        x.frac <- x[1:(length(x)/2)]
        x.abs <- x[((length(x)/2) + 1):length(x)]
        # iterate over SLCs with rules
        for (l in 1:length(lt)) {
          # current SLC
          j <- slc.ids[l]
          # condition: current slc class below cleaning threshold for absolute areas
          if (x.abs[j] < thres[l, 5] && x.abs[j] > 0) {
            # identify transfer SLCs which exist (do have an area > 0) in current SUBID. 
            slc.trans <- lt[[l]][which(x.frac[lt[[l]][, 2]] > 0), 2]
            # condition: at least one transfer SLC exists
            if (length(slc.trans) > 0) {
              
              # condition: transfer to first SLC in vector, this is the preferred one according to the precedence order in the transfer table
              if (prec) {
                x.frac[slc.trans[1]] <- x.frac[slc.trans[1]] + x.frac[j]
                x.frac[j] <- 0
              }
              # condition: transfer to SLC with largest area fraction of the available transfer SLCs
              if (!prec) {
                x.frac[slc.trans[which.max(x.frac[slc.trans])]] <- x.frac[slc.trans[which.max(x.frac[slc.trans])]] + x.frac[j]
                x.frac[j] <- 0
              }
              
              #print(paste("SLC", j, "cleaned."))
            }
          }
        }
        return(x.frac)
      }
      
      if (verbose) {
        cat("\nClean-up using method 1 with absolute areas.\n")
      }
      
      # apply row-wise to combined slc/slc.abs data frame, THIS LINE IS A MAIN COMPUTATION
      if (progbar) {
        slc <- as.data.frame(t(pbapply(cbind(slc, slc.abs), 1, IntTransferAreaAbs, lt = list.transfer, thr = ind.thr, prec = m1.precedence[2])))  
      } else {
        slc <- as.data.frame(t(apply(cbind(slc, slc.abs), 1, IntTransferAreaAbs, lt = list.transfer, thr = ind.thr, prec = m1.precedence[2])))  
      }
      
      
      # for function efficiency statistic: total number of SLC instances in GeoData after cleaning
      n.m1.abs <- length(c(as.matrix(slc))[c(as.matrix(slc)) != 0])
    }
  }
  
  
  
  
  ############################
  ## Conditional: Rigid cleaning with thresholds
  if (!is.null(m2.frac) || !is.null(m2.abs)) {
    
    # Conditional: cleaning for area fraction threshold
    if (!is.null(m2.frac)) {
      
      if (verbose) {
        cat("\nStarting clean-up using method 2 with area fractions.\n")
      }
      
      # error check: fraction(s) must be between 0 and 1 and must not contain NAs
      if (any(m2.frac >= 1, na.rm = TRUE) || any(m2.frac < 0, na.rm = TRUE) || length(m2.frac) != length(na.omit(m2.frac))) {
        stop("Area fraction threshold(s) given in 'm2.frac' must be within 0 >= x < 1 and must not contain NAs.")
      }
      
      # expand fraction threshold if single value is given, assign to internal variable
      if (length(m2.frac) == 1) {
        frc <- rep(m2.frac, times = nrow(gcl))
      } else {
        frc <- m2.frac
      }
      
      # error check: number of SLCs in gcl and number of change fractions must match
      if (length(frc) != nrow(gcl)) {
        stop("Number of SLCs in Geoclass and number of thresholds in 'm2.frac' do not match.")
      }
      
      
      # Internal function to move all area fraction below threshold to largest fraction
      IntTransferRigid <- function(x, fracthres) {
        # Conditional: more or less theoretical case that the larges slc is below threshold
        if (max(x) < fracthres[which.max(x)]) {
          # sum of SLCs smaller threshold will be moved to the SLC with largest fraction (and that sum includes the SLC itself in this case)
          largest <- which.max(x)
          x[largest] <- sum(x[x < fracthres & x > 0])
          # all but the largest are filled with 0
          x[-largest][x[-largest] < fracthres[-largest] & x[-largest] > 0] <- 0 
        # remaining case: the largest SLC is not below threshold, the largest SLC does not need to be tracked and omitted
        } else {
          x[which.max(x)] <- x[which.max(x)] + sum(x[x < fracthres & x > 0])
          x[x < fracthres & x > 0] <- 0 
        }
        return(x)
      }
      
      if (verbose) {
        cat("\nClean-up using method 2 with area fractions.\n")
      }
      # apply row-wise to slc data frame, THIS LINE IS A MAIN COMPUTATION
      if (progbar) {
        slc <- as.data.frame(t(pbapply(slc, 1, IntTransferRigid, fracthres = frc)))
      } else {
        slc <- as.data.frame(t(apply(slc, 1, IntTransferRigid, fracthres = frc)))
      }
      
      if (verbose) {
        cat("\nClean-up using method 2 with area fractions completed.\n")
      }
      
      # for function efficiency statistic: total number of SLC instances in GeoData after cleaning
      n.m2.frac <- length(c(as.matrix(slc))[c(as.matrix(slc)) != 0])
    }
    
    # Conditional: cleaning for absolute area threshold
    if (!is.null(m2.abs)) {
      
      if (verbose) {
        cat("\nStarting clean-up using method 2 with absolute areas.\n")
      }
      
      # error check: area must be positive and must not contain NAs
      if (any(m2.abs < 0, na.rm = TRUE) || length(m2.abs) != length(na.omit(m2.abs))) {
        stop("Absolute area threshold(s) given in 'm2.abs' must be x > 0 and must not contain NAs.")
      }
      
      # expand area threshold if single value is given, assign to internal variable
      if (length(m2.abs) == 1) {
        abso <- rep(m2.abs, times = nrow(gcl))
      } else {
        abso <- m2.abs
      }
      
      # error check: number of SLCs in gcl and number of change fractions must match
      if (length(abso) != nrow(gcl)) {
        stop("Number of SLCs in Geoclass and number of thresholds in 'm2.frac' do not match.")
      }
      
      # (re-)calculate absolute areas from fractions (potentially after clean-up with area fraction threshold)
      slc.abs <- as.data.frame(apply(slc, 2, function(x, y) {x * y}, y = area))
      
      
      # Internal function to move all area fraction below threshold to largest fraction
      IntTransferRigidAbs <- function(x, absthres) {
        
        # separate slc and slc.abs from input (needs to be done inside this function because I don't know how to iterate ofer an argument in apply())
        x.frac <- x[1:(length(x)/2)]
        x.abs <- x[((length(x)/2) + 1):length(x)]
        
        # Conditional: in the case that the largest slc is below threshold
        if (max(x.abs) < absthres[which.max(x.abs)]) {
          # sum of SLCs smaller threshold will be moved to the SLC with largest fraction (and that sum includes the SLC itself in this case)
          largest <- which.max(x.abs)
          x.frac[largest] <- sum(x.frac[x.abs < absthres & x.abs > 0])
          # all but the largest are filled with 0
          x.frac[-largest][x.abs[-largest] < absthres[-largest] & x.abs[-largest] > 0] <- 0 
          # remaining case: the largest SLC is not below threshold, the largest SLC does not need to be tracked and omitted
        } else {
          x.frac[which.max(x.abs)] <- x.frac[which.max(x.abs)] + sum(x.frac[x.abs < absthres & x.abs > 0])
          x.frac[x.abs < absthres & x.abs > 0] <- 0 
        }
        return(x.frac)
      }
      
      if (verbose) {
        cat("\nClean-up using method 2 with absolute areas.\n")
      }
      
      # apply row-wise to slc data frame, THIS LINE IS A MAIN COMPUTATION
      if (progbar) {
        slc <- as.data.frame(t(pbapply(cbind(slc, slc.abs), 1, IntTransferRigidAbs, absthres = abso)))
      } else {
        slc <- as.data.frame(t(apply(cbind(slc, slc.abs), 1, IntTransferRigidAbs, absthres = abso)))
      }
      
      # for function efficiency statistic: total number of SLC instances in GeoData after cleaning
      n.m2.abs <- length(c(as.matrix(slc))[c(as.matrix(slc)) != 0])
    }
  }
  
  # round to requested number of digits
  if (verbose && !is.null(signif.digits)) {
    cat(paste("\nRounding to", signif.digits, "significant digits."))
  }
  
  if (!is.null(signif.digits)) {
    slc <- apply(slc, 2, signif, digits = signif.digits)
  }
  
  # update GeoData data frame
  gd[, gdcols.slc] <- slc
  
  if (verbose) {
    cat(paste("\nTotal number of SLCs before cleaning:", n.before, 
              "\nTotal number of SLCs after method 1 with area fractions:", ifelse(exists("n.m1.frac"), n.m1.frac, "<method not used>"),
              "\nTotal number of SLCs after method 1 with absolute areas:", ifelse(exists("n.m1.abs"), n.m1.abs, "<method not used>"),
              "\nTotal number of SLCs after method 2 with area fractions:", ifelse(exists("n.m2.frac"), n.m2.frac, "<method not used>"),
              "\nTotal number of SLCs after method 2 with absolute areas:", ifelse(exists("n.m2.abs"), n.m2.abs, "<method not used>"),
              "\n\n", "Total computation time (secs):", as.numeric(round(difftime(Sys.time(), starttime, units = "secs"))), 
              "\n\n")
          )
  }
  
  return(gd)
}

