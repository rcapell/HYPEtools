
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   S3 class HypeGeoData (simple data.frame, class added on import with ReadGeoData()), herein:
#
#     - HypeGeoData (constructor function)
#     - [.HypeGeoData (indexing method)
#     - summary method
#     - print method for summary list
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#--------------------------------------------------------------------------------------------------------------------------------------
# Constructor function

#' HypeGeoData data frames
#' 
#' Constructor function for data frames which hold HYPE GeoData tables with information on sub-basins.
#' 
#' @param x Data frame with at least five mandatory columns, see details.
#' 
#' @details 
#' S3 constructor function for data frames which hold HYPE GeoData tables. These are normal data frames with at least five mandatory 
#' columns, all \code{numeric}: \emph{AREA}, \emph{SUBID}, \emph{MAINDOWN}, \emph{RIVLEN}, and \emph{SLC_n}, where \emph{n} are 
#' consecutive SLC class numbers (up to 999).See also the 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE file description} 
#'  for GeoData.txt files for reference. 
#' 
#' Usually, this class will be assigned to GeoData tables on import with \code{\link{ReadGeoData}}. A \code{summary} method exists for 
#' \code{HypeGeoData} data frames.
#' 
#' @return 
#' Returns a data frame with added \code{\link{class}} attribute \code{HypeGeoData}.
#' 
#' @seealso 
#' \code{\link{ReadGeoData}}
#' 
#' @examples
#' \dontrun{HypeGeoData(x = )}
#' @export

HypeGeoData <- function(x) {
  
  ## check for mandatory column names and corresponding column data types
  
  # mandatory columns except SLCs and their positions
  m <- c("AREA", "SUBID", "MAINDOWN", "RIVLEN")
  pos.m <- match(m, names(x))
  
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(x), 1, 4) == "SLC_")
  # extract numbers
  if (length(pos.s) > 0) {
    suppressWarnings(n.s <- as.numeric(substr(names(x)[pos.s], 5, 99)))
    # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
    pos.s <- pos.s[!is.na(n.s)]
    n.s <- n.s[!is.na(n.s)]
    # sort so that consecutiveness can be tested below
    n.s <- sort(n.s)
    # SLC number increase, tested below, 0 padded to check SLC_1 existence
    dn.s <- diff(c(0, n.s))
  }
    
  if (any(is.na(pos.m))) {
    # stop if mandatory columns are missing
    stop(paste0("Mandatory column(s) '", paste(m[is.na(pos.m)], collapse = "', '"), "' missing."))
    
  } else if (any(!apply(x[, pos.m], 2, is.numeric))) {
    # stop if data type is wrong
    stop(paste0("Mandatory column(s) '", paste(m[!apply(x[, pos.m], 2, is.numeric)], collapse = "', '"), "' non-numeric."))
    
  }  else if (length(pos.s) == 0) {
    # stop if there are no SLC columns
    stop("Mandatory 'SLC_n' column(s) missing.")
    
  }  else if (any(!apply(x[, pos.s], 2, is.numeric))) {
    # stop if data type in SLC class columns is wrong
    stop(paste0("SLC column(s) '", paste(names(x)[pos.s[!apply(x[, pos.s], 2, is.numeric)]], collapse = "', '"), "' non-numeric."))
    
  }  else if (any(dn.s > 1) || any(dn.s == 0)) {
    # warn if there are SLC classes missing or duplicated
    if (any(dn.s > 1)) {
      te1 <- n.s[dn.s > 1]
      te2 <- sapply(dn.s[dn.s > 1] - 1, function(x) 1:x)
      slc.miss <- sort(unlist(sapply(1:length(te1), function(x, y, z) y[x] - z[[x]], y = te1, z = te2)))
      warning(paste0("SLC class column(s) missing in 'x': ", paste0("SLC_", slc.miss, collapse = ", ")))
    }
    if (any(dn.s == 0)) {
      # warn if there are SLC class duplicates
      warning(paste0("SLC class column duplicate(s) in 'x': ", 
                     paste0("SLC_", n.s[dn.s == 0], " (", dn.s[dn.s == 0], ")", collapse = ", ")))
    }
    class(x) <- c("HypeGeoData", "data.frame")
    return(x)
    
  } else {
    # no problems encountered
    class(x) <- c("HypeGeoData", "data.frame")
    return(x)
    
  }
}

#--------------------------------------------------------------------------------------------------------------------------------------


# indexing method, drops HypeGeoData class if mandatory columns are removed
#' @export
`[.HypeGeoData` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], drop = T) {
  y <- NextMethod("[", drop)
  
  # mandatory columns except SLCs and their positions
  m <- c("AREA", "SUBID", "MAINDOWN", "RIVLEN")
  pos.m <- match(m, names(y))
  
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(y), 1, 4) == "SLC_")
  # extract numbers
  if (length(pos.s) > 0) {
    suppressWarnings(n.s <- as.numeric(substr(names(y)[pos.s], 5, 99)))
    # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
    pos.s <- pos.s[!is.na(n.s)]
    # n.s <- n.s[!is.na(n.s)]
    # # sort so that consecutiveness can be tested below
    # n.s <- sort(n.s)
    # # SLC number increase, tested below, 0 padded to check SLC_1 existence
    # dn.s <- diff(c(0, n.s))
  } # else {
  #   n.s <- integer(0)
  #   dn.s <- integer(0)
  # }
  
  if (any(is.na(pos.m))) {
    # drop class if mandatory columns are missing
    # warning(paste0("Mandatory column(s) '", paste(m[is.na(pos.m)], collapse = "', '"), "' removed. Class 'HypeGeoData' dropped."))
    class(y) <- class(y)[-1]
  }
  
  if (length(pos.s) == 0) {
    # drop class if there are no SLC columns
    # warning("Mandatory 'SLC_n' column(s) removed.")
    if (class(y)[1] == "HypeGeoData") {
      class(y) <- class(y)[-1]
    }
  }
  
  ## Section deactivated for the moment because warnings were excessive in day-to-day work. Left for reference
  # if (any(dn.s > 1) || any(dn.s == 0)) {
  #   # warn if there are SLC classes missing or duplicated
  #   if (any(dn.s > 1)) {
  #     te1 <- n.s[dn.s > 1]
  #     te2 <- sapply(dn.s[dn.s > 1] - 1, function(x) 1:x)
  #     slc.miss <- sort(unlist(sapply(1:length(te1), function(x, y, z) y[x] - z[[x]], y = te1, z = te2)))
  #     warning(paste0("SLC class column(s) missing in 'x': ", paste0("SLC_", slc.miss, collapse = ", ")))
  #   }
  #   if (any(dn.s == 0)) {
  #     # warn if there are SLC class duplicates
  #     warning(paste0("SLC class column duplicate(s) in 'x': ", 
  #                    paste0("SLC_", n.s[dn.s == 0], " (", dn.s[dn.s == 0], ")", collapse = ", ")))
  #   }
  # }
  
  return(y)
}




#--------------------------------------------------------------------------------------------------------------------------------------

# summary method
#' @export
summary.HypeGeoData <- function(x, ...) {
  
  # initialise result list
  res <- list()
  
  # number of subbasins
  res$n.subid <- nrow(x)
  
  ## number of SLC, SCR, and DHSLC classes
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(x), 1, 4) == "SLC_")
  # extract numbers
  suppressWarnings(n.s <- as.numeric(substr(names(x)[pos.s], 5, 99)))
  # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
  pos.s <- pos.s[!is.na(n.s)]
  n.s <- n.s[!is.na(n.s)]
  # number
  res$n.slc <- length(n.s)
  # SCR positions and their SCR numbers
  pos.r <- which(substr(names(x), 1, 4) == "SCR_")
  # extract numbers
  suppressWarnings(n.r <- as.numeric(substr(names(x)[pos.r], 5, 99)))
  # remove comment columns which happen to look like SCR columns, e.g. "SCR_98old"
  pos.r <- pos.r[!is.na(n.r)]
  n.r <- n.r[!is.na(n.r)]
  # number
  res$n.scr <- length(n.r)
  # DHSLC positions and their DHSLC numbers
  pos.d <- which(substr(names(x), 1, 6) == "DHSLC_")
  # extract numbers
  suppressWarnings(n.d <- as.numeric(substr(names(x)[pos.d], 7, 99)))
  # remove comment columns which happen to look like SCR columns, e.g. "SCR_98old"
  pos.d <- pos.d[!is.na(n.d)]
  n.d <- n.d[!is.na(n.d)]
  # number
  res$n.dhslc <- length(n.d)
  
  # subbasin area range in km2
  res$subbasin.area <- signif(c(mean(x$AREA), median(x$AREA), min(x$AREA), max(x$AREA)) * 10^-6, digits = 3)
  
  # unit river length range in km/km2
  rlu <- tryCatch(x$RIVLEN / x$AREA * 10^3, error = function(e) numeric(0))
  res$unit.river.length <- suppressWarnings(tryCatch(signif(c(mean(rlu), median(rlu), min(rlu), max(rlu)), digits = 3), error = function (e) return(NULL)))
  
  # internal lake catchment fraction
  if (exists("x$ICATCH")) {
    res$icatch <- suppressWarnings(tryCatch(signif(c(mean(x$ICATCH), median(x$ICATCH), min(x$ICATCH), max(x$ICATCH)), digits = 3), error = function (e) return(rep("-", 4))))
  } else {
    res$icatch <- rep("-", 4)
  }
  
  
  # outlet id(s)
  res$outlet.ids <- OutletIds(x)
  
  # column names and classes except SLC and SCR classes
  # get classes of all columns except SLC, SCR, and DHSLC
  col.cls <- sapply(x[, -c(pos.s, pos.r, pos.d)], class)
  # create dataframe with column numbers, names and classes
  res$columns <- data.frame(column = as.character((1:ncol(x))[-c(pos.s, pos.r, pos.d)]), name = names(col.cls), class = as.character(col.cls))
  
  class(res) <- "summaryHypeGeoData"
  print(res)
  invisible(res)
}



#--------------------------------------------------------------------------------------------------------------------------------------

# print method for summary list object
#' @export
print.summaryHypeGeoData <- function(x, ...) {
  
  ## columns in table
  cat("\nColumns, except 'SLC_n', 'SCR_n', and 'DHSLC_n':\n\n")
  # format columns dataframe so that it plots nicely on-screen (in three columns)
  te <- ceiling(nrow(x$columns)/3)
  tep <- te - (nrow(x$columns) - 2 * te)
  print(cbind(x$columns[1:te, ], "   |" = rep("|", te), 
        x$columns[(te + 1):(2 * te), ], "   |" = rep("|", te), 
        rbind(x$columns[(2 * te + 1):nrow(x$columns), ], data.frame(column = rep("", tep), name = rep("", tep), class = rep("", tep)))
        ), row.names = F)
  
  ## range table
  # subbasin area range
  tes <- c("Sub-basin area (km2):", format(x$subbasin.area, scientific = F, trim = T, drop0trailing = T))
  # unit river length range
  ter <- c("Unit river length (km/km2):", format(x$unit.river.length, scientific = F, trim = T, drop0trailing = T))
  # icatch range
  tei <- c("ilake drainage area fraction (-):", format(x$icatch, scientific = F, trim = T, drop0trailing = T))
  tesri <- as.data.frame(matrix(c(tes, ter, tei), ncol = 5, byrow = T))
  names(tesri) <- c(" ", "mean", "median", "minimum", "maximum")
  cat("\n")
  print(tesri, row.names = F)
  
  
  ## number of subbasins
  cat(paste("\n             Number of sub-basins:", x$n.subid), "\n")
  
  # outlet id(s)
  teo <- as.character(x$outlet.ids[1:min(c(length(x$outlet.ids), 10))])
  cat(paste("   ID(s) of outlets in 'maindown':", paste(ifelse(length(teo) == 1, teo, c(teo, "...")), collapse = " "), "\n"))
  
  # number of SLC classes
  cat(paste("            Number of SLC classes:", x$n.slc, "\n"))
  
  # number of SCR classes
  cat(paste("            Number of SCR classes:", x$n.scr, "\n"))
  
  # number of DHSLC classes
  cat(paste("          Number of DHSLC classes:", x$n.dhslc, "\n"))
  
}



#--------------------------------------------------------------------------------------------------------------------------------------

# merge method
#' @export
merge.HypeGeoData <- function(x, ...) {
  y <- NextMethod("merge")
  y <- HypeGeoData(y)
  return(y)
}
