
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#   S3 class HypeGeoData (simple data.frame, class added on import with ReadGeoData()), herein:
#
#     - HypeGeoData (constructor function)
#     - [.HypeGeoData (indexing method)
#     - summary method
#     - print method for summary list
#     - merge method with class preservation
#     - 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#--------------------------------------------------------------------------------------------------------------------------------------
# Constructor function
#--------------------------------------------------------------------------------------------------------------------------------------

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
#' te <- data.table::fread(file = system.file("demo_model",
#' "GeoData.txt", package = "HYPEtools"), data.table = FALSE)
#' HypeGeoData(x = te)
#' summary(te)
#' 
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
    
  }  else if (any(duplicated(x$SUBID))) {
    # stop if any SUBID is duplicated
    stop(paste0("SUBID(s) '", paste(x$SUBID[duplicated(x$SUBID)], collapse = "', '"), "' duplicated."))
    
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
# Indexing method, drops HypeGeoData class if mandatory columns are removed
#--------------------------------------------------------------------------------------------------------------------------------------

#' @export

`[.HypeGeoData` <- function(x, i = 1:dim(x)[1], j = 1:dim(x)[2], drop = TRUE) {
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
# summaryHypeGeoData constructor (internal access only), used for printing summaries
#--------------------------------------------------------------------------------------------------------------------------------------

summaryHypeGeoData <- function(x) {
  
  # expects a list with elements n.subid, pos.slc, n.slc, pos.scr, n.scr, pos.dhslc, n.dhslc, subbasin.area, unit.river.length, 
  # icatch, outlet.ids, columns
  
  class(x) <- "summaryHypeGeoData"
  x
  
}



#--------------------------------------------------------------------------------------------------------------------------------------
# Summary method
#--------------------------------------------------------------------------------------------------------------------------------------

#' @method summary HypeGeoData
#' @importFrom stats median
#' @export

summary.HypeGeoData <- function(object, ...) {
  
  # initialise result list
  res <- list()
  
  # number of subbasins
  res$n.subid <- nrow(object)
  
  ## number of SLC, SCR, and DHSLC classes
  # SLC positions and their SLC numbers
  pos.s <- which(substr(names(object), 1, 4) == "SLC_")
  # extract numbers
  suppressWarnings(n.s <- as.numeric(substr(names(object)[pos.s], 5, 99)))
  # remove comment columns which happen to look like SLC columns, e.g. "SLC_98old"
  pos.s <- pos.s[!is.na(n.s)]
  n.s <- n.s[!is.na(n.s)]
  # number of SLC classes and column positions into result list
  res$pos.slc <- pos.s
  res$n.slc <- length(n.s)
  # SCR positions and their SCR numbers
  pos.r <- which(substr(names(object), 1, 4) == "SCR_")
  # extract numbers
  suppressWarnings(n.r <- as.numeric(substr(names(object)[pos.r], 5, 99)))
  # remove comment columns which happen to look like SCR columns, e.g. "SCR_98old"
  pos.r <- pos.r[!is.na(n.r)]
  n.r <- n.r[!is.na(n.r)]
  # number of SCR columns and column positions into result list
  res$pos.scr <- pos.r
  res$n.scr <- length(n.r)
  # DHSLC positions and their DHSLC numbers
  pos.d <- which(substr(names(object), 1, 6) == "DHSLC_")
  # extract numbers
  suppressWarnings(n.d <- as.numeric(substr(names(object)[pos.d], 7, 99)))
  # remove comment columns which happen to look like SCR columns, e.g. "SCR_98old"
  pos.d <- pos.d[!is.na(n.d)]
  n.d <- n.d[!is.na(n.d)]
  # numberof DHSLC columns and column positions into result list
  res$pos.dhslc <- pos.d
  res$n.dhslc <- length(n.d)
  
  # subbasin area range in km2
  res$subbasin.area <- signif(c(mean(object$AREA), median(object$AREA), min(object$AREA), max(object$AREA)) * 10^-6, digits = 3)
  
  # unit river length range in km/km2
  rlu <- tryCatch(object$RIVLEN / object$AREA * 10^3, error = function(e) numeric(0))
  res$unit.river.length <- suppressWarnings(tryCatch(signif(c(mean(rlu), median(rlu), min(rlu), max(rlu)), digits = 3), error = function (e) return(NULL)))
  
  # internal lake catchment fraction
  if ("ICATCH" %in% toupper(names(object))) {
    res$icatch <- suppressWarnings(tryCatch(signif(c(mean(object$ICATCH), median(object$ICATCH), min(object$ICATCH), max(object$ICATCH)), digits = 3), 
                                            error = function (e) return(rep("-", 4))))
  } else {
    res$icatch <- rep("-", 4)
  }
  
  
  # outlet id(s)
  res$outlet.ids <- OutletIds(object)
  
  # column names and classes except SLC and SCR classes
  # get classes of all columns except SLC, SCR, and DHSLC
  col.cls <- sapply(object[, -c(pos.s, pos.r, pos.d)], class)
  # create dataframe with column numbers, names and classes
  res$columns <- data.frame(column = as.character((1:ncol(object))[-c(pos.s, pos.r, pos.d)]), name = names(col.cls), class = as.character(col.cls))
  
  res <- summaryHypeGeoData(x = res)
  print(res)
  invisible(res)
}



#--------------------------------------------------------------------------------------------------------------------------------------

# print method for summary list object
#' @importFrom stats na.omit
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
        ), row.names = FALSE)
  
  
  ## range table
  # subbasin area range
  tes <- c("Sub-basin area (km2):", format(x$subbasin.area, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  # unit river length range
  ter <- c("Unit river length (km/km2):", format(x$unit.river.length, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  # icatch range
  tei <- c("ilake drainage area fraction (-):", format(x$icatch, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  tesri <- as.data.frame(matrix(c(tes, ter, tei), ncol = 5, byrow = TRUE))
  names(tesri) <- c(" ", "mean", "median", "minimum", "maximum")
  cat("\n")
  print(tesri, row.names = FALSE)
  
  
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
  
  
  ## SLC class columns as ASCII text representation, copy-paste-friendly (friendlier than dput() output with disjointed columns)
  # create comparison data
  te <- x$pos.slc
  te1 <- c(-2, -1, te)
  te2 <- c(-1, te, te[length(te)] + 2)
  te3 <- c(te, te[length(te)] + 2:3)
  
  # replace column number with colon, if either next or previous SLC column is a direct neighbour
  te.str <- ifelse(te3 - te2 > 1 | te2 - te1 > 1, te2, ":")[-c(1, length(te) + 2)]
  
  # string formatting, condense into c()-function string
  te.str <- paste0("c(", gsub(", :, ", ":", paste(na.omit(ifelse(te.str == ":" & c(te.str[-1], "") == ":", NA, te.str)), collapse = ", ")), ")")
  
  # print columns
  cat(paste("                      SLC columns:", te.str, "\n"))
  
  
  ## SCR columns as ASCII text representation, copy-paste-friendly (friendlier than dput() output with disjointed columns)
  # create comparison data
  te <- x$pos.scr
  
  # just print if there are any SCR classes
  if (length(te) > 0) {
    te1 <- c(-2, -1, te)
    te2 <- c(-1, te, te[length(te)] + 2)
    te3 <- c(te, te[length(te)] + 2:3)
    
    # replace column number with colon, if either next or previous SLC column is a direct neighbour
    te.str <- ifelse(te3 - te2 > 1 | te2 - te1 > 1, te2, ":")[-c(1, length(te) + 2)]
    
    # string formatting, condense into c()-function string
    te.str <- paste0("c(", gsub(", :, ", ":", paste(na.omit(ifelse(te.str == ":" & c(te.str[-1], "") == ":", NA, te.str)), collapse = ", ")), ")")
    
    # print columns
    cat(paste("                      SCR columns:", te.str, "\n"))
    
  }
  
  
  ## DHSLC columns as ASCII text representation, copy-paste-friendly (friendlier than dput() output with disjointed columns)
  # create comparison data
  te <- x$pos.dhslc
  
  # just print if there are any SCR classes
  if (length(te) > 0) {
    te1 <- c(-2, -1, te)
    te2 <- c(-1, te, te[length(te)] + 2)
    te3 <- c(te, te[length(te)] + 2:3)
    
    # replace column number with colon, if either next or previous SLC column is a direct neighbour
    te.str <- ifelse(te3 - te2 > 1 | te2 - te1 > 1, te2, ":")[-c(1, length(te) + 2)]
    
    # string formatting, condense into c()-function string
    te.str <- paste0("c(", gsub(", :, ", ":", paste(na.omit(ifelse(te.str == ":" & c(te.str[-1], "") == ":", NA, te.str)), collapse = ", ")), ")")
    
    # print columns
    cat(paste("                    DHSLC columns:", te.str, "\n"))
    
  }
  
}




#--------------------------------------------------------------------------------------------------------------------------------------

# merge method

#' Merge HypeGeoData object
#' 
#' Merge an imported HYPE GeoData table of class \code{link{HypeGeoData}} with another data frame. 
#' 
#' @param x \code{\link{HypeGeoData}} data frame, HYPE GeoData table to be extended with new columns.
#' @param y Data frame, with mandatory \code{SUBID} column.
#' @param all.x Logical, keep all rows from \code{x}. Defaults to \code{TRUE}, as opposed to default method, thus extending the GeData 
#' table with columns in \code{y}.
#' @param sort Logical, result sorting by \code{by} columns. In addition to the default method's choices \code{TRUE, FALSE}, a third 
#' option \code{NA} (default) will use sorting of \code{x} for results. I.e. a sorted GeoData table will be runnable in HYPE even after 
#' merging.
#' @param ... Arguments passed to S3 method for data frames, see \code{\link[base]{merge}} and Details.
#' 
#' @details 
#' \code{merge.HypeGeoData} allows to merge new columns to an existing HYPE GeoData table, while preserving the \code{HypeGeoData} 
#' class attribute. Duplicate columns are marked with a \code{".y"}-suffix for the merged \code{y} data frame.
#' 
#' The following arguments of the default method are hard-coded:
#' 
#' \itemize{
#' \item{\code{by, by.x, by.y}, set to \code{"SUBID"}}
#' \item{\code{suffixes}, set to \code{c("", ".y")}}
#' }
#' 
#' The method warns if any of these arguments is supplied by the user. To override, use the GeoData table as argument \code{y} or 
#' call the data frame method explicitly (\code{merge.data.frame()}).
#' 
#' @return 
#' A \code{HypeGeoData} data frame.
#' 
#' @examples 
#' # import and create dummy data
#' te1 <- ReadGeoData(filename = system.file("demo_model",
#' "GeoData.txt", package = "HYPEtools"))
#' te2 <- data.frame(SUBID = sample(x = te1$SUBID, size = 10),
#' loc_vol = runif(n = 10, 10, 50))
#' merge(x = te1, y = te2)
#' 
#' @name merge
#' @method merge HypeGeoData
#' 
#' @seealso \code{\link[base]{merge}}, the S3 generic function.
#' 
#' @importFrom stats na.omit
#' @export

merge.HypeGeoData <- function(x, y, all.x = TRUE, sort = NA, ...) {
  
  # check user-supplied arguments
  if (!(sort %in% c(NA, TRUE, FALSE))) {
    stop("Argument 'sort' must be of type logical.")
  }
  if (!(all.x %in% c(TRUE, FALSE))) {
    stop("Argument 'all.x' must be of type logical.")
  }
  
  # prepare for NA sorting, which will be resolved after merge function call
  if (is.na(sort)) {
    sort <- TRUE
    sort.na <- TRUE
  } else {
    sort.na <- FALSE
  }
  
  ## check if a hard-coded argument is supplied by user and warn accordingly
  # list user-supplied arguments
  dots <- list(...)
  # hard-coded argument names
  hardcoded <- c("suffixes", "by", "by.x", "by.y")
  # match
  te <- which(names(dots) %in% hardcoded)
  
  # conditional: duplicates found
  if (length(te) > 0) {
    
    # duplicated arguments
    dupli.arg <- hardcoded[hardcoded %in% names(dots)]
    
    # remove duplicated from list
    dots <- dots[-te]
    
    # throw meaningful warning
    warning(paste0("Argument(s) '", paste(dupli.arg, collapse = "', '"), "' are hard-coded in this method and will be ignored."))
  }
  
  # combine user-defined arguments with hard-coded ones in one list
  combined.args <- c(list(x = x, y = y, all.x = all.x, sort = sort, by = "SUBID", suffixes = c("", ".y")), dots)
  
  # call function (do.call construct to be able to pass combined argument list into the function)
  z <- do.call("merge.default", args = combined.args)
  
  # conditional: sort in order of x
  if (sort.na) {
    z <- z[na.omit(match(x$SUBID, z$SUBID)), ]
  }
  
  # add class and return result
  z <- HypeGeoData(z)
  return(z)
  
}
