#'
#' Summarize subbasin attributes
#'
#' Prepare data frame containing summary of subbasin attributes.
#'
#' @param subids Vector containing SUBIDs of subbasins to summarize.
#' @param gd Imported HYPE GeoData.txt file. See \code{\link{ReadGeoData}}.
#' @param bd Imported HYPE BranchData.txt file. See \code{\link{ReadBranchData}}.
#' @param gc Imported HYPE GeoClass.txt file. See \code{\link{ReadGeoClass}}.
#' @param desc Optional, Imported HYPE Description file. If provided, then dataframe columns will be renamed using the short names in the description file. See \code{\link{ReadDescription}}.
#' @param group Optional, Integer vector of same length as number of SLC classes in gd. Alternative grouping index specification to gcl + type for \code{\link{UpstreamGroupSLCClasses}}.
#' @param signif.digits Optional, Integer specifying number of significant digits to round outputs to. Used by \code{\link{UpstreamGroupSLCClasses}} and \code{link{UpstreamGeoData}}.
#' @param progbar Logical, display a progress bar while calculating summary information. Used by \code{\link{UpstreamGroupSLCClasses}} and \code{link{UpstreamGeoData}}.
#' @param summarize.landuse Logical, specify whether or not subbasin upstream landuse fractions should be calculated.
#' @param summarize.soil Logical, specify whether or not subbasin upstream soil fractions should be calculated.
#' @param summarize.crop Logical, specify whether or not subbasin upstream crop fractions should be calculated.
#' @param summarize.upstreamarea Logical, specify whether or not subbasin upstream area should be calculated.
#' @param unweighted.gd.cols Vector, names of \code{gd} columns which should be joined to the output data frame without any additional processing.
#' @param upstream.gd.cols Vector, specify column names of \code{gd} which should be summarized using \code{\link{UpstreamGeoData}}.
#' @param olake.slc Integer, SLC class number representing outlet lake fractions. Used by \code{\link{UpstreamGeoData}}.
#' @param bd.weight Logical, if set to TRUE, flow weights will be applied for areas upstream of stream bifurcations. See \code{\link{UpstreamGeoData}}.
#' @param mapoutputs Vector, paths to mapoutput files that should be read by \code{\link{ReadMapOutput}} and joined to the output data frame.
#' 
#' @details
#' \code{SubidAttributeSummary} can be used to create a data frame object containing subbasin attribute summary information. This data frame can then be used as the \code{attributes}
#' input for \code{\link{PlotPerformanceByAttribute}}. The function can summarize subbasin upstream landuse, soil, and crop fractions using \code{\link{UpstreamGroupSLCClasses}}. In addition, the
#' function can summarize upstream GeoData information using \code{\link{UpstreamGeoData}}. Finally, the function can join mapoutput and GeoData columns directly to the output data frame (i.e without further processing). 
#'
#' @return
#' \code{SubidAttributeSummary} returns a data frame object containing subbasin attribute summary information.
#'
#' @seealso
#' \code{\link{UpstreamGroupSLCClasses}}, \code{\link{UpstreamGeoData}}, \code{\link{ReadMapOutput}} for subbasin attribute summary functions; \code{\link{PlotPerformanceByAttribute}} for related plotting function.

#' @examples
#' \donttest{
#' subass <- ReadSubass(filename = system.file("demo_model", "results",
#'   "subass1.txt",
#'   package = "HYPEtools"
#' ), check.names = TRUE)
#' gd <- ReadGeoData(filename = system.file("demo_model",
#'   "GeoData.txt",
#'   package = "HYPEtools"
#' ))
#' gc <- ReadGeoClass(filename = system.file("demo_model",
#'   "GeoClass.txt",
#'   package = "HYPEtools"
#' ))
#' 
#' SubidAttributeSummary(subids <- subass$SUBID,
#'   gd = gd, gc = gc,
#'   mapoutputs = c(system.file("demo_model", "results", "mapCOUT.txt", package = "HYPEtools")),
#'   upstream.gd.cols = c("SLOPE_MEAN")
#' )
#' }
#' 
#' @importFrom dplyr filter all_of left_join rename_with select
#' @importFrom data.table setnames
#' @importFrom purrr reduce
#' @importFrom rlang .data
#' @export

# Create Johan summary dataframe
SubidAttributeSummary <- function(subids, gd, bd = NULL, gc = NULL, desc = NULL, group = NULL, signif.digits = NULL, progbar = FALSE,
                                  summarize.landuse = TRUE, summarize.soil = TRUE, summarize.crop = TRUE,
                                  summarize.upstreamarea = TRUE, unweighted.gd.cols = NULL, upstream.gd.cols = NULL, olake.slc = NULL, bd.weight = FALSE,
                                  mapoutputs = NULL) {

  # Create list to store data
  data <- vector("list")

  # Summarize SLC Fractions --------------------------------------------------------------------------------------------------------------------

  # Create list to store types
  type <- c()
  if (summarize.landuse == TRUE) {
    type <- append(type, "landuse")
  }
  if (summarize.soil == TRUE) {
    type <- append(type, "soil")
  }
  if (summarize.crop == TRUE) {
    type <- append(type, "crop")
  }

  # Summarize upstream SLC fractions
  if (any(summarize.landuse, summarize.soil, summarize.crop)) {
    slc <- UpstreamGroupSLCClasses(
      subid = subids,
      gd = gd, bd = bd, gcl = gc,
      type = type, group = group,
      signif.digits = signif.digits, progbar = progbar
    )

    # Rename columns if given a description file
    if (!is.null(desc)) {
      slc <- slc %>%
        setnames(paste0("landuse_", desc$lu.id), paste0("Landuse ", desc$lu), skip_absent = TRUE) %>% # Rename landuse columns
        setnames(paste0("soil_", desc$so.id), paste0("Soil ", desc$so), skip_absent = TRUE) %>% # Rename soil columns
        setnames(paste0("crop_", desc$cr.id), paste0("Crop ", desc$cr), skip_absent = TRUE) # Rename crop columns
    }

    data[["slc"]] <- slc
  }

  # Summarize Upstream Area & Upstream GeoData Columns --------------------------------------------------------------------------------------------------------------------

  if (summarize.upstreamarea == TRUE | !is.null(upstream.gd.cols)) {

    # Summarize upstream area & GeoData columns
    uparea <- UpstreamGeoData(subid = subids, gd = gd %>% select("SUBID", "MAINDOWN", "AREA", all_of(upstream.gd.cols)), bd = bd, olake.slc = olake.slc, bd.weight = bd.weight, signif.digits = signif.digits, progbar = progbar) %>%
      select(-"MAINDOWN")

    # Remove upstream area column if summarize.upstreamarea is FALSE
    if (summarize.upstreamarea == FALSE) {
      uparea <- uparea %>% select(-"UP_AREA")
    }

    data[["uparea"]] <- uparea
  }

  # Add mapoutputs --------------------------------------------------------------------------------------------------------------------

  if (!is.null(mapoutputs)) {
    for (mapoutput in mapoutputs) {
      mapdf <- ReadMapOutput(mapoutput) %>%
        filter(.data$SUBID %in% subids)

      data[[basename(mapoutput)]] <- mapdf %>%
        rename_with(~ paste0("map_", attr(mapdf, "variable")), .cols = 2)
    }
  }

  # Merge Data --------------------------------------------------------------------------------------------------------------------

  data <- data %>% reduce(full_join, by = "SUBID")

  # Add unweighted GeoData columns --------------------------------------------------------------------------------------------------------------------
  if (!is.null(unweighted.gd.cols)) {
    data <- data %>%
      left_join(gd %>% select("SUBID", all_of(unweighted.gd.cols)), by = "SUBID")
  }

  # Return data frame
  return(data)
}
