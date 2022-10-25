
#--------------------------------------------------------------------------------------------------------------------------------------
#   Collection of HYPE export functions to generate Xobs harmonized format files, herein:
#
#     - WriteHarmonizedData()
#     - WriteHarmonizedSpatialDescription()
#--------------------------------------------------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------------------------------------------------
# WriteHarmonizedData
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a Harmonized Data File
#'
#' This is a convenience wrapper function to export a data frame to the required Harmonized Data File format. See the
#' \href{https://git.smhi.se/fouh/hypeobsmetadatatools}{HYPEObsMetadataTools documentation}.
#'
#' @param df Data frame containing the harmonized data.
#' @param filename Path to and file name (including ".csv" file extension) of the Harmonized Data CSV file to export. Windows users: Note that
#' Paths are separated by '/', not '\\'.
#' @param replace.accents Logical, if \code{TRUE}, then accented characters (e.g. ä, ö, å) will be replaced with non-accented characters in all strings.
#' If \code{FALSE}, then strings will be left unmodified.
#' @param strip.punctuation Logical, if \code{TRUE}, then punctuation characters (e.g. "-", ".", ".") will be removed from all strings.
#' If \code{FALSE}, then strings will be left unmodified.
#' @param ignore.cols Vector of columns in \code{df} that should be ignored when \code{replace.accents} or \code{strip.punctuation} are set to \code{TRUE}.
#' @param nThread Integer, set number of thereads to be used when writing file. See \code{\link{getDTthreads}}
#'
#' @details
#' \code{WriteHarmonizedData} is a convenience wrapper function of \code{\link[data.table]{fread}} to export harmonized data in the HYPEObsMetadataTools Harmonized Data Format.
#' The function checks that all required columns are present, includes options to format strings, and exports data to output CSV files with the correct encoding and formatting.
#'
#' @return
#' \code{WriteHarmonizedData} exports a CSV file if \code{filename} is specified. Otherwise, the function outputs a data frame to the console.
#'
#' @examples
#' df <- data.frame(
#'   "STATION_ID" = "A1",
#'   "DATE_START" = "2002-06-18 12:00",
#'   "DATE_END" = "2002-06-18 12:00",
#'   "PARAMETER" = "NH4_N",
#'   "VALUE" = 0.050,
#'   "UNIT" = "mg/L",
#'   "QUALITY_CODE" = "AA"
#' )
#' WriteHarmonizedData(df)
#'
#' @importFrom dplyr all_of across mutate relocate
#' @importFrom data.table fwrite getDTthreads
#' @export

WriteHarmonizedData <- function(df, filename = "", replace.accents = FALSE, strip.punctuation = FALSE, ignore.cols = NULL, nThread = getDTthreads()) {

  # Required Column Names
  required_cols <- c("STATION_ID", "DATE_START", "DATE_END", "PARAMETER", "VALUE", "UNIT", "QUALITY_CODE")

  # Check filename
  if (!filename == "") {
    if (!grepl("*.csv$", filename)) {
      stop('"filename" must end in ".csv', call. = FALSE)
    }
  }

  # Convert column names to upper
  colnames(df) <- toupper(colnames(df))

  # Check that all required columns are present
  if (!all(required_cols %in% colnames(df))) {
    stop(paste0("The following required columns are missing from df: ", paste0(required_cols[which(!required_cols %in% colnames(df))], collapse = ", ")), call. = FALSE)
  }

  # Check for extra columns
  extra_cols <- colnames(df)[which(!colnames(df) %in% c(required_cols, grep("KEY_|OPT_", colnames(df)[which(!colnames(df) %in% required_cols)], value = TRUE)))]
  if (length(extra_cols) > 0) {
    warning(paste0("Column names not matching harmonzied format were identified in df: ", paste0(extra_cols, collapse = ", ")), call. = FALSE)
  }
  
  # Convert STATION_ID to string
  if(!typeof(df$STATION_ID) == "character"){
    df$STATION_ID <- as.character(df$STATION_ID)
    warning("Converted STATION_ID field to string type")
  }

  # Check strings
  if (replace.accents == TRUE | strip.punctuation == TRUE) {

    # Get columns with character type
    character_cols <- names(sapply(df, typeof)[which(sapply(df, typeof) == "character")])
    character_cols <- character_cols[which(!grepl("DATE", character_cols))] # Don't format dates
    if(!is.null(ignore.cols)){
      character_cols <- character_cols[which(!character_cols %in% ignore.cols)] # Don't format ignore columns
    }

    # Remove accented characters (e.g. ä, ö, å)
    if (replace.accents == TRUE) {
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~ iconv(.x, to = "ASCII//TRANSLIT")))
    }

    # Remove punctuation characters (e.g. ".", "-", ",")
    if (strip.punctuation == TRUE) {
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~ gsub("[[:punct:]]", "", .x, )))
    }
  }

  # Set column order
  df <- df %>%
    relocate(all_of(required_cols), .before = 1)

  # Write file
  fwrite(x = df, file = filename, sep = ",", dec = ".", dateTimeAs = "ISO", nThread = nThread)
}

#--------------------------------------------------------------------------------------------------------------------------------------
# WriteHarmonizedSpatialDescription
#--------------------------------------------------------------------------------------------------------------------------------------

#' Write a Harmonized Spatial Description File
#'
#' This is a convenience wrapper function to export a data frame to the required Harmonized Spatial Description File format. See the
#' \href{https://git.smhi.se/fouh/hypeobsmetadatatools}{HYPEObsMetadataTools documentation}.
#'
#' @param df Data frame containing the harmonized spatial description data.
#' @param filename Path to and file name (including ".csv" file extension) of the Harmonized Spatial Description CSV file to export. Windows users: Note that
#' Paths are separated by '/', not '\\'.
#' @param replace.accents Logical, if \code{TRUE}, then accented characters (e.g. ä, ö, å) will be replaced with non-accented characters in all strings.
#' If \code{FALSE}, then strings will be left unmodified.
#' @param strip.punctuation Logical, if \code{TRUE}, then punctuation characters (e.g. "-", ".", ".") will be removed from all strings.
#' If \code{FALSE}, then strings will be left unmodified.
#' @param ignore.cols Vector of columns in \code{df} that should be ignored when \code{replace.accents} or \code{strip.punctuation} are set to \code{TRUE}.
#' @param nThread Integer, set number of thereads to be used when writing file. See \code{\link{getDTthreads}}
#'
#' @details
#' \code{WriteHarmonizedSpatialDescrption} is a convenience wrapper function of \code{\link[data.table]{fread}} to export harmonized spatial description data in the HYPEObsMetadataTools Harmonized Spatial Descrption Format.
#' The function checks that all required columns are present, includes options to format strings, and exports data to output CSV files with the correct encoding and formatting.
#'
#' @return
#' \code{WriteSpatialDescrption} exports a CSV file if \code{filename} is specified. Otherwise, the function outputs a data frame to the console.
#'
#' @examples
#' df <- data.frame(
#'   "STATION_ID" = "A1",
#'   "SRC_NAME" = "Example",
#'   "DOWNLOAD_DATE" = "2022-10-19",
#'   "SRC_STATNAME" = "Station",
#'   "SRC_RIVNAME" = "River",
#'   "SRC_UAREA" = NA,
#'   "SRC_XCOORD" = 28.11831,
#'   "SRC_YCOORD" = -25.83053,
#'   "SRC_EPSG" = 4326,
#'   "ADJ_XCOORD" = 28.11831,
#'   "ADJ_YCOORD" = -25.83053,
#'   "ADJ_EPSG" = 4326
#' )
#' 
#' WriteHarmonizedSpatialDescription(df)
#'
#' @importFrom dplyr all_of across mutate relocate
#' @importFrom data.table fwrite getDTthreads
#' @export

WriteHarmonizedSpatialDescription <- function(df, filename = "", replace.accents = FALSE, strip.punctuation = FALSE, ignore.cols = NULL, nThread = getDTthreads()) {

  # Required Column Names
  required_cols <- c("STATION_ID", "SRC_NAME", "DOWNLOAD_DATE", "SRC_STATNAME", "SRC_RIVNAME", "SRC_UAREA", "SRC_XCOORD", "SRC_YCOORD", "SRC_EPSG", "ADJ_XCOORD", "ADJ_YCOORD", "ADJ_EPSG")

  # Check filename
  if (!filename == "") {
    if (!grepl("*.csv$", filename)) {
      stop('"filename" must end in ".csv', call. = FALSE)
    }
  }

  # Convert column names to upper
  colnames(df) <- toupper(colnames(df))

  # Check that all required columns are present
  if (!all(required_cols %in% colnames(df))) {
    stop(paste0("The following required columns are missing from df: ", paste0(required_cols[which(!required_cols %in% colnames(df))], collapse = ", ")), call. = FALSE)
  }

  # Check for extra columns
  extra_cols <- colnames(df)[which(!colnames(df) %in% required_cols)]
  if (length(extra_cols) > 0) {
    warning(paste0("Column names not matching harmonzied format were identified in df: ", paste0(extra_cols, collapse = ", ")), call. = FALSE)
  }
  
  # Convert STATION_ID to string
  if(!typeof(df$STATION_ID) == "character"){
    df$STATION_ID <- as.character(df$STATION_ID)
    warning("Converted STATION_ID field to string type")
  }

  # Check strings
  if (replace.accents == TRUE | strip.punctuation == TRUE) {

    # Get columns with character type
    character_cols <- names(sapply(df, typeof)[which(sapply(df, typeof) == "character")])
    character_cols <- character_cols[which(!grepl("DATE", character_cols))] # Don't format dates
    if(!is.null(ignore.cols)){
      character_cols <- character_cols[which(!character_cols %in% ignore.cols)] # Don't format ignore columns
    }

    # Remove accented characters (e.g. ä, ö, å)
    if (replace.accents == TRUE) {
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~ iconv(.x, to = "ASCII//TRANSLIT")))
    }

    # Remove punctuation characters (e.g. ".", "-", ",")
    if (strip.punctuation == TRUE) {
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~ gsub("[[:punct:]]", "", .x, )))
    }
  }

  # Set column order
  df <- df %>%
    relocate(all_of(required_cols), .before = 1)

  # Write file
  fwrite(x = df, file = filename, sep = ",", dec = ".", dateTimeAs = "ISO", nThread = nThread)
}
