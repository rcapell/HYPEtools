
#--------------------------------------------------------------------------------------------------------------------------------------
#   Collection of HYPE export functions to generate Xobs harmonized format files, herein:
#
#     - WriteHarmonizedData()
#     - WriteHarmonizedSpatialDescription()
#--------------------------------------------------------------------------------------------------------------------------------------





#--------------------------------------------------------------------------------------------------------------------------------------
# WriteHarmonizedData
#--------------------------------------------------------------------------------------------------------------------------------------

#' Read a 'GeoClass.txt' File
#'
#' This is a convenience wrapper function to import a GeoClass file as data frame into R. GeoClass files contain definitions
#' of SLC (\bold{S}oil and \bold{L}and use \bold{C}rop) classes in twelve to 14 predefined columns, see 
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:geoclass.txt}{GeoClass.txt documentation}.
#' 
#' @param filename Path to and file name of the GeoClass file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param encoding Character string, encoding of non-ascii characters in imported text file. Particularly relevant when 
#' importing files created under Windows (default encoding "Latin-1") in Linux (default encoding "UTF-8") and vice versa. See 
#' also argument description in \code{\link[data.table]{fread}}.
#' @param verbose Print information on number of data columns in imported file.
#' 
#' @details
#' \code{ReadGeoClass} is a convenience wrapper function of \code{\link[data.table]{fread}}, with treatment of leading 
#' comment rows. Column names are created on import, optional comment rows are imported as strings in \code{attribute} 'comment'. 
#' Optional inline comments (additional non-numeric columns) are automatically identified and imported along with data columns. 
#' 
#' @return
#' \code{ReadGeoClass} returns a data frame with added attribute 'comment'.
#' 
#' @examples
#' te <- ReadGeoClass(filename = system.file("demo_model", "GeoClass.txt", package = "HYPEtools"))
#' te
#' 
#' @importFrom data.table fread
#' @export

WriteHarmonizedData <- function(df, filename = "", remove.accents = FALSE, remove.punct = FALSE, nThread = getDTthreads()){
  
  # Required Column Names
  required_cols <- c("STATION_ID", "DATE_START", "DATE_END", "PARAMETER", "VALUE", "UNIT", "QUALITY_CODE")
  
  # Convert column names to upper
  colnames(df) <- toupper(colnames(df))
  
  # Check strings
  if(remove.accents == TRUE | remove.punct == TRUE){
    
    # Get columns with character type
    character_cols <- names(sapply(df, typeof)[which(sapply(df, typeof) == "character")])
    
    # Remove accented characters (e.g. ä, ö, å)
    if(remove.accents == TRUE){
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~iconv(.x, to='ASCII//TRANSLIT')))
    }
    
    # Remove punctuation characters (e.g. ".", "-", ",")
    if(remove.punct == TRUE){
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~gsub("[[:punct:]]", "", .x, )))
    }
  }
  
  # Check that all required columns are present
  if(!all(required_cols %in% colnames(df))){
    stop(paste0("The following required columns are missing from df: ", paste0(required_cols[which(!required_cols %in% colnames(df))], collapse = ", ")))
  }
  
  # Check for extra columns
  extra_cols <- colnames(df)[which(!colnames(df) %in% c(required_cols, grep("KEY_|OPT_",colnames(df)[which(!colnames(df)%in%required_cols)], value = TRUE)))]
  if(length(extra_cols) > 0){
    warning(paste0("Column names not matching harmonzied format were identified in df: ", paste0(extra_cols, collapse = ", ")))
  }
  
  # Write file
  fwrite(x = df, file = filename, sep = ",", dec = ".", dateTimeAs = "ISO", nThread = nThread)
}

#--------------------------------------------------------------------------------------------------------------------------------------
# WriteHarmonizedSpatialDescription
#--------------------------------------------------------------------------------------------------------------------------------------

#' Read an 'Xobs.txt' file
#'
#' This is a convenience wrapper function to import an Xobs file into R.
#' 
#' @param filename Path to and file name of the Xobs file to import. Windows users: Note that 
#' Paths are separated by '/', not '\\'. 
#' @param dt.format Date-time \code{format} string as in \code{\link{strptime}}. 
#' @param variable Character vector, HYPE variable ID(s) to select for import. Not case-sensitive. If \code{NULL} (default), all 
#' variables are imported. See \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:xobs.txt}{Xobs.txt documentation} 
#' for a list of variable IDs.
#' @param nrows Integer, number of rows to import. A value of \code{-1} indicates all rows, a positive integer gives 
#' the number of rows to import.
#' @param verbose Logical, throw warning if class \code{HypeXobs}'s attribute \code{timestep} cannot be computed.
#'  
#' @details
#' \code{ReadXobs} is a convenience wrapper function of \code{\link[data.table]{fread}} from package  
#' \code{\link{data.table}}, 
#' with conversion of date-time strings to POSIX time representations. Variable names, SUBIDs, comment, and timestep are returned as 
#' attributes (see \code{\link{attr}} on how to access these).
#' 
#' Duplicated variable-SUBID combinations are not allowed in HYPE Xobs files, and the function will throw a warning if any are found.
#' 
#' @return
#' If datetime import to POSIXct worked, \code{ReadXobs} returns a \code{\link{HypeXobs}} object, a data frame with four 
#' additional attributes \code{variable}, \code{subid}, \code{comment}, and \code{timestep}: \code{variable} 
#' and \code{subid} each contain a vector with column-wise HYPE IDs (first column with date/time information omitted). 
#' \code{comment} contains the content of the Xobs file comment row as single string. \code{timestep} contains a keyword string.
#' Column names of the returned data frame are composed of variable names and SUBIDs, separated by an underscore, 
#' i.e. \code{[variable]_[subid]}. If datetime conversion failed on import, the returned object is a data frame 
#' (i.e. no class \code{HypeXobs}).
#' 
#' @note
#' For the conversion of date/time strings, time zone "UTC" is assumed. This is done to avoid potential daylight saving time 
#' side effects when working with the imported data (and e.g. converting to string representations during the process).
#' 
#' @examples
#' te <- ReadXobs(filename = system.file("demo_model", "Xobs.txt", package = "HYPEtools"))
#' te
#' 
#' @importFrom data.table fread
#' @importFrom stats na.fail
#' @export

WriteHarmonizedSpatialDescription <- function(df, filename = "", remove.accents = FALSE, remove.punct = FALSE, nThread = getDTthreads()){
  
  # Required Column Names
  required_cols <- c("STATION_ID", "DATE_START", "DATE_END", "PARAMETER", "VALUE", "UNIT", "QUALITY_CODE")
  
  # Convert column names to upper
  colnames(df) <- toupper(colnames(df))
  
  # Check strings
  if(remove.accents == TRUE | remove.punct == TRUE){
    
    # Get columns with character type
    character_cols <- names(sapply(df, typeof)[which(sapply(df, typeof) == "character")])
    
    # Remove accented characters (e.g. ä, ö, å)
    if(remove.accents == TRUE){
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~iconv(.x, to='ASCII//TRANSLIT')))
    }
    
    # Remove punctuation characters (e.g. ".", "-", ",")
    if(remove.punct == TRUE){
      df <- df %>%
        mutate(across(.cols = all_of(character_cols), .fns = ~gsub("[[:punct:]]", "", .x, )))
    }
  }
  
  # Check that all required columns are present
  if(!all(required_cols %in% colnames(df))){
    stop(paste0("The following required columns are missing from df: ", paste0(required_cols[which(!required_cols %in% colnames(df))], collapse = ", ")))
  }
  
  # Check for extra columns
  extra_cols <- colnames(df)[which(!colnames(df) %in% c(required_cols, grep("KEY_|OPT_",colnames(df)[which(!colnames(df)%in%required_cols)], value = TRUE)))]
  if(length(extra_cols) > 0){
    warning(paste0("Column names not matching harmonzied format were identified in df: ", paste0(extra_cols, collapse = ", ")))
  }
  
  # Write file
  fwrite(x = df, file = filename, sep = ",", dec = ".", dateTimeAs = "ISO", nThread = nThread)
}
  
  

  #--------------------------------------------------------------------------------------------------------------------------------------
# library(data.table)
# library(dplyr)
# 
# # Set path to source files in the old harmonized format
# source_path <- "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/uMngeni_catchment/WQobs/Xobs_2020/01_NP/Outputs/1_DataBySite/NH4_N_Diss_Water"
# 
# # Set path to output files in the new harmonized format
# out_path <- "C:/Users/a002416/Desktop/test"
# 
# # Specify number of files to create
# n_files <- 10
# 
# # _____________________________________________________________________________________________________________________________________
# # 2) Parse Files #####
# # _____________________________________________________________________________________________________________________________________
# 
# # Read all filenames
# files <- list.files(source_path)
# 
# # Get sample of n files
# files <- files[sample(length(files), n_files, replace = F)]
# 
# file = files[1]
# 
# # Read file
# table <- fread(file.path(source_path, file))
# 
# # Generate filename
# filename <- paste(table$STAT_ID[1], "NH4_N_Diss_Water", "s", table$Ref_ID[1], sep = "_")
# 
# # Format Table
# table <- table %>%
#   # select(1, 4, 5, 6, 7) %>%
#   mutate(PARAMETER = "NH4_N", .after = "DateEnd") %>%
#   mutate(UNIT = "mg/L", .after = "NH4_N_Diss_Water") %>%
#   mutate(OPT_1 = "Optional", .after = ncol(.))
# 
# # Set column names
# colnames(table) <- c("STATION_ID", "KEY_1", "KEY_2", "DATE_START", "DATE_END", "PARAMETER", "VALUE", "UNIT", "QUALITY_CODE", "OPT_1")
# 
# WriteHarmonizedData(table %>% mutate(STATION_ID = paste0(STATION_ID, ".ö-ä,å")), paste0(filename,".csv"),remove.accents = F, remove.punct = T, showProgress = T)
