#' Compare HYPE model files to identify any differences.
#'
#' Compare HYPE model files to identify any differences, typically used to check that no undesired changes were made when writing a new file.
#'
#' @param x Path to a HYPE model file to read, or an existing list/data frame object for a HYPE model file.
#' File contents are compared to those of \code{y}.
#' @param y Path to a HYPE model file to read, or an existing list/data frame object for a HYPE model file.
#' File contents are compared to those of \code{x}.
#' @param type Character string identifying the type of HYPE model file. Used to determine appropriate read function. One of
#' \code{AquiferData}, \code{BasinOutput}, \code{BranchData}, \code{CropData}, \code{DamData}, \code{ForcKey}, \code{GeoClass},
#' \code{GeoData}, \code{Info}, \code{LakeData}, \code{MapOutput}, \code{MgmtData}, \code{Optpar}, \code{Par}, \code{PointsourceData}, \code{PTQobs},
#' \code{TimeOutput}, or \code{Xobs}.
#' @param by Character vector, names of columns in \code{x} and \code{y} to use to join data. See [dplyr::full_join()].
#' @param compare.order Logical, whether or not the order of the rows should be compared. If \code{TRUE}, then \code{x} and \code{y}
#' will also be joined by row number. See \code{\link{full_join}}.
#'
#' @details
#' \code{CompareFiles} compares two HYPE model files and identifies any differences in values. The function reads two model files, compares
#' the values in columns with corresponding names, and returns a data frame consisting of rows/columns with any differences. Values that are
#' the same in both files are set to \code{NA}. The function is primarily intended as a check to ensure that no unintended changes were made when writing
#' model files using the various HYPEtools write functions. However, it can also be used to e.g. compare files between different model versions.
#'
#' @return
#' Returns invisibly a data frame containing rows and columns in which differences exist between \code{x} and \code{y}. Values that are the same in both
#' files are set to \code{NA}. If the returned data frame has 0 row, then there were no differences between the files.
#'
#' @examples
#' # Import demo model GeoData file, edit a SUBID
#' te1 <- ReadGeoData(filename = system.file("demo_model", "GeoData.txt", package = "HYPEtools"))
#' te1$SUBID[1] <- 1
#' # Compare with original file
#' te2 <- CompareFiles(system.file("demo_model", "GeoData.txt", package = "HYPEtools"), te1, 
#'                     type = "GeoData")
#' te2
#'
#' @importFrom dplyr distinct filter full_join if_any relocate rename_with select syms %>%
#' @importFrom tidyselect matches all_of
#' @importFrom rlang .data
#' @export

CompareFiles <- function(x, y, type, by = NULL, compare.order = TRUE) {

  # Create function to read files
  ReadFile <- function(file, type) {
    # Given path to a file (i.e. not given an object)
    if (typeof(file) == "character") {
      if (type == "AquiferData") {
        file <- ReadAquiferData(file)
      } else if (type == "BasinOutput") {
        file <- ReadBasinOutput(file)
      } else if (type == "BranchData") {
        file <- ReadBranchData(file)
      } else if (type == "CropData") {
        file <- ReadCropData(file)
      } else if (type == "DamData") {
        file <- ReadDamData(file)
      } else if (type == "ForcKey") {
        file <- ReadForcKey(file)
      } else if (type == "GeoClass") {
        file <- ReadGeoClass(file)
      } else if (type == "GeoData") {
        file <- ReadGeoData(file)
      } else if (type %in% c("Info", "Par")) {
        list <- ReadPar(file)
        file <- data.frame(FILE_ROW = as.numeric(), Name = as.character(), Value = as.character())
        for (i in 1:length(list)) {
          file[i, "FILE_ROW"] <- i
          file[i, "Name"] <- names(list[i])
          file[i, "Value"] <- paste(unlist(list[i]), collapse = " ")
        }
        file
      } else if (type == "LakeData") {
        file <- ReadLakeData(file)
      } else if (type == "MapOutput") {
        file <- ReadMapOutput(file)
      } else if (type == "MgmtData") {
        file <- ReadMgmtData(file)
      } else if (type == "Optpar") {
        list <- ReadOptpar(file)
        file <- data.frame(list) %>%
          mutate("tasks.NA" = do.call(paste, .[grep("tasks.NA", colnames(.), value = TRUE)])) %>% # Merge tasks.NA columns
          select(-grep("tasks.NA.*.", colnames(.), value = TRUE)) # Drop original tasks.NA columns
        file
        # } else if(type=="Outregions"){
        #   file <- ReadOutregions(file) # Untested
        # } else if(type=="Pmsf"){
        #   file <- ReadPmsf(file) # Untested
      } else if (type == "PointSourceData") {
        file <- ReadPointSourceData(file)
      } else if (type == "PTQobs") {
        file <- ReadPTQobs(file)
      } else if (type == "TimeOutput") {
        file <- ReadTimeOutput(file)
      } else if (type == "Xobs") {
        file <- ReadXobs(file)
      }
      # Given an object from R environment
    } else {
      file <- file

      # Format Info/Par if given object from R environment
      if (type %in% c("Info", "Par")) {
        list <- file
        file <- data.frame(FILE_ROW = as.numeric(), Name = as.character(), Value = as.character())
        for (i in 1:length(list)) {
          file[i, "FILE_ROW"] <- i
          file[i, "Name"] <- names(list[i])
          file[i, "Value"] <- paste(unlist(list[i]), collapse = " ")
        }
        file
        # Format Optpar if given object from R environment
      } else if (type == "Optpar") {
        file <- data.frame(file) %>%
          mutate("tasks.NA" = do.call(paste, .[grep("tasks.NA", colnames(.), value = TRUE)])) %>% # Merge tasks.NA columns
          select(-grep("tasks.NA.*.", colnames(.), value = TRUE)) # Drop original tasks.NA columns
      }
    }

    # Add Row Number for types where row number wasn't added on import
    if (!type %in% c("Info", "Par")) {
      file["FILE_ROW"] <- 1:nrow(file)
    }

    # Return File
    file
  }

  # Add "FILE_ROW" to Join By Argument
  if (compare.order == TRUE) {
    by <- c("FILE_ROW", by)
  }

  # Import Files
  message("Reading Files")
  x <- ReadFile(x, type)
  y <- ReadFile(y, type)

  # Check that join is unique
  if (!nrow(x %>% distinct(!!!syms(by))) == nrow(x)) {
    warning('Column names in "by" do not identify unique rows in "x". Add additional column names or set "compare.order" to TRUE.')
  }
  if (!nrow(y %>% distinct(!!!syms(by))) == nrow(y)) {
    warning('Column names in "by" do not identify unique rows in "y". Add additional column names or set "compare.order" to TRUE.')
  }

  # Get column names that are in both x and y
  intersect_cols <- intersect(colnames(x)[which(!colnames(x) %in% by)], colnames(y)[which(!colnames(y) %in% by)])

  # Join Files
  message("Comparing Files")
  compare <- full_join(
    x = x %>% rename_with(~ paste0(., ".x"), !matches(by)), # Add ".x" suffix to columns in x
    y = y %>% rename_with(~ paste0(., ".y"), !matches(by)), # Add ".y" suffix to columns in y
    by = by
  ) %>%
    # Reorder Columns
    relocate(c(
      by[which(!by == "FILE_ROW")], # Columns specified with "by" argument
      paste(rep(intersect_cols, each = 2), c("x", "y"), sep = "."), # Columns that are in both x and y
      paste0(setdiff(colnames(x), colnames(y)), ".x", recycle0 = TRUE), # Columns that are unique to x
      paste0(setdiff(colnames(y), colnames(x)), ".y", recycle0 = TRUE)
    ), # Columns that are unique to y
    .after = ifelse("FILE_ROW" %in% colnames(.), "FILE_ROW", by)
    )

  # Test if values match for columns that are in both x and y
  for (col in intersect_cols) {

    # Test if values in columns are the same
    same <- compare[paste0(col, ".x")] == compare[paste0(col, ".y")]

    # Set values to NA if they are the same
    compare[which(same == TRUE), paste0(col, ".x")] <- NA
    compare[which(same == TRUE), paste0(col, ".y")] <- NA
  }

  # Remove columns and rows that are all NA
  compare <- compare %>%
    select(where(~ any(!is.na(.)))) %>% # Remove columns that are all NA
    filter(if_any(!all_of(by), ~ !is.na(.))) # Remove rows that are all NA

  # Return data frame
  # Return invisible data frame because printing to console takes a long time and e.g. for info.txt files the object that is returned to the console looks really funky because of the spacing in the text file
  invisible(compare)
}
