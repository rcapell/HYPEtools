library(data.table)
library(dplyr)

# Set path to source files in the old harmonized format
source_path <- "//winfs-proj/data/proj/Fouh/Global/SouthAfrica/uMngeni_catchment/WQobs/Xobs_2020/01_NP/Outputs/1_DataBySite/NH4_N_Diss_Water"

# Set path to output files in the new harmonized format
out_path <- "C:/Users/a002416/Desktop/test"

# Specify number of files to create
n_files <- 10

# _____________________________________________________________________________________________________________________________________
# 2) Parse Files #####
# _____________________________________________________________________________________________________________________________________

# Read all filenames
files <- list.files(source_path)

# Get sample of n files
files <- files[sample(length(files), n_files, replace = F)]

file = files[1]
  
# Read file
table <- fread(file.path(source_path, file))

# Generate filename
filename <- paste(table$STAT_ID[1], "NH4_N_Diss_Water", "s", table$Ref_ID[1], sep = "_")

# Format Table
table <- table %>%
  # select(1, 4, 5, 6, 7) %>%
  mutate(PARAMETER = "NH4_N", .after = "DateEnd") %>%
  mutate(UNIT = "mg/L", .after = "NH4_N_Diss_Water") %>%
  mutate(OPT_1 = "Optional", .after = ncol(.))

# Set column names
colnames(table) <- c("STATION_ID", "KEY_1", "KEY_2", "DATE_START", "DATE_END", "PARAMETER", "VALUE", "UNIT", "QUALITY_CODE", "OPT_1")

WriteHarmonized <- function(df, filename = "", nThread = getDTthreads(), showProgress = FALSE, remove.accents = FALSE, remove.punct = FALSE){
  
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


WriteHarmonized(table %>% mutate(STATION_ID = paste0(STATION_ID, ".ö-ä,å")), remove.accents = F, remove.punct = T)


WriteSpatial <- function()
