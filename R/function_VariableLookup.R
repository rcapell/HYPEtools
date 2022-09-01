#' Lookup Functions For HYPE Variables
#'
#' Lookup information (e.g. Name, Units) for a specific HYPE variable ID, or find HYPE variable information for a search term.
#'
#' @param variable String, HYPE Variable ID (e.g. "COUT").
#' @param info A vector of strings describing HYPE variable attribute information to return/search: "ID", "Name", "Unit", "Description", "Aggregation", and/or "Component".
#' @param search String, search HYPE variable info for string matches in \code{info} attributes.
#' @param ignore_case Logical, should case differences be ignored in the match?  
#' @details
#' The \code{VariableInfo} and \code{VariableSearch} functions provide features to lookup information on HYPE variables from the
#' \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables}{HYPE Wiki}.
#' \code{VariableInfo} can be used to return information (e.g. Name, Units) for a known HYPE Variable ID.
#' \code{VariableSearch} can be used to search for e.g. an unknown HYPE variable ID based on a \code{search} term.
#' The \code{info} argument can be used to select which information to return or search.
#'
#' @return
#' \code{VariableInfo} Returns a named list of the selected \code{info} for the specified \code{variable} ID.
#' \code{VariableInfo} returns a tibble of the search results.
#'
#' @examples
#' VariableInfo(variable = "COUT", info = c("Name","Unit"))
#' VariableSearch(search = "ccSS", info = c("ID", "Name", "Description"))
#' 
#' @name VariableLookup
NULL

#' @export VariableInfo
#' @export VariableSearch
#' @importFrom dplyr filter if_any select %>%
#' @importFrom stringr fixed str_detect

#' @rdname VariableLookup
VariableInfo <- function(variable, info = "Name") {
  result <- INTERNAL.hype.vars.info %>%
    filter(str_detect(.$ID, fixed(variable, ignore_case = TRUE))) %>% # Search Variable IDs, Ignore Case
    select(info) %>% # Subset to info Columns
    as.list()

  # Return Output or List of Outputs
  if (length(info) == 1) {
    return(result[[1]])
  } else {
    return(result)
  }
}

#' @rdname VariableLookup
VariableSearch <- function(search, info = c("ID", "Name", "Unit", "Description", "Aggregation", "Component"), ignore_case = TRUE) {
  result <- INTERNAL.hype.vars.info %>%
    filter(if_any(info, ~ str_detect(., fixed(search, ignore_case = ignore_case))))
  return(result)
}

# # _____________________________________________________________________________________________________________________________________
# # Create Internal Package Data #####
# # _____________________________________________________________________________________________________________________________________
# 
# # Import Packages
# library(xml2)
# library(rvest)
# library(dplyr)
# library(stringr)
# 
# # _____________________________________________________________________________________________________________________________________
# # 1) Scrape Data from Wiki #####
# # _____________________________________________________________________________________________________________________________________
# # Wiki URL
# url <- "http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt:variables"
# 
# # Scrape HTML
# html <- read_html(url)
# 
# # Extract Tables
# tables <- html%>%
#   html_elements("table")%>%
#   html_table()
# 
# # Get Simulated and Observed Variables
# vars.info <- full_join(tables[[1]]%>%filter(!is.na(.[,"#"])), # Simulated Variables, Remove Rows with NA Number
#                       tables[[2]])%>% # Observed Variables
#   rename("ID"="Variable ID","Reference"="Reference area","Aggregation"="Agg.") # Rename Columns
# 
# # _____________________________________________________________________________________________________________________________________
# # 2) Create Function to Create Short Description #####
# # _____________________________________________________________________________________________________________________________________
# 
# desc <- function(string){
# 
#   # Find substrings within parentheses
#   string_par <- regmatches(string, gregexpr("(?=\\().*?(?<=\\))", string, perl=T))[[1]]
# 
#   if(length(string_par)>0){ # If any substrings within parentheses
#     string_par_s <- grepl(" ",string_par) # Identify if there are any spaces within the substrings in the parentheses
#     if(any(string_par_s==T)){ # If any spaces within the substrings in the parentheses
#       string_out <- sub(paste0("\\",string_par[min(which(string_par_s==T),na.rm=T)],".*"), "", string) # Get original string before first substring with space inside parentheses
#     } else{ # No spaces within substrings in parentheses
#       string_out <- string
#     }
#   } else{ # No substrings within parentheses
#     string_out <- string
#   }
#   if(grepl(",",string_out)==T){ # If there is a comma in the output string
#     string_out <- gsub(",.*","",string_out) # Only get output string before first comma
# 
#   }
#   tools::toTitleCase(gsub(" $","", string_out, perl=T)) # Remove Trailing Spaces and convert to title case
# }
# 
# # _____________________________________________________________________________________________________________________________________
# # 3) Format Variables Data Frame #####
# # _____________________________________________________________________________________________________________________________________
# 
# # Add Variable Name
# vars.info$Name<- unlist(lapply(vars.info$Description,function(X){desc(X)}))
# 
# # Format Units
# vars.info[which(grepl("\\p{So}C",vars.info$Unit,perl=T)),"Unit"] <- "C" # Remove Degree Symbol from Temperatures
# vars.info[which(vars.info$Unit=="106m3"),"Unit"] <- "10^6 m3" # Fix Volume Units
# vars.info$Unit <- gsub("[\xb5]","micro ",vars.info$Unit) # Replace Mu (Hex Code &#xb5) with word Micro
# 
# 
# 
# # _____________________________________________________________________________________________________________________________________
# # 4) Make Lists of Variables into Unique Rows #####
# # _____________________________________________________________________________________________________________________________________
# 
# # Identify Rows with Lists of Variables
# fix_rows <- which(grepl(",",vars.info$ID))
# 
# for(row in rev(fix_rows)){
#   # Get Variable Names
#   row_vars <- strsplit(vars.info$ID[row],",")[[1]]
# 
#   # Add Rows for each Variable
#   for(var_name in rev(row_vars)){
#     vars.info <- vars.info%>%
#       add_row(vars.info[row,]%>%mutate("ID"=var_name),
#               .after=row)
#   }
# 
#   # Remove Original Row
#   vars.info <- vars.info%>%slice(-row)
# 
# }
# 
# # Save Data to package sysdata
# load("R/sysdata.rda") # Load existing data in R/sysdata.rda
# INTERNAL.hype.vars.info <- vars.info # Update with new vars.info
# save(INTERNAL.hype.vars.info,INTERNAL.hype.vars, file = "R/sysdata.rda") # Save data from environment to sysdata
