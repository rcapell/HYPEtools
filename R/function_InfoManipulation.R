#' Functions to Manipuate HYPE Info Files
#'
#' Add/Remove lines to HYPE info.txt files 
#'
#' @param info Named list containing the info.txt file data, typically created using \code{\link{ReadInfo}} with the \code{exact} mode.
#' @param name Name of info.txt code to add/remove.
#' @param value Value of the info.txt code to add/remove.
#' @param after String vector containing the name(s) of info.txt codes that the new info.txt code should be inserted below.
#' If multiple values are specified and all codes are present in \code{info}, then the new code will be inserted below the match that is farthest down in the info.txt file.
#' @details
#' The \code{AddInfoLine} and \code{RemoveInfoLine} functions provide features to add/remove lines to an imported info.txt
#' file. Info.txt codes can be found on the \href{http://www.smhi.net/hype/wiki/doku.php?id=start:hype_file_reference:info.txt}{HYPE Wiki}.
#'
#' @return
#' \code{AddInfoLine} and \code{RemoveInfoLine} return a named list in the info.txt file structure.
#'
#' @examples
#' info <- ReadInfo(filename = system.file("demo_model",
#' "info.txt", package = "HYPEtools"))
#' info <- AddInfoLine(info, name = "testline", value = "testvalue")
#' info <- RemoveInfoLine(info, name = "testline")
#' 
#' @name InfoManipulation
NULL

#' @export AddInfoLine
#' @export RemoveInfoLine
#' @importFrom dplyr %>%

#' @rdname InfoManipulation
AddInfoLine <- function(info, name, value, after = NULL){
  # Replace value if element already exists
  if(name %in% names(info)){
    # Replace value in original location
    if(is.null(after)){
      info[name] <- value
      # Replace value and relocate
    } else{
      if(any(names(info) %in% after)){
        position <- max(which(names(info) %in% after)) # Place element after last "after" element line
        info <- info %>% append(c("newline" = value), after = position) # Add new element
        info <- info[which(!names(info) %in% name)] # Remove original element
        names(info)[which(names(info) == "newline")] <- name # Rename new item
      } else{
        info[name] <- value
        warning(paste0('Position Element(s) "', paste(after, collapse = "; "), '" not found in info. Replacing value for "', name, '" without relocating element.'), call. = FALSE)
      }
    }
    # Add new element
  } else{
    position <- max(0, which(names(info) %in% after)) # Place element at top of info file if "after" element doesn't exist
    info <- info %>% append(c("newline" = value), after = position)
    names(info)[which(names(info) == "newline")] <- name # Rename item
    
    # Warning if "after" not found
    if(position == 0){
      if(is.null(after)){
        warning(paste0('Element "', name, '" not found in info and no position specified using "after" argument. Placing line for "', name, '" at the top of info.'), call. = FALSE)
      } else{
        warning(paste0('Position Element(s) "', paste(after, collapse = "; "), '" not found in info. Placing line for "', name, '" at the top of info.'), call. = FALSE)
      }
    }
  }
  return(info)
}

#' @rdname InfoManipulation
RemoveInfoLine <- function(info, name){
  if(any(name %in% names(info))){
    info <- info[which(!names(info) %in% name)]
  } else{
    warning(paste0('Element(s) "', paste(name, collapse = "; "), '" not found in info'), call. = FALSE)
  }
  return(info)
}
