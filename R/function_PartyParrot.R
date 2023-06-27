#' Create a Party Parrot.
#'
#' Creates a Party Parrot.
#' 
#' @param sound Character string or number specifying which sound to play when showing the Party Parrot. See the \code{beep} function in the \code{beepr} package.
#'
#' @details
#' \code{PartyParrot} generates a Party Parrot. Uses for Party Parrots include, for example, celebrating the successful execution of a script.
#'
#' @return
#' Returns a Party Parrot to the Console.
#'
#' @examples
#' PartyParrot()
#'
#' @export

PartyParrot <- function(sound = 8) {

  # Check/Load Dependencies - do this here so that these packages are not required for the base HYPEtools installation
  if (!all(
    requireNamespace("beepr", quietly = TRUE)
  )) {
    # Warn that a dependency is not installed
    stop('To use this function, please ensure that the following packages are installed: c("beepr")', call.=FALSE)

    # Perform function
  } else {
    
    # Set Message Rows
    od <- c(17, 6, 21, 5, 12, 9, 1, 15, 3, 18, 10, 7, 11, 14, 22, 20, 19, 16, 13, 8, 4, 2)
    
    # Create Message
    msg <- c("            #KH*KKNp|||||||||||||||||||||||||||||||||||*TMKKNwp", 
             "              ]KKL||KKKN||]KKbbbbbb#K#KKKH]KH", "             1KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKw", 
             "               ,#KRL$#N@||L#KKDBDMKKNL#Np1KN", "           BKH||||||||||||||*KKHP#KN||||||||||||]KN", 
             "            KKL|||||||||||4KNbbbbbKBKH|||||||?KKp", "                          ,wwwwwwp,", 
             "            1KK@||||||||||||||||||||||||||||||||KKNmw", "                   y#KMSL|||||||||||*TKKN,", 
             "            ]KH||*RKKNpL|||||||||||||||||||||||||||||||||||L*MKKNw,", 
             "           ]KN|||||||||||||KK@bbbb]KK|||||||||LVKW", "             ]KKL|||*MML||]KKbbbbbb]KHMMH|*KKp", 
             "           #KH|||||||||||||*KN@bb0KKH|||||||||||$Kp", "            KN@||||||||||||||||?KH|||||||||||||LKK", 
             "", "             KN|||||||||L*2TL||||||||||||||||||||||||||||||||||||||LTKKNw", 
             "            ]KH|||||L5KKKNNWp||||||||||||||||||||||||||||||||||L*MKKNw", 
             "            ]KKKN@|||||||||||||||||||||||||||||||L**RKKKWw", 
             "           ]KH|||||||||||||||L9KKKN|||||||||||||]KM", "            jKKL||||||||||]KKbbbbbb]KH|||||*9KW", 
             "                 zKKRL||||||,a#WWNpL|||*KKN", "                      a#KKMMMRT*MMKKKWw"
    )
    
    # Create Party Parrot
    beepr::beep(sound, expr = cat(msg[order(od)], sep = "\n"))
  }
}
