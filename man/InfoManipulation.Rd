% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_InfoManipulation.R
\name{InfoManipulation}
\alias{InfoManipulation}
\alias{AddInfoLine}
\alias{RemoveInfoLine}
\title{Functions to Manipulate HYPE Info Files}
\usage{
AddInfoLine(info, name, value, after = NULL)

RemoveInfoLine(info, name)
}
\arguments{
\item{info}{Named list containing the info.txt file data, typically created using \code{\link{ReadInfo}} with the \code{exact} mode.}

\item{name}{Name of info.txt code to add/remove.}

\item{value}{Value of the info.txt code to add/remove.}

\item{after}{String vector containing the name(s) of info.txt codes that the new info.txt code should be inserted below.
If multiple values are specified and all codes are present in \code{info}, then the new code will be inserted below the match that is farthest down in the info.txt file.}
}
\value{
\code{AddInfoLine} and \code{RemoveInfoLine} return a named list in the info.txt file structure.
}
\description{
Add/Remove lines to HYPE info.txt files
}
\details{
The \code{AddInfoLine} and \code{RemoveInfoLine} functions provide features to add/remove lines to an imported info.txt
file. Info.txt codes can be found on the \href{http://hype.smhi.net//wiki/doku.php?id=start:hype_file_reference:info.txt}{HYPE Wiki}.
}
\examples{
info <- ReadInfo(filename = system.file("demo_model",
"info.txt", package = "HYPEtools"))
info <- AddInfoLine(info, name = "testline", value = "testvalue")
info <- RemoveInfoLine(info, name = "testline")

}
