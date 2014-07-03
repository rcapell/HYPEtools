\name{WriteTimeOutput}
\alias{WriteTimeOutput}
\title{Write a 'timeXXXX.txt' file}
\usage{
WriteTimeOutput(x, filename, dt.format = "\%Y-\%m-\%d")
}
\arguments{
  \item{x}{The object to be written, a dataframe with
  \code{comment} and \code{subid} attributes, as an object
  returned from \code{\link{ReadTimeOutput}}.}

  \item{filename}{A character string naming a file to write
  to. Windows users: Note that Paths are separated by '/',
  not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}. Incomplete format strings for
  monthly and annual values allowed, e.g. '\%Y'.}
}
\description{
Function to export a time output file from R.
}
\details{
\code{WriteTimeOutput} exports a dataframe with headers and
formatting options adjusted to match HYPE's time output
files.
}
\examples{
\dontrun{WriteTimeOutput(x = myCCTN, filename = "timeCCTN.txt")}
}
