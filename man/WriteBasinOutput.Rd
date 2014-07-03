\name{WriteBasinOutput}
\alias{WriteBasinOutput}
\title{Write a basin output '[SUBID].txt' file}
\usage{
WriteBasinOutput(x, filename, dt.format = "\%Y-\%m-\%d")
}
\arguments{
  \item{x}{The object to be written, a dataframe with
  \code{unit} attribute, as an object returned from
  \code{\link{ReadBasinOutput}}.}

  \item{filename}{A character string naming a file to write
  to. Windows users: Note that Paths are separated by '/',
  not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}. Incomplete format strings for
  monthly and annual values allowed, e.g. '\%Y'.}
}
\description{
Function to export a basin output file from R.
}
\details{
\code{WriteBasinOutput} exports a dataframe with headers
and formatting options adjusted to match HYPE's basin
output files.
}
\examples{
\dontrun{WriteBasinOutput(x = mybasin, filename = "000001.txt")}
}
