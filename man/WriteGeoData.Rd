\name{WriteGeoData}
\alias{WriteGeoData}
\title{Write a 'GeoData.txt' file}
\usage{
WriteGeoData(x, filename = "GeoData.txt")
}
\arguments{
  \item{x}{The object to be written, a dataframe, as an
  object returned from \code{\link{ReadGeoData}}. No
  \code{NA}s in parameter values allowed.}

  \item{filename}{A character string naming a file to write
  to. Windows users: Note that Paths are separated by '/',
  not '\\'.}
}
\description{
This is a convenience wrapper function to export a
'GeoData.txt' file from R.
}
\details{
\code{WriteGeoData} exports a GeoData dataframe with
formatting options adjusted for the output to be read by
HYPE. HYPE does neither allow empty values in any GeoData
column nor any string elements with more than 50
characters, the function will return with warnings if
\code{NA}s or long strings were exported. (these are
exported as "NA" to the text file).
}
\examples{
\dontrun{WriteGeoData(x = mygeodata)}
}
