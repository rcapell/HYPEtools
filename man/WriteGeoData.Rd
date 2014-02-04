\name{WriteGeoData}
\alias{WriteGeoData}
\title{Write a 'GeoData.txt' file}
\usage{
WriteGeoData(x, filename = "GeoData.txt")
}
\arguments{
  \item{x}{The object to be written, a dataframe, as an
  object returned from \code{\link{ReadGeoData}}. No
  \code{NA}s allowed.}

  \item{filename}{Path to and file name of the GeoData file
  to import. Windows users: Note that Paths are separated
  by '/', not '\\'.}
}
\description{
This is a convenience wrapper function to export a
'GeoData.txt' file from R.
}
\details{
\code{WriteGeoData} exports a GeoData dataframe with
formatting options adjusted for the output to be read by
HYPE. HYPE does not allow empty values in any GeoData
column, the function will return with a warning if
\code{NA}s were exported (these are exported as "NA" to the
text file).
}
\examples{
\dontrun{WriteGeoData(x = mygeodata)}
}

