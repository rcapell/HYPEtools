\name{WriteGeoClass}
\alias{WriteGeoClass}
\title{Write a 'GeoClass.txt' file}
\usage{
WriteGeoClass(x, filename = "GeoClass.txt")
}
\arguments{
  \item{x}{The object to be written, a dataframe, as an
  object returned from \code{\link{ReadGeoClass}}.}

  \item{filename}{A character string naming a file to write
  to. Windows users: Note that Paths are separated by '/',
  not '\\'.}
}
\description{
This is a convenience wrapper function to export a
'GeoClass.txt' file from R.
}
\details{
\code{WriteGeoClass} exports a GeoClass dataframe with
formatting options adjusted for the output to be read by
HYPE. HYPE accepts comment rows with a leading '!' in the
beginning rows of a GeoClass file. The export function
looks for those in \code{attribute} 'comment', where
\code{\link{ReadGeoClass}} stores such comments.
}
\examples{
\dontrun{WriteGeoClass(x = mygeoclass)}
}

