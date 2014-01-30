\name{ReadGeoData}
\alias{ReadGeoData}
\title{Read a 'GeoData.txt' file}
\usage{
ReadGeoData(filename = "GeoData.txt")
}
\arguments{
  \item{filename}{Path to and file name of the Xobs file to
  import. Windows users: Note that Paths are separated by
  '/', not '\\'.}
}
\value{
\code{ReadGeoData} returns a data frame.
}
\description{
This is a convenience wrapper function to import a GeoData
file as data frame into R.
}
\details{
\code{ReadGeoData} is just \code{read.table(file =
filename, header = T)}, mainly added to provide a
comparable function to the other RHYPE imports.
}
\examples{
\dontrun{ReadGeoData("GeoData.txt")}
}

