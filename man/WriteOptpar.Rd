% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functioncollection_export.R
\name{WriteOptpar}
\alias{WriteOptpar}
\title{Write an 'optpar.txt' File}
\usage{
WriteOptpar(x, filename, digits = 10, nsmall = 1)
}
\arguments{
\item{x}{The object to be written, a list with named elements, as an object returned from \code{\link{ReadOptpar}}.}

\item{filename}{A character string naming a file to write to. Windows users: Note that
Paths are separated by '/', not '\\'.}

\item{digits}{Integer, number of significant digits to export. See \code{\link{format}}.}

\item{nsmall}{Integer, number of significant decimals to export. See \code{\link{format}}.}
}
\value{
No return value, called for export to text files.
}
\description{
\code{WriteOptpar} prints a HYPE parameter optimization list to a file.
}
\examples{
te <- ReadOptpar(filename = system.file("demo_model", "optpar.txt", package = "HYPEtools"))
WriteOptpar(x = te, filename = tempfile())

}
\seealso{
\code{\link{ReadOptpar}} with a description of the expected content of \code{x}.
}
