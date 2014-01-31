\name{WritePar}
\alias{WritePar}
\title{Write a 'par.txt' File}
\usage{
WritePar(x, filename = "par.txt")
}
\arguments{
  \item{The}{object to be written, a list with named vector
  elements, as an object returned from
  \code{\link{ReadPar}}.}

  \item{A}{character string naming a file to write to.
  Windows users: Note that Paths are separated by '/', not
  '\\'.}
}
\description{
\code{WritePar} prints its required argument \code{x} to a
file.
}
\details{
\code{WritePar} writes a 'par.txt' file, typically
originating from an imported and modified 'par.txt'.
}
\examples{
\dontrun{WritePar(mypar)}
}

