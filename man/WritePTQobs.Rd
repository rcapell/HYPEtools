\name{WritePTQobs}
\alias{WritePTQobs}
\title{Write 'Pobs.txt', 'Tobs.txt', or 'Qobs.txt' files}
\usage{
WritePTQobs(x, filename, dt.format = "\%Y-\%m-\%d", digits = 1,
  nsmall = 1, subid = NULL)
}
\arguments{
  \item{x}{The object to be written, a \code{dataframe}
  containing observation date-times in first and
  observations in SUBIDs in remaining columns, with an
  additional attribute \code{subid} containing SUBIDs in
  column order.}

  \item{filename}{Path to and file name of the file to
  import. Windows users: Note that Paths are separated by
  '/', not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}.}

  \item{subid}{Integer vector containing SUBIDs in same
  order as columns in \code{x}. To be exported as header in
  the obs file. Must contain the same number of SUBIDs as
  \code{x}. If \code{NULL}, an attribute \code{subid} in
  \code{x} is mandatory. An existing \code{subid} argument
  takes precedence over a \code{subid} attribute.}

  \item{digits}{Integer, number significant digits to
  export. See \code{\link{format}}.}

  \item{nsmall}{Integer, number of significant decimals to
  export. See \code{\link{format}}.}
}
\description{
Export precipitation, temperature, or discharge observation
files from R.
}
\details{
\code{WritePTQobs} is a convenience wrapper function of
\code{\link{write.table}} to export a HYPE compliant
observation file. headers are generated from attribute
\code{subid} on export (see \code{\link{attr}} on how to
create and access it).
}
\examples{
\dontrun{WritePTQobs(mytops, "Tobs.txt")}
}

