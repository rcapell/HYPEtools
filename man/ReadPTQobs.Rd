\name{ReadPTQobs}
\alias{ReadPTQobs}
\title{Read 'Pobs.txt', 'Tobs.txt', or 'Qobs.txt' files}
\usage{
ReadPTQobs(filename, dt.format = "\%Y-\%m-\%d", nrows = -1)
}
\arguments{
  \item{filename}{Path to and file name of the file to
  import. Windows users: Note that Paths are separated by
  '/', not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}.}

  \item{nrows}{Number of rows to import. A value of
  \code{-1} indicates all rows, a positive integer gives
  the number of rows to import.}
}
\value{
\code{ReadPTQobs} returns a data frame with an additional
attribute \code{subid}.
}
\description{
Import precipitation, temperature, or discharge observation
files as data frame into R.
}
\details{
\code{ReadPTQobs} is a convenience wrapper function of
\code{\link{read.table}}, with conversion of date-time
strings to POSIX time representations. SUBIDs are returned
as integer attribute \code{subid} (see \code{\link{attr}}
on how to access it).

This function can only be used with reasonably small data
files. Attempts to read large files can lead to exceedance
of R's memory space.
}
\note{
For the conversion of date/time strings, time zone "GMT" is
assumed. This is done to avoid potential daylight saving
time side effects when working with the imported data (and
possibly converting to string representations during the
process).
}
\examples{
\dontrun{ReadPTQobs("Tobs.txt")}
}

