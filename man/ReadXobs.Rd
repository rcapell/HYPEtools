\name{ReadXobs}
\alias{ReadXobs}
\title{Read an 'Xobs.txt' file}
\usage{
ReadXobs(filename = "Xobs.txt", dt.format = "\%Y-\%m-\%d", nrows = -1)
}
\arguments{
  \item{filename}{Path to and file name of the Xobs file to
  import. Windows users: Note that Paths are separated by
  '/', not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}.}

  \item{nrows}{Number of rows to import. A value of
  \code{-1} indicates all rows, a positive integer gives
  the number of rows to import.}
}
\value{
\code{ReadXobs} returns a data frame with three additional
attributes: \code{variable} and \code{subid} each
containing a vector with column-wise information (exept the
first column with date/time). A third attribute
\code{comment} contains the content of the Xobs file
comment row as single string.
}
\description{
This is a convenience wrapper function to import an Xobs
file as data frame into R.
}
\details{
\code{ReadXobs} is a convenience wrapper function of
\code{\link{read.table}}, with conversion of date-time
strings to POSIX time representations. Variable names,
SUBIDs, and comments are returned as attributes (see
\code{\link{attr}} on how to access these).
}
\note{
For the conversion of date/time strings, time zone "GMT" is
assumed. This is done to avoid potential daylight saving
time side effects when working with the imported data (and
possibly converting to string representations during the
process).
}
\examples{
\dontrun{ReadXobs("Xobs.txt")}
}

