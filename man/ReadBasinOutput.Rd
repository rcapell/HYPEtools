\name{ReadBasinOutput}
\alias{ReadBasinOutput}
\title{Read a Basin Output File}
\usage{
ReadBasinOutput(filename, dt.format = "\%Y-\%m-\%d", outformat = "df")
}
\arguments{
  \item{filename}{Path to and file name of the basin output
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}

  \item{dt.format}{Date-time format string as in
  \code{\link{strptime}}. Incomplete format strings for
  monthly and annual values allowed, e.g. '\%Y'.}

  \item{outformat}{Format of the returned object. Character
  string, either \code{'dataframe'} or \code{'matrix'}, can
  be abbreviated.}
}
\value{
\code{ReadBasinOutput} returns a data frame or a matrix,
see argument 'outformat'. In the matrix case, date-time
information is converted to numeric POSIX representations
(seconds since 1970-01-01). This will lead to NAs if
Date-time conversion failed. Variable units ar imported as
string \code{attribute} 'unit'. If a matrix is returned,
this attribute will not be preserved.
}
\description{
This is a convenience wrapper function to import a basin
output file as data frame or matrix into R.
}
\details{
\code{ReadBasinOutput} is a convenience wrapper function of
\code{\link{read.table}}, with conversion of date-time
strings to POSIX time representations. Monthly and annual
time steps are returned as first day of the time step
period.
}
\note{
For the conversion of date/time strings, time zone "GMT" is
assumed. This is done to avoid potential daylight saving
time side effects when working with the imported data (and
possibly converting to string representations during the
process).
}
\examples{
\dontrun{ReadBasinOutput("0000001.txt")}
}

