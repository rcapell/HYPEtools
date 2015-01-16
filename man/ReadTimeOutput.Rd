\name{ReadTimeOutput}
\alias{ReadTimeOutput}
\title{Read a Time Output File}
\usage{
ReadTimeOutput(filename, dt.format = "\%Y-\%m-\%d")
}
\arguments{
  \item{filename}{Path to and file name of the time output
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}

  \item{dt.format}{Date-time \code{format} string as in
  \code{\link{strptime}}. Incomplete format strings for
  monthly and annual values allowed, e.g. '\%Y'. If set to
  \code{NULL}, no date-time conversion will be attempted
  and the column will be imported as \code{character},
  applicable e.g. for files containing just one row of
  summary values over the model period.}
}
\value{
\code{ReadTimeOutput} returns a data frame. Information on
the output variable is stored in attribute \code{comment},
a vector of subid integers in attribute \code{subid}.
}
\description{
This is a convenience wrapper function to import a time
output file as data frame or matrix into R.
}
\details{
\code{ReadTimeOutput} is a convenience wrapper function of
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

HYPE results are printed to files using a user-specified
accuracy. This accuracy is specified in 'info.txt' as a
number of decimals to print. If large numbers are printed,
this can result in a total number of digits which is too
large to print. Results will then contain values of
'****************'. \code{ReadTimeOutput} will convert
those cases to 'NaN' entries and throw a warning.
}
\examples{
\dontrun{ReadTimeOutput("timeCCIN.txt, dt.format = "\%Y-\%m"")}
}

