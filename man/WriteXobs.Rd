\name{WriteXobs}
\alias{WriteXobs}
\title{Write an 'Xobs.txt' File}
\usage{
WriteXobs(x, filename = "Xobs.txt", append = F, comment = NA,
  variable = NA, subid = NA, lastDate = NA, timestep = "d")
}
\arguments{
  \item{x}{A data frame, e.g. an object originally imported
  with \code{\link{ReadXobs}}. Date-time information in the
  first column and measured values in the remaining
  columns. Column names are ignored on export, but if
  attributes \code{comment}, \code{variable} , and
  \code{subid} are available, these can be exported as Xobs
  headers (see also arguments of the same names below).}

  \item{filename}{A character string naming a file to write
  to. Windows users: Note that Paths are separated by '/',
  not '\\'.}

  \item{append}{Logical. If \code{TRUE}, \code{x} will be
  appended to file \code{filename}. File must exist and
  have an identical column structure as \code{x}. If
  \code{FALSE}, existing file in \code{filename} will be
  overwritten!}

  \item{comment}{An object to be exported as first row
  comment in the Xobs file. Will take precedence over a
  \code{comment} attribute of \code{x}. Comments are only
  exported if \code{append} is \code{FALSE}.}

  \item{variable}{An character vector to be exported as
  second row in the Xobs file. Must contain the same number
  of variables as \code{x}. If omitted or \code{NA}, an
  attribute \code{variable} in \code{x} is mandatory. Will
  take precedence over a \code{variable} attribute of
  \code{x}. If \code{append} is \code{TRUE} the values are
  used to test for consistency between export object and
  the existing file.}

  \item{subid}{Third row in Xobs, containing SUBIDs
  (numeric). Behaviour otherwise as argument
  \code{variable}.}

  \item{lastDate}{Date-time of last observation in
  \code{\link{POSIXct}} format. See details.}

  \item{timestep}{Character string, either "daily" or
  "hourly", giving the time step between observations. Can
  be abbreviated.}
}
\description{
\code{WriteXobs} writes or appends an observation data set
to an Xobs file.
}
\details{
\code{WriteXobs} writes a 'Xobs.txt' file, typically
originating from an imported and modified 'Xobs.txt'. HYPE
Xobs files contain a three-row header, with a comment line
first, next a line of variables, and then a line of subids.
Objects imported with \code{\link{ReadXobs}} include
attributes holding this information, and \code{WriteXobs}
will use this information. Otherwise, these attributes can
be added to objects prior to calling code{WriteXobs}, or
passed as function arguments.

The function currenctly requires daily or hourly time steps
as input, when the \code{append} argument is \code{TRUE}.
The date-time column must be of class \code{POSIXct}, see
\code{\link{as.POSIXct}}. Objects returned from
\code{\link{ReadXobs}} per default have the correct class
for the date-time column. Argument \code{lastDate} gives
the last observation date-time in the existing 'Xobs' file
and must be of the same type. Create with e.g.
'as.POSIXct(strptime("1999-01-01", format = "%Y-%m-%d", tz
= "GMT"))'. Missing time steps between existing and new
observations will be added, with '-9999' values in all
columns. If time periods overlap, the export will stop with
an error message.
}
\note{
Both \code{variable} and \code{subid} do not include
elements for the first column in the Xobs file/object, in
accordance with \code{\link{ReadXobs}}. These elements will
be added by the function.

There is no automatic checking of last observation dates in
existing 'Xobs' files when appending because R internal
functions available are time-consuming for large files.
Google results for 'R read last line in text file' point to
platform-dependent solutions using e.g. 'tail', 'gawk', or
'wc'.
}
\examples{
\dontrun{WriteXobs(myxobs)}
}

