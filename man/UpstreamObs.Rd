\name{UpstreamObs}
\alias{UpstreamObs}
\title{Upstream forcing data averages}
\usage{
UpstreamObs(filename, subid, gd, bd = NULL, nr.obs = NULL, verbose = T)
}
\arguments{
  \item{filename}{Path to and file name of the forcing data
  text file to extract from. Windows users: Note that Paths
  are separated by '/', not '\\'.}

  \item{subid}{}

  \item{gd}{A data frame, containing 'SUBID' and 'MAINDOWN'
  columns, e.g. an imported 'GeoData.txt' file. Mandatory
  argument.}

  \item{bd}{A data frame, containing 'BRANCHID' and
  'SOURCEID' columns, e.g. an imported 'BranchData.txt'
  file. Optional argument. If provided, upstream areas will
  include areas linked through bifurcations.}

  \item{nr.obs}{Integer, number of rows in forcing data
  file. Optional argument, computed if \code{NULL}, which
  can be costly depending on file size. \code{UpstreamObs}
  prints a message}

  \item{verbose}{Logical, if set to \code{TRUE},
  information about the current computational status will
  be printed. NOT YET IMPLEMENTED.}
}
\value{
\code{UpstreamObs} returns a data frame with two columns,
first column \code{DATE} containing POSIX dates and second
column \code{meanobs} containing the area-weighted upstream
average forcing.
}
\description{
Calculate average upstream forcing time series for a single
SUBID from area-weighted upstream forcing data, given as
HYPE forcing data text file.
}
\details{
\code{UpstreamObs} reads potentially very large text files,
function calls can be time-consuming
}
\examples{
\dontrun{UpstreamObs(filename = "Pobs.txt", subid = 1000, gd = mygeodata)}
}

