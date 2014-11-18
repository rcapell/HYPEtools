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
}
\value{
\code{MergeXobs} returns a data frame with attributes for
Xobs data.
}
\description{
Calculate average upstream forcing time series for a single
SUBID from area-weighted forcing in upstream subcatchments.
}
\details{
#

\code{UpstreamObs} reads potentially very large text files,
function calls can be time-consuming
}
\examples{
\dontrun{MergeXobs(x = myxobs1, y = myxobs2)}
}

