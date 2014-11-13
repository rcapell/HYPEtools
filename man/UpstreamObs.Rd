\name{UpstreamObs}
\alias{UpstreamObs}
\title{Upstream forcing data averages}
\usage{
UpstreamObs(filename, subid, gd, bd)
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
  file. Optional argument.}
}
\value{
\code{MergeXobs} returns a data frame with attributes for
Xobs data.
}
\description{
Calculate average upstream forcing for a single SUBID
}
\details{
\code{MergeXobs} handles time steps of different lengths
(e.g. daily, hourly), but requires identical time step
lengths from both input data frames.
}
\examples{
\dontrun{MergeXobs(x = myxobs1, y = myxobs2)}
}

