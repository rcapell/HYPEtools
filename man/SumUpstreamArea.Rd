\name{SumUpstreamArea}
\alias{SumUpstreamArea}
\title{Calculate upstream area sums}
\usage{
SumUpstreamArea(subid = NULL, gd, bd = NULL, progbar = F)
}
\arguments{
  \item{subid}{Integer vector of SUBIDs to calculate
  upstream areas for (must exist in \code{gd}). If
  \code{NULL}, upstream areas for all SUBIDs will be
  calculated.}

  \item{gd}{A data frame containing a column with SUBIDs
  and a column with areas, an imported 'GeoData.txt' file.}

  \item{bd}{A data frame, an imported 'BranchData.txt'
  file. Optional argument.}

  \item{progbar}{Logical, display a progress bar while
  calculating upstream areas. Adds overhead to calculation
  time but useful when \code{subid} is \code{NULL} or
  contains many SUBIDs.}
}
\value{
\code{SumUpstreamAreas} returns a data frame with two
columns containing SUBIDs and total upstream areas (in area
units as provided in \code{gd}). Upstream areas include
areas of outlet SUBIDs.
}
\description{
Function to calculate upstream areas of a vector of SUBIDs
or all SUBIDs in a GeoData table.
}
\details{
\code{SumUpstreamAreas} sums upstream areas of all
connected upstream SUBIDs, including branch connections in
case of stream bifurcations but not including potential
irrigation links or groundwater flows.
}
\examples{
\dontrun{SumUpstreamAreas(gd = mygeodata, progbar = T)}
}
\seealso{
\code{\link{AllUpstreamSubids}}
}
