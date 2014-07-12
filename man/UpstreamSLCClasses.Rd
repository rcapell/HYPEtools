\name{UpstreamSLCClasses}
\alias{UpstreamSLCClasses}
\title{Calculate SLC class fractions of all upstream areas}
\usage{
UpstreamSLCClasses(subid = NULL, gd, bd = NULL, signif.digits = 3,
  progbar = T)
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

  \item{signif.digits}{Integer, number of significant
  digits to round upstream SLCs to. See also
  \code{\link{signif}}. Set to \code{NULL} to prevent
  rounding.}

  \item{progbar}{Logical, display a progress bar while
  calculating SLC class fractions. Adds overhead to
  calculation time but useful when \code{subid} is
  \code{NULL} or contains many SUBIDs.}
}
\value{
\code{UpstreamSLCClasses} returns a data frame with columns
containing SUBIDs, total upstream areas (in area unit as
provided in \code{gd}), and SLC class fractions for
upstream areas.
}
\description{
Function to calculate SLC class fractions over all upstream
areas of a vector of SUBIDs or all SUBIDs in a GeoData
table.
}
\details{
\code{UpstreamSLCClasses} sums upstream areas of all
connected upstream SUBIDs, including branch connections in
case of stream bifurcations but not including potential
irrigation links or groundwater flows.
}
\examples{
\dontrun{UpstreamSLCClasses(subid = 21, gd = mygeodata, bd = mybranchdata)}
}
\seealso{
\code{\link{SumUpstreamAreas}}
}

