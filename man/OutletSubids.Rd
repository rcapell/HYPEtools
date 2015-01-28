\name{OutletSubids}
\alias{OutletSubids}
\title{Find all Outlet SUBIDs of a model domain}
\usage{
OutletSubids(gd)
}
\arguments{
  \item{gd}{A data frame, with two columns \code{subid} and
  \code{maindown}, (not case-sensitive). Typically a
  'GeoData.txt' file imported using
  \code{\link{ReadGeoData}}.}
}
\value{
\code{OutletSubids} returns a vector of outlet SUBIDs.
}
\description{
Function to find all outlet SUBIDs of a HYPE model domain.
}
\details{
\code{OutletSubids} finds all outlet SUBIDs of a model
domain as provided in a 'GeoData.txt' file, i.e. all SUBIDs
from which stream water leaves the model domain.
}
\examples{
\dontrun{OutletSubids(gd = mygeodata)}
}
\seealso{
\code{\link{AllDownstreamSubids}}, \code{\link{OutletIds}}
}
