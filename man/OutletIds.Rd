\name{OutletIds}
\alias{OutletIds}
\title{Find Outlet IDs}
\usage{
OutletIds(gd)
}
\arguments{
  \item{gd}{A data frame, an imported 'GeoData.txt' file.
  Mandatory argument.}
}
\value{
\code{AllDownstreamSubids} returns a vector of outlet
SUBIDs.
}
\description{
Function to find all outlet IDs in a GeoData file. This is
typically just one number, but can be one or several SUBIDs
if the GeoData file originates from a HYPE sub-model
set-up, e.g. created with the 'SelectAro' tool.
}
\details{
\code{OutletSubids} finds all SUBIDs of a given SUBID
(including itself but not including potential irrigation
links or groundwater flows).
}
\examples{
/dontrun{OutletIds(gd = mygeodata)}
}

