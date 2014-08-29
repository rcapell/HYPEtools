\name{OutletSubids}
\alias{OutletSubids}
\title{Find Outlet SUBIDs}
\usage{
OutletSubids(gd)
}
\arguments{
  \item{gd}{A data frame, an imported 'GeoData.txt' file.
  Mandatory argument. See 'Details'.}
}
\value{
\code{AllDownstreamSubids} returns a vector of outlet
SUBIDs.
}
\description{
Function to find all outlet SUBIDs in a GeoData file. This
is typically just one number
}
\details{
\code{OutletSubids} finds all SUBIDs of a given SUBID
(including itself but not including potential irrigation
links or groundwater flows).
}
\examples{
/dontrun{OutletSubids(gd = mygeodata)}
}

