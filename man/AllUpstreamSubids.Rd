\name{AllUpstreamSubids}
\alias{AllUpstreamSubids}
\title{Find All Upstream SUBIDs}
\usage{
AllUpstreamSubids(subid, gd, bd = NULL, write.arcgis = FALSE)
}
\arguments{
  \item{subid}{SUBID of a target sub-catchment (must exist
  in \code{gd}).}

  \item{gd}{A data frame, an imported 'GeoData.txt' file.
  Mandatory argument. See 'Details'.}

  \item{bd}{A data frame, an imported 'BranchData.txt'
  file. Optional argument.}

  \item{write.arcgis}{Logical. If \code{TRUE}, a string
  containing an SQL expression suitable for ArcGIS's
  'Select By Attributes' feature will be written to the
  clipboard. Works just for Windows.}
}
\value{
\code{AllUpstreamSubids} returns a vector of SUBIDs.
}
\description{
Function to find all SUBIDs of upstream sub-catchments for
a single sub-catchment in a GeoData file, connected through
'maindown' and 'branchid' SUBIDs, i.e the full upstream
catchment.
}
\details{
\code{AllUpstreamSubids} finds the upstream SUBIDs of a
given SUBID (including itself but not including potential
irrigation links or groundwater flows).
}
\examples{
\dontrun{AllUpstreamSubids(subid = 21, gd = mygeodata)}
}

