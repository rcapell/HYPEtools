\name{DirectUpstreamSubids}
\alias{DirectUpstreamSubids}
\title{Find Direct Upstream SUBIDs, with Flow Fractions}
\usage{
DirectUpstreamSubids(subid = NULL, gd, bd = data.frame(BRANCHID = integer(),
  SOURCEID = integer(), MAINPART = numeric(), MAXQMAIN = numeric()))
}
\arguments{
  \item{subid}{SUBID of a target sub-catchment (must exist
  in \code{gd}), defaults to \code{NULL}. If
  non-\code{NULL}, \code{DirectUpstreamSubids} returns only
  the upstream SUBIDs for this sub-catchment, otherwise for
  all in \code{gd}.}

  \item{gd}{A data frame, an imported 'GeoData.txt' file.
  Mandatory argument. See 'Details'.}

  \item{bd}{A data frame, an imported 'BranchData.txt'
  file. Optional argument, defaults to an empty
  placeholder. See 'Details'.}
}
\value{
\code{DirectUpstreamSubids} always returns a \link{list}:
\itemize{ \item If argument \code{subid} is
non-\code{NULL}, returns a list with two elements:
\code{subid} contains the target SUBID, \code{upstr.df}
contains a data frame with columns \code{upstream}
(upstream SUBID), \code{is.main} (logical, \code{TRUE} if
it is a MAINDOWN connection), \code{fraction} (fraction of
flow going into the target SUBID), and \code{limit}
(maximum flow contribution to target SUBID).  \item
Otherwise returns a list with list elements as detailed
above. }
}
\description{
Function to find all SUBIDs of \bold{direct} upstream
sub-catchments for one or all sub-catchments in a GeoData
file, connected through 'maindown' and 'branchid' SUBIDs.
Includes extraction of the appropriate flow fractions for
each connection.
}
\details{
\code{DirectUpstreamSubids} matches \bold{direct} upstream
SUBIDs to a target SUBID, or all SUBIDs in an imported
GeoData file. Strictly speaking, it only requires a data
frame with tow columns 'SUBID' and 'MAINDOWN' as \code{gd},
and optionally a data frame with four columns 'BRANCHID',
'SOURCEID', 'MAINPART', and 'MAXQMAIN' as input, butwill
select these columns from imported files automatically.
}
\examples{
\dontrun{DirectUpstreamSubids(subid = 1, gd = mygeodata)}
}

