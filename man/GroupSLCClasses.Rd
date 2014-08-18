\name{GroupSLCClasses}
\alias{GroupSLCClasses}
\title{Calculate grouped sums for SLC classes in a GeoData file}
\usage{
GroupSLCClasses(gd, gc = NULL, type = "landuse", group = NULL,
  abs.area = FALSE, verbose = T)
}
\arguments{
  \item{gd}{Data frame containing columns with SUBIDs, SLC
  fractions, and SUBID areas if \code{abs.area = TRUE}.
  Typically a 'GeoData.txt' file imported with
  \code{\link{ReadGeoData}}.}

  \item{gc}{Data frame containing columns with SLCs and
  corresponding landuse and soil class IDs, typically a
  'GeoClass.txt' file imported with
  \code{\link{ReadGeoClass}}. Must be provided if no
  \code{group} argument is given.}

  \item{type}{Keyword character string for use with
  \code{gc}. Type of grouping index, either
  \code{"landuse"} or \code{"soil"}, can be abbreviated.}

  \item{group}{Integer vector, of same length as number of
  SLC classes in \code{gd}. Alternative grouping index
  specification to \code{gc} + \code{type}.}

  \item{abs.area}{Logical, if \code{TRUE}, absolute areas
  will be calculated for eatch group, rather than area
  fractions.}

  \item{verbose}{Logical, if \code{TRUE} information and
  progress bar will be printed.}
}
\value{
\code{GroupSLClasses} returns the data frame with SUBIDs
and grouped SLC class columns.
}
\description{
\code{GroupSLCClasses} calculates grouped sums for SLC
classes (area fractions or absolute areas) based on land
use or soil groups in a GeoClass table, or any other
user-provided grouping index.
}
\details{
If absolute areas are calculated, area units will
correspond to areas provided in \code{gd}.
}
\examples{
\dontrun{
GroupSLCClasses(gd = mygeodata, gc = mygeoclass, type = "s")
}
}

