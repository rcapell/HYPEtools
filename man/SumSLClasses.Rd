\name{SumSLClasses}
\alias{SumSLClasses}
\title{Calculate sums of SLC classes in a GeoData file}
\usage{
SumSLClasses(gd, plot.box = T, ...)
}
\arguments{
  \item{gd}{Data frame containing columns with SLC
  fractions, typically a 'GeoData.txt' file imported with
  \code{\link{ReadGeoData}}.}

  \item{plot.box}{Logical, if \code{TRUE}, a box plot of
  SLC area sums is returned.}

  \item{...}{Other arguments to be passed to
  \code{\link{boxplot}}.}
}
\value{
\code{SumSLClasses} a vector of SLC sums, invisibly if
\code{plot.box} is \code{TRUE}.
}
\description{
\code{SumSLClasses} sums all SLC classes for each SUBID in
a GeoData data frame and optionally plots the results.
}
\details{
\code{SumSLClasses} is a wrapper for \code{\link{colSums}}
with a boxplot output option, and allows to quickly control
if SLCs of all SUBIDs in a GeoData data frame sum up to 1.
}
\examples{
\dontrun{my.gd <- ReadGeoData("GeoData.txt")
SumSLClasses(gd = my.gd, plot.box = F)
}
}

