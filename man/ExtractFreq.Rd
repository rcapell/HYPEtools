\name{ExtractFreq}
\alias{ExtractFreq}
\title{Extract quantiles for use in a frequency distribution plot, e.g. a flow duration curve}
\usage{
ExtractFreq(data)
}
\arguments{
  \item{data}{either a numeric vector or an all-numeric
  dataframe (\code{NA}s allowed) which holds the variables
  for which quantiles are computed.}
}
\value{
\code{ExtractFreq} returns a dataframe with probabilities
in the first column, and quantiles of data in the following
columns. Number of observations per variable in \code{data}
are given in an attribute \code{n.obs} (see
\code{\link{attributes}}).
}
\description{
This function calculates quantiles suitable for duration
curves of environmental time series data.
}
\details{
\code{ExtractFreq} is a convenience wrapper function, it
uses \code{\link{quantile}} to calculate the quantiles of
one or more time series with a density appropriate for
duration curves. \code{NA}s are allowed in the input data.
For the results to be meaningful, input should represent
equally-spaced time series, e.g. HYPE basin output files.
}
\examples{
ExtractFreq(rnorm(1000))
}
\seealso{
\code{\link{PlotDurationCurve}}
}

