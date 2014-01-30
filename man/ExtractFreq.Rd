\name{ExtractFreq}
\alias{ExtractFreq}
\title{Extract quantiles for use in a frequency distribution plot, e.g. a flow duration curve}
\usage{
ExtractFreq(data)
}
\arguments{
  \item{data}{either a numeric vector or an all-numeric
  data frame (\code{NA}s allowed)}
}
\value{
\code{ExtractFreq} returns a list object with two elements:
\itemize{ \item probs.res: a dataframe with probabilities
(first column) and quantiles of indata (other columns)
\item no.obs: a numeric vector with no. of observations for
each indata column }
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
equally-spaced time series.
}
\examples{
ExtractFreq(rnorm(1000))
}

