\name{AnnualRegime}
\alias{AnnualRegime}
\title{Calculate annual regimes}
\usage{
AnnualRegime(x)
}
\arguments{
  \item{x}{A dataframe with column-wise time series and
  date-times in \code{\link{POSIXct}} format, typically an
  imported basin or time output file from HYPE.}
}
\value{
\code{AnnualRegime} returns a list.
}
\description{
Calculate annual regimes based on long-term time series,
typically imported HYPE basin output and time output result
files.
}
\details{
\code{AnnualRegime} .
}
\examples{
\dontrun{AnnualRegime(x = mybasinoutput)}
}

