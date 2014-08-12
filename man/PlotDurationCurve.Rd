\name{PlotDurationCurve}
\alias{PlotDurationCurve}
\title{Plot a duration curve}
\usage{
PlotDurationCurve(freq, xscale = "lin", yscale = "log",
  print.n.obs = FALSE, ylim = NULL, xlab = "Flow exceedance percentile",
  ylab = "m3s", col = "blue", mar = c(3, 3, 1, 1) + 0.1, ...)
}
\arguments{
  \item{freq}{Data frame with two columns, containing
  probabilities in the first column and data quantiles in
  the second. Typically an object as returned by
  \code{\link{ExtractFreq}}, or a subset thereof.}

  \item{xscale}{Character string, keyword for x-axis
  scaling. Either \code{"lin"} for linear scaling or
  \code{"gauss"} for gaussian scaling as in a normal
  probability plot, which allows for for better comparison
  of low flow and high flow frequencies.}

  \item{yscale}{Character string, keyword for y-axis
  scaling. Either \code{"lin"} for linear scaling or
  \code{"log"} for common logarithm scaling.}

  \item{print.n.obs}{Logical. If \code{TRUE} and
  \code{freq} is a result from \code{\link{ExtractFreq}},
  the number of observations on which the quantiles are
  based will be plotted on the outer margin.}

  \item{ylim}{Numeric vector of length two, giving y-axis
  limits. \code{NULL} for default values.}

  \item{xlab}{Character string, x-axis label.}

  \item{ylab}{Character or \code{\link{plotmath}}
  expression string. Y-axis label, either as keyword
  \code{"m3s"} or \code{"mmd"} for pre-defined pretty
  discharge labels, or any other string which will be
  plotted unchanged.}

  \item{col}{Line color specification, see
  \code{\link{par}} for details. Defaults to blue.}

  \item{mar}{Numeric vector of length 4, margin
  specification as in \code{\link{par}} with modified
  default. Details see there.}

  \item{...}{Further graphical parameters (see
  \code{\link{par}}) to be supplied as arguments to
  \code{\link{lines}}, e.g. line type and width, for
  details see there.}
}
\value{
\code{PlotDurationCurve} returns a plot to the currently
active plot device.
}
\description{
Convenience wrapper function for a line \code{\link{plot}},
with pretty defaults for axis annotation and a Gaussian
scaling option for the x-axis.
}
\details{


\code{PlotDurationCurve} plots a duration curve with pretty
formatting defaults. The function sets \code{\link{par}}
parameters \code{tcl} and \code{mgp} internally and will
override previously set values for the returned plot. It
typically uses results from \code{\link{ExtractFreq}} as
input data and via that function it can be used to
visualise and compare time series properties.
}
\examples{
\dontrun{PlotDurationCurve(freq = myfreq, print.n.obs = T)}
}
\seealso{
\code{\link{ExtractFreq}}
}

