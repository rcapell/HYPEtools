\name{PlotDurationCurve}
\alias{PlotDurationCurve}
\title{Plot a duration curve}
\usage{
PlotDurationCurve(freq, ylab = "m3s", print.n.obs = FALSE, ylim = NULL,
  xlab = "Flow exceedance percentile", col = "blue", ...)
}
\arguments{
  \item{freq}{Data frame with two columns, containing
  probabilities in the first column and data quantiles in
  the second. Typically an object as returned by
  \code{\link{ExtractFreq}}, or a subset thereof.}

  \item{ylab}{Character or \code{\link{plotmath}}
  expression string. Y-axis label, either as keyword
  \code{"m3s"} or \code{"mmd"} for pre-defined pretty
  discharge labels, or any other string which will be
  plotted unchanged.}

  \item{print.n.obs}{Logical. If \code{TRUE} and
  \code{freq} is a result from \code{\link{ExtractFreq}},
  the number of observations on which the quantiles are
  based will be plotted.}

  \item{ylim}{Numeric vector of length two, giving y-axis
  limits. \code{NULL} for default values.}

  \item{xlab}{Character string, x-axis label.}

  \item{col}{Line color specification, see
  \code{\link{par}} for details. Defaults to blue.}

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
with pretty defaults for axis annotation.
}
\details{


\code{PlotDurationCurve} plots a duration curve on a
log-scaled y-axis with pretty formatting defaults. The
function sets \code{\link{par}} parameters \code{mar},
\code{tcl}, and \code{mgp} internally and will override
previously set values for the returned plot.
}
\examples{
\dontrun{PlotDurationCurve(freq = myfreq, print.n.obs = T)}
}
\seealso{
\code{\link{ExtractFreq}}
}

