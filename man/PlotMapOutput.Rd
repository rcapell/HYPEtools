\name{PlotMapOutput}
\alias{PlotMapOutput}
\title{Plot function for HYPE map results.}
\usage{
PlotMapOutput(data, map, map.subid.column, var.name = "", plot.scale = T,
  plot.legend = T, legend.pos = "right", legend.title = "",
  legend.inset = c(0, 0), col.ramp.fun = "auto", col.breaks = NA,
  par.mar = rep(0, 4) + 0.1)
}
\arguments{
  \item{data}{Map results, data frame object with two
  columns, first column containing SUBIDs and second column
  containing model results. See details.}

  \item{map}{A \code{SpatialPolygonsDataFrame} object,
  typically an imported SUBID map, requires package
  \code{rgdal}.}

  \item{map.subid.column}{Integer, index of the column in
  the \code{map} data slot that holds the SUBID indices.}

  \item{var.name}{Character string. HYPE variable name to
  be plotted. Mandatory for automatic color ramp selection
  of pre-defined HYPE variables (\code{col.ramp.fun =
  "auto"}). Not case-sensitive. See details.}

  \item{plot.scale}{Logical, plot a scale bar and a North
  arrow in the lower right corner. NOTE: works only with
  projected maps based on meter units, not geographical
  projections}

  \item{plot.legend}{Logical, plot a legend along with the
  map. Uses function \code{\link{legend}}.}

  \item{legend.pos}{Legend position, keyword string. One of
  \code{"left"}, \code{"topleft"}, \code{"top"},
  \code{"topright"}, \code{"right"}, \code{"bottomright"},
  \code{"bottom"}, \code{"bottomleft"}.}

  \item{legend.title}{Character string. An optional title
  for the legend.}

  \item{legend.inset}{Inset distance(s) from the margins as
  a fraction of the plot region. See \code{\link{legend}}
  and details below.}

  \item{col.ramp.fun}{Color ramp palette to use for the
  map. One of the following: \itemize{ \item \code{"auto"}
  to allow for automatic selection from pre-defined color
  ramp palettes based on argument \code{var.name} \item One
  of the pre-defined palette functions for HYPE output
  variables or user-calculated differences between model
  results:

  \code{"ColNitr"} for nitrogen, \code{"ColPhos"} for
  phosphorus, \code{"ColPrec"} for precipitation,
  \code{"ColTemp"} for temperatures, \code{"ColQ"} for
  runoff, \code{"ColDiffTemp"} for temperature differences,
  \code{"ColDiffGeneric"} for generic differences (see
  details) \item A color ramp palette function, e.g. as
  returned from a call to \code{\link{colorRampPalette}} }}

  \item{col.breaks}{Break points for discretisation of
  model result values into classes. Numeric vector of break
  points, will be interpreted as right-closed, i.e upper
  boundaries included in class. Lowest class boundary
  included in lowest class as well. Meaningful results
  require the lowest and uppermost breaks to bracket all
  model result values, otherwise there will be unclassified
  white spots on the map plot. Only necessary if a
  user-defined \code{col.ramp.fun} is supplied (otherwise a
  generic classification with 10 classes based on 10%
  quantiles will be applied), but can optionally be
  combined with one of the pre-defined palettes.}

  \item{par.mar}{Plot margins as in \code{\link{par}}
  argument \code{mar}. Defaults to a nearly margin-less
  plot.}
}
\description{
Draw HYPE map results, with pretty scale discretisations
and color ramp defaults.
}
\details{
\code{PlotMapOutput} plots HYPE results from 'map[variable
name].txt' files, typically imported using
\code{\link{ReadMapOutput}}. \code{data} arguments
\strong{must} contain the variable of interest in the
second column. For multicolumn map results, i.e. with
several time periods, pass index selections to \code{data},
e.g. \code{mymapresult[, c(1, 3)]}.

Mapped variables are visualised using color-coded data
intervals. \code{PlotMapOutput} uses several in-built color
ramp functions suitable for some common HYPE result
variables as listed under argument \code{col.ramp.fun}. and
provides

Legends are positioned by keyword, defaulting to the right
side of the map. Depending on the form of the displayed
area and the size of the legend, there might be no optimal
place inside the plot area. In that case, the legend can be
moved to the outer margins by specifying negative inset
values for details on inset specification see \code{inset}
in \code{\link{legend}}. Make sure to also adapt the plot
margin sizes appropriately through \code{par.mar}, see
\code{mar} in \code{\link{par}}.
}
\examples{
\dontrun{require(rgdal)
PlotMapOutput(data = mymapresult, map = readOGR(dsn = "../gisdata", layer = "myHYPEsubids", var.name = "CCTN"))}
}

