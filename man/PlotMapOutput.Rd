\name{PlotMapOutput}
\alias{PlotMapOutput}
\title{Plot function for HYPE map results.}
\usage{
PlotMapOutput(data, map, map.subid.column = 1, var.name = "",
  plot.scale = T, plot.legend = T, legend.pos = "right",
  legend.title = NULL, legend.inset = c(0, 0), col.ramp.fun = "auto",
  col.breaks = NULL, par.mar = rep(0, 4) + 0.1)
}
\arguments{
  \item{data}{HYPE map results, data frame object with two
  columns, first column containing SUBIDs and second column
  containing model results. See details.}

  \item{map}{A \code{SpatialPolygonsDataFrame} object.
  Typically an imported SUBID shape file, shape file import
  requires package \code{rgdal}.}

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

  \item{legend.title}{Character string or mathematical
  expression. An optional title for the legend. If none is
  provided here, \code{var.name} is used as legend title
  string. For select HYPE variables, pretty legend titles
  are in-built.}

  \item{legend.inset}{Numeric, inset distance(s) from the
  margins as a fraction of the plot region. See
  \code{\link{legend}} and details below.}

  \item{col.ramp.fun}{Color ramp palette to use for the
  map. One of the following: \itemize{ \item \code{"auto"}
  to allow for automatic selection from pre-defined color
  ramp palettes and break points based on argument
  \code{var.name}, see details \item One of the pre-defined
  palette functions for HYPE output variables or
  user-calculated differences between model results:
  \code{"ColNitr"} for nitrogen, \code{"ColPhos"} for
  phosphorus, \code{"ColPrec"} for precipitation,
  \code{"ColTemp"} for temperatures, \code{"ColQ"} for
  runoff, \code{"ColDiffTemp"} for temperature differences,
  \code{"ColDiffGeneric"} for generic differences, see
  details \item A color ramp palette function, e.g. as
  returned from a call to \code{\link{colorRampPalette}} }}

  \item{col.breaks}{A numeric vector, specifying break
  points for discretisation of model result values into
  classes. Class boundaries will be interpreted as
  right-closed, i.e upper boundaries included in class.
  Lowest class boundary included in lowest class as well.
  Meaningful results require the lowest and uppermost
  breaks to bracket all model result values, otherwise
  there will be unclassified white spots on the map plot.
  Not mandatory, can optionally be combined with one of the
  pre-defined palettes, including \code{"auto"} selection.
  Per default, a generic classification will be applied
  (see details).}

  \item{par.mar}{Plot margins as in \code{\link{par}}
  argument \code{mar}. Defaults to a nearly margin-less
  plot.}
}
\description{
Draw HYPE map results, with pretty scale discretisations
and color ramp defaults for select HYPE variables.
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
intervals. \code{PlotMapOutput} can use one of several
internal color ramp functions suitable for some common HYPE
result variables as listed under argument
\code{col.ramp.fun}. These are called by keyword.
Alternatively, any color ramp function can be provided. The
internal color ramps are mostly single color ramps with
less saturated colors for smaller values and more saturated
values for higher values, suitable for e.g. concentration
or volume ranges. \code{ColTemp} is an exception to this
rule, with blue-to-turquoise colors and yellow-to-red
colors to represent temperature ranges below and above
zero. Two further ramps provided are suitable to represent
calculated differences, e.g. between two model runs, a
generic ramp with reds on the low end and blues on the high
end, and a temperature-specific one in reversed order.

Break points between color classes of in-built or
user-provided color ramp palettes can optionally be
provided in argument \code{col.breaks}. This is
particularly useful when specific pretty class boundaries
are needed, e.g. for publication figures. Per default,
break points for internal single color ramps and
user-provided ramps are calculated based on 10\%
percentiles of HYPE results given in \code{data}. Default
break points for internal color ramp
\code{"ColDiffGeneric"} are based on an equal distance
classification of log-scaled \code{data} ranges, centered
around zero. For internal color ramp \code{"ColDiffTemp"},
they are breaks in an interval from -7.5 to 7.5 K.

For select common HYPE variables, given in argument
\code{var.name}, an automatic color ramp selection
including pretty breaks and legend titles is built into
\code{PlotMapOutput}. These are 'CCTN', 'CCTP', 'COUT', and
'TEMP'. Automatic selection is activated by chosing keyword
\code{"auto"} in \code{col.ramp.fun}. All other HYPE
variables will be plotted using a generic color ramp
palette and generic break points.

Legends are positioned by keyword, defaulting to the right
side of the map. Depending on the form of the displayed
area and the size of the legend, there might be no optimal
place inside the plot area. In that case, the legend can be
moved to the outer margins by specifying negative
\code{legend.inset} values. For details on inset
specification see \code{inset} in \code{\link{legend}}.
Make sure to also adapt the plot margin sizes appropriately
through \code{par.mar}, see \code{mar} in
\code{\link{par}}.
}
\examples{
\dontrun{require(rgdal)
PlotMapOutput(data = mymapresult, map = readOGR(dsn = "../gisdata", layer = "myHYPEsubids"), map.subid.column = 2, var.name = "CCTN")}
}

