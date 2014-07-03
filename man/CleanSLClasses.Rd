\name{CleanSLClasses}
\alias{CleanSLClasses}
\title{Clean Soil-Landuse classes from small fractions}
\usage{
CleanSLClasses(gd, gc, m1.file = NULL, m1.class = "s",
  m1.clean = rep(TRUE, 2), m1.precedence = rep(TRUE, 2), m2.frac = NULL,
  m2.abs = NULL, verbose = T)
}
\arguments{
  \item{gd}{Data frame containing columns with SUBIDs,
  SUBID areas in m^2, and SLC fractions, typically a
  'GeoData.txt' file imported with
  \code{\link{ReadGeoData}}.}

  \item{gc}{Data frame containing columns with SLCs and
  corresponding soil and landuse group IDs, typically a
  'GeoClass.txt' file imported with
  \code{\link{ReadGeoClass}}.}

  \item{m1.file}{Character string, path and file name of
  the soil or landuse class transfer table, a tab-separated
  text file. Format see details.}

  \item{m1.class}{Character string, either "soil" or
  "landuse", can be abbreviated. Gives the type of transfer
  class table. See Details.}

  \item{m1.clean}{A logical vector of length 2 which
  indicates if cleaning should be performed for area
  fraction thresholds (position 1) and absolute area
  thresholds (position 2).}

  \item{m1.precedence}{A logical vector of length 2 which
  indicates if areas below cleaning threshold should be
  moved to similar areas according to precedence in the
  transfer table (\code{TRUE}) or to the largest area of
  available transfer classes (\code{FALSE}). For area
  fraction thresholds (position 1) and absolute area
  thresholds (position 2).}

  \item{m2.frac}{Numeric, area fraction threshold for
  method 2 cleaning, i.e. addition of small SLCs to largest
  SLC in each SUBID without considering similarity between
  classes. Either a single value or a vector of the same
  length as the number of SLC classes in \code{gd}, giving
  area fraction thresholds for each SLC seperately, with
  \code{0} for SLCs to omit. A value of \code{NULL}
  (default) prevents method 2 area fraction cleaning.}

  \item{m2.abs}{Numeric, see \code{m2.frac}. Threshold(s)
  for absolute areas in \eqn{m^{2}}{m^2}.}

  \item{verbose}{Logical, print some information during
  runtime.}
}
\value{
\code{CleanSLClasses} returns the GeoData data frame passed
to the function in argument \code{gd} with cleaned SLC
class columns.
}
\description{
\code{CleanSLClasses} attempts to clean small SLCs
fractions from an imported GeoData file. Cleaning can be
performed along class similarity rules or along SLC size.
}
\details{
\code{CleanSLClasses} performs a clean-up of small SLC
fractions in an imported GeoData file. Small SLCs are
eliminated either by moving their area to similar classes
according to rules which are passed to the function in a
text file (\emph{Method 1}), or by simply moving their area
to the largest SLC in the SUBID (\emph{Method 2}). Moving
rules for the first method can be based on either soil
classes or land use classes but these cannot be combined in
one function call. Run the function two times to combine
soil and land use based clean-up. Method 1 and 2, however,
can be combined in one function call, in which case the
rule-based classification will be executed first. Clean-up
precedence in method one: if clean-ups based on area
fractions and absolute areas are combined (\code{m1.clean =
rep(TRUE, 2)}), then area fractions will be cleaned first.
In order to reverse precedence, call \code{CleanSLClasses}
two times with absolute area cleaning activated in first
call and area fraction cleaning in second.

\bold{Method 1}

For method one, small SLC fractions are moved to either
similar land use classes within the same soil class, or
vice versa. Similarities are defined by the user in a
tab-separated text file, which is read by
\code{CleanSLClasses} during runtime. Soil and land use
classes correspond to the classes given in column two and
three in the \code{GeoClass} file. The file must have the
following format:

\tabular{cccccc}{
\emph{class.1}\tab\emph{thres.frac.1}\tab\emph{thres.abs.1}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.n}\cr
\emph{class.2}\tab\emph{thres.frac.2}\tab\emph{thres.abs.2}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.o}\cr
\emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\tab\emph{...}\cr
\emph{class.m}\tab\emph{thres.frac.m}\tab\emph{thres.abs.m}\tab\emph{transfer.1}\tab\emph{...}\tab\emph{transfer.p}\cr
}

Column 1 contains the source land use or soil classes
subjected to clean-up, columns 2 and 3 contain threshold
values for area fractions and absolute areas. The remaining
columns contain classes to which areas below threshold will
be transferred, in order of precedence. Each class can have
one or several transfer classes. No header is allowed. At
least one transfer class must exist, but classes can be
omitted and will then be ignored by \code{CleanSLClasses}.
The order of transfer classes indicates transfer
preference. \code{CleanSLClasses} constructs a transfer
list for each SLC class in the model set-up and uses the
order to choose a preferred SLC to transfer to. However, if
several SLCs exist for a given soil or land use class, one
of them will be chosen without further sorting. Area
fraction thresholds are given as fractions of 1, absolute
area thresholds as values in \eqn{m^{2}}{m^2}. If an area
below threshold is identified but there are no fitting SLCs
available to transfer to, the area will remain unchanged.

\bold{Method 2}

This method is more rigid than method one and is likely to
be applied as a post-processor after clean-up using method
one.

(\code{m1.clean = c(FALSE, TRUE)}).  \code{\link{apply}}
SLCs in each SUBID are cleaned iteratively in numerical
order, starting with SLC_1. This implies a greater
likelihood of elimination for
}
\examples{
\dontrun{my.gd <- ReadGeoData("GeoData.txt")
my.gc <- ReadGeoClass("GeoClass.txt")
CleanSLClasses()
apply(data[, -c(1:2)], 2, ConvertDischarge, data$AREA)
}
}

