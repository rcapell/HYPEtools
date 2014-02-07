\name{ConvertDischarge}
\alias{ConvertDischarge}
\title{Calculate Specific Discharge}
\usage{
ConvertDischarge(q, area)
}
\arguments{
  \item{q}{An object of type \code{numeric}, containing
  volumetric discharge in \eqn{m^3 s^{-1}}, typically HYPE
  variables COUT or ROUT.}

  \item{area}{An object of type \code{numeric}, containing
  a catchment area in \eqn{m^2}.}
}
\description{
\code{ConvertDischarge} converts volumetric discharge to
specific discharge from a unit area.
}
\details{
\code{ConvertDischarge} is a simple conversion function,
most likely to be used in combination with
\code{\link{apply}} or related functions. It converts input
discharge from \eqn{m^3 s^{-1}} to \eqn{mm y^{-1}}
}
\examples{
\dontrun{WritePar(mypar)}
}

