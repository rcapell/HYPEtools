% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_OptimisedClasses.R
\name{OptimisedClasses}
\alias{OptimisedClasses}
\alias{OptimizedClasses}
\title{Get optimized classes from an imported optpar.txt file}
\usage{
OptimisedClasses(x)
}
\arguments{
\item{x}{list with named elements, as an object returned from \code{\link{ReadOptpar}}.}
}
\value{
\code{OptimisedClasses} returns a named list with one vector element for each parameter found in \code{x}. List element
names are HYPE parameter names. Each vector contains the optimized class numbers for the respective parameter.
}
\description{
\code{OptimisedClasses} checks which classes (land use or soil) of parameters in an imported optpar list are actually
optimized, i.e. have a min/max range larger than zero.
}
\details{
\code{OptimisedClasses} allows to quickly check which classes of parameters in an optpar.txt file are actually optimized
during a HYPE optimization run. The function compares min and max values in the \code{pars} element of an imported HYPE
optpar.txt file to identify those.
}
\examples{
te <- ReadOptpar(filename = system.file("demo_model", "optpar.txt", package = "HYPEtools"))
OptimisedClasses(te)

}
