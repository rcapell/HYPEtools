% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_hydroGOF.R
\name{NSE.HypeSingleVar}
\alias{NSE.HypeSingleVar}
\title{Nash-Sutcliffe Efficiency}
\usage{
\method{NSE}{HypeSingleVar}(sim, obs, na.rm = TRUE, progbar = TRUE, ...)
}
\arguments{
\item{sim}{\code{\link{HypeSingleVar}} array with simulated variable (one or several iterations).}

\item{obs}{\code{\link{HypeSingleVar}} array with observed variable, (one iteration). If several iterations are present
in the array, only the first will be used.}

\item{na.rm}{Logical. If \code{TRUE}, incomplete sim-obs pairs will be removed prior to NSE computation.}

\item{progbar}{Logical, if \code{TRUE} progress bars will be printed for main computational steps.}

\item{...}{ignored}
}
\value{
\code{NSE.HypeSingleVar} returns a 2-dimensional array of NSE performances for all SUBIDs and model iterations provided in
argument \code{sim}, with values in the same order
as the second and third dimension in \code{sim}, i.e. \code{[subid, iteration]}.
}
\description{
Nash-Sutcliffe Efficiency calculation for imported HYPE outputs with single variables for several catchments, i.e. time and
map files, optionally multiple model run iterations combined.
}
\examples{
# Create dummy data, discharge observations with added white noise as model simulations
te1 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
te1 <- HypeSingleVar(x = array(data = unlist(te1[, -1]) + 
                                      runif(n = nrow(te1), min = -.5, max = .5), 
                               dim = c(nrow(te1), ncol(te1) - 1, 1), 
                               dimnames = list(rownames(te1), colnames(te1)[-1])), 
                     datetime = te1$DATE, subid = obsid(te1), hype.var = "cout")
te2 <- ReadObs(filename = system.file("demo_model", "Qobs.txt", package = "HYPEtools"))
te2 <- HypeSingleVar(x = array(data = unlist(te2[, -1]), 
                               dim = c(nrow(te2), ncol(te2) - 1, 1), 
                               dimnames = list(rownames(te2), colnames(te2)[-1])), 
                     datetime = te2$DATE, subid = obsid(te2), hype.var = "rout")
# Nash-Sutcliffe Efficiency
NSE(sim = te1, obs = te2, progbar = FALSE)




}
