\name{ReadMapOutput}
\alias{ReadMapOutput}
\title{Read a Map Output File}
\usage{
ReadMapOutput(filename)
}
\arguments{
  \item{filename}{Path to and file name of the basin output
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}
}
\value{
\code{ReadMapOutput} returns a data frame.
}
\description{
This is a convenience wrapper function to import a map
output file ('map<\emph{HYPE_output_variable}>.txt') as
data frame into R.
}
\details{
\code{ReadMapOutput} is a convenience wrapper function of
\code{\link{read.table}}, with treatment of a leading
comment row in HYPE's output file. The comment row is
imported as string \code{attribute} 'comment'.
}
\examples{
\dontrun{ReadMapOutput("mapCOUT.txt")}
}

