\name{ReadLakeData}
\alias{ReadLakeData}
\title{Read a 'LakeData.txt' file}
\usage{
ReadLakeData(filename = "LakeData.txt")
}
\arguments{
  \item{filename}{Path to and file name of the LakeData
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}
}
\value{
\code{ReadLakeData} returns a data frame.
}
\description{
This is a convenience wrapper function to import a LakeData
file as data frame into R.
}
\details{
\code{ReadLakeData} is a simple \code{\link{read.table}}
wrapper, mainly added to provide a comparable function to
the other RHYPE imports.
}
\examples{
\dontrun{ReadLakeData("LakeData.txt")}
}

