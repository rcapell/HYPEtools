\name{ReadBranchData}
\alias{ReadBranchData}
\title{Read a 'BranchData.txt' file}
\usage{
ReadBranchData(filename = "BranchData.txt")
}
\arguments{
  \item{filename}{Path to and file name of the Branchdata
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}
}
\value{
\code{ReadBranchData} returns a data frame.
}
\description{
This is a convenience wrapper function to import a
BranchData file as data frame into R.
}
\details{
\code{ReadBranchData} is just \code{read.table(file =
filename, header = T)}, mainly added to provide a
comparable function to the other RHYPE imports.
}
\examples{
\dontrun{ReadBranchData("BranchData.txt")}
}

