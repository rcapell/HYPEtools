\name{ReadMgmtData}
\alias{ReadMgmtData}
\title{Read a 'MgmtData.txt' file}
\usage{
ReadMgmtData(filename = "MgmtData.txt")
}
\arguments{
  \item{filename}{Path to and file name of the MgmtData
  file to import. Windows users: Note that Paths are
  separated by '/', not '\\'.}
}
\value{
\code{ReadMgmtData} returns a data frame.
}
\description{
This is a convenience wrapper function to import a HYPE
MgmtData file as data frame into R.
}
\details{
\code{ReadMgmtData} is a simple \code{\link{read.table}}
wrapper, mainly added to provide a comparable function to
the other RHYPE imports. Will check for \code{NA} values in
imported data and return a warning if any are found. HYPE
requires \code{NA}-free input in required 'AquiferData.txt'
columns, but empty values are allowed in comment columns
which are not read.
}
\examples{
\dontrun{ReadMgmtData("MgmtData.txt")}
}

