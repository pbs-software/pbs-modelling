\name{readPBSoptions}
\alias{readPBSoptions}
\title{Read PBS Options from an External File}
\description{
  Load options that were saved using \code{writePBSoptions}, for use
  with \code{openFile}, \code{getPBSoptions} or interfaces such as
  \code{loadC}.
}
\usage{
readPBSoptions(fname="PBSoptions.txt")
}
\arguments{
  \item{fname}{file name or full path of file from which the options will be loaded.}
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If an option exists in R memory but not in the saved file, 
  the option is not cleared from memory.
}
\seealso{
  \code{\link{writePBSoptions}}, \code{\link{getGUIoptions}},
  \code{\link{openFile}}, \code{\link{getPBSoptions}}
}
\keyword{connection}

