\name{writePBSoptions}
\alias{writePBSoptions}
\title{Write PBS Options to an External File}
\description{
  Save options that were set using \code{setPBSoptions},
  \code{setPBSext}, or interfaces such as \code{loadC}.  
  These options can be reloaded using \code{readPBSoptions}.
}
\usage{
writePBSoptions(fname="PBSoptions.txt")
}
\arguments{
  \item{fname}{file name or full path of file to which the options will be saved.}
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
Options with names starting with \code{"."} will not be saved.
}

\seealso{
  \code{\link{readPBSoptions}}, \code{\link{setPBSoptions}},
  \code{\link{setPBSext}}, \code{\link{promptWriteOptions}}
}
