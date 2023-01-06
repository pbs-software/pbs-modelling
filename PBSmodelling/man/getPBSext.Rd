\name{getPBSext}
\alias{getPBSext}
\title{Get Command Associated With File Name Extension}

\description{
  Display all locally defined file extensions and their associated commands, 
  or search for the command associated with a specific file extension
  \code{ext}.
}
\note{
  These file associations are not saved from one \emph{PBS Modelling} session to
  the next unless explicitly saved and loaded (see \code{writePBSoptions} and
  \code{readPBSoptions}).
}
\usage{getPBSext(ext)}
\arguments{
  \item{ext}{\code{character} -- optional string specifying a file extension.}
}
\value{
  Command associated with file extension.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-01-05}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{setPBSext}},
  \code{\link[PBSmodelling]{openFile}},
  \code{\link[PBSmodelling]{clearPBSext}}
}
\keyword{methods}
