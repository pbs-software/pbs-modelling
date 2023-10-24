\name{testLty}
\alias{testLty}
\title{Display Line Types Available}
\description{
  Display line types available.
}
\usage{
testLty(newframe=TRUE, n=1:18, ...)
}
\arguments{
  \item{newframe}{\code{logical} -- if \code{TRUE}, create a new blank frame, otherwise overlay current frame.}
  \item{n}{\code{numeric} -- vector of line type numbers.}
  \item{...}{\code{dots} -- additional arguments for function \code{lines}.}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\note{
  Quick representation of line types for reference purposes.
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{testLwd}},
  \code{\link[PBSmodelling]{testCol}}\cr
In package \pkg{PBStools}:\cr
  \code{testPch} 
}
\keyword{utilities}
\keyword{color}
\concept{plotFuns}


