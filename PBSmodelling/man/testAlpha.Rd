\name{testAlpha}
\alias{testAlpha}
\title{ Test Various Alpha Transparency Values }
\description{
  Display how the alpha transparency for \code{rgb()} varies. 
}
\usage{
testAlpha(alpha=seq(0,1,len=25), fg="blue", bg="yellow",
      border="black", grid=FALSE, ...)
}
\arguments{
  \item{alpha}{\code{numeric} -- vector of alpha transparency values values from 0 to 1. }
  \item{fg}{\code{character} -- foreground colour of the top shape that varies in transparency. }
  \item{bg}{\code{character} -- background colour (remains constant) of the underlying shape. }
  \item{border}{\code{character} -- border colour (which also changes in transparency) of the foreground polygon. }
  \item{grid}{\code{logical} -- if \code{TRUE}, lay a grey grid on the background colour. }
  \item{\dots}{\code{dots} -- additional graphical arguments to send to the the ploting functions. }
}
\value{
  Invisibly returns the compound RGB matrix for \code{fg}, \code{alpha}, 
  \code{bg}, and \code{border}.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Research Scientist Emeritus\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{testCol}},
  \code{\link[PBSmodelling]{testLty}},
  \code{\link[PBSmodelling]{testLwd}}\cr
In package \pkg{PBStools}:\cr
  \code{testPch}
 }
\keyword{color}
\concept{plotFuns}

