\name{addLabel}
\alias{addLabel}
\title{Add a Label to a Plot Using Relative (0:1) Coordinates}

\description{
	Place a label in a plot using relative (0:1) coordinates
}

\usage{
addLabel(x, y, txt, ...)
}

\arguments{
  \item{x}{ x-axis coordinate in the range (0:1); can step outside.}
  \item{y}{ y-axis coordinate in the range (0:1); can step outside.}
  \item{txt}{ desired label at (\code{x,y}).}
  \item{...}{additional arguments passed to the function \code{text}.}
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{addArrows}}, \code{\link{addLegend}}
}
\examples{
resetGraph()
addLabel(0.75,seq(from=0.9,to=0.1,by=-0.10),c('a','b','c'), col="#0033AA")
}

\keyword{iplot}
