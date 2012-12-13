\name{addLegend}
\alias{addLegend}
\title{Add a Legend to a Plot Using Relative (0:1) Coordinates}

\description{
  Place a legend in a plot using relative (0:1) coordinates.
}

\usage{
addLegend(x, y, ...) 
}

\arguments{
  \item{x}{ x-axis coordinate in the range (0:1); can step outside.}
  \item{y}{ y-axis coordinate in the range (0:1); can step outside.}
  \item{...}{ arguments used by the function \code{legend}, 
    such as \code{lines}, \code{text}, or \code{rectangle}.}
}
\author{
  Jon Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{addArrows}}, \code{\link{addLabel}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  resetGraph()
  n <- sample(1:length(colors()),15); clrs <- colors()[n]
  addLegend(.2,1,fill=clrs,leg=clrs,cex=1.5)
  par(oldpar)
})
}

\keyword{iplot}
