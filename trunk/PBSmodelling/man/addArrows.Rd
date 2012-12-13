\name{addArrows}
\alias{addArrows}
\title{Add Arrows to a Plot Using Relative (0:1) Coordinates}

\description{
	Call the \code{arrows} function using relative (0:1) coordinates.
}

\usage{
addArrows(x1, y1, x2, y2, ...)
}

\arguments{
	\item{x1}{ x-coordinate (0:1) at base of arrow.}
	\item{y1}{ y-coordinate (0:1) at base of arrow.}
	\item{x2}{ x-coordinate (0:1) at tip of arrow.}
	\item{y2}{ y-coordinate (0:1) at tip of arrow.}
	\item{...}{ additional paramaters for the function \code{arrows}.}
}

\details{
	Lines will be drawn from \code{(x1[i],y1[i])} to \code{(x2[i],y2[i])}
}
\author{
  Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada , Nanaimo BC
}
\seealso{
	\code{\link{addLabel}}, \code{\link{addLegend}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  tt=seq(from=-5,to=5,by=0.01)
  plot(sin(tt), cos(tt)*(1-sin(tt)), type="l")
  addArrows(0.2,0.5,0.8,0.5)
  addArrows(0.8,0.95,0.95,0.55, col="#FF0066")
  par(oldpar)
})
}

\keyword{iplot}
