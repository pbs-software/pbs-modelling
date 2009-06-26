\name{drawBars}
\alias{drawBars}
\title{Draw a Linear Barplot on the Current Plot}

\description{
	Draw a linear barplot on the current plot.
}
\usage{drawBars(x, y, width, base = 0, ...)}

\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{width}{bar width, computed if missing}
  \item{base}{y-value of the base of each bar}
  \item{...}{further graphical parameters (see \code{par}) may also be supplied as arguments}
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\examples{
plot(0:10,0:10,type="n")
drawBars(x=1:9,y=9:1,col="deepskyblue4",lwd=3)
}

\keyword{hplot}
