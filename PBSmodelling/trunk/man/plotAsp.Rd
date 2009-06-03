\name{plotAsp}
\alias{plotAsp}
\title{Construct a Plot with a Specified Aspect Ratio}

\description{
	Plot \code{x} and \code{y} coordinates using a specified aspect ratio.
}

\usage{
plotAsp(x, y, asp=1, ...)
}

\arguments{
	\item{x}{vector of x-coordinate points in the plot.}
	\item{y}{vector of y-coordinate points in the plot.}
	\item{asp}{\code{y}/\code{x} aspect ratio.}
	\item{...}{additional arguments for \code{plot}.}
}

\details{
	The function \code{plotAsp} differs from \code{plot(x,y,asp=1)} in the way axis 
	limits are handled. Rather than expand the range, \code{plotAsp} expands the 
	margins through padding to keep the aspect ratio accurate.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\examples{
x <- seq(0,10,0.1)
y <- sin(x)
par(mfrow=2:1)
plotAsp(x,y,asp=1,xlim=c(0,10),ylim=c(-2,2), main="sin(x)")
plotAsp(x,y^2,asp=1,xlim=c(0,10),ylim=c(-2,2), main="sin^2(x)")
}

\keyword{hplot}
