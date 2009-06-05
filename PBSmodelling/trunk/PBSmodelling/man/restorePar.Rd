\name{restorePar}
\alias{restorePar}
\title{Get Actual Parameters from Scaled Values}
\description{
	Restore scaled parameters to their original units. Used in minimization by \code{calcMin}.
}
\usage{restorePar(S,pvec)}
\arguments{
	\item{S}{scaled parameter vector.}
	\item{pvec}{a data frame comprising four columns - 
		\code{c("val","min","max","active")} and as many rows as there are model 
		parameters. The \code{"active"} field (logical) determines whether the 
		parameters are estimated (\code{TRUE}) or remain fixed (\code{FALSE}).}
}
\value{
	Parameter vector converted from scaled units to original units specified by \code{pvec}.
}
\details{
	Restoration algorithm:  \eqn{ P = P_{min} + (P_{max} - P_{min}) (sin(\frac{\pi S}{2}))^2 }{%
	P = Pmin + (Pmax - Pmin) (sin(pi*S/2))^2 }
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{scalePar}}, \code{\link{calcMin}}, \code{\link{GT0}}
}
\examples{
pvec <- data.frame(val=c(1,100,10000),min=c(0,0,0),max=c(5,500,50000),
        active=c(TRUE,TRUE,TRUE))
S    <- c(.5,.5,.5)
P    <- restorePar(S,pvec)
print(cbind(pvec,S,P))
}
\keyword{optimize}