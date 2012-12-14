\name{scalePar}
\alias{scalePar}
\title{Scale Parameters to [0,1]}

\description{Scale parameters for function minimization by \code{calcMin}.}

\usage{scalePar(pvec)}

\arguments{
  \item{pvec}{a data frame comprising four columns - 
    \code{c("val","min","max","active")} and as many rows as there are model
    parameters. The \code{"active"} field (logical) determines whether the 
    parameters are estimated (\code{TRUE}) or remain fixed (\code{FALSE}).}
}

\details{
  Scaling algorithm:  \eqn{ S = \frac{2}{\pi} asin \sqrt{ \frac{P - P_{min}}{P_{max} - P_{min}} } }{%
  S = (2/pi) asin sqrt((P - Pmin)/(Pmax - Pmin)) }
}
\value{
  Parameter vector scaled between 0 and 1.
}
\author{
  Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{restorePar}}, \code{\link{calcMin}}, \code{\link{GT0}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  pvec <- data.frame(val=c(1,100,10000),min=c(0,0,0),max=c(5,500,50000),
    active=c(TRUE,TRUE,TRUE))
  S    <- scalePar(pvec)
  print(cbind(pvec,S))
})
}
\keyword{optimize}