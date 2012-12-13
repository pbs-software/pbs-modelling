\name{GT0}
\alias{GT0}
\title{Restrict a Numeric Variable to a Positive Value}

\description{
  Restrict a numeric value \code{x} to a positive value using a differentiable function. 
  GT0 stands for \dQuote{greater than zero}.
}

\usage{GT0(x,eps=1e-4)}

\arguments{
  \item{x}{vector of values}
  \item{eps}{minimum value greater than zero.}
}

\details{
  \preformatted{
   if (x >= eps)..........GT0 = x
   if (0 < x < eps).......GT0 = (eps/2) * (1 + (x/eps)^2)
   if (x <= 0)............GT0 = eps/2
  }
}
\author{
  Jon Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{scalePar}}, \code{\link{restorePar}}, \code{\link{calcMin}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  plotGT0 <- function(eps=1,x1=-2,x2=10,n=1000,col="black") {
    x <- seq(x1,x2,len=n); y <- GT0(x,eps);
    lines(x,y,col=col,lwd=2); invisible(list(x=x,y=y)); }

  testGT0 <- function(eps=c(7,5,3,1,.1),x1=-2,x2=10,n=1000) {
    x <- seq(x1,x2,len=n); y <- x;
    plot(x,y,type="l");
    mycol <- c("red","blue","green","brown","violet","orange","pink");
    for (i in 1:length(eps)) 
      plotGT0(eps=eps[i],x1=x1,x2=x2,n=n,col=mycol[i]);
    invisible(); };

  testGT0()
  par(oldpar)
})
}
\keyword{optimize}

