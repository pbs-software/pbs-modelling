\name{plotTrace}
\alias{plotTrace}
\title{Plot Trace Lines from a Data Frame, Matrix, or Vector}
\description{
  Plot trace lines from a data frame or matrix where the first field contains x-values, 
  and subsequent fields give y-values to be traced over x. If input is a vector, 
  this is traced over the number of observations.
}
\usage{
plotTrace(file, clrs=c("blue","red","green","magenta","navy"), ...)
}
\arguments{
  \item{file}{data frame or matrix of x and y-values, or a vector of y-values.}
  \item{clrs}{vector of colours. Patterns are repeated if the number 
    of traces (y-fields) exceed the length of \code{clrs}.}
  \item{...}{additional arguments for \code{plot} or \code{lines}.}
}
\details{
  This function is designed primarily to give greater flexibility when viewing 
  results from the R-package \code{BRugs}. Use \code{plotTrace} in conjunction with 
  \code{samplesHistory("*",beg=0,plot=FALSE)} rather than \code{samplesHistory} 
  which calls \code{plotHistory}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  z <- data.frame(x=1:50,y1=rnorm(50,sd=3),y2=rnorm(50,sd=1),y3=rnorm(50,sd=.25))
  plotTrace(z,lwd=3)
  par(oldpar)
})
}
\keyword{graphs}