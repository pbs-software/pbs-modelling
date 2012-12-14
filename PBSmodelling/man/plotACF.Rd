\name{plotACF}
\alias{plotACF}
\title{
  Plot Autocorrelation Bars From a Data Frame, Matrix, or Vector
}
\description{
  Plot autocorrelation bars (ACF) from a data frame, matrix, or vector.
}
\usage{
plotACF(file, lags=20, 
    clrs=c("blue","red","green","magenta","navy"), ...)
}
\arguments{
  \item{file}{data frame, matrix, or vector of numeric values.}
  \item{lags}{maximum number of lags to use in the ACF calculation.}
  \item{clrs}{vector of colours. Patterns are repeated if the number 
    of fields exceeed the length of \code{clrs}.}
  \item{...}{additional arguments for \code{plot} or \code{lines}.}
}
\details{
  This function is designed primarily to give greater flexibility when viewing 
  results from the R-package \code{BRugs}. Use \code{plotACF} in conjuction with 
  \code{samplesHistory("*",beg=0,plot=FALSE)} rather than \code{samplesAutoC} 
  which calls \code{plotAutoC}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  resetGraph(); plotACF(trees,lwd=2,lags=30)
  par(oldpar)
})
}
\keyword{graphs}