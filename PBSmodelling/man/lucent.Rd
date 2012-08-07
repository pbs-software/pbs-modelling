\name{lucent}
\alias{lucent}
\title{
  Convert Solid Colours to Translucence
}
\description{
  Convert a vector of solid colours to a vector of translucent ones (or vice versa)
}
\usage{
lucent(col.pal=1, a=1)
}
\arguments{
  \item{col.pal}{vector of colours}
  \item{a}{alpha transparency value (\code{0} = fully transparent, \code{1} = opaque) }
}
\details{
  The function acts as a small wrapper to the \code{rgb} function.
}
\value{
  Vector of transformed colours depending on the alpha transparancy value \code{a}.
}
\author{
  Steve Martell, University of British Columbia, Vancouver BC
}
\seealso{
  \code{\link[PBSmodelling]{pickCol}}, \code{\link[PBSmodelling]{testCol}},
  \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}
}
\examples{
pbsfun = function(clrs=c("moccasin",rainbow(3))){
  clrs = c(clrs,lucent(clrs,a=0.25))
  testCol(clrs)
  invisible() }
pbsfun()
}
\keyword{color}

