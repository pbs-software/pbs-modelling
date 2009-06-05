\name{testAlpha}
\alias{testAlpha}
\title{ Test Various Alpha Transparency Values }
\description{
  Display how the alpha transparency for \code{rgb()} varies. 
}
\usage{
testAlpha(alpha=seq(0,1,len=25), fg="blue", bg="yellow",
      border="black", grid=FALSE, ...)
}
\arguments{
  \item{alpha}{ numeric vector of alpha transparency values values from 0 to 1. }
  \item{fg}{ forground colour of the top shape that varies in trnasparency. }
  \item{bg}{ background colour (remains constant) of the underlying shape. }
  \item{border}{ border colour (which also changes in transparency) of the foreground polygon. }
  \item{grid}{ logical: if \code{TRUE}, lay a grey grid on the background colour. }
  \item{\dots}{ additional graphical arguments to send to the the ploting functions. }
}
\value{
  Invisibly returns the compound RGB matrix for \code{fg}, \code{alpha}, 
  \code{bg}, and \code{border}.
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{testCol}}, \code{\link{testPch}}, \code{\link{testLty}}, \code{\link{testLwd}} }
\keyword{ color }
