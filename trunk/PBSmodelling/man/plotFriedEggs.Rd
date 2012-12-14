\name{plotFriedEggs}
\alias{plotFriedEggs}
\title{Render a Pairs Plot as Fried Eggs and Beer}
\description{
  Create a pairs plot where the lower left half comprises either 
  fried egg contours or smoke ring contours, the upper right half 
  comprises glasses of beer filled to the correlation point, and 
  the diagonals show frequency histograms of the input data.
}
\usage{
plotFriedEggs(A, eggs=TRUE, rings=TRUE, levs=c(0.01,0.1,0.5,0.75,0.95),
    pepper=200, replace=FALSE, jitt=c(1,1), bw=25, histclr=NULL) 
}
\arguments{
  \item{A}{data frame or matrix for use in a pairs plot.}
  \item{eggs}{logical: if \code{TRUE}, fry eggs in the lower panels.}
  \item{rings}{logical: if \code{TRUE}, blow smoke rings in the lower panels.}
  \item{levs}{explicit contour levels expressed as quantiles.}
  \item{pepper}{number of samples to draw from \code{A} to pepper the plots.}
  \item{replace}{logical: if \code{TRUE}, sample \code{A} with replacement.}
  \item{jitt}{argument \code{factor} used by function \code{base::jitter} when peppering.
    If user supplies two numbers, the first will jitter \code{x}, the second will 
    jitter \code{y}.}
  \item{bw}{argument \code{bandwidth} used by function \code{KernSmooth::bkde2D}.}
  \item{histclr}{user-specified colour(s) for histogram bars along the diagonal.}
}
\details{
  This function comes to us from Dr. Steve Martell of the Fisheries Science 
  Centre at UBC. Obviously many hours of contemplation with his students at
  the local pub have contributed to this unique rendition of a pairs plot.
}
\author{
  Steve Martell, International Pacific Halibut Commission, Seattle WA
}
\note{
  If \code{eggs=TRUE} and \code{rings=FALSE}, fried eggs are served.\cr
  If \code{eggs=FALSE} and \code{rings=TRUE}, smoke rings are blown.\cr
  If \code{eggs=TRUE} and \code{rings=TRUE}, only fried eggs are served.\cr
  If \code{eggs=FALSE} and \code{rings=FALSE}, only pepper is sprinkled.
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  x=rnorm(5000,10,3); y=-x+rnorm(5000,1,4); z=x+rnorm(5000,1,3)
  A=data.frame(x=x,y=y,z=z)
  for (i in 1:3)
    switch(i,
    {plotFriedEggs(A,eggs=TRUE,rings=FALSE);
     pause("Here are the eggs...(Press Enter for next)")},
    {plotFriedEggs(A,eggs=FALSE,rings=TRUE);
     pause("Here are the rings...(Press Enter for next)")},
    {plotFriedEggs(A,eggs=FALSE,rings=FALSE);
     cat("Here is the pepper alone.\n")} )
  par(oldpar)
})
}
\seealso{
  \code{\link{plotBubbles}}, \code{\link{scalePar}}

  \code{KernSmooth::bkde2D}, \code{grDevices::contourLines}, \code{graphics::contour}
}
\keyword{hplot}

