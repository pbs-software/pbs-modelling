\name{drawBars}
\alias{drawBars}
\title{Draw a Linear Barplot on the Current Plot}

\description{
  Draw a linear barplot on the current plot. Fill if desired.
}
\usage{drawBars(x, y, width, base=0, fill=NULL, ...)
}
\arguments{
  \item{x}{x-coordinates}
  \item{y}{y-coordinates}
  \item{width}{bar width, computed if missing}
  \item{base}{y-value of the base of each bar}
  \item{fill}{valid R colour to fill the bars}
  \item{...}{further graphical parameters (see \code{par}) for the \code{lines} function.}
}
\author{
  Jon T. Schnute, Scientist Emeritus,\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
    if (dev.cur()>1) {
      oldpar=par(no.readonly=TRUE); on.exit(par(oldpar)) }
  plot(0:10,0:10,type="n")
  drawBars(x=1:9,y=9:1,col="deepskyblue4",fill="cyan",lwd=3)
})
}

\keyword{hplot}

