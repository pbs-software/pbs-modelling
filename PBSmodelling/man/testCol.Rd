\name{testCol}
\alias{testCol}
\title{
  Display Colours Available Using a Set of Strings
}
\description{
  Display colours as round patches in a plot. Useful for programming purposes. 
  Colours can be specified in any of 3 different ways: \cr
  (i) by colour name, \cr
  (ii) by hexadecimal colour code created by \code{rgb()}, or \cr
  (iii) by calling one of the colour palettes.
}
\usage{
testCol(cnam=colors()[sample(length(colors()),15)])
}
\arguments{
  \item{cnam}{\code{character} -- vector of colour names to display. Defaults to 15 random names 
    from the \code{color} palette to use as patterns.}
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{pickCol}},
  \code{\link[PBSmodelling]{testAlpha}}\cr
In package \pkg{PBStools}:\cr
  \code{testPch}\cr
In package \pkg{grDevices}:\cr
  \code{\link[grDevices]{palettes}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  # mix and match patterns
  testCol(c("sky","fire","sea","wood"))
})
local(envir=.PBSmodEnv,expr={
  # display transparencies of blue
  testCol(rgb(0,0,1,seq(0.05,1,0.05)))
})
local(envir=.PBSmodEnv,expr={
  # display colours of the rainbow
  testCol(rainbow(64,end=0.75))
})
local(envir=.PBSmodEnv,expr={
  # display basic palette colours
  testCol(1:length(palette()))
})
local(envir=.PBSmodEnv,expr={
  # mix colour types
  testCol(c("#9e7ad3", "purple", 6))
})
}
\keyword{utilities}
\keyword{color}
\concept{plotFuns}

