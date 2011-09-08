\name{testCol}
\alias{testCol}
\title{Display Colours Available Using a Set of Strings}
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
	\item{cnam}{vector of colour names to display. Defaults to 15 random names 
		from the \code{color} palette to use as patterns.}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
	\code{\link[PBSmodelling]{pickCol}}, \code{\link[PBSmodelling]{testAlpha}}, 
	\code{\link[PBSmodelling]{testPch}}, \code{\link[grDevices]{palettes}}
}
\examples{
# mix and match patterns
testCol(c("sky","fire","sea","wood"))

# display transparencies of blue
testCol(rgb(0,0,1,seq(0.05,1,0.05)))

# display colours of the rainbow
testCol(rainbow(64,end=0.75))

# display basic palette colours
testCol(1:length(palette()))

# mix colour types
testCol(c("#9e7ad3", "purple", 6))
}

\keyword{utilities}
\keyword{color}
