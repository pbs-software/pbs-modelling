\name{testCol}
\alias{testCol}
\title{Display Named Colours Available Based on a Set of Strings}
\description{
	Display colours as patches in a plot. Useful for programming purposes. 
	Colours can be specified in any of 3 different ways: (i) by colour name, 
	(ii) by hexidecimal colour code created by \code{rgb()}, or (iii) by an 
	index to the \code{color()} palette.
}
\usage{
testCol(cnam=colors()[sample(length(colors()),15)])
}
\arguments{
	\item{cnam}{vector of colour names to display. Defaults to 15 random names 
		from the \code{color} palette.}
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
	\code{\link{pickCol}}
}
\examples{
testCol(c("sky","fire","sea","wood"))

testCol(c("plum","tomato","olive","peach","honeydew"))

testCol(substring(rainbow(63),1,7))

#display all colours set in the colour palette
testCol(1:length(palette()))

#they can even be mixed
testCol(c("#9e7ad3", "purple", 6))
}

\keyword{utilities}
\keyword{color}
