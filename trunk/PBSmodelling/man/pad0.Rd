\name{pad0}
\alias{pad0}
\title{Pad Numbers with Leading Zeroes}

\description{
	Convert numbers to integers then text, and pad them with leading zeroes.
}

\usage{
pad0(x, n, f = 0)
}

\arguments{
	\item{x}{vector of numbers}
	\item{n}{number of text characters representing a padded integer}
	\item{f}{factor of 10 transformation on x before padding}
}

\value{
	A character vector representing \code{x} with leading zeroes.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\examples{
resetGraph(); x <- pad0(x=123,n=10,f=0:7);
addLabel(.5,.5,paste(x,collapse="\n"),cex=1.5);
}

\keyword{print}
