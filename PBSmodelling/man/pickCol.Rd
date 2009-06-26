\name{pickCol}
\alias{pickCol}
\title{Pick a Colour From a Palette and get the Hexadecimal Code}

\description{
	Display an interactive colour palette from which the user can choose a colour. 
}

\usage{pickCol(returnValue=TRUE)}

\arguments{
	\item{returnValue}{If \code{TRUE}, display the full colour palette, choose a colour, 
		and return the hex value to the R session. \cr If \code{FALSE}, use an intermediate 
		GUI to interact with the palette and display the hex value of the chosen colour.}
}

\value{ A hexidecimal colour value. }
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{ \code{\link{testCol}} }

\examples{
\dontrun{
junk<-pickCol(); resetGraph(); addLabel(.5,.5,junk,cex=4,col=junk);
}
}
\keyword{color}
