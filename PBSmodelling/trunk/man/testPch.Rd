\name{testPch}
\alias{testPch}
\title{Display Plotting Symbols and Backslash Characters}

\description{
	Display plotting symbols. User can specify particular ranges (increasing 
	continuous integer) for \code{pch}.
}
\usage{
testPch(pch=1:100, ncol=10, grid=TRUE, newframe=TRUE, bs=FALSE)
}

\arguments{
	\item{pch}{symbol codes to view.}
	\item{ncol}{number of columns in display (can only be 2, 5, or 10). Most 
		sensibly this is set to 10.}
	\item{grid}{logical: if \code{TRUE}, grid lines are plotted for visual aid.}
	\item{newframe}{logical: if \code{TRUE} reset the graph, otherwise overlay 
		on top of the current graph.}
	\item{bs}{logical: if \code{TRUE}, show backslash characters used in text 
		statements (e.g., \code{30\\272C} = 30\eqn{^\circ}{ degrees}C).}
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\examples{
testPch(123:255)
testPch(1:25,ncol=5)
testPch(41:277,bs=TRUE)
}
\keyword{utilities}
\keyword{color}
