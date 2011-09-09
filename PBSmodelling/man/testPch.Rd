\name{testPch}
\alias{testPch}
\title{Display Plotting Symbols or Octal Strings}

\description{
  Display plotting symbols or octal strings. 
  User can specify particular ranges (increasing continuous integer) for \code{pch}.
}
\usage{
testPch(pch=1:100, ncol=10, grid=TRUE, newframe=TRUE, octal=FALSE, ...)
}

\arguments{
  \item{pch}{symbol codes or octal string numbers.}
  \item{ncol}{number of columns in display (can only be 2, 5, or 10). Most 
    sensibly this is set to 10.}
  \item{grid}{logical: if \code{TRUE}, grid lines are plotted for visual aid.}
  \item{newframe}{logical: if \code{TRUE} reset the graph, otherwise overlay 
    on top of the current graph.}
  \item{octal}{logical: if \code{TRUE}, show octal strings (backslash characters) used in text 
    statements (e.g., \code{"30\\272C"} = 30\eqn{^\circ}{ degrees}C).}
  \item{...}{additional arguments for functions \code{points} or \code{text}.}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBSmodelling]{testLty}}, \code{\link[PBSmodelling]{testLwd}}, \code{\link[PBSmodelling]{addLabel}}
}
\examples{
testPch(123:255)
testPch(1:25,ncol=5)
testPch(41:277,octal=TRUE)
}
\keyword{utilities}
\keyword{color}
