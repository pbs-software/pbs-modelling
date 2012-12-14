\name{parseWinFile}
\alias{parseWinFile}
\title{Convert a Window Description File into a List Object}
\description{
  Parse a \emph{window description file} (markup file) into 
  the list format expected by \code{createWin}.
}
\note{
  All widgets are forced into a 1-column by N-row grid.
}
\usage{
parseWinFile(fname, astext=FALSE)
}
\arguments{
  \item{fname}{file name of the \emph{window description file}.}
  \item{astext}{if \code{TRUE}, \code{fname} is interpreted as a vector of strings, 
    with each element representing a line of code in a \emph{window description file}.}
}
\value{
  A list representing a parsed \emph{window description file} that can be directly 
  passed to \code{createWin}.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{createWin}}, \code{\link{compileDescription}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  x <- parseWinFile(system.file("examples/LissFigWin.txt",package="PBSmodelling"))
  createWin(x)
})
}
}
\keyword{list}
