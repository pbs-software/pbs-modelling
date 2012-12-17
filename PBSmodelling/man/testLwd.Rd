\name{testLwd}
\alias{testLwd}
\title{Display Line Widths}

\description{
  Display line widths. User can specify particular ranges for \code{lwd}. 
  Colours can also be specified and are internally repeated as necessary.
}
\usage{
testLwd(lwd=1:20, col=c("black","blue"), newframe=TRUE)
}
\arguments{
  \item{lwd}{line widths to display. Ranges can be specified.}
  \item{col}{colours to use for lines. Patterns are repeated if 
    \code{length(lwd) > length(col)}}.
  \item{newframe}{if \code{TRUE}, create a new blank frame, otherwise overlay current frame.}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  testLwd(3:15,col=c("salmon","aquamarine","gold"))
})
}
\keyword{utilities}
\keyword{color}
