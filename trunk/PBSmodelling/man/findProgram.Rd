\name{findProgram}
\alias{findProgram}
\title{Locates a program in the PATH environment variable }
\description{
  Returns the complete filename and path of a program in the PATH environment variable.
  This is a wrapper for \code{Sys.which}, and may be deprecated in the future.
}
\usage{
findProgram( name, includename=FALSE )
}
\arguments{
  \item{name}{name of a program to locate}
  \item{includename}{boolean: if true, include the filename in the path returned, otherwise just the directory.}
}
\value{
  A string containing the location of the program. NULL is returned if the program is not located.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  print(list(
  gcc =     findProgram( "gcc" ),
  notepad = findProgram( "notepad" ),
  R       = findProgram( "R", includename=TRUE ) ))
})
}
\seealso{
  \code{\link[base]{Sys.which}}
}
\keyword{file}
