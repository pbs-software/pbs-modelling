\name{findProgram}
\alias{findProgram}
\title{Locates a program in the PATH environment variable }
\description{
  Returns the complete filename and path of a program in the PATH environment variable.
  findProgram is similar to the which command under Unix, but works under Windows too.
}
\usage{
findProgram(name)
}
\arguments{
	\item{name}{name of a program to locate}
}
\value{
  A string containing the location of the program. NULL is returned if the program is not located.
}
\author{
  Alex Couture-Beil
}
\seealso{
  \code{\link{dirname}}
}
\keyword{file}
