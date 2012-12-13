\name{findPrefix}
\alias{findPrefix}
\alias{findSuffix}
\title{Find a Prefix Based on Names of Existing Files}

\description{
  Find the prefixes or suffixes of files with a given suffix or prefix in a directory.
}
\usage{
findPrefix(suffix,path=".")
findSuffix(prefix,path=".")
}
\arguments{
  \item{suffix}{character vector of suffixes}
  \item{prefix}{character vector of prefixes}
  \item{path}{directory to look for files in}
}
\details{
  The function \code{findPrefix} locates all files in a directory that end with
  one of the provided suffixes; where as \code{findSuffix} locates all files
  that start with the given prefixes.
}
\value{
  A character vector of all the prefixes or sufixes of files in the working directory that
  matched to one of the given suffixes.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
	edir = system.file("examples", package="PBSmodelling")
	print(findPrefix(suffix=c(".txt", ".r"),path=edir)); cat("\n")

	#or use R's dir for similar functionality
	print(dir(pattern="txt$",path=edir)); cat("\n")
	print(dir(pattern="^[a-h]",path=edir)); cat("\n")
})
}
\keyword{file}