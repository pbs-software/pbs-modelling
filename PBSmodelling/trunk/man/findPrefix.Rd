\name{findPrefix}
\alias{findPrefix}
\title{Find a Prefix Based on Names of Existing Files}

\description{
  Find the prefixes of files with a given suffix in the working directory.
}
\usage{
findPrefix(suffix)
}
\arguments{
  \item{suffix}{character vector of suffixes}
}
\details{
  The function \code{findPrefix} locates all files in the working directory that end with
  one of the provided suffixes. The suffixes may contain wildcards (\code{"*"} to match 0
  or more characters, \code{"?"} to match any single character).

  If \code{findPrefix} was called from a widget as specified in a 
  \emph{window description file}, then the value of a widget named 
  \code{prefix} will be set to the prefix of the first matching file 
  found, with an exception: if the value of the prefix widget
  matches one of the file prefixes found, it will not be changed.

  To use this function in a \emph{window description file}, the action of the widget is
  used to specify the suffixes to match, with the suffixes separated by commas.
  For example, \code{action=.c,.cpp} would set a prefix widget to the first file found
  with an extension \code{.c} or \code{.cpp}.
}
\value{
  A character vector of all the prefixes of files in the working directory that
  matched to one of the given suffixes.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{setwdGUI}}
}
\examples{
\dontrun{
# Match files that end with '.a' followed by 0 or more characters,
# '.b' followed by any single character, '.c', or '-old.d'
# (a suffix does not have to be a file extension)
findPrefix(".a*", ".b?", ".c", "-old.d")
}
}
\keyword{file}