\name{findPat}
\alias{findPat}
\title{Search a Character Vector to Find Multiple Patterns}

\description{
	Use all available patterns in \code{pat} to search in \code{vec}, 
	and return the matched elements in \code{vec}.
}

\usage{
findPat(pat, vec)
}

\arguments{
	\item{pat}{character vector of patterns to match in \code{vec}}
	\item{vec}{character vector where matches are sought}
}

\value{
	A character vector of all matched strings.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  #find all strings with a vowel, or that start with a number
  print(findPat(c("[aeiou]", "^[0-9]"), c("hello", "WORLD", "1over")))
})
}

\keyword{utilities}
