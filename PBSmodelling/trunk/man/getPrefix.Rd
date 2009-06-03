\name{getPrefix}
\alias{getPrefix}
\title{ Get Prefix of System Files with Specified Suffix }
\description{
  Search for and return all string prefixes of system files 
  with the specified suffix and system path.
}
\usage{
getPrefix(suffix, path=".")
}
\arguments{
  \item{suffix}{ string value of suffix (e.g., \code{".txt"}. }
  \item{path}{ string specifying system path location in which to search. }
}
\value{
  Vector of string prefixes that have the specified suffix.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{getSuffix}}, \code{\link{findPrefix}} }
\keyword{ character }
