\name{getSuffix}
\alias{getSuffix}
\title{ Get Suffix of System Files with Specified Prefix }
\description{
  Search for and return all string suffixes of system files 
  with the specified prefix and system path.
}
\usage{
getSuffix(prefix, path=".")
}
\arguments{
  \item{prefix}{ string value of prefix (e.g., \code{"temp"}. }
  \item{path}{ string specifying system path location in which to search. }
}
\value{
  Vector of string suffixes that have the specified prefix.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{getPrefix}}, \code{\link{findPrefix}} }
\keyword{ character }
