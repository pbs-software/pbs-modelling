\name{show0}
\alias{show0}
\title{Convert Numbers into Text with Specified Decimal Places}

\description{
  Return a character representation of a number with added zeroes 
  out to a specified number of decimal places.
}

\usage{
show0(x, n, add2int=FALSE, round2n=FALSE)
}

\arguments{
  \item{x}{numeric data (scalar, vector, or matrix).}
  \item{n}{number of decimal places to show, including zeroes.}
  \item{add2int}{if \code{TRUE}, add zeroes to integers after the decimal.}
  \item{round2n}{if \code{TRUE}, round \code{x} first to \code{n} decimal places.}
}
\value{
  A scalar/vector of strings representing numbers. Useful for labelling purposes.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  By default, this function does not round or truncate numbers. It simply adds zeroes if 
  \code{n} is greater than the available digits in the decimal part of a number. The user can 
  choose to round the numbers first by setting argument \code{round2n = TRUE}.
}
\seealso{
  \code{\link[PBSmodelling]{pad0}}, \code{\link[PBSmodelling]{GT0}}
}
\examples{
frame()

#do not show decimals on integers
addLabel(0.25,0.75,show0(15.2,4))
addLabel(0.25,0.7,show0(15.1,4))
addLabel(0.25,0.65,show0(15,4))

#show decimals on integers
addLabel(0.25,0.55,show0(15.2,4,TRUE))
addLabel(0.25,0.5,show0(15.1,4,TRUE))
addLabel(0.25,0.45,show0(15,4,TRUE))
}
\keyword{print}
