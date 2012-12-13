\name{getPBSoptions}
\alias{getPBSoptions}
\title{Retrieve A User Option}
\description{
  Get a previously defined user option.
}
\usage{
getPBSoptions(option)
}
\arguments{
  \item{option}{name of option to retrieve. If omitted, a list containing all options is returned.}
}
\value{
  Value of the specified option, or \code{NULL} if the specified option is not found.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{ \code{\link{getPBSext}}, \code{\link{readPBSoptions}} }

\keyword{methods}
