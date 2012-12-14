\name{setPBSoptions}
\alias{setPBSoptions}
\title{Set A User Option}

\description{
  Options set by the user for use by other functions.
}

\usage{
setPBSoptions(option, value, sublist=FALSE)
}
\arguments{
  \item{option}{name of the option to set.}
  \item{value}{new value to assign this option.}
  \item{sublist}{if \code{value} is a sublist (list component) of \code{option},
    this list component can be changed individually using \code{sublist=TRUE}.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\note{
  A value \code{.PBSmod$.options$.optionsChanged} is set to \code{TRUE} when an option is changed,
  so that the user doesn't always have to be prompted to save the options file. \cr
  By default, \code{.PBSmod$.options$.optionsChanged} is not set or \code{NULL}. \cr
  Also, if an option is set to \code{""} or \code{NULL} then it is removed. \cr
  \code{.initPBSoptions()} is now called first (options starting with a dot "." 
  do not set \code{.optionsChanged}).
}
\seealso{
  \code{\link{getPBSoptions}}, \code{\link{writePBSoptions}},
  \code{\link{readPBSoptions}}
}
\keyword{methods}

