\name{getOptions}
\alias{getOptions}
\alias{setOptions}
\title{
  Get and Set User Options
}
\description{
  Functions to get and set user options within an option class object.
}
\usage{
getOptions(option.object, key)
setOptions(option.object, ...)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{\dots}{any number of user options to set, where the named argument is the option key and the value is the option value}
  \item{key}{name of option to retrieve -- if missing all options are returned}
}
\value{
  \code{getOptions}: returns the value of the option specified by \code{key}. \cr
  If \code{key} is missing, a list of all options.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example using \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}

