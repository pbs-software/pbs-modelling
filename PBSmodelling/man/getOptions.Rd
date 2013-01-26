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
  \item{\dots}{any number of user options to set where either (a) the
    named argument is the option key and the value is the option value
    or (b) the single unnamed argument is a list object where each named
    list element is the option key and the value is the element's value}
  \item{key}{name of option to retrieve; if missing, all options are returned}
}
\value{
  Value of the option specified by \code{key} (if specified) or a list
  of all options (if missing).
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example that uses \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}
