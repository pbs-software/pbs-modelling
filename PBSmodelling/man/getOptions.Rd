\name{getOptions}
\alias{getOptions}
\alias{setOptions}
\alias{.showOptions}
\title{
  Get and Set User Options
}
\description{
  Functions to get and set user options within an option class object.
}
\usage{
getOptions(option.object, key)
setOptions(option.object, ...)
.showOptions(object)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{key}{name of option to retrieve; if missing, all options are returned}
  \item{\dots}{any number of user options to set where either\cr
    (a) the named argument is the option key and the value is the option value or\cr
    (b) the single unnamed argument is a list object where each named
    list element is the option key and the value is the element's value}
  \item{object}{options class object used for storing package options}
}
\value{
  Value of the option specified by \code{key} (if specified) or a list
  of all options (if missing).
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer,
  Earthly Technologies, Victoria BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-11-04}
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example that uses \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}
\concept{M09_options_manager}

