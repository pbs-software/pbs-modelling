\name{getOptionsFileName}
\alias{getOptionsFileName}
\alias{setOptionsFileName}
\title{
  Get and Set File Name for Saving and Loading of Options
}
\description{
  Functions for retrieving and setting the default file name used by \code{loadOptions} and \code{saveOptions}.
}
\usage{
getOptionsFileName(option.object)
setOptionsFileName(option.object, name)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{name}{new name for default file name}
}
\value{
  \code{getOptionsFileName}: the default file name
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{loadOptions}}, \code{\link{saveOptions}} \cr
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example using \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}

