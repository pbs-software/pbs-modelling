\name{clearPBSext}
\alias{clearPBSext}
\title{Clear File Extension Associations}

\description{
  Disassociate any number of file extensions from commands previously saved
  with \code{setPBSext}.
}

\usage{clearPBSext(ext)}

\arguments{
	\item{ext}{optional character vector of file extensions to clear; if
    unspecified, all associations are removed}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
	\code{\link{setPBSext}}, \code{\link{getPBSext}}, \code{\link{openFile}}
}

\keyword{methods}
