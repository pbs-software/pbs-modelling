\name{readList}
\alias{readList}
\title{Read a List from a File in PBS Modelling Format}

\description{
  Read in a list previously saved to a file by \code{writeList}. 
  At present, only two formats are supported - R's native format 
  used by the \code{dput} function or an ad hoc \code{PBSmodelling} format. 
  The function \code{readList} detects the format automatically.

  For information about the \code{PBSmodelling} format, see \code{writeList}.
}

\usage{
readList(fname)
}
\arguments{
  \item{fname}{file name of the text file containing the list.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{packList}}, \code{\link{unpackList}}, \code{\link{writeList}}
}
\keyword{list}
\keyword{file}
