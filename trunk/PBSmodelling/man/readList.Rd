\name{readList}
\alias{readList}
\title{Read a List from an ASCII File}

\description{
  Read in a list previously saved to an ASCII file by various R functions such
  as \code{dput}, \code{dump}, and \code{writeList}. The latter can render
  lists in a convenient \pkg{PBSmodelling} format. The function \code{readList}
  detects the format automatically.

  For information about the \pkg{PBSmodelling} format, see \code{writeList}.
}
\usage{
  readList(fname)
}
\arguments{
  \item{fname}{file name of the text file containing the list.}
}
\value{
  Returns a list object from ASCII files originally formatted in one of the 
  following ways: \cr
  \code{"D"} = created by the R functions \code{dput} or \code{dump};\cr
  \code{"R"} = R list object that uses `structure' (e.g., Windows History file);\cr
  \code{"P"} = PBS-formatted file (see \code{writeList});\cr
  \code{"C"} = comment-delimited file (e.g., Awatea/Coleraine input files).
}
\section{Warning}{
  When importing a list in the \code{PBSmodelling} (\code{"P"}) format,
  if two list elements share the same name, the list will import
  incorrectly.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{packList}}, \code{\link{unpackList}}, \code{\link{writeList}}
}
\keyword{list}
\keyword{file}
