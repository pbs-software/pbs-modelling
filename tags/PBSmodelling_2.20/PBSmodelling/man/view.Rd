\name{view}
\alias{view}
\title{View First/Last/Random n Elements/Rows of an Object}

\description{
  View the first or last or random \code{n} elements or rows of an object. 
  Components of lists will be subset also.
}
\usage{
view(obj, n=5, last=FALSE, random=FALSE, ...)
}

\arguments{
  \item{obj}{object to view.}
  \item{n}{first (default)/last/random \code{n} elements/rows of \code{obj} to view.}
  \item{last}{logical: if \code{TRUE}, last \code{n} elements/rows of \code{obj} are displayed.}
  \item{random}{logical: if \code{TRUE}, \code{n} random elements/rows 
    (without replacement) of \code{obj} are displayed.}
  \item{...}{additional arguments (e.g., \code{replace=T} if specifying \code{random=T}).}
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\keyword{print}
