\name{view}
\alias{view}
\title{View First/Last/Random n Elements/Rows of an Object}

\description{
  View the first or last or random \code{n} elements or rows of an object. 
  Components of lists will be subset using iterative calls to \code{view}.
}
\usage{
view(obj, n=5, last=FALSE, random=FALSE, print.console=TRUE, ...)
}

\arguments{
  \item{obj}{object to view.}
  \item{n}{first (default)/last/random \code{n} elements/rows of \code{obj} to view.}
  \item{last}{logical: if \code{TRUE}, last \code{n} elements/rows of \code{obj} are displayed.}
  \item{random}{logical: if \code{TRUE}, \code{n} random elements/rows 
    (without replacement) of \code{obj} are displayed.}
  \item{print.console}{logical: if \code{TRUE}, print the results to the console (default).\cr
    The results are also returned invisibly should the user wish to assign the output to an object.}
  \item{...}{additional arguments (e.g., \code{replace=TRUE} if specifying \code{random=TRUE}).}
}
\value{
  Invisibly returns the results of the call to \code{view}.
}
\note{
  If \code{random=TRUE}, random sampling will take place before the \code{last} operator is applied.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  Generic functions \code{\link[utils]{head}} and \code{\link[utils]{tail}} exist in the package \pkg{utils}.
}
\keyword{print}
