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
  \item{obj}{\code{object} -- an R object to view.}
  \item{n}{\code{numeric} -- first (default)/last/random \code{n} elements/rows of \code{obj} to view.}
  \item{last}{\code{logical} -- if \code{TRUE}, last \code{n} elements/rows of \code{obj} are displayed.}
  \item{random}{\code{logical} -- if \code{TRUE}, \code{n} random elements/rows 
    (without replacement) of \code{obj} are displayed.}
  \item{print.console}{\code{logical} -- if \code{TRUE}, print the results to the console (default).\cr
    The results are also returned invisibly should the user wish to assign the output to an object.}
  \item{...}{\code{dots} -- additional arguments (e.g., \code{replace=TRUE} if specifying \code{random=TRUE}).}
}
\value{
  Invisibly returns the results of the call to \code{view}.
}
\note{
  If \code{random=TRUE}, random sampling will take place before the \code{last} operator is applied.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-12}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{lisp}},
  \code{\link[PBSmodelling]{showArgs}},
  \code{\link[PBSmodelling]{testCol}},
  \code{\link[PBSmodelling]{viewCode}}\cr
In package \pkg{utils}:\cr
  \code{\link[utils]{head}}, \code{tail}
}
\keyword{print}
