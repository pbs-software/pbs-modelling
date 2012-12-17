\name{unpackList}
\alias{unpackList}
\title{Unpack List Elements into Variables}

\description{
  Make local or global variables (depending on the scope specified) from 
  the named components of a list.
}
\usage{
unpackList(x, scope="L")
}
\arguments{
  \item{x}{named list to unpack.}
  \item{scope}{If \code{"L"}, create variables local to the parent
    frame that called the function. If \code{"P"}, create variables
    in the temporary package workspace called \code{.PBSmodEnv}. 
    If \code{"G"}, create global variables.}
}
\value{
  A character vector of unpacked variable names.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{packList}}, \code{\link{readList}}, \code{\link{writeList}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  x <- list(a=21,b=23);
  unpackList(x);
  print(a);
})
}
\keyword{file}
\keyword{list}
