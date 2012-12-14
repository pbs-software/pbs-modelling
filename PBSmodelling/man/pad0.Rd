\name{pad0}
\alias{pad0}
\title{Pad Values with Leading Zeroes}
\description{
  Pad numbers and/or text with leading and/or trailing zeroes.
}
\usage{
pad0(x, n, f = 0)
}
\details{
  Converts numbers (or text coerced to numeric) to integers and then to text,
  and pads them with leading zeroes. If the factor \code{f} is >0, then 
  trailing zeroes are also added.
}
\arguments{
  \item{x}{vector of numbers and/or strings}
  \item{n}{number of text characters representing a padded integer}
  \item{f}{factor of 10 transformation on \code{x} before padding}
}
\value{
  If \code{length(f)==1} or \code{length(x)==1}, the function returns 
  a character vector representing \code{x} with leading zeroes.

  If both \code{f} and \code{x} have lengths >1, then a list of 
  character vectors indexed by \code{f} is returned.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  resetGraph(); x <- pad0(x=123,n=10,f=0:7);
  addLabel(.5,.5,paste(x,collapse="\n"),cex=1.5);
})
}
\seealso{
  \code{\link[PBSmodelling]{show0}}, \code{\link[PBSmodelling]{GT0}}
}
\keyword{print}
