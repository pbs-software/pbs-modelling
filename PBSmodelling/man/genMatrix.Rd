\name{genMatrix}
\alias{genMatrix}
\title{Generate Test Matrices for plotBubbles}

\description{
  Generate a test matrix of random numbers (\code{mu} = mean 
  and \code{signa} = standard deviation), primarily for \code{plotBubbles}.
}
\usage{
genMatrix(m,n,mu=0,sigma=1)
}
\arguments{
  \item{m}{number of rows}
  \item{n}{number of columns}
  \item{mu}{mean of normal distribution}
  \item{sigma}{standard deviation of normal distribution}
}
\value{
  An \code{m} by \code{n} matrix with normally distributed random values.
}
\author{
  Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
\code{\link{plotBubbles}} 
}
\examples{
local(envir=.PBSmodEnv,expr={
  plotBubbles(genMatrix(20,6))
})
}
\keyword{array}
