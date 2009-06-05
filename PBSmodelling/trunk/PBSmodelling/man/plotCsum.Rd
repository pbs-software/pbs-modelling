\name{plotCsum}
\alias{plotCsum}
\title{Plot Cumulative Sum of Data}

\description{
	Plot the cumulative frequency of a data vector or matrix, 
	showing the median and mean of the distribution.
}

\usage{
plotCsum(x, add = FALSE, ylim = c(0, 1), xlab = "Measure", 
ylab = "Cumulative Proportion", ...)  
}

\arguments{
	\item{x}{vector or matrix of numeric values.}
	\item{add}{logical: if \code{TRUE}, add the cumulative frequency curve to a current plot.}
	\item{ylim}{limits for the y-axis.}
	\item{xlab}{label for the x-axis.}
	\item{ylab}{label for the y-axis.}
	\item{...}{additional arguments for the \code{plot} function.}
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\examples{
x <- rgamma(n=1000,shape=2)
plotCsum(x)
}

\keyword{hplot}
