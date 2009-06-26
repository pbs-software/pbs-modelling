\name{showRes}
\alias{showRes}
\title{Show Results of Expression Represented by Text}

\description{
  Evaluate the supplied expression, reflect it on the command line, 
  and show the results of the evaluation.
}
\usage{
showRes(x, cr=TRUE, pau=TRUE)
}
\arguments{
	\item{x}{an R expression to evaluate}
	\item{cr}{logical: if \code{TRUE}, introduce extra carriage returns}
	\item{pau}{logical: if \code{TRUE}, pause after expression reflection and execution}
}
\value{
  The results of the expression are return invisibly.
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\examples{
showRes("x=rnorm(100)",pau=FALSE)
}
\keyword{utilities}
