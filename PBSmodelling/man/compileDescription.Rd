\name{compileDescription}
\alias{compileDescription}
\title{Convert and Save a Window Description as a List}

\description{
	Convert a \emph{window description file} (ASCII markup file) to an equivalent 
	\emph{window description list}. The output list (an ASCII file containing R-source code) 
	is complete, i.e., all default values have been added.
}

\usage{compileDescription(descFile, outFile)}

\arguments{
	\item{descFile}{name of \emph{window description file} (markup file).}
	\item{outFile}{name of output file containing R source code.}
}

\details{
	The \emph{window description file} \code{descFile} is converted to a list, 
	which is then converted to R code, and saved to \code{outFile}.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{parseWinFile}}, \code{\link{createWin}}
}

\keyword{utilities}
