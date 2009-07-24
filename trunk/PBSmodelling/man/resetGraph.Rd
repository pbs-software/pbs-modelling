\name{resetGraph}
\alias{resetGraph}
\title{Reset par Values for a Plot}

\description{
	Reset \code{par()} to default values to ensure that a new plot 
	utilizes a full figure region. This function helps manage the device 
	surface, especially after previous plotting has altered it.
}

\usage{resetGraph(reset.mf=TRUE)}
\arguments{
	\item{reset.mf}{if \code{TRUE} reset the multi-frame status; otherwise
	  preserve \code{mfrow}, \code{mfcol}, and \code{mfg}}
}
\details{
	This function resets \code{par()} to its default values.
	If \code{reset.mf=TRUE}, it also clears the graphics device with 
	\code{frame()}. Otherwise, the values of \code{mfrow}, \code{mfcol}, 
	and \code{mfg} are preserved, and graphics continues as usual in
	the current plot. Use \code{resetGraph} only before a high level
	command that would routinely advance to a new frame.
}
\value{
	invisible return of the reset value \code{par()}
}
\author{
	Jon Schnute, Pacific Biological Station, Nanaimo BC
}

\keyword{device}
