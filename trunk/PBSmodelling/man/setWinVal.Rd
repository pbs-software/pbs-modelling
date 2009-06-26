\name{setWinVal}
\alias{setWinVal}
\title{Update Widget Values}
\description{
	Update a widget with a new value.
}
\usage{
setWinVal(vars, winName)
}
\arguments{
	\item{vars}{a list or vector with named components.}
	\item{winName}{window from which to select GUI widget values. The default 
		takes the window that has most recently received new user input.}
}
\details{
	The \code{vars} argument expects a list or vector with named elements. 
	Every element name corresponds to the widget name which will be updated 
	with the supplied element value.

	The \code{vector}, \code{matrix}, and \code{data} widgets can be updated in 
	several ways. If more than one name is specified for the \code{names} argument 
	of these widgets, each element is treated like an \code{entry} widget. 

	If however, a single \code{name} describes any of these three widgets, the entire 
	widget can be updated by passing an appropriately sized object.

	Alternatively, any element can be updated by appending its index in square brackets 
	to the end of the \code{name}. The \code{data} widget is indexed differently 
	than the \code{matrix} widget by adding "d" after the brackets. This tweak is necessary 
	for the internal coding (bookkeeping) of \emph{PBS Modelling}. Example: \code{"foo[1,1]d"}.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{getWinVal}}, \code{\link{createWin}}
}
\examples{
\dontrun{
winDesc <- c("vector length=3 name=vec",
             "matrix nrow=2 ncol=2 name=mat",
             "slideplus name=foo");
createWin(winDesc, astext=TRUE)
setWinVal(list(vec=1:3, "mat[1,1]"=123, foo.max=1.5, foo.min=0.25, foo=0.7))
}
}
\keyword{methods}
