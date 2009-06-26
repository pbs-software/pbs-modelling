\name{setWidgetState}
\alias{setWidgetState}
\title{Update Widget State}
\description{
	Update the read-only state of a widget.
}
\usage{
setWidgetState( varname, state, radiovalue, winname )
}
\arguments{
	\item{varname}{the name of the widget}
	\item{state}{"normal" or "disabled"}
	\item{radiovalue}{if specified, disable a particular radio option, as identified by the value, rather than the complete set (identified by the common name)}
	\item{winname}{window from which to select the GUI widget. The default 
		takes the window that has most recently received new user input.}
}
\details{
	The \code{varname} argument expects a name which corresponds to a widget name.
	Alternatively, any element can be updated by appending its index in square brackets 
	to the end of the \code{name}. The \code{data} widget is indexed differently 
	than the \code{matrix} widget by adding "d" after the brackets. This tweak is necessary 
	for the internal coding (bookkeeping) of \emph{PBS Modelling}. Example: \code{"foo[1,1]d"}.
	
	The state can either be "normal" which allows the user to edit values, or "disabled" which
	restricts the user from editing the values. Entry widgets also support "readonly" which will
	allow the user to copy and paste data.
}
\author{
  Alex Couture-Beil
}
\examples{
\dontrun{
winDesc <- c('vector length=3 name=vec labels="normal disabled readonly" values="1 2 3"',
             "matrix nrow=2 ncol=2 name=mat" );
createWin(winDesc, astext=TRUE)

setWidgetState( "vec[1]", "normal" )
setWidgetState( "vec[2]", "disabled" )
setWidgetState( "vec[3]", "readonly" )

setWidgetState( "mat", "readonly" )
setWinVal( list( mat = matrix( 1:4, 2, 2 ) ) )
}
}
\keyword{methods}
