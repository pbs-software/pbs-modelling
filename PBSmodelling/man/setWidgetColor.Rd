\name{setWidgetColor}
\alias{setWidgetColor}
\title{Update Widget Color}
\description{
	Update the foreground and background colors of a widget
}
\usage{
setWidgetColor( name, winName, ... )
}
\arguments{
	\item{name}{the name of the widget}
	\item{winname}{window from which to select the GUI widget. The default 
		takes the window that has most recently received new user input.}
	\item{...}{ any combination of "fg", "bg", "entryfg", "entrybg" depending on type of widget - see details}
}
\details{
	Depending on the type of widget, different color options can be modified.
	entry widgets accept entryfg, entrybg (but do not currently support fg/bg)
	
	droplist, check, label, button widgets accept fg/bg
}
\author{
  Alex Couture-Beil
}
\examples{
createWin("label \"hello world\" name=hello", astext=TRUE)
setWidgetColor( "hello", bg="lightgreen", fg="purple" )
}
\keyword{methods}
