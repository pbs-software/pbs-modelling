\name{setWinAct}
\alias{setWinAct}
\title{Add a Window Action to the Saved Action Vector}
\description{
	Append a string value specifying an action to the first position of an 
	action vector.
}
\usage{
setWinAct(winName, action)
}

\arguments{
	\item{winName}{window name where action is taking place.}
	\item{action}{string value describing an action.}
}
\details{
	When a function is called from a GUI, a string descriptor associated with 
	the action of the function is stored internaly (appended to the first position 
	of the action vector). A user can utilize this action as a type of argument 
	for programming purposes. The command \code{getWinAct()[1]} yields the latest action.

	Sometimes it is useful to \dQuote{fake} an action. Calling \code{setWinAct} allows 
	the recording of an action, even if a button has not been pressed.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\keyword{methods}
