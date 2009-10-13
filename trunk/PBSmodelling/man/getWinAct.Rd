\name{getWinAct}
\alias{getWinAct}
\title{Retrieve the Last Window Action}

\description{
	Get a string vector of actions (latest to earliest).
}

\usage{getWinAct(winName)}

\arguments{
	\item{winName}{name of window to retrieve action from}
}

\details{ 
	When a function is called from a GUI, a string descriptor associated with 
	the action of the function is stored internaly (appended to the first position 
	of the action vector). A user can utilize this action as a type of argument 
	for programming purposes. The command \code{getWinAct()[1]} yields the latest action.
}

\value{
	String vector of recorded actions (latest first).
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\keyword{methods}
