\name{updateGUI}
\Rdversion{1.1}
\alias{updateGUI}
\title{
  Update Active GUI With Local Values
}
\description{
  Update the currently active GUI with values from R's memory at the specified location.
}
\usage{
updateGUI(scope = "L")
}
\arguments{
  \item{scope}{either \code{"L"} for the parent frame, \code{"G"} for the global environment,
    or an explicit R environment}
}
\details{
  If the characteristics of the local R objects do not match those of the GUI objects,
  the update will fail.
}
\value{
  Invisibly returns a Boolean vector that specifies whether the objects in the local R
  environment match items in the active GUI.
}
\author{
  Rob Kronlund, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{getWinVal}}, \code{\link{setWinVal}}
}
\keyword{methods}

