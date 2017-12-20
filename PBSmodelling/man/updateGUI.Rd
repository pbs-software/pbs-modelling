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
  \item{scope}{either \code{"L"} for the parent frame, \code{"P"} for
    the temporary work environment \code{.PBSmodEnv}, \code{"G"} for
    the global environment, or an explicit R environment.}
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
  Rob Kronlund, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{getWinVal}}, \code{\link{setWinVal}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  #law of free food: http://www.phdcomics.com/comics.php?f=1223
  createWin( c( 
    "vector names=\"foodquality hunger cost\" values=\"0.6 0.8 0.1\" width=10",
    "entry name=taste edit=F label=taste:" ), astext=TRUE )
  getWinVal( scope="P" )
  taste <- foodquality * hunger / cost
  updateGUI()
})
}
}
\keyword{methods}

