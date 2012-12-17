\name{setPathOption}
\alias{setPathOption}
\title{Set a PBS Path Option Interactively}

\description{
  Set a PBS option by browsing for a directory. This function provides 
  an alternative to using \code{setPBSoptions} when setting an option 
  that has a path as its value.
}
\usage{
setPathOption(option)
}
\arguments{
  \item{option}{name PBS option to change}
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If all the required arguments are missing, it is assumed that 
  the function is being called by a GUI widget.
}
\seealso{
  \code{\link{setFileOption}}, \code{\link{setPBSoptions}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  setPathOption("myPath")
})
}
}

