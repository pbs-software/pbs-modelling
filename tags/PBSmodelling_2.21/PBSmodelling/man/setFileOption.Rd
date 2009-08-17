\name{setFileOption}
\alias{setFileOption}
\title{Set a PBS File Path Option Interactively}

\description{
  Set a PBS option by browsing for a file. This function provides 
  an alternative to using \code{setPBSoptions} when setting an 
  option that has a path to a file as its value.
}
\usage{
setFileOption(option)
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
  \code{\link{setPathOption}}, \code{\link{setPBSoptions}}
}
\examples{
\dontrun{
setPathOption("editor")
}
}