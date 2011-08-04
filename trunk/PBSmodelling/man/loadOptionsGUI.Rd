\name{loadOptionsGUI}
\alias{loadOptionsGUI}
\alias{saveOptionsGUI}
\title{
  Load and Save Options Values to and from a GUI
}
\description{
  These functions are used to move option values to and from a GUI. 
  Option values are stored within an R object (as referenced by the \code{option.object}). \cr
  \code{loadOptionsGUI} copies the values from the R object to the GUI. \cr
  \code{saveOptionsGUI} copies the GUI values from the tcltk GUI to the R object.
}
\usage{
loadOptionsGUI(option.object)
saveOptionsGUI(option.object)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example using \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}

