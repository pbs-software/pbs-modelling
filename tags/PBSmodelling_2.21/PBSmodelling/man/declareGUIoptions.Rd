\name{declareGUIoptions}
\alias{declareGUIoptions}
\title{Declare Option Names that Correspond with Widget Names}

\description{
  This function allows a GUI creator to specify widget names that 
  correspond to names in PBS options. These widgets can then be 
  used to load and set PBS options uing \code{getGUIoptions} and 
  \code{setGUIoptions}.
}
\usage{
declareGUIoptions(newOptions)
}
\arguments{
  \item{newOptions}{a character vector of option names}
}
\details{
  \code{declareGUIoptions} is typically called in a GUI initialization function.
  The option names are remembered and used for the functions 
  \code{getGUIoptions}, \code{setGUIoptions}, and \code{promptSave}.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{getGUIoptions}}, \code{\link{setGUIoptions}},
  \code{\link{promptWriteOptions}}
}
\examples{
\dontrun{
declareGUIOptions("editor")
}
}