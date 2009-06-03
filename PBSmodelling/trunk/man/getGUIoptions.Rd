\name{getGUIoptions}
\alias{getGUIoptions}
\title{Get PBS Options for Widgets}

\description{
  Get the PBS options declared for GUI usage and set their 
  corresponding widget values.
}
\usage{
getGUIoptions()
}
\details{
  The options declared using \code{declareGUIoptions} are 
  copied from the R environment into widget values. These widgets 
  should have names that match the names of their corresponding options.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{declareGUIoptions}}, \code{\link{setGUIoptions}},
  \code{\link{promptWriteOptions}}, \code{\link{readPBSoptions}}
}
\examples{
\dontrun{
getPBSoptions() #loads from default PBSoptions.txt
}
}