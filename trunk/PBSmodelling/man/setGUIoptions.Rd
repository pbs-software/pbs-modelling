\name{setGUIoptions}
\alias{setGUIoptions}
\title{Set PBS Options from Widget Values}
\description{
  Set PBS options from corresponding values of widgets in a GUI.
}
\usage{
setGUIoptions(option)
}
\arguments{
  \item{option}{the name of a single option or the string \code{"*"}.}
}
\details{
  A GUI may have PBS options that it uses, which have corresponding widgets that
  are used for entering values for these options. These are declared by
  \code{declareGUIoptions}.

  If the \code{option} argument is the name of an option, 
  \code{setGUIoptions} transfers the value of this option from a 
  same-named widget into PBS options global R environment database.

  If the \code{option} argument is \code{"*"}, then all the 
  options that have been declared by \code{declareGUIoptions} 
  will be transferred in this fashion.

  To use this function in a \emph{window description file}, the 
  \code{option} argument must be specified as the action of the 
  widget that calls \code{setGUIoptions} -- \code{action=editor} 
  or \code{action=*} for example.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If all the required arguments are missing, it is assumed that 
  the function is being called by a GUI widget.
}
\seealso{
  \code{\link{declareGUIoptions}}, \code{\link{getGUIoptions}},
  \code{\link{setPBSoptions}},
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  setGUIoptions("editor")
})
}
}

