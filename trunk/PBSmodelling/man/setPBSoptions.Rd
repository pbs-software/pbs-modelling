\name{setPBSoptions}
\alias{setPBSoptions}
\title{Set A User Option}

\description{
  Options set by the user for use by other functions.
}

\usage{
setPBSoptions(option, value, sublist=FALSE)
}
\arguments{
  \item{option}{name of the option to set.}
  \item{value}{new value to assign this option.}
  \item{sublist}{if \code{value} is a sublist (list component) of \code{option},
    this list component can be changed individually using \code{sublist=TRUE}.}
}
\details{
  Objects can be placed into the PBS options manager (see \code{\link[PBSmodelling]{PBSoptions-class}}).\cr
  If the user wishes to change the object associated with an option, issue the command:
  \preformatted{setPBSoptions("someOldOption",someNewOption)}

  If an option comprises a list object, a user can alter specific components of the list 
  by activating the \code{sublist} argument:
  \preformatted{setPBSoptions(option="myList", value=list(gamma=130), sublist=TRUE)}
  See example below.
}
\note{
  A value \code{.PBSmod$.options$.optionsChanged} is set to \code{TRUE} when an option is changed,
  so that the user doesn't always have to be prompted to save the options file. \cr
  By default, \code{.PBSmod$.options$.optionsChanged} is not set or \code{NULL}. \cr
  Also, if an option is set to \code{""} or \code{NULL} then it is removed. \cr
  \code{.initPBSoptions()} is now called first (options starting with a dot "." 
  do not set \code{.optionsChanged}).
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  myList=list(alpha=1,beta=2,gamma=3,delta=4)
  setPBSoptions(option="myList", myList)
  cat("Original myList:\n---------------\n")
  print(getPBSoptions("myList"))
  setPBSoptions(option="myList", value=list(gamma=130), sublist=TRUE)
  cat("Revised myList:\n--------------\n")
  print(getPBSoptions("myList"))
})
}
\seealso{
  \code{\link[PBSmodelling]{getPBSoptions}}, \code{\link[PBSmodelling]{writePBSoptions}},
  \code{\link[PBSmodelling]{readPBSoptions}}
}
\keyword{methods}

