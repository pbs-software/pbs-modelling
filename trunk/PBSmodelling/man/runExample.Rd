\name{runExample}
\alias{runExample}
\title{
  Run a Single GUI Example Included with PBS Modelling
}
\description{
  Display a GUI to demonstrate one PBS Modelling example.

  The example source files can be found in the R directory 
  \code{.../library/PBSmodelling/examples}.
}
\usage{
  runExample(ex, pkg="PBSmodelling")
}
\arguments{
  \item{ex}{string specifying an example in the \code{pkg} directory \code{examples}. }
  \item{pkg}{package with an \code{examples} subdirectory. }
}
\details{
  If no example is specified or if the example does not exist, a GUI pops up
  informing you of potential choices. Note that the string choice is case-sensitive.

  Some examples use external packages which must be installed to work correctly:

  \code{BRugs} - \code{LinReg}, \code{MarkRec}, and \code{CCA};

  \code{odesolve/PBSddesolve} - \code{FishRes};

  \code{PBSmapping} - \code{FishTows}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\note{
  The examples are copied from \code{.../library/PBSmodelling/examples} to R's current 
  temporary working directory and run from there.
}
\seealso{
  \code{\link{runDemos}}, \code{\link{runExamples}}
}
\keyword{utilities}
