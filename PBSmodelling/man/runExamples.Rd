\name{runExamples}
\alias{runExamples}
\title{Run GUI Examples Included with PBS Modelling}

\description{
  Display an interactive GUI to demonstrate PBS Modelling examples.

  The example source files can be found in the R directory 
  \code{.../library/PBSmodelling/examples}.
}
\usage{runExamples()}

\details{
  Some examples use external packages which must be installed to work correctly:

  \code{BRugs} - \code{LinReg}, \code{MarkRec}, and \code{CCA};

  \code{odesolve/PBSddesolve} - \code{FishRes};

  \code{PBSmapping} - \code{FishTows}.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\note{
  The examples are copied from \code{.../library/PBSmodelling/examples} to R's current 
  temporary working directory and run from there.
}
\seealso{
  \code{\link{runDemos}}, \code{\link{runExample}}
}
\keyword{utilities}

