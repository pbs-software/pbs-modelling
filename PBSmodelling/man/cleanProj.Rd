\name{cleanProj}
\alias{cleanProj}
\title{Launch a GUI for Project File Deletion}

\description{
  Launches a new window which contains an interface for deleting 
  junk files associated with a prefix and a set of suffixes 
  (e.g., PBSadmb project) from the working directory.
}
\usage{
cleanProj(prefix, suffix, files)
}
\arguments{
  \item{prefix}{default prefix for file names.}
  \item{suffix}{character vector of suffixes used for clean options.}
  \item{files}{character vector of file names used for clean options.}
}
\details{
  All arguments may contain wildcard characters (\code{"*"} to match 0 or
  more characters, \code{"?"} to match any single character).

  The GUI includes the following:
  \tabular{ll}{
  \bold{1} \tab An entry box for the prefix. \cr
           \tab The default value of this entry box is taken from \code{prefix}. \cr
  \bold{2} \tab Check boxes for each suffix in the \code{suffix} argument and \cr
           \tab for each file name in the \code{files} argument.\cr
  \bold{3} \tab Buttons marked "Select All" and "Select None" for \cr 
           \tab selecting and clearing all the check boxes, respectively.\cr
  \bold{4} \tab A "Clean" button that deletes files in the working directory \cr
           \tab matching one of the following criteria: \cr
           \tab (i) file name matches both an expansion of a concatenation of a \cr
           \tab prefix in the entry box and a suffix chosen with a check box; or \cr
           \tab (ii) file name matches an expansion of a file chosen with a check box.
  }
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  cleanProj(prefix="foo",suffix=c(".a*",".b?",".c","-old.d"),files=c("red","blue"))
})
}
}
\keyword{utilities}
