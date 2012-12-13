\name{cleanWD}
\alias{cleanWD}
\title{Launch a GUI for File Deletion}

\description{
  Launches a new window which contains an interface for deleting 
  specified files from the working directory.
}
\usage{
cleanWD(files)
}
\arguments{
  \item{files}{character vector of file names used for clean options.}
}
\details{
  All arguments may contain wildcard characters (\code{"*"} to match 0 or
  more characters, \code{"?"} to match any single character).

  The GUI includes the following:
  \tabular{ll}{
  \bold{1} \tab Check boxes for each suffix in the \code{suffix} argument and \cr
           \tab for each file name in the \code{files} argument.\cr
  \bold{2} \tab Buttons marked "Select All" and "Select None" for \cr 
           \tab selecting and clearing all the check boxes, respectively.\cr
  \bold{3} \tab A "Clean" button that deletes files in the working directory \cr
           \tab matching file name expansion of files chosen with a check box.
  }
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  cleanWD(c("*.bak","*.tmp","junk*"))
})
}
}
\keyword{utilities}