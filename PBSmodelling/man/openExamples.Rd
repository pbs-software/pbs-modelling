\name{openExamples}
\alias{openExamples}
\title{Open Example Files from a Package}

\description{
  Open examples from the examples subdirectory of a given package.
}
\usage{
openExamples(package, prefix, suffix)
}
\arguments{
  \item{package}{name of the package that contains the examples.}
  \item{prefix}{prefix of the example file(s).}
  \item{suffix}{character vector of suffixes for the example files.}
}
\details{
  Copies of each example file are placed in the working directory 
  and opened. If files with the same name already exist, the user 
  is prompted with a choice to overwrite.

  To use this function in a \emph{window description file}, the 
  \code{package}, \code{prefix} and \code{suffix} arguments must 
  be specified as the action of the widget that calls 
  \code{openExamples}. Furthermore, \code{package}, \code{prefix}, 
  and each \code{suffix} must be separated by commas. For example, 
  \code{action=myPackage,example1,.r,.c} will copy \code{example1.r} 
  and \code{example2.c} from the \code{examples} directory of the 
  package \pkg{myPackage} to the working directory and open these 
  files. If the function was called by a widget, a widget named 
  \code{prefix} will be set to the specified prefix.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If all the required arguments are missing, it is assumed 
  that the function is being called by a GUI widget.
}
\seealso{
  \code{\link{openFile}}
}
\examples{
\dontrun{
# Copies fib.c and fib.r from the examples directory in 
# PBSmodelling to the temporary working directory, and opens these files.
local(envir=.PBSmodEnv,expr={
  cwd = getwd(); setwd(tempdir())
  openExamples("PBSmodelling", c("fib"), c(".r", ".c"))
  setwd(cwd)
})
}
}
\keyword{file}
