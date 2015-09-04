\name{tget}
\alias{tget}
\alias{tcall}
\alias{tprint}
\alias{tput}
\title{
  Get/Print Objects From or Put Objects Into Temporary Work Environment
}
\description{
  Get/print objects from or put objects into a temporary work environment
  called \code{.PBSmodEnv}. These objects include \code{.PBSmod},
  which controls the GUI system.
}
\usage{
tget(x, penv=NULL, tenv=.PBSmodEnv)
tcall(x, penv=NULL, tenv=.PBSmodEnv)
tprint(x, penv=NULL, tenv=.PBSmodEnv)
tput(x, penv=NULL, tenv=.PBSmodEnv)
}
\arguments{
  \item{x}{name (with or without quotes) of an object to retrieve or store in the temporary environment.}
  \item{penv}{parent environment, defaults to \code{parent.frame()} called from within the function.}
  \item{tenv}{temporary working environment, defaults to \code{.PBSmodEnv}.}
}
\details{
  These accessor functions were developed as a response to the CRAN
  repository policy statement: \dQuote{Packages should not modify the
  global environment (user's workspace).}
  
  There are also wrapper functions called \code{.win.tget}, 
  \code{.win.tcall}, and \code{.win.tprint} that can be used in 
  \emph{window description files} to launch functions or print objects
  from the \code{.PBSmodEnv} workspace. 
  The wrapper uses \code{getWinAct} to get the function (or object) 
  name that a user specifies in the \code{action} argument of a 
  widget command.
}
\note{
  Additional wrapper functions to access functions in \code{.PBSmodEnv}
  are named with the prefix \code{.win}.
}
\value{
  Objects are retrieved from or sent to the temporary working 
  environment to/from the place where the function(s) are called.
  Additionally, \code{tcall} invisibly returns the object without 
  transferring, which is useful when the object is a function that the
  user may wish to call, for example, \code{tcall(myfunc)()}.
}
\references{
  CRAN Repository Policy: 
  \url{https://cran.r-project.org/web/packages/policies.html}
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\examples{
local(envir=.PBSmodEnv,expr={
  str(tcall(.PBSmod),2)
})
}

%\seealso{}
\keyword{manip}
\keyword{environment}

