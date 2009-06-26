\name{evalCall}
\alias{evalCall}
\title{ Evaluate a Function Call }
\description{
  Evaluates a function call after resolving potential
  argument conflicts.
}
\usage{
evalCall(fn, argu, ..., envir = parent.frame(),
    checkdef=FALSE, checkpar=FALSE)
}
\arguments{
  \item{fn}{ R function }
  \item{argu}{ list of explicitly named arguments and their values to pass to \code{fn}. }
  \item{\dots}{ additional arguments that a user might wish to pass to \code{fn}. }
  \item{envir}{ environment from which the call originates (currently has no use or effect). }
  \item{checkdef}{ logical: if \code{TRUE}, gather additional formal arguments from the 
    functions default function. }
  \item{checkpar}{ logical: if \code{TRUE}, gather additional graphical arguments from 
    the list object \code{par}. }
}
\details{
  This function builds a call to the specified function and executes it. 
  During the build, optional arguments \dots are checked for \cr
  (i) duplication with explicit arguments \code{argu}: if any are duplicated,
  the user-supplied arguments supercede the explict ones; \cr
  (ii) availability as usuable arguments in \code{fn}, \code{fn.default} if
  \code{checkdef=TRUE}, and \code{par} if \code{checkpar=TRUE}.
}
\value{
  Invisibly returns the string expression of the function call that is
  passed to \code{eval(parse(text=expr))}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{doAction}} }
\keyword{ programming }
\keyword{ character }
