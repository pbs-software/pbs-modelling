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
  During the build, optional arguments (\dots) are checked for \cr
  (i) duplication with explicit arguments \code{argu}: if any are duplicated,
  the user-supplied arguments supersede the explicit ones; \cr
  (ii) availability as usable arguments in \code{fn}, \code{fn.default} if
  \code{checkdef=TRUE}, and \code{par} if \code{checkpar=TRUE}.
}
\value{
  Invisibly returns the string expression of the function call that is
  passed to \code{eval(parse(text=expr))}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBSmodelling]{doAction}}, \code{\link[PBSmodelling]{plotAsp}}  
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  # A user may have a function that calls other functions
  # using specific defaults (e.g., blue triangles)
  #------------------------------------------------------
  pbsfun = function(..., use.evalCall=TRUE) {
    plotAsp(0,0,type="n",xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),
      axes=FALSE, frame.plot=TRUE, xlab="",ylab="")
    if (use.evalCall)
      evalCall(polygon, ...,
        argu=list(x=c(-1,1,0),y=c(1,1,-1), col="dodgerblue", border="grey"))
    else
      polygon(x=c(-1,1,0),y=c(1,1,-1),col="dodgerblue",border="grey",...) 
  }
  par(mfrow=c(2,1))
  pbsfun(lwd=4,use.evalCall=FALSE)
  #------------------------------------------------------
  # But what if the user wants pink triangles?
  pbsfun(col="pink",lwd=4,use.evalCall=TRUE,checkpar=TRUE)
  par(oldpar)
})

# Without 'evalCall' an error occurs due to duplicated arguments
\dontrun{pbsfun(col="pink",lwd=4,use.evalCall=FALSE)}
}
\keyword{ programming }
\keyword{ character }

