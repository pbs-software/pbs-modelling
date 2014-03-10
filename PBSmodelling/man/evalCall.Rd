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
\note{
  Sometimes the user may wish to pass arguments into a function to be used
  by other functions within, but may not want all the arguments to be used, 
  depending on the functions subsequently called. In this case, the user needs
  to create a list object called \code{dots}, which is passed to \code{evalCall}.

  For instance, if the user passes \code{lwd=4} but only wants this 
  used in a call to \code{lines} but not in a call to \code{points},
  the function might look like this:
  \preformatted{
myfunc = function(x=seq(0,360,5), ...) {
  pdots = ldots = list(...)
  pdots[["lwd"]] = NULL
  ldots[["col"]] = "cyan"
  xrad = x*pi/180
  plot(sin(xrad),type="n")
  evalCall(lines, argu=list(x=sin(xrad)), dots=ldots, checkpar=TRUE)
  evalCall(points,argu=list(x=sin(xrad)), dots=pdots, checkpar=TRUE)
}
myfunc(lwd=4,pch=20,col=" blue")
}
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

