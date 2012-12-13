\name{createVector}
\alias{createVector}
\title{Create a GUI with a Vector Widget}

\description{
  Create a basic window containing a vector and a submit button. 
  This provides a quick way to create a window without the need 
  for a \emph{window description file}.
}

\usage{
createVector(vec, vectorLabels=NULL, func="", 
    windowname="vectorwindow", env=NULL)
}

\arguments{
  \item{vec}{a vector of strings representing widget variables. 
    The values in \code{vec} become the default values for the widget.
    If \code{vec} is named, the names are used as the variable names. }
  \item{vectorLabels}{an optional vector of strings to use as labels above each widget.}
  \item{func}{string name of function to call when new data are entered 
    in widget boxes or when "GO" is pressed.}
  \item{windowname}{unique window name, required if multiple vector windows are created.}
  \item{env}{an environment in which to evaluate widget callback functions.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{createWin}}
}

\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
#user defined function which is called on new data  
drawLiss <- function() {
  oldpar = par(no.readonly=TRUE); on.exit(par(oldpar))
  getWinVal(scope="L");
  tt <- 2*pi*(0:k)/k; x <- sin(2*pi*m*tt); y <- sin(2*pi*(n*tt+phi));
  plot(x,y,type="p"); invisible(NULL); };

#create the vector window
createVector(c(m=2, n=3, phi=0, k=1000), func="drawLiss",
  vectorLabels=c("x cycles","y cycles", "y phase", "points"));
})
}
}

\keyword{utilities}
