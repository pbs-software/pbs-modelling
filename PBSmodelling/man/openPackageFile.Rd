\name{openPackageFile}
\alias{openPackageFile}
\title{Open a File from a Package Subdirectory}

\description{

	***this function will be depricated - use \link{openFile} or doAction with openFile***

  Open a file from a package in the R library, given the package 
  name and the file path relative to the package root directory.
}
\usage{
openPackageFile(package, filepath)
}
\arguments{
  \item{package}{name of the package}
  \item{filepath}{path to file from the package's root directory}
}
\details{
  The \code{openFile} function is used to open the file, using 
  associations set by \code{setPBSext}.

  To use this function in a \emph{window description file}, the 
  \code{package} and \code{filepath} arguments must be specified 
  as the action of the widget that calls \code{openPackageFile}. 
  Furthermore, \code{package} and \code{filepath} must be 
  separated by commas (e.g., \code{action=myPackage,/doc/help.pdf}).
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If all the required arguments are missing, it is assumed that 
  the function is being called by a GUI widget.
}

\seealso{
  \code{\link{openFile}}, \code{\link{setPBSext}}, \code{\link{openProjFiles}},
  \code{\link{openExamples}}
}

\examples{
\dontrun{
#use openFile directly:
openFile( "doc/PBSmodelling-UG.pdf", package="PBSmodelling" )

#via doAction in a window description file:
createWin( "button text=help func=doAction action=\"openFile( `doc/PBSmodelling-UG.pdf`, package=`PBSmodelling` )\"", astext=TRUE )
	
}
}
\keyword{file}