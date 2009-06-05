\name{showVignettes}
\alias{showVignettes}
\title{Display Vignettes for Packages}
\description{
  Create a GUI that displays all vignettes for installed packages. 
  The user can choose to view the source file for building the vignette 
  or the final \code{.pdf} file.
}
\usage{
showVignettes(package)
}
\arguments{
  \item{package}{character string specifying package name that exists in the user's R library}
}
\details{
  If the argument \code{package} is not specified, the function
  will look for vignettes in all packages installed on the user's 
  system. The user can choose to view the source file 
  for building the vignette (usually \code{*.Rnw} or \code{*.Snw} files)
  or the final build from the source code (\code{*.pdf}).

  \code{showVignettes} uses the \pkg{PBSmodelling} function 
  \code{openFile} to display the \code{.Rnw} and \code{.pdf} files 
  using programs that the system associates with these extensions. 
  On systems that do not support file extension associations, the 
  function \code{setPBSext} can temporarily set a command to associate 
  with an extension.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{ 
  \code{\link{showHelp}}, \code{\link{openFile}}, \code{\link{setPBSext}}, \code{\link{getPBSext}}
}
\keyword{utilities}
