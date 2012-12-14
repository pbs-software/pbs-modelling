\name{openFile}
\alias{openFile}
\title{Open a File with an Associated Program}
\description{
  Open a file using the program associated with its extension 
  defined by the Windows shell.  Non-windows users, or users 
  wishing to overide the default application, can specify a 
  program association using \code{setPBSext}.
}
\usage{
openFile(fname, package=NULL)
}
\arguments{
  \item{fname}{name(s) of file(s) to open.}
  \item{package}{(optional) open files relative to this package}
}
\value{
  An invisible string vector of the file names and/or commands + file names.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\note{
  If a command is registered with \code{setPBSext}, then 
  \code{openFile} will replace all occurrences of \code{"\%f"} 
  with the absolute path of the filename, before executing the command.
}
\section{Warning}{
  Windows only: \code{openFile( ".." )} does not work as expected, but openFile( "../.." ) only opens the first parent directory.
}

\seealso{
  \code{\link{getPBSext}}, \code{\link{setPBSext}}, \code{\link{clearPBSext}},
  \code{\link{writePBSoptions}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  #use openFile directly:
  openFile( "doc/PBSmodelling-UG.pdf", package="PBSmodelling" )
})
local(envir=.PBSmodEnv,expr={
  #via doAction in a window description file:
  createWin( "button text=help func=doAction action=\"openFile( `doc/PBSmodelling-UG.pdf`, package=`PBSmodelling` )\"", astext=TRUE )
})
local(envir=.PBSmodEnv,expr={
  #############################
  # Set up firefox to open .html files (only applicable if firefox is NOT the default web browser
  setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
  openFile("foo.html")
})
}
}
\keyword{file}
