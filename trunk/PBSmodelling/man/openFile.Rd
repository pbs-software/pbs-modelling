\name{openFile}
\alias{openFile}
\title{Open a File with an Associated Program}
\description{
  Open a file using the program that the operating system (Windows/Mac
  OS X/Linux) associates with its type.  Users wishing to override the
  default application can specify a program association using
  \code{setPBSext}.
}
\usage{
openFile(fname, package=NULL)
}
\arguments{
  \item{fname}{character vector, containing file names to open.}
  \item{package}{(optional) open files relative to this package.}
}
\value{
  An invisible string vector of the file names and/or commands with file names.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\note{
  If a command is registered with \code{setPBSext}, then 
  \code{openFile} will replace all occurrences of \code{"\%f"} 
  with the absolute path of the filename before executing the command.
}

\seealso{
  \code{\link{getPBSext}}, \code{\link{setPBSext}}, \code{\link{clearPBSext}},
  \code{\link{writePBSoptions}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  # use openFile directly:
  openFile( "doc/PBSmodelling-UG.pdf", package="PBSmodelling" )
})
local(envir=.PBSmodEnv,expr={
  # via doAction in a window description file:
  createWin( "button text=help func=doAction width=20 pady=25 bg=green
  action=\"openFile(`doc/PBSmodelling-UG.pdf`,package=`PBSmodelling`)\"", astext=TRUE)
})
local(envir=.PBSmodEnv,expr={
  # Set up Firefox to open .html files (only applicable if Firefox is NOT default web browser)
  setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
  openFile("foo.html")
})
}
}
\keyword{file}
