\name{openFile}
\alias{openFile}
\title{Open File with Associated Program}
\description{
  Open a file using the program that the operating system (Windows / Mac
  OS X / Linux) associates with its type.  Users wishing to override the
  default application can specify a program association using \code{'setPBSext'}.
}
\usage{
openFile(fname="", package=NULL, select=FALSE)
}
\arguments{
  \item{fname}{\code{character} -- vector containing file names to open.}
  \item{package}{\code{character} -- (optional) package name; open files relative to this package.}
  \item{select}{\code{logical} -- if \code{TRUE}, force the use of \code{'selectFile'}.}
}
\value{
  An invisible string vector of the file names and/or commands with file names.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-01-05}
}
\note{
  If a command is registered with \code{setPBSext}, then 
  \code{openFile} will replace all occurrences of \code{"\%f"} 
  with the absolute path of the filename before executing the command.
}

\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{getPBSext}},
  \code{\link[PBSmodelling]{setPBSext}},
  \code{\link[PBSmodelling]{clearPBSext}},
  \code{\link[PBSmodelling]{writePBSoptions}}
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
  # Set up 'Firefox' to open '.html' files (only applicable if Firefox is NOT default web browser)
  setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
  openFile("foo.html")
})
}
}
\keyword{file}
