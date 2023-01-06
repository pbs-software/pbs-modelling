\name{setPBSext}
\alias{setPBSext}
\title{Set Command to Associate with File Name Extension}

\description{
  Set a command with an associated extension, for use in 
  \code{openFile}.  The command must specify where the target file 
  name is inserted by indicating a \code{'\%f'}.
}
\usage{
setPBSext(ext, cmd)
}
\arguments{
  \item{ext}{\code{character} -- string specifying the extension suffix.}
  \item{cmd}{\code{character} -- command string to associate with the extension.}
}
\note{
  These values are not saved from one \emph{PBS Modelling} session to the next.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Regional Headquarters (RHQ), Vancouver BC\cr
  Last modified \code{Rd: 2023-01-05}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{getPBSext}},
  \code{\link[PBSmodelling]{openFile}},
  \code{\link[PBSmodelling]{clearPBSext}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  ## Set 'Firefox' to open '.html' files (only applicable if Firefox is NOT default web browser)
  setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
  openFile("foo.html")

  ## Set 'notepad' to open '.txt' files (only applicable if notepad is NOT default editor)
  ## Note that commands to editors (at least in Windows) do not use 'file://'.
  setPBSext('txt', '"C:/Windows/notepad.exe" \%f')
  openFile("foo.txt")
})
}
}
\keyword{methods}

