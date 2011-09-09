\name{clearRcon}
\Rdversion{1.1}
\alias{clearRcon}
\alias{focusRgui}
\title{Clear the R Console / Focus on the RGui Window}
\description{
  Clear the R console window or focus on the RGui window using Visual Basic shell scripts.
}
\usage{
clearRcon(os=.Platform$OS.type)
focusRgui(os=.Platform$OS.type)
}
\arguments{
  \item{os}{operating system (e.g., \code{"windows"}, \code{"unix"}). }
}
\details{
  Creates a VB shell script file called \code{clearRcon.vbs} or \code{focusRgui.vbs}
  in R's temporary working directory, then executes the script using the \code{shell} command.
  
  While \code{clearRcon} clears the R console, \code{focusRgui} returns the desktop 
  focus back to the RGui window.
  
  These commands will only work on Windows operating platforms, using the system's executable \cr
  \code{\%SystemRoot\%\\system32\\cscript.exe}.
}
\author{
  Norm Olsen, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{cleanWD}}, \code{\link{clearPBSext}}, \code{\link{clearWinVal}} 
}
\examples{
\dontrun{
createWin( c("window title=Focus",
  "button text=\"Go to RGui\" width=20 bg=aliceblue func=focusRgui"), astext=T)
}
}
\keyword{device}
