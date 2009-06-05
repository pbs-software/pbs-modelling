\name{clearRcon}
\Rdversion{1.1}
\alias{clearRcon}
\alias{focusRcon}
\title{Clear the R Console Window}
\description{
  Clear the R console window using a Visual Basic shell script.
}
\usage{
clearRcon(os=.Platform$OS.type)
}
\arguments{
  \item{os}{operating system (e.g., \code{"windows"}, \code{"unix"}). }
}
\details{
  Creates a VB shell script file called \code{clearRcon.vba} in R's temporary 
  working directory, then executes the script using the \code{shell} command.
  
  Similarly, \code{focusRcon()} gives the focus to the R console window by 
  creating a Visual Basic shell script called \code{focusRgui.vba} in R's 
  temporary working directory, then executes it using the \code{shell} command.
  
  These commands will only work on Windows operating platforms, 
  using the system's executable \cr
  \code{\%SystemRoot\%\\system32\\cscript.exe}.
}
\author{
  Norm Olsen, Pacific Biological Station, Nanaimo BC
}
\seealso{ \code{\link{cleanWD}}, \code{\link{clearPBSext}}, \code{\link{clearWinVal}} }
\keyword{device}
