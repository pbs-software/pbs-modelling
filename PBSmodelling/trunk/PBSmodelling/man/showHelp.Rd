\name{showHelp}
\alias{showHelp}
\title{Display Help Pages for Packages in HTML Browser}
\description{
  Display the help pages for installed packages that match the 
  supplied pattern in an HTML browser window.
}
\usage{
showHelp(pat="methods")
}
\arguments{
  \item{pat}{string pattern to match to package names}
}
\details{
  The specified pattern is matched to R-packages installed on 
  the user's system. The code uses the \code{PBSmodelling} 
  function \code{openFile} to display the HTML Help Pages using 
  a program that the system associates with \code{html} 
  extensions. On systems that do not support file extension 
  associations, the function \code{setPBSext} can temporarily 
  set a command to associate with an extension.
}
\value{
  A list is invisibly returned, comprising:
  \item{Apacks}{all packages installed on user's system}
  \item{Spacks}{selected packages based on specified pattern}
  \item{URLs}{path and file name of HTML Help Page}
  Help pages are displayed in a separate browser window.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\note{
  The connection time for browsers (at least in Windows OS)
  is slow. If the HTML browser program is not already running,
  multiple matching pages will most likely not be displayed. However, 
  subsequent calls to \code{showHelp} should show all matches.
}
\seealso{ 
	\code{\link{openFile}}, \code{\link{setPBSext}}, \code{\link{getPBSext}}
}
\keyword{device}
\keyword{utilities}
