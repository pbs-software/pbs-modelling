\name{showHelp}
\alias{showHelp}
\title{Display HTML Help Pages for Packages in Browser}
\description{
  Display the help pages for installed packages that match the 
  supplied pattern in an HTML browser window.
}
\usage{
showHelp(pattern="methods", ...)
}
\arguments{
  \item{pattern}{string pattern to match to package names}
  \item{...}{allows user to specify two additional arguments: \cr
    \code{remote} - character string giving a valid URL for the \code{R_HOME}
    directory on a remote location; \cr
    \code{update} - logical: if \code{TRUE}, attempt to update the package
    index to reflect the currently available packages. 
    (Not attempted if \code{remote} is non-\code{NULL}.) }
}
\details{
  The specified pattern is matched to R-packages installed on 
  the user's system. The code uses the \code{utils} 
  function \code{browseURL} to display the HTML Help Pages using 
  a browser that the system associates with \code{html} 
  extensions. (See help for \code{browseURL} for other operating systems.
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
	\code{\link{viewCode}}, \code{\link{showPacks}}
}
\keyword{device}
\keyword{utilities}
