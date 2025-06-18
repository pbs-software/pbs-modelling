\name{selectDir}
\alias{selectDir}
\title{
  Display Dialogue: Select directory
}
\description{
  Display and select a system directory using a call to \code{tcltk::tkchooseDirectory}.
}
\usage{
selectDir(initialdir=getwd(), mustexist=TRUE, title="", usewidget=NULL,
   targetdir=NULL, relative=FALSE)
}
\arguments{
  \item{initialdir}{\code{character} -- initially selected directory}
  \item{mustexist}{\code{logical} -- if \code{TRUE}, only an existing directory can be selected}
  \item{title}{\code{character} -- title for the prompt window}
  \item{usewidget}{\code{character} -- store the selected directory in the named entry widget}
  \item{targetdir}{\code{character} -- a alternative directory to \code{initialdir} from which to start a search}
  \item{relative}{\code{logical} -- if \code{TRUE}, express the selected directory relative to \code{initialdir} or \code{targetdir}}
}
\details{
  When used in a GUI, the coupling of \code{selectDir} with \code{setGUIoptions} 
  necessarily ties the \code{initialdir} to the \code{usewidget} directory.
  A new argument \code{targetdir} allows the user to specify an alternative directory
  from which to search that is independent of the \code{initialdir}-\code{usewidget} coupling.
}
\value{
  The directory path selected by the user.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer,
  Earthly Technologies, Victoria BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: remote office, Vancouver BC\cr
  Last modified \code{Rd: 2025-05-26}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{selectFile}},
  \code{\link[PBSmodelling]{setGUIoptions}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  dir(selectDir(title="select a directory to list contents of"))

  #integration with widget via doAction
  createWin( c( "entry foo mode=character width=25", 
    "button text=\"select dir\" 
    func=doAction action=\"selectDir(usewidget=`foo`)\"" ), astext=TRUE )
})
}
}
\keyword{file}
\concept{M06_supportFuns.r}

