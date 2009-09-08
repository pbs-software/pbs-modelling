\name{selectDir}
\alias{selectDir}
\title{Display Dialogue: Select directory}
\description{
	Display the default directory chooser prompt provided by the Operating System.
}
\usage{
selectDir(initialdir=getwd(),mustexist=TRUE,title="",usewidget=NULL)
}
\arguments{
	\item{initialdir}{initially selected directory}
	\item{mustexist}{if logical value is \code{TRUE}, only a existing directory can be selected}
	\item{title}{title for the prompt window}
	\item{usewidget}{store the selected directory in the named entry widget}
}
\value{
	The directory path selected by the user
}
\author{
  Alex Couture-Beil
}
\seealso{
	\code{\link{selectFile}}
}
\examples{
\dontrun{
dir(selectDir(title="select a directory to list contents of"))

#integration with widget via doAction
createWin( c( "entry foo mode=character", 
  "button text=\"select dir\" func=doAction action=\"selectDir(usewidget=`foo`)\"" ), astext=TRUE )
}
}
\keyword{file}
