\name{selectDir}
\alias{selectDir}
\title{Display Dialogue: Select directory}
\description{
	Display the default directory chooser prompt provided by the Operating System.
}
\usage{
selectFile <- function( initialdir=getwd(), mustexist = TRUE, title="" )
}
\arguments{
	\item{initialdir}{initially selected directory}
	\item{mustexist}{if logical value is \code{TRUE}, only a existing directory can be selected}
	\item{title}{title for the prompt window}
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
}
}
\keyword{file}
