\name{openFile}
\alias{openFile}
\title{Open a File with an Associated Program}
\description{
  Open a file using the program associated with its extension 
  defined by the Windows shell.  Non-windows users, or users 
  wishing to overide the default application, can specify a 
  program association using \code{setPBSext}.
}
\usage{
openFile(fname)
}
\arguments{
	\item{fname}{name of file to open.}
}
\value{
  An invisible string vector of the file names and/or commands + file names.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\note{
  If a command is registered with \code{setPBSext}, then 
  \code{openFile} will replace all occurrences of \code{"\%f"} 
  with the absolute path of the filename, before executing the command.
}
\seealso{
  \code{\link{getPBSext}}, \code{\link{setPBSext}}, \code{\link{clearPBSext}},
  \code{\link{writePBSoptions}}
}
\examples{
\dontrun{
# Set up firefox to open .html files
setPBSext("html", '"c:/Program Files/Mozilla Firefox/firefox.exe" file://\%f')
openFile("foo.html")
}
}
\keyword{file}
