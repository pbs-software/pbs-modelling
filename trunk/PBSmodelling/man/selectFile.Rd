\name{selectFile}
\alias{selectFile}
\title{Display Dialogue: Open or Save File}
\description{
	Display the default \bold{Open} or  \bold{Save} prompt provided by the Operating System.
}
\usage{
selectFile <- function( initialfile="", initialdir=getwd(), filetype=list(c("*", "All Files")), mode="open", multiple = FALSE, title="", defaultextension="" )
}
\arguments{
	\item{initialfile}{initially selected file}
	\item{initialdir}{initially directory the dialog opens }
	\item{filetype}{a list of character vectors indicating file types made available 
		to users of the GUI. Each vector is of length one or two. The first element 
		specifies either the file extension or \code{"*"} for all file types. The second 
		element gives an optional descriptor name for the file type. The supplied 
		\code{filetype} list appears as a set of choices in the pull-down box labelled 
		\dQuote{Files of type:}.}
	\item{mode}{string: if \code{"save"} display \bold{Save As} prompt, 
		if \code{"open"} display \bold{Open} prompt.}
	\item{multiple}{if \code{TRUE} the open prompt can select multiple files. This has no
		effect for the save prompt.}
	\item{title}{title for the prompt window}
	\item{defaultextension}{default file extension if none is provided by the user}
}
\value{
	The file name and path of the file(s) selected by the user.
}
\author{
  Alex Couture-Beil
}
\seealso{
	\code{\link{selectDir}}
}
\examples{
\dontrun{
# Open a filename, and return it line by line in a vector
scan(promptOpenFile(),what=character(),sep="\n")

# Illustrates how to set filetype.
promptOpenFile("intial_file.txt", filetype=list(c(".txt", "text files"), 
               c(".r", "R files"), c("*", "All Files")))
}
}
\keyword{file}
