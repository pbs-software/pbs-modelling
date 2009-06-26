\name{promptOpenFile}
\alias{promptOpenFile}
\title{Display Dialogue: Open File}
\description{
	Display the default \bold{Open} prompt provided by the Operating System.
}
\usage{
promptOpenFile(initialfile="", filetype=list(c("*","All Files")), 
               open=TRUE)
}
\arguments{
	\item{initialfile}{file name of the text file containing the list.}
	\item{filetype}{a list of character vectors indicating file types made available 
		to users of the GUI. Each vector is of length one or two. The first element 
		specifies either the file extension or \code{"*"} for all file types. The second 
		element gives an optional descriptor name for the file type. The supplied 
		\code{filetype} list appears as a set of choices in the pull-down box labelled 
		\dQuote{Files of type:"}.}
	\item{open}{logical: if \code{TRUE} display \bold{Open} prompt, 
		if \code{FALSE} display \bold{Save As} prompt.}
}
\value{
	The file name and path of the file selected by the user.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{promptSaveFile}}
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
