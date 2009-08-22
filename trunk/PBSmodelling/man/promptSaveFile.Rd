\name{promptSaveFile}
\alias{promptSaveFile}
\title{Depricated: Display Dialogue: Save File}
\description{
	Depricated: use \code{\link{selectFile}} instead
}
\usage{
promptSaveFile(initialfile="", filetype=list(c("*", "All Files")), 
               save=TRUE)
}
\arguments{
	\item{initialfile}{file name of the text file containing the list.}
	\item{filetype}{a list of character vectors indicating file types made available 
		to users of the GUI. Each vector is of length one or two. The first element 
		specifies either the file extension or \code{"*"} for all file types. The second 
		element gives an optional descriptor name for the file type. The supplied 
		\code{filetype} list appears as a set of choices in the pull-down box labelled 
		\dQuote{Files of type:}.}
	\item{save}{logical: if \code{TRUE} display \bold{Save As} prompt, 
		if \code{FALSE} display \bold{Open} prompt.}
}
\value{
	The file name and path of the file selected by the user.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{selectFile}}
}

\keyword{file}
