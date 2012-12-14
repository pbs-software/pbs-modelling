\name{selectFile}
\alias{selectFile}
\title{Display Dialogue: Open or Save File}
\description{
  Display the default \bold{Open} or  \bold{Save} prompt provided by the Operating System.
}
\usage{
selectFile(initialfile="", initialdir=getwd(), 
    filetype=list(c("*","All Files")), mode="open", multiple=FALSE,
    title="", defaultextension="", usewidget=NULL)
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
  \item{usewidget}{store the selected file in the named entry widget}
}
\value{
  The file name and path of the file(s) selected by the user.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{selectDir}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  #integration with widget via doAction
  createWin( c( "entry foo mode=character width=60", 
    "button text=\"select file\" 
    func=doAction action=\"selectFile(usewidget=`foo`)\"" ), astext=TRUE )
})
}
}
\keyword{file}
