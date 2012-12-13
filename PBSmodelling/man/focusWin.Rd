\name{focusWin}
\alias{focusWin}
\title{Set the Focus on a Particular Window}

\description{
	Bring the specified window into focus, and set it as the active window. 
	\code{focusWin} will fail to bring the window into focus if it is called from the R 
	console, since the R console returns focus to itself once a function returns. 
	However, it will work if \code{focusWin} is called as a result of calling a function 
	from the GUI window. (i.e., pushing a button or any other widget that has a 
	function argument).
}

\usage{focusWin(winName, winVal=TRUE)}

\arguments{
	\item{winName}{name of window to focus}
	\item{winVal}{if \code{TRUE}, associate \code{winName} with the default window 
		for \code{setWinVal} and \code{getWinVal}}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
focus <- function() {
  winName <- getWinVal()$select;
  focusWin(winName);
  mess = paste("Calling focusWin(\"", winName, "\")\n",
    "getWinVal()$myvar = ", getWinVal()$myvar, "\n\n", sep="",collapse="")
  cat(mess); invisible()
}
#create three windows named win1, win2, win3
#each having three radio buttons, which are used to change the focus
for(i in 1:3) {
  winDesc <- c(
    paste('window onclose=closeWin name=win',i,' title="Win',i,'"', sep=''),
    paste('entry myvar ', i, sep=''),
    'radio name=select value=win1 text="one" function=focus mode=character',
    'radio name=select value=win2 text="two" function=focus mode=character',
    'radio name=select value=win3 text="three" function=focus mode=character');
  createWin(winDesc, astext=TRUE); };
})
}
}
\keyword{methods}

