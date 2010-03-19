\name{initHistory}
\alias{initHistory}
\alias{rmHistory}
\alias{addHistory}
\alias{forwHistory}
\alias{backHistory}
\alias{jumpHistory}
\alias{clearHistory}
\alias{clearHistory}
\alias{lastHistory}
\alias{firstHistory}
\title{Create Structures for a New History Widget}

\description{
	PBS history functions (below) are available to those who would like to 
	use the package's history functionality, without using the pre-defined history widget. 
	These functions allow users to create customized history widgets.
}

\usage{
initHistory(hisname, indexname=NULL, sizename=NULL,
    buttonnames=NULL, modename=NULL, func=NULL, overwrite=TRUE)
rmHistory(hisname="", index="")
addHistory(hisname="")
forwHistory(hisname="")
backHistory(hisname="")
lastHistory(hisname="")
firstHistory(hisname="")
jumpHistory(hisname="", index="")
clearHistory(hisname="")
}

\arguments{
	\item{hisname}{name of the history "list" to manipulate. If it is omitted, 
		the function uses the value of \code{getWinAct()[1]} as the history name. 
		This allows the calling of functions directly from the \emph{window description file} 
		(except \code{initHistory}, which must be called before \code{createWin()}).}
	\item{indexname}{name of the index entry widget in the \emph{window description file}. 
		If \code{NULL}, then the current index feature will be disabled.}
	\item{sizename}{name of the current size entry widget. If \code{NULL}, then the 
		current size feature will be disabled.}
	\item{buttonnames}{named list of names of the first, prev, next, and last buttons. If \code{NULL}, then the 
		buttons are not disabled ever}
	\item{modename}{name of the radio widgets used to change addHistory\'s mode. If \code{NULL}, then the 
		default mode will be to insert after the current index.}
	\item{index}{index to the history item. The default (\code{""}) causes the value to be 
		extracted from the widget identified by \code{indexname}.}
	\item{func}{name of user supplied function to call when viewing history items.}
	\item{overwrite}{if \code{TRUE}, history (matching \code{hisname}) will be cleared. 
		Otherwise, the imported history will be merged with the current one.}
}

\details{
	PBS Modelling includes a pre-built history widget designed to collect interesting choices of
	GUI variables so that they can be redisplayed later, rather like a slide show. 

	Normally, a user would invoke a history widget simply by including a reference to it 
	in the \emph{window description file}. However, PBS Modelling includes support functions (above) 
	for customized applications.

	To create a customized history, each button must be described separately in the 
	\emph{window description file} rather than making reference to the history widget.

	The history "List" must be initialized before any other functions may be called. 
	The use of a unique history name (\code{hisname}) is used to associate a unique 
	history session with the supporting functions.
	
	The \code{indexname} and \code{sizename} arguments correspond to the given names 
	of entry widgets in the \emph{window description file}, which will be used to display the 
	current index and total size of the list. The \code{indexname} entry widget can also 
	be used by \code{jumpHistory} to retrieve a target index.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{importHistory}}, \code{\link{exportHistory}}
}

\examples{
\dontrun{
# Example of creating a custom history widget that saves values 
# whenever the "Plot" button is pressed. The user can tweak the 
# inputs "a", "b", and "points" before each "Plot" and see the 
# "Index" increase. After sufficient archiving, the user can review 
# scenarios using the "Back" and "Next" buttons. 
# A custom history is needed to achieve this functionality since 
# the packages pre-defined history widget does not update plots.

# To start, create a Window Description to be used with createWin 
# using astext=TRUE. P.S. Watch out for special characters which 
# must be "escaped" twice (first for R, then PBSmodelling).

winDesc <- '
	window title="Custom History"
	vector names="a b k" labels="a b points" font="bold" \\\
	values="1 1 1000" function=myPlot
	grid 1 3
		button function=myHistoryBack text="<- Back"
		button function=myPlot text="Plot"
		button function=myHistoryForw text="Next ->"
	grid 2 2
		label "Index"
		entry name="myHistoryIndex" width=5
		label "Size"
		entry name="myHistorySize" width=5
'
# Convert text to vector with each line represented as a new element
winDesc <- strsplit(winDesc, "\n")[[1]]

# Custom functions to update plots after restoring history values
myHistoryBack <- function() {
	backHistory("myHistory");
	myPlot(saveVal=FALSE); # show the plot with saved values
}
myHistoryForw <- function() {
	forwHistory("myHistory");
	myPlot(saveVal=FALSE); # show the plot with saved values 
}
myPlot <- function(saveVal=TRUE) {
	# save all data whenever plot is called (directly)
	if (saveVal) addHistory("myHistory");
	getWinVal(scope="L");
	tt <- 2*pi*(0:k)/k;
	x <- (1+sin(a*tt));  y <- cos(tt)*(1+sin(b*tt));
	plot(x, y);
}

initHistory("myHistory", "myHistoryIndex", "myHistorySize")
createWin(winDesc, astext=TRUE)
}
}

\keyword{utilities}
