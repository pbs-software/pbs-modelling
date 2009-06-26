\name{chooseWinVal}
\alias{chooseWinVal}
\title{Choose and Set a String Item in a GUI}
\description{
  Prompts the user to choose one string item from a list of 
  choices displayed in a GUI, then sets a specified variable in 
  a target GUI.
}
\usage{
chooseWinVal(choice, varname, winname="window")
}
\arguments{
  \item{choice}{ vector of strings from which to choose }
  \item{varname}{ variable name to which \code{choice} is assigned in the target GUI }
  \item{winname}{ window name for the target GUI }
}
\details{
  \code{chooseWinVal} activates a \code{setWinVal} command through an
  \code{onClose} function created by the \code{getChoice} command and 
  modified by \code{chooseWinVal}.
}
\value{
  No value is returned directly. The choice is written to the PBS options 
  workspace, accessible through \cr \code{getPBSoptions("getChoice")}. Also set 
  in PBS options is the window name from which the choice was activated.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\note{ 
  Microsoft Windows users may experience difficulties switching focus between the 
  R console and GUI windows. The latter frequently disappear from the screen and 
  need to be reselected (either clicking on the task bar or pressing <Alt><Tab>. 
  This issue can be resolved by switching from MDI to SDI mode. From the R console 
  menu bar, select <Edit> and <GUI preferences>, then change the value of 
  \dQuote{single or multiple windows} to SDI.
}
\seealso{ 
  \code{\link{getChoice}}, \code{\link{getWinVal}}, \code{\link{setWinVal}}
}
\examples{
\dontrun{
dfnam <-
	c("airquality","attitude","ChickWeight","faithful","freeny",
	"iris","LifeCycleSavings","longley","morley","Orange",
	"quakes","randu","rock","stackloss","swiss","trees")

wlist <- c(
	"window name=choisir title=\"Test chooseWinVal\"",
	"label text=\"Press <ENTER> in the green entry box
	\\nto choose a file, then press <GO>\" sticky=W pady=5",
	"grid 1 3 sticky=W",
	"label text=File: sticky=W",
	"entry name=fnam mode=character width=23 value=\"\" 
	func=chFile entrybg=darkolivegreen1 pady=5",
	"button text=GO bg=green sticky=W func=test",
	"")

chFile <- function(ch=dfnam,fn="fnam") 
	{chooseWinVal(ch,fn,winname="choisir")};

#-- Example 1 GUI test
test <- function() {
	getWinVal(winName="choisir",scope="L")
	if (fnam!="" && any(fnam==dfnam)) {
		file <- get(fnam);
		pairs(file,gap=0); }
	else {
		resetGraph(); 
		addLabel(.5,.5,"Press <ENTER> in the green entry box
		\nto choose a file, then press <GO>", col="red",cex=1.5)}};

#-- Example 2 Non-GUI test
#To try the non-GUI version, type 'test2()' on the command line
test2 <- function(fnames=dfnam) {
  frame();resetGraph()
  again <- TRUE;
  while (again) {
    fnam <- sample(fnames,1); file <- get(fnam); 
    flds <- names(file);
    xfld <- getChoice(paste("Pick x-field from",fnam),flds,gui=F);
    yfld <- getChoice(paste("Pick y-field from",fnam),flds,gui=F)
    plot(file[,xfld],file[,yfld],xlab=xfld,ylab=yfld,
      pch=16,cex=1.2,col="red");
    again <- getChoice("Plot another pair?",gui=F) }
  }
require(PBSmodelling)
createWin(wlist,astext=T); test();
}
}
\keyword{ device }
\keyword{ utilities }
