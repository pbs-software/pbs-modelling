\name{getChoice}
\alias{getChoice}
\title{Choose One String Item from a List of Choices}
\description{
  Prompts the user to choose one string item from a list of 
  choices displayed in a GUI. The simplest case \code{getChoice()} 
  yields \code{TRUE} or \code{FALSE}.

}
\usage{
getChoice(choice=c("Yes","No"), question="Make a choice: ",
          winname="getChoice", horizontal=TRUE, radio=FALSE,
          qcolor="blue", gui=FALSE, quiet=FALSE)
}
\arguments{
  \item{choice }{vector of strings from which to choose.}
  \item{question }{question or prompting statement.}
  \item{winname }{window name for the \code{getChoice} GUI.}
  \item{horizontal }{logical: if \code{TRUE}, display the choices horizontally, else vertically.}
  \item{radio }{logical: if \code{TRUE}, display the choices as radio buttons, else as buttons.}
  \item{qcolor }{colour for \code{question}.}
  \item{gui }{logical: if \code{TRUE}, \code{getChoice} is functional when called from a GUI,
    else it is functional from command line programs.}
  \item{quiet }{logical: if \code{TRUE}, don't print the choice on the command line.}
}
\details{
  The user's choice is stored in \code{.PBSmod$options$getChoice} 
  (or whatever \code{winname} is supplied).
  
  \code{getChoice} generates an \code{onClose} function that returns focus
  to the calling window (if applicable) and prints out the choice.
}
\value{
  If called from a GUI (\code{gui=TRUE}), no value is returned directly. Rather, 
  the choice is written to the PBS options workspace, accessible through 
  \code{getPBSoptions("getChoice")} (or whatever \code{winname} was supplied).
  
  If called from a command line program (\code{gui=FASLE}), the choice is returned 
  directly as a string scalar (e.g., \code{answer <- getChoice(gui=F)} ). 
  
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
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
	\code{\link{chooseWinVal}}, \code{\link{getWinVal}}, \code{\link{setWinVal}}
}
\examples{
\dontrun{
#-- Example 1
local(envir=.PBSmodEnv,expr={
  getChoice(c("Fame","Fortune","Health","Beauty","Lunch"),
    "What do you want?",qcolor="red",gui=FALSE)
})

#-- Example 2
local(envir=.PBSmodEnv,expr={
  getChoice(c("Homer Simpson","Wilberforce Humphries","Miss Marple","Gary Numan"),
    "Who`s your idol?",horiz=FALSE,radio=TRUE,gui=FALSE)
})
}
}
\keyword{ device }
\keyword{ utilities }
