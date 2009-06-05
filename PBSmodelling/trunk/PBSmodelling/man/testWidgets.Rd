\name{testWidgets}
\alias{testWidgets}
\alias{widgets}
\title{Display Sample GUIs and their Source Code}

\description{
	Display an interactive GUI to demonstrate the available widgets in PBS Modelling.
	A \code{text} window displays the \emph{window description file} source code. The user
	can modify this sample code and recreate the test GUI by pressing the button below.

	The \emph{Window Description Files} can be found in the R directory \cr
	\code{.../library/PBSmodelling/testWidgets}.
}

\usage{testWidgets()}

\details{
	Following are the widgets and default values supported by PBS Modelling.
	For detailed descriptions, see Appendix A in \sQuote{PBSModelling-UG.pdf} 
	located in the R directory \code{.../library/PBSmodelling/doc}.

\preformatted{
button text="Calculate" font="" fg="black" bg="" width=0 
   function="" action="button" sticky="" padx=0 pady=0

check name mode=logical checked=FALSE text="" font="" fg="black" bg="" 
   function="" action="check" sticky="" padx=0 pady=0

data nrow ncol names modes="numeric" rowlabels="" collabels=""
   rownames="X" colnames="Y" font="" fg="black" bg="" entryfont=""
   entryfg="black" entrybg="white" values="" byrow=TRUE function=""
   enter=TRUE action="data" width=6 sticky="" padx=0 pady=0

entry name value="" width=20 label="" font="" fg="" bg="" 
   entryfont="" entryfg="black" entrybg="white" function="" 
   enter=TRUE action="entry" mode="numeric" sticky="" padx=0 pady=0

grid nrow=1 ncol=1 toptitle="" sidetitle="" topfont="" sidefont=""
   byrow=TRUE borderwidth=1 relief="flat" sticky="" padx=0 pady=0

history name="default" function="" import="" sticky="" padx=0 pady=0

label text="" font="" fg="black" bg="" sticky="" justify="left" 
   wraplength=0 padx=0 pady=0

matrix nrow ncol names rowlabels="" collabels="" rownames="" 
   colnames="" font="" fg="black" bg="" entryfont="" entryfg="black" 
   entrybg="white" values="" byrow=TRUE function="" enter=TRUE 
   action="matrix" mode="numeric" width=6 sticky="" padx=0 pady=0

menu nitems=1 label font=""

menuitem label font="" function action="menuitem"

null padx=0 pady=0

object name font="" fg="black" bg="" entryfont="" 
   entryfg="black" entrybg="white" vertical=FALSE function="" 
   enter=TRUE action="data" width=6 sticky="" padx=0 pady=0

radio name value text="" font="" fg="black" bg="" function=""
   action="radio" mode="numeric" selected=FALSE sticky="" padx=0 pady=0

slide name from=0 to=100 value=NA showvalue=FALSE 
   orientation="horizontal" font="" fg="black" bg="" function="" 
   action="slide" sticky="" padx=0 pady=0

slideplus name from=0 to=1 by=0.01 value=NA function=""
   enter=FALSE action="slideplus" sticky="" padx=0 pady=0

text name height=8 width=30 edit=FALSE scrollbar=TRUE
   fg="black" bg="white" mode="character" font="" value=""
   borderwidth=1 relief="sunken" sticky="" padx=0 pady=0

vector names length=0 labels="" values="" vecnames="" font=""
   fg="black" bg="" entryfont="" entryfg="black" entrybg="white"
   vertical=FALSE function="" enter=TRUE action="vector"
   mode="numeric" width=6 sticky="" padx=0 pady=0

window name="window" title="" vertical=TRUE bg="#D4D0C8"
   fg="#000000" onclose=""
}
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{createWin}}, \code{\link{showArgs}}
}
\keyword{utilities}
