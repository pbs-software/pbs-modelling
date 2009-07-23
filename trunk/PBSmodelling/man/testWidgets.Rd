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
button text="Calculate" font="" fg="black" bg="" width=0 name=NULL
   function="" action="button" sticky="" padx=0 pady=0

check name mode="logical" checked=FALSE text="" font="" fg="black"
   bg="" function="" action="check" edit=TRUE sticky="" padx=0 pady=0

data nrow ncol names modes="numeric" rowlabels="" collabels=""
   rownames="X" colnames="Y" font="" fg="black" bg="" entryfont=""
   entryfg="black" entrybg="white" values="" byrow=TRUE function=""
   enter=TRUE action="data" edit=TRUE width=6 borderwidth=0 sticky=""
   padx=0 pady=0

droplist name values=NULL choices=NULL labels=NULL selected=1
   add=FALSE font="" fg="black" bg="white" function="" enter=TRUE
   action="droplist" edit=TRUE mode="character" width=20 sticky=""
   padx=0 pady=0

entry name value="" width=20 label=NULL font="" fg="" bg=""
   entryfont="" entryfg="black" entrybg="white" edit=TRUE
   password=FALSE function="" enter=TRUE action="entry"
   mode="numeric" sticky="" padx=0 pady=0

grid nrow=1 ncol=1 toptitle="" sidetitle="" topfont="" sidefont=""
   topfg=NULL sidefg=NULL fg="black" topbg=NULL sidebg=NULL bg=""
   byrow=TRUE borderwidth=1 relief="flat" sticky="" padx=0 pady=0

history name="default" function="" import="" fg="black" bg=""
   entryfg="black" entrybg="white" text=NULL textsize=0 sticky=""
   padx=0 pady=0

include file=NULL name=NULL

label text="" name="" mode="character" font="" fg="black" bg=""
   sticky="" justify="left" wraplength=0 width=0 padx=0 pady=0

matrix nrow ncol names rowlabels="" collabels="" rownames=""
   colnames="" font="" fg="black" bg="" entryfont="" entryfg="black"
   entrybg="white" values="" byrow=TRUE function="" enter=TRUE
   action="matrix" edit=TRUE mode="numeric" width=6 borderwidth=0
   sticky="" padx=0 pady=0

menu nitems=1 label font="" fg="" bg=""

menuitem label font="" fg="" bg="" function action="menuitem"

null bg="" padx=0 pady=0

object name rowshow=0 font="" fg="black" bg="" entryfont=""
   entryfg="black" entrybg="white" vertical=FALSE collabels=TRUE
   rowlabels=TRUE function="" enter=TRUE action="data" edit=TRUE
   width=6 borderwidth=0 sticky="" padx=0 pady=0

radio name value text="" font="" fg="black" bg="" function=""
   action="radio" edit=TRUE mode="numeric" selected=FALSE sticky=""
   padx=0 pady=0

slide name from=0 to=100 value=NA showvalue=FALSE
   orientation="horizontal" font="" fg="black" bg="" function=""
   action="slide" sticky="" padx=0 pady=0

slideplus name from=0 to=1 by=0.01 value=NA font="" fg="black" bg=""
   entryfont="" entryfg="black" entrybg="white" function=""
   enter=FALSE action="slideplus" sticky="" padx=0 pady=0

spinbox name from to by=1 value=NA label="" font="" fg="black" bg=""
   entryfont="" entryfg="black" entrybg="white" function=""
   enter=TRUE edit=TRUE action="droplist" width=20 sticky="" padx=0
   pady=0

table name rowshow=0 font="" fg="black" bg="white" rowlabels=""
   collabels="" function="" action="table" edit=TRUE width=10
   sticky="" padx=0 pady=0

text name height=8 width=30 edit=FALSE scrollbar=TRUE fg="black"
   bg="white" mode="character" font="" value="" borderwidth=1
   relief="sunken" sticky="" padx=0 pady=0

vector names length=0 labels="" values="" vecnames="" font=""
   fg="black" bg="" entryfont="" entryfg="black" entrybg="white"
   vertical=FALSE function="" enter=TRUE action="vector" edit=TRUE
   mode="numeric" width=6 borderwidth=0 sticky="" padx=0 pady=0

window name="window" title="" vertical=TRUE bg="#D4D0C8" fg="#000000"
   onclose="" remove=FALSE


}
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
	\code{\link{createWin}}, \code{\link{showArgs}}
}
\keyword{utilities}
