\name{setWidgetState}
\alias{setWidgetState}
\title{Update Widget State}
\description{
  Update the read-only state of a widget.
}
\usage{
setWidgetState( varname, state, radiovalue, winname, warn=TRUE )
}
\arguments{
  \item{varname}{the name of the widget}
  \item{state}{"normal" or "disabled" and for some widgets "readonly" as described under Details below.}
  \item{radiovalue}{if specified, disable a particular radio option, as identified by the value, rather than the complete set (identified by the common name)}
  \item{winname}{window from which to select the GUI widget. The window that most recently receive user input is used by default if winname is not supplied.}
  \item{warn}{if TRUE, display a warning if readonly is converted to disabled (only applies for widgets that don't accept readonly)}
}
\details{
The setWidgetState function allows dynamic control of widget functioning during program execution.  The function serves as a wrapper for the tkconfigure function available in the underlying Tk libraries used by PBS Modelling.  Thus, setWidgetState is only available for those widgets that use Tk library widgets.

The state of the following PBS Modelling widgets can be set to "normal" or "disabled": button, check, data, droplist, entry, matrix, object, radio, slide, spinbox, table, text, and vector.  When the state variable is set to "disabled", values displayed in the widget cannot be changed or copied except in the case of the object and table widgets which permit the values to be copied.

The data, entry, matrix, and vector widgets support a "readonly" state that allows values displayed in the widget to be copied but not changed.  The displayed value can be selected using the keyboard or mouse.  However, the copy and paste operations can only be accomplished via Ctrl-C and Ctrl-V, respectively, not the mouse.

Be aware that Tk uses gray for the highlight color during a selection operation.  This means that when the background colour is also gray, there is no visual clue that the value has been selected for a copy operation.

Exceptions to the behavior determined by state include the object, table and text widgets.  There is no "readonly" state applicable to these widgets.  Nevertheless, the values displayed can be copied even when the state is "disabled".

Individual radio widgets grouped by the name variable of a radio declaration can be updated by specifying radiovalue in the call to setWidgetState.

The state of individual elements in the data, matrix, and vector widgets can be updated by indexing.  For the vector and matrix widgets any element can be addressed by appending the desired index to the widget name using square brackets (e.g., "myVec[2]" or "myMatrix[2,3]").  The data widget is indexed differently than the matrix widget by adding "d" after the brackets (e.g., "myData[1,1]d").  This change in syntax is required for internal coding of PBS Modelling.
}
\author{
  Alex Couture-Beil (VIU, Nanaimo BC) and Allen R. Kronlund (PBS, Nanaimo BC)
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  winDesc <- c('vector length=3 name=vec labels="normal disabled readonly" values="1 2 3"',
             "matrix nrow=2 ncol=2 name=mat", "button name=but_name" );
  createWin(winDesc, astext=TRUE)

  setWidgetState( "vec[1]", "normal" )
  setWidgetState( "vec[2]", "disabled" )
  setWidgetState( "vec[3]", "readonly" )

  setWidgetState( "mat", "readonly" )
  setWinVal( list( mat = matrix( 1:4, 2, 2 ) ) )

  #works for buttons too
  setWidgetState( "but_name", "disabled" )
})
}
}
\keyword{methods}
