\name{setWidgetColor}
\alias{setWidgetColor}
\title{Update Widget Color}
\description{
  Update the foreground and background colors of a widget
}
\usage{
setWidgetColor(name, radioValue, winName = .PBSmodEnv$.PBSmod$.activeWin, ...)
}
\arguments{
  \item{name}{the name of the widget}
  \item{radioValue}{if specified, modify a particular radio option, as identified by the value, rather than the complete set (identified by the common name)}
  \item{winName}{window from which to select the GUI widget. 
  The window that most recently receive user input is used by default if winname is not supplied}
  \item{...}{ any combination of "fg", "bg", "disablefg", "disablebg", "entryfg", "entrybg", "noeditfg", "noeditbg" arguments, depending on type of widget - see details}
}
\details{
  The setWidgetColor function allows dynamic updating of widget colors during program execution.
  However, two factors determine whether dynamic color updating is possible for a particular
  widget: (i) the type of widget, and (ii) the nature of the Tk implementation in the underlying
  widget library.  Thus, a given widget may not support all combinations of colour variables. 
  The following widgets support the corresponding options:

  \describe{
    \item{button:}{fg, bg, disablefg}
    \item{check:}{fg, bg, disablefg, entryfg, entrybg}
    \item{data:}{entryfg, entrybg, noeditfg, noeditbg}
    \item{droplist:}{fg, bg}
    \item{entry:}{entryfg, entrybg, noeditfg, noeditbg}
    \item{label:}{fg, bg}
    \item{matrix:}{entryfg, entrybg, noeditfg, noeditbg}
    \item{object:}{entryfg, entrybg, noeditfg, noeditbg}
    \item{progressbar:}{fg, bg}
    \item{radio:}{fg, bg}
    \item{slide:}{fg, bg}
    \item{spinbox:}{entryfg, entrybg}
    \item{text:}{fg, bg}
    \item{vector:}{entryfg, entrybg, noeditfg, noeditbg}
  }

  These options are described in the PBSmodelling User Guide under Appendix A.

  Be aware that Tk uses gray for the highlight color during a selection operation.
  This means that when the background colour is also gray, there is no visual clue
  that the value has been selected for a copy operation.

}
\author{
  Alex Couture-Beil (VIU, Nanaimo BC) and Allen R. Kronlund (PBS, Nanaimo BC)
}
\examples{
local(envir=.PBSmodEnv,expr={
  createWin("label \"hello world\" name=hello", astext=TRUE)
  setWidgetColor( "hello", bg="lightgreen", fg="purple" )
})
local(envir=.PBSmodEnv,expr={
  createWin("vector names=v length=3 values=\"1 2 3\"", astext=TRUE)
  setWidgetColor( "v[1]", entrybg="lightgreen", entryfg="purple" )
  setWidgetColor( "v[2]", entrybg="green", entryfg="purple" )
  setWidgetColor( "v[3]", entrybg="forestgreen", entryfg="purple" )
})
}
\keyword{methods}
