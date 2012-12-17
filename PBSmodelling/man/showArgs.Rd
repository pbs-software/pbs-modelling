\name{showArgs}
\alias{showArgs}
\title{Display Expected Widget Arguments}

\description{
  For each widget specified, display its arguments in order with their default values. 
  The display list can be expanded to report each argument on a single line.
}
\usage{
showArgs(widget, width=70, showargs=FALSE)
}

\arguments{
  \item{widget}{vector string of widget names; if not specified (default), 
    the function displays information about all widgets in alphabetical order.}
  \item{width}{numeric width used by \code{strwrap} to wrap lines of the widget
    usage section.}
  \item{showargs}{logical:, if \code{TRUE}, the display also lists each argument
    on single line after the widget usage section.}
}
\value{
  A text stream to the R console. Invisibly returns the widget usage lines.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\keyword{utilities}
\keyword{character}

