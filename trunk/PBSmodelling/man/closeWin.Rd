\name{closeWin}
\alias{closeWin}
\title{Close GUI Window(s)}

\description{
  Close (destroy) one or more windows made with \code{createWin}.
}

\usage{closeWin(name)}

\arguments{
  \item{name}{a vector of window names that indicate which windows to close. These 
    names appear in the \emph{window description file}(s) on the line(s) defining WINDOW widgets.
    If \code{name} is omitted, all active windows will be closed.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{createWin}}
}

\keyword{utilities}
