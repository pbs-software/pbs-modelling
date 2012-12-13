\name{exportHistory}
\alias{exportHistory}
\title{Export a Saved History}
\description{
  Export the current history list.
}
\usage{
exportHistory(hisname="", fname="")
}
\arguments{
  \item{hisname}{name of the history list to export. If set to \code{""}, 
    the value from \code{getWinAct()[1]} will be used instead.}
  \item{fname}{file name where history will be saved. If it is set to \code{""}, 
    a <Save As> window will be displayed.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{importHistory}}, \code{\link{initHistory}}, \code{\link{promptSaveFile}}
}
\keyword{list}
