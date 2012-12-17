\name{sortHistory}
\alias{sortHistory}
\title{Sort an Active or Saved History}
\description{
  Utility to sort history. When called without any arguments, an interactive GUI
  is used to pick which history to sort. When called with \code{hisname}, sort
  this active history widget. When called with \code{file} and \code{outfile},
  sort the history located in \code{file} and save to \code{outfile}.
}
\details{
  After selecting a history to sort (either from given arguments, or interactive GUI)
  the R data editor window will be displayed. The editor will have one column named
  \"new\" which will have numbers 1,2,3,...,n. This represents the current ordering
  of the history. You may change the numbers around to define a new order. The list
  is sorted by reassigning the index in row i as index i.
  
  For example, if the history had three items 1,2,3. Reordering this to 3,2,1 will
  reverse the order; changing the list to 1,2,1,1 will remove entry 3 and 
  create two duplicates of entry 1.
}
\usage{
sortHistory(file="", outfile=file, hisname="")
}
\arguments{
  \item{file}{file name of saved history to sort.}
  \item{outfile}{file to save sorted history to.}
  \item{hisname}{name of active history widget and window it is located in, given
                 in the form \code{WINDOW.HISTORY}.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{importHistory}}, \code{\link{initHistory}}
}
\keyword{list}
