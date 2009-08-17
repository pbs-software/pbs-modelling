\name{importHistory}
\alias{importHistory}
\title{Import a History List from a File}

\description{
	Import a history list from file \code{fname}, and place it 
	into the history list \code{hisname}.
}

\usage{
importHistory(hisname="", fname="", updateHis=TRUE)
}

\arguments{
  \item{hisname}{name of the history list to be populated. 
    The default (\code{""}) uses the value from \code{getWinAct()[1]}.}
  \item{fname}{file name of history file to import. 
    The default (\code{""}) causes an open-file window to be displayed.}
  \item{updateHis}{logical: if \code{TRUE}, update the history widget to reflect the change
    in size and index.}
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\seealso{
\code{\link{exportHistory}}, \code{\link{initHistory}}, \code{\link{promptOpenFile}}
}

\keyword{list}
