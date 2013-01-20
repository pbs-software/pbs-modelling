\name{getPBSext}
\alias{getPBSext}
\title{Get a Command Associated With a File Name}

\description{
  Display all locally defined file extensions and their associated commands, 
  or search for the command associated with a specific file extension
  \code{ext}.
}
\note{
  These file associations are not saved from one \emph{PBS Modelling} session to
  the next unless explicitly saved and loaded (see \code{writePBSoptions} and
  \code{readPBSoptions}).
}
\usage{getPBSext(ext)}
\arguments{
  \item{ext}{optional string specifying a file extension.}
}
\value{
  Command associated with file extension.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{setPBSext}}, \code{\link{openFile}}, \code{\link{clearPBSext}}
}
\keyword{methods}
