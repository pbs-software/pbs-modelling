\name{setPBSext}
\alias{setPBSext}
\title{Set a Command Associated with a File Name Extension}

\description{
  Set a command with an associated extension, for use in 
  \code{openFile}.  The command must specify where the target file 
  name is inserted by indicating a \code{"\%f"}.
}

\note{
  These values are not saved from one \emph{PBS Modelling} session to the next.
}

\usage{
setPBSext(ext, cmd)
}

\arguments{
  \item{ext}{string specifying the extension suffix.}
  \item{cmd}{command string to associate with the extension.}
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{getPBSext}}, \code{\link{openFile}}, \code{\link{clearPBSext}}
}

\keyword{methods}

