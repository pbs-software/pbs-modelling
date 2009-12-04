\name{openUG}
\alias{openUG}
\title{
  Open Package User Guide
}
\description{
  Open package User's Guide 'pkg-UG.pdf' if it exists.
  This function is essentially a wrapper for code{openFile}.
}
\usage{
openUG(pkg = "PBSmodelling")
}
\arguments{
  \item{pkg}{Full name (with or without quotes) of a package installed on the user's system.}
}
\details{
  We assume that the name of the User's Guide follows 'PBS' conventions.
  This means a user's guide in PDF format with extension \code{pdf}.
  The name of the PDF file will be '<pkg>-UG.pdf' (e.g., \code{PBSmodelling-UG.pdf}.
}
\author{
  Rowan Haigh, PAcific Biological station, Nanaimo BC
}
\seealso{
\code{\link{openFile}}, \code{\link{showHelp}}, \code{\link{viewCode}}
}
\keyword{package}
\keyword{documentation}

