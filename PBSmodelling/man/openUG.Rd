\name{openUG}
\alias{openUG}
\title{
  Open Package User Guide
}
\description{
  Open package User's Guide \sQuote{<pkg>-UG.pdf} if it exists.
  This function is essentially a wrapper for \code{openFile}.
}
\usage{
openUG(pkg = "PBSmodelling")
}
\arguments{
  \item{pkg}{\code{character} -- full name (with or without quotes) of a package installed on the user's system.}
}
\details{
  The user's guide is assumed to be PDF format with extension \code{pdf}.
  The name of the PDF file will be \sQuote{<pkg>-UG.pdf} (e.g., \code{PBSmodelling-UG.pdf}.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{openFile}},
  \code{\link[PBSmodelling]{showHelp}},
  \code{\link[PBSmodelling]{viewCode}}
}
\keyword{package}
\keyword{supportFuns}

