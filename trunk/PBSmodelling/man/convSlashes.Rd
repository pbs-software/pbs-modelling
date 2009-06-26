\name{convSlashes}
\alias{convSlashes}
\title{ Convert Slashes from UNIX to DOS }
\description{
  Convert slashes in a string from \samp{/} to \samp{\\\\} if the 
  operating system is \samp{windows}. Do the reverse if the OS is
  \samp{unix}.
}
\usage{
convSlashes(expr, os=.Platform$OS.type, addQuotes=FALSE)
}
\arguments{
  \item{expr}{ String value (usually a system pathway). }
  \item{os}{ operating system (either \code{"windows"} or \code{"unix"}). }
  \item{addQuotes}{ logical: if \code{TRUE}, enclose the string expression
    in escaped double quotation marks. }
}
\value{
  Returns the input string modified to have the appropriate slashes for the 
  specified operating system.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\keyword{ character }
