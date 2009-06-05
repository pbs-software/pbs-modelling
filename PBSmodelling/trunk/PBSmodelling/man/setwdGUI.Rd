\name{setwdGUI}
\alias{setwdGUI}
\title{Browse for Working Directory and Optionally Find Prefix}

\description{
  Allows the user to browse a directory tree to set the working directory.
  Optionally, files with given suffixes can be located in the new directory.
}
\usage{
setwdGUI(suffix)
}
\arguments{
  \item{suffix}{character vector of suffixes or \code{""} (See Details).}
}
\details{
  The \code{suffix} argument is passed to a call to 
  \code{findPrefix} after the working directory is changed 
  (See \code{setwd}). If \code{suffix} is set to the empty
  string \code{""}, then \code{findPrefix} will not be called.

  To use this function in a \emph{window description file}, 
  the \code{suffix} argument must be specified as the action of 
  the widget that calls \code{setwdGUI}. Furthermore, the
  suffixes must be separated by commas (e.g., \code{action=.c,.cpp}). 
  If \code{action=,} is specified, then \code{findPrefix} will not be called.
}
\value{
  If suffixes are given, a character vector of prefixes of all files in
  the working directory that match one of the given suffixes is returned; 
  otherwise, the function returns \code{invisible()}.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  If all the required arguments are missing, it is assumed that the function
  is being called by a GUI widget.
}
\seealso{
  \code{\link{findPrefix}}, \code{\link{setwd}}
}
\examples{
\dontrun{
#match files that end with ".a" followed by 0 or more characters, ".b" followed
#by any single character, ".c", or "-old.d" (a suffix does not have to be a
#file extension)
findPrefix(".a*", ".b?", ".c", "-old.d")
}
}