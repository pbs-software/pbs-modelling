\name{dot-c.support.funs}
\alias{.libName}
\alias{.guiSource}
\alias{.guiCompileC}
\alias{.guiDyn}
\alias{.cleanLoadC}
\alias{.loadCRunComparison}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Dot C Support Functions
}
\description{
  Various dot C-support functions used by PBSmodelling code internally.
}
\usage{
.libName(lib="")
.guiSource()
.guiCompileC()
.guiDyn()
.cleanLoadC()
.loadCRunComparison()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lib}{\code{character} -- vector of file names without extensions}
}
\details{
\tabular{ll}{
  \code{.libName}   \tab given a character vector of shared library object names, returns the file names with the\cr
                    \tab appropriate extension for the user's platform (\code{.dll} for Windows or \code{.so} for Unix)\cr
  \code{.guiSource} \tab sources the .r file in the working directory indicated by the prefix entry widget in the GUI\cr
  \code{.guiCompileC}    \tab gets the prefix and libPrefix arguments from the GUI and uses them to call \code{.compileC}\cr
  \code{.guiDyn}    \tab based on the previous GUI action, either tries to load or unload the library with the \code{lib}\cr
                    \tab prefix specified in the GUI (or the project file prefix if this is left blank)\cr
  \code{.cleanLoadC}\tab clean function\cr
  \code{.loadCRunComparison}  \tab runs the provided C and R functions a number of times specified in the GUI and writes\cr
                              \tab into text boxes the elapsed time for each
}
}
\value{
	Varied lists and other objects to facilitate GUI manipulation.
}
\author{
  Anisa Egeli, Software Developer,
  Alumnus of Vancouver Island University, Nanaimo BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-11-04}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSmodelling]{loadC}},
  \code{\link[PBSmodelling]{compileC}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M06_supportFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
