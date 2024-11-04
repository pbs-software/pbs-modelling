\name{dot-support.funs}
\alias{.fibCall}
\alias{.fibC}
\alias{.fibR}
\alias{.fibClosedForm}
%===== Demo Support Functions ====================
\alias{.viewPkgDemo}
\alias{.dClose}
\alias{.dUpdateDesc}
\alias{.viewPkgVignette}
%===== Misc Support Functions ====================
\alias{.addslashes}
\alias{.findSquare}
\alias{.forceMode}
\alias{.getArrayPts}
\alias{.initPBSoptions}
\alias{.mapArrayToVec}
%\alias{.removeFromList}
\alias{.tclArrayToVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Dot Support Functions
}
\description{
  Various dot support functions used by PBSmodelling code internally.
}
\usage{
%===== C Support Functions =======================
.fibCall(n, len=1)
.fibC(n, len=1)
.fibR(n, len=1)
.fibClosedForm(n, len=1)
%===== Demo Support Functions ====================
.viewPkgDemo()
.dClose()
.dUpdateDesc()
.viewPkgVignette()
%===== Misc Support Functions ====================
.addslashes(x, asvector=FALSE )
.findSquare(nc)
.forceMode(x, mode)
.getArrayPts(d, byright=FALSE, byrow=TRUE)
.initPBSoptions()
.mapArrayToVec(x, d, byright=FALSE)
%.removeFromList (l, items)
.tclArrayToVector(str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{\code{numeric} -- calculate the nth number}
  \item{len}{\code{numeric} -- return the last \code{len} calculated numbers}
  \item{x}{\code{character} - string to escape |  variable to convert | array index (numeric vector)}
  \item{asvector}{\code{logical} -- if \code{TRUE}, always force strings to be printed as charactervector,
    e.g., \code{"h w"} becomes \code{"'h w'"}}
  \item{nc}{\code{numeric} -- number of panels (figures) in a multi-panel plot}
  \item{mode}{\code{various} -- mode to convert to}
  \item{d}{\code{numeric} -- dimensions of the array}
  \item{byright}{\code{logical} -- if \code{TRUE}, vary indices by the most right number first}
  \item{byrow}{\code{logical} -- if \code{TRUE}, populate array by row}
%  \item{l}{\code{character} -- infer: item to be removed}
%  \item{items}{\code{character} -- infer: list of items}
  \item{str}{\code{character} -- infer: a \code{tcl} string}
}
\details{
\tabular{ll}{
  \code{.fibCall}   \tab used by \code{calcFib()} to xalculate a vector containing fibonacci numbers\cr
  \code{.viewPkgDemo}    \tab display a GUI to display something equivalent to R's \code{demo()}\cr
  \code{.dClose}    \tab function to execute on closing \code{runDemos()}\cr
  \code{.dUpdateDesc}    \tab update demo window with demo descriptions\cr
  \code{.viewPkgVignettes}    \tab display a GUI to display something equivalent to R's \code{vignette()}\cr
  \code{.addslashes}\tab escape special characters from a string, which can then be used in the \code{"P"} format;\cr
                    \tab if \code{x} has more than one element, then it will returned as a nested \code{characterVector}\cr
                    \tab e.g., \verb{c("it's", "O K")} becomes \verb{"'it\'s' 'O K'"}\cr
  \code{.convertVecToArray}   \tab convert a vector to an array\cr
  \code{.findSquare}     \tab calculate number of rows and columns from a single number of panels\cr
  \code{.forceMode} \tab force a variable into a mode without showing any warnings\cr
  \code{.getArrayPts}    \tab return all possible indices of an array\cr
  \code{.initPBSoptions} \tab called from zzz.r's \code{.First.lib()} intialization function\cr
  \code{.mapArrayToVec}  \tab determine which index to use for a vector, when given an N-dim index of an array\cr
%  \code{.removeFromList} \tab remove items from a list\cr
  \code{.tclArrayToVector}    \tab infer: change tcl code to represent array as a vector
}
}
\value{
	Varied lists and other objects to facilitate GUI manipulation.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer,
  Earthly Technologies, Victoria BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-11-04}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSmodelling]{calcFib}},
  \code{\link[PBSmodelling]{runDemos}},
  \code{\link[PBSmodelling]{showVignettes}},
  \code{\link[PBSmodelling]{readPBSoptions}},
  \code{\link[PBSmodelling]{runExamples}}
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
