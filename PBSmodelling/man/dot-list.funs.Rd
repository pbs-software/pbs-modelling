\name{dot-list.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.mergeLists}
\alias{.readList.C}
\alias{.readList.P}
\alias{.readList.P.convertData}
\alias{.writeList.P}
\title{
  Dot List Functions
}
\description{
  Various dot-list functions used by PBSmodelling code internally.
}
\usage{
.mergeLists(x, y) 
.readList.C(fname)
.readList.P(fname)
.readList.P.convertData(varOptions, varData, fname="", sourcefile=list())
.writeList.P(x, fname="", comments, prefix="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{character} -- element x in a list object | data to write to file}
  \item{y}{\code{character} -- element y in a list object}
  \item{fname}{\code{character} -- name of file to read or wtite}
  \item{varOptions}{\code{compound} -- options for a variable}
  \item{varData}{\code{character} -- data for a variable}
  \item{sourcefile}{\code{character} -- using in \code{cat} statements (not sure)}
  \item{comments}{\code{character} -- include string as comment at the top of file}
  \item{prefix}{\code{character} -- prefix for line entries}
}
\details{
  Various dot functions used by PBSmodelling code to control list objects.

  \code{.mergeLists} -- taken from R Curl : merge \code{x} and \code{y},
    if elements match in both \code{x} and \code{y}, then elements from \code{y}
    are stored (overwriting \code{x}'s elements).

  \code{.readList.C} -- read ADMB-like input file and create a list;
    adapted from \code{'readAD'} in PBSawatea.

  \code{.readList.P} -- read list in \code{'P'} format (PBS format).

  \code{.readList.P.convertData} -- helper function to convert data into proper mode.

  \code{.writeList.P} -- saves list \code{x} to disk using \code{'P'} format (PBS format).
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-31}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{lisp}},
  \code{\link[PBSmodelling]{packList}},
  \code{\link[PBSmodelling]{readList}},
  \code{\link[PBSmodelling]{unpackList}},
  \code{\link[PBSmodelling]{writeList}},
  \code{\link[PBSmodelling]{.pFormatDefs}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M04_listFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.

