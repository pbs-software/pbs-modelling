\name{dot-convert.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.convertMatrixListToDataFrame}
\alias{.convertMatrixListToMatrix}
\alias{.convertMode}
\alias{.convertOldGridToNewGrid}
\alias{.convertParamStrToList}
\alias{.convertParamStrToVector}
\alias{.convertVecToArray}
\title{
  Dot Convert Functions
}
\description{
  Various dot-convert functions used by PBSmodelling code internally.
}
\usage{
.convertMatrixListToDataFrame(mList, colName="Y", rowNames="X")
.convertMatrixListToMatrix(mList)
.convertMode(x, mode)
.convertOldGridToNewGrid(grid)
.convertParamStrToList(x, fname="", line.start=0, line.end=0, sourcefile=list())
.convertParamStrToVector(x, fname="", line=0)
.convertVecToArray(x, d, byright=FALSE, byrow=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mList}{\code{list} -- matrix list object}
  \item{colName}{\code{character} -- name of column}
  \item{rowNames}{\code{character} -- vector of row names}
  \item{x}{\code{character|numeric} -- string object or vector of data to use to create array}
  \item{mode}{\code{various} -- infer: mode of object}
  \item{grid}{\code{numeric} -- infer: grid Polyset}
  \item{fname}{\code{character} -- name of file for warning messages}
  \item{line.start}{\code{numeric} -- line number where widget is first found}
  \item{line.end}{\code{numeric} -- line number of last line of widget (ie extended line)}
  \item{sourcefile}{\code{list} -- list object}
  \item{line}{\code{numeric} -- line number for warning messages}
  \item{d}{\code{numeric} -- dimensions of the array}
  \item{byright}{\code{logical} -- if \code{TRUE}, vary indices by the most right number first}
  \item{byrow}{\code{logical} -- if \code{TRUE}, process indices by row?}
}
\details{
  Various convert functions used by PBSmodelling code.
  
  \code{.convertMatrixListToMatrix} -- Convert a list into an N-dim array

  \code{.convertMatrixListToDataFrame} -- Convert a list into a data frame

  \code{.convertMode} -- Convert a variable into a mode without showing any warnings (deprecated?)

  \code{.convertOldGridToNewGrid} -- Convert old style (where grid has a grid$.widgets[[i]][[j]] data structure)
    to new format: list( grid, child, child, ..., child ); this function should never be used by new code
    (hopefuly it will be removed one day)

  \code{.convertParamStrToList} -- Convert a given string of values seperated by spaces into a list
    while preserving space and escaped quotes within quotes (kindof -- the value must still be 
    stripped depending if its a single string, or vector of strings)

  \code{.convertParamStrToVector} -- Convert a string, x, into a vector of  elements seperated by whitespace;
    whitespace can be interupted as values if it is enclosed by quotes;
    special characters (newline, tab, \, ', ") must be escaped with \

  \code{.convertVecToArray} -- Convert a vector to an array
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-30}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{.createWidget.data}},
  \code{\link[PBSmodelling]{.createWidget.history}},
  \code{\link[PBSmodelling]{.createWidget.matrix}},
  \code{\link[PBSmodelling]{.extractVar}},
  \code{\link[PBSmodelling]{.getParamFromStr}},
  \code{\link[PBSmodelling]{.readList.P.convertData}},
  \code{\link[PBSmodelling]{.superobject.saveValues}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_guiFuns}
\concept{M06_supportFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.

