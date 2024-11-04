\name{dot-history.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.createWidget.history}
\alias{.sortHelper}
\alias{.sortHelperActive}
\alias{.sortHelperFile}
\alias{.sortWidget}
\alias{.updateFile}
\alias{.updateHistory}
\alias{.updateHistoryButtons}
\title{
  Dot History Functions
}
\description{
  Various dot functions used by PBSmodelling code internally for the history widget.
}
\usage{
.createWidget.history(tk, widgetList, winName)
.sortHelper()
.sortHelperActive(hisname)
.sortHelperFile(openfile, savefile)
.sortWidget(d, hisname)
.updateFile()
.updateHistory(hisname)
.updateHistoryButtons(hisname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tk}{\code{character} -- frame to attach widget to}
  \item{widgetList}{\code{character} -- widget list}
  \item{winName}{\code{character} -- active window name}
  \item{hisname}{\code{character} -- history instance name if multiple are active}
  \item{openfile}{\code{character} -- file name of repository containing windows GUI settings}
  \item{savefile}{\code{character} -- file name of repository for windows GUI settings}
  \item{d}{\code{numeric|character} -- data.frame object}
}
\details{
  Various dot functions used by PBSmodelling code to control the history widget.

  \code{.createWidget.history} -- create a history widget

  \code{.sortHelper} -- sort entries in an existing history instance

  \code{.updateHistory} -- update widget values
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
  \code{\link[PBSmodelling]{testWidgets}}, 
  \code{\link[PBSmodelling]{showArgs}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M08_historyFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.

