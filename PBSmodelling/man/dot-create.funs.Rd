\name{dot-create.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.createTkFont}
\alias{.createWidget}
\alias{.createWidget.button}
\alias{.createWidget.check}
\alias{.createWidget.data}
\alias{.createWidget.droplist}
\alias{.createWidget.entry}
\alias{.createWidget.grid}
\alias{.createWidget.image}
\alias{.createWidget.include}
\alias{.createWidget.label}
\alias{.createWidget.matrix}
\alias{.createWidget.notebook}
\alias{.createWidget.null}
\alias{.createWidget.object}
\alias{.createWidget.object.scrolling}
\alias{.createWidget.progressbar}
\alias{.createWidget.radio}
\alias{.createWidget.slide}
\alias{.createWidget.slideplus}
\alias{.createWidget.spinbox}
\alias{.createWidget.table}
\alias{.createWidget.text}
\alias{.createWidget.vector}
\title{
  Dot Create Functions
}
\description{
  Various dot-create functions used by PBSmodelling code internally.
}
\usage{
.createTkFont(fontstr)
.createWidget(tk, widgetList, winName)
.createWidget.button(tk, widgetList, winName)
.createWidget.check(tk, widgetList, winName)
.createWidget.data(tk, widgetList, winName)
.createWidget.droplist(tk, widgetList, winName)
.createWidget.entry(tk, widgetList, winName)
.createWidget.grid(tk, widgetList, winName)
.createWidget.image(tk, widgetList, winName)
.createWidget.include(tk, widgetList, winName)
.createWidget.label(tk, widgetList, winName)
.createWidget.matrix(tk, widgetList, winName)
.createWidget.notebook(tk, widgetList, winName)
.createWidget.null(tk, widgetList, winName)
.createWidget.object(tk, widgetList, winName, userObject=NULL )
.createWidget.object.scrolling(tk, widgetList, winName)
.createWidget.progressbar(tk, widgetList, winName)
.createWidget.radio(tk, widgetList, winName)
.createWidget.slide(tk, widgetList, winName)
.createWidget.slideplus(tk, widgetList, winName)
.createWidget.spinbox(tk, widgetList, winName)
.createWidget.table(tk, widgetList, winName)
.createWidget.text(tk, widgetList, winName)
.createWidget.vector(tk, widgetList, winName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fontstr}{\code{character} -- string describing a font and colour}
  \item{tk}{\code{character} -- frame to attach widget to}
  \item{widgetList}{\code{character} -- widget list}
  \item{winName}{\code{character} -- active window name}
  \item{userObject}{\code{numeric|character} -- matrix or data frame}
}
\details{
  Various create functions used by PBSmodelling code.

  \code{.createTkFont} -- create a usable TK font from a given string

  \code{.createWidget} -- Generic function to create most widgets, 
    which calls appropriate\cr \code{.createWidget.<widgetType>()} function
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer,
  Earthly Technologies, Victoria BC

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-31}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{testWidgets}}, 
  \code{\link[PBSmodelling]{showArgs}},
  \code{\link[PBSmodelling]{.createWidget.history}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_guiFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.

