\name{dot-widget.defs}
\alias{.widgetDefs}
\alias{.pFormatDefs}
\alias{.tagDefs}
\alias{.tagOptionChecks}
\alias{.maxActionSize}
%===== Regular Expression Strings ================
\alias{.regex.complex}
\alias{.regex.numeric}
\alias{.regex.logical}
\docType{data}
\title{
  Dot Widget Definitions
}
\description{
  Primary controller of the various widgets in \code{PBSmodelling}.
}
\usage{
.widgetDefs
.pFormatDefs
.tagDefs
.tagOptionChecks
.maxActionSize
.regex.complex
.regex.numeric
.regex.logical
}
\format{
List of 25 widgets:
\tabular{llllllllll}{
  window      \tab list  8 \tab
  grid        \tab list 19 \tab
  notebook    \tab list 18 \tab
  image       \tab list  7 \tab
  progressbar \tab list 15\cr
  menu        \tab list  6 \tab
  menuitem    \tab list  7 \tab
  label       \tab list 14 \tab
  droplist    \tab list 19 \tab
  spinbox     \tab list 21\cr
  table       \tab list 15 \tab
  null        \tab list  4 \tab
  include     \tab list  3 \tab
  entry       \tab list 22 \tab
  button      \tab list 13\cr
  check       \tab list 15 \tab
  radio       \tab list 15 \tab
  slide       \tab list 15 \tab
  slideplus   \tab list 18 \tab
  vector      \tab list 25\cr
  matrix      \tab list 28 \tab
  data        \tab list 28 \tab
  object      \tab list 23 \tab
  history     \tab list 13 \tab
  text        \tab list 16
}
}
\details{
  Storage list that specifies characteristics of each widget's arguments.
\tabular{ll}{
  \code{.widgetDefs}     \tab defines allowed widget parameters names and default values\cr
  \code{.pFormatDefs}    \tab defines allowed P format parameters names and default values\cr
                         \tab to be used in \code{'readList'} (when P format is detected)\cr
  \code{.tagDefs}   \tab defines formatting for each valid tag used in \code{'presentTalk'},\cr
                    \tab but presumably could be applied to other appliances\cr
  \code{.tagOptionChecks}\tab provides validation check for each tag option\cr
  \code{.maxActionSize}  \tab maximum amount of actions to save in memory (default=50)\cr
  \code{.regex.complex}  \tab catches all valid complex except it also catches \code{"-"}\cr
  \code{.regex.numeric}  \tab catches numeric strings, but also catches \code{"-"}\cr
  \code{.regex.logical}  \tab catches all logical values
}
}
\examples{
  str(.widgetDefs,1)
}
\seealso{
  \code{\link[PBSmodelling]{testWidgets}}, 
  \code{\link[PBSmodelling]{showArgs}},
  \code{\link[PBSmodelling]{presentTalk}},
  \code{\link[PBSmodelling]{readList}}
}
\keyword{datasets}
\concept{M01_widgetDefs}

