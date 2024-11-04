\name{dot-project.funs}
\alias{.doClean}
\alias{.doCleanWD}
\alias{.getHiddenEnv}
\alias{.getHome}
\alias{.getPrefix}
\alias{.makeCleanVec}
\alias{.mergeLists2}
\alias{.mergeVectors}
\alias{.optionsNotUpdated}
\alias{.removeFromList}
\alias{.selectCleanBoxes}
\alias{.setOption}
\alias{.showLog}
\alias{.stripExt}
\alias{.tryOpen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Dot Project Functions
}
\description{
  Various dot functions used by PBSmodelling code internally to support project functions.
}
\usage{
.doClean()
.doCleanWD()
.getHiddenEnv()
.getHome()
.getPrefix(quiet=FALSE)
.makeCleanVec(vecName, items, rowLen)
.mergeLists2(list1, list2)
.mergeVectors(v1, v2)
.optionsNotUpdated()
.removeFromList(l, items)
.selectCleanBoxes()
.setOption(option, type)
.showLog(logText, fname, noWindow=FALSE, width=80, height=30)
.stripExt(x)
.tryOpen(filename, quiet=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quiet}{\code{logical} -- if \code{TRUE}, no errors/alerts will be displayed}
  \item{vecName}{\code{character} -- infer: name of vector}
  \item{items}{\code{character} -- items in vector to be cleaned | vector of names of components to remove}
  \item{rowLen}{\code{numeric} -- infer: number of rows in vector if dimensioned}
  \item{list1}{\code{list} -- first list}
  \item{list2}{\code{list} -- second list}
  \item{v1}{\code{numeric} -- first vector}
  \item{v2}{\code{numeric} -- second vector}
  \item{l}{\code{list} -- a list}
  \item{option}{\code{character} -- infer: name of option}
  \item{type}{\code{character} -- either \code{"dir"} or \code{"file"} for each function}
  \item{logText}{\code{character} -- the text for the log}
  \item{fname}{\code{character} -- name for log file (optional)}
  \item{noWindow}{\code{logical} -- if \code{TRUE}, log window will not be shown}
  \item{width}{\code{numeric} -- width of log window}
  \item{height}{\code{numeric} -- height of log window}
  \item{x}{\code{character} -- vector of filenames, e.g., \code{"foo.c"}}
  \item{filename}{\code{character} -- the file to open}
}
\details{
\tabular{ll}{
  \code{.doClean}   \tab used by \code{cleanProj()}; function called when Clean button is pressed\cr
  \code{.doCleanWD} \tab Anisa's \code{.doClean} function modified for file names only\cr
  \code{.getHiddenEnv}   \tab allow GUIs from outside PBSmodelling to call PBSmodelling's internal hidden functions\cr
  \code{.getHome}   \tab returns platform dependent home drive (Windows) or user home (Unix)\cr
  \code{.getPrefix} \tab get the project prefix from the focused GUI; used to standardise error message\cr
  \code{.makeCleanVec}   \tab used by \code{cleanProj()} to create strings describing checkbox vectors\cr
  \code{.mergeLists2}     \tab add a second list to a first list; if both lists share any keys, the values from the second list\cr
                         \tab overwrite those in the first list; order of components in list is preserved\cr
  \code{.mergeVectors}   \tab add a second vector to a first vector; if both vectors share any values, the resulting vector will\cr
                         \tab only contain this value once\cr
  \code{.optionsNotUpdated}   \tab check if any of the options given by \code{declareGUIoptions} are different in the GUI from their\cr
                              \tab stored values; a blank GUI entry is equivalent to the option not being set\cr
  \code{.removeFromList} \tab remove components of a list with the given names; \code{"NA"} can be used to remove \code{NA} names\cr
  \code{.selectCleanBoxes}    \tab used by \code{cleanProj}; selects or deselects all of the checkboxes\cr
  \code{.setOption} \tab used for \code{setPathOption} and \code{setFileOption}; see these functions for a descriptions\cr
  \code{.showLog}   \tab given output for a log, will make pop-up log window containing this output and/or\cr
                    \tab write this output to a logfile; \code{"\r"} is stripped from the log text for display\cr
  \code{.stripExt}  \tab remove file extension from end of filename\cr
  \code{.tryOpen}   \tab tries to open a given file using an editor entered by the GUI; if an editor wasn't set,\cr
                    \tab tries to open using \code{openFile}; appropriate alerts are shown if \code{quiet} isn't turned on\cr
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
  Last modified \code{Rd: 2024-11-01}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSmodelling]{cleanProj}},
  \code{\link[PBSmodelling]{cleanWD}},
  \code{\link[PBSmodelling]{findPrefix}},
  \code{\link[PBSmodelling]{getGUIoptions}},
  \code{\link[PBSmodelling]{setGUIoptions}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M05_projectFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
