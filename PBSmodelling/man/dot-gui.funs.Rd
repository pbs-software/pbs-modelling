\name{dot-gui.funs}
%===== Create Functions ==========================
%----- see dot-create.funs.Rd --------------------
\alias{.buildgrid}
%===== Map Functions =============================
\alias{.map.add}
\alias{.map.get}
\alias{.map.getAll}
\alias{.map.init}
\alias{.map.set}
%===== Convert Functions =========================
%----- see dot-convert.funs.Rd -------------------
\alias{.setMatrixElement}
\alias{.getMatrixListSize}
\alias{.matrixHelp}
\alias{.autoConvertMode}
%===== Extract Functions =========================
\alias{.extractData}
\alias{.extractFuns}
\alias{.extractVar}
\alias{.getParamFromStr}
\alias{.table.getvalue}
\alias{.table.setvalue}
\alias{.getValueForWidgetSetup}
%===== Helper Functions ==========================
\alias{.getSimilarColour}
\alias{.adjustAllColours}
\alias{.do.gui.call}
\alias{.catError}
\alias{.catError2}
\alias{.check.object.exists}
\alias{.inCollection}
\alias{.packWidgetsIntoGrid}
\alias{.parsegrid}
\alias{.parsemenu}
\alias{.PBSdimnameHelper}
\alias{.searchCollection}
\alias{.setWinValHelper}
\alias{.stopWidget}
\alias{.stripComments}
\alias{.stripSlashes}
\alias{.stripSlashesVec}
\alias{.superobject.redraw}
\alias{.superobject.saveValues}
\alias{.trimWhiteSpace}
\alias{.validateWindowDescList}
\alias{.validateWindowDescWidgets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Dot GUI Functions
}
\description{
  Various dot functions used by PBSmodelling code internally to support the GUI functions.
}
\usage{
%===== Create Functions ==========================
%----- see dot-create.funs.Rd --------------------
.buildgrid(tk, grid, winName, childWidgets)
%===== Map Functions =============================
.map.add(winName, key, ...)
.map.get(winName, key)
.map.getAll(winName)
.map.init(winName)
.map.set(winName, key, ...)
%===== Convert Functions =========================
%----- see dot-convert.funs.Rd -------------------
.setMatrixElement(m, a, ind=NULL)
.getMatrixListSize(m, d=NULL, big=0)
.matrixHelp(matrixList, ind, value)
.autoConvertMode(x)
%===== Extract Functions =========================
.extractData(command, action, winName)
.extractFuns(data)
.extractVar(winName)
.getParamFromStr(inputStr, fname="", line.start=0, line.end=0, sourcefile=list(),
	paramOrder=.widgetDefs)
.table.getvalue(winName, widgetName)
.table.setvalue(winName, widgetName, value)
.getValueForWidgetSetup(varname, widget, winName)
%===== Helper Functions ==========================
.getSimilarColour(col)
.adjustAllColours(tObj)
.do.gui.call(what, args, quote=FALSE, envir=parent.frame()) 
.catError(err, fname, line.start, line.end, sourcefile=list(), errorType="GUI parse error")
.catError2(err, fname, line.start)
.check.object.exists(tk, widget, winName)
.inCollection(haystack, needle)
.packWidgetsIntoGrid(widgets, vertical=TRUE)
.parsegrid(data, nRow, nCol)
.parsemenu(data, nItems)
.PBSdimnameHelper(rownames, colnames, dim)
.searchCollection(haystack, needle)
.setWinValHelper(varname, value, winName)
.stopWidget(err, wid.debug, winName)
.stripComments(x)
.stripSlashes(x, fname="", line.start=0, line.end=0, sourcefile=list())
.stripSlashesVec(x, fname="", line.start=0, line.end=0, sourcefile=list())
.superobject.redraw(winName, widget_name)
.superobject.saveValues(winName, widget_name)
.trimWhiteSpace(x)
.validateWindowDescList(x)
.validateWindowDescWidgets(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tk}{\code{tcltk} -- parent \code{tk} frame to attach widget to}
  \item{grid}{\code{character} -- widget list describing the grid}
  \item{winName}{\code{character} -- active or target window name | map to process}
  \item{childWidgets}{\code{character} -- infer: widgets used by other widgets}
  \item{key}{\code{character} -- name of item to extract (i.e., widget name)}
  \item{...}{\code{dots} -- named items to save (in a list)}
  \item{m}{\code{numeric} -- infer: a matrix}
  \item{a}{\code{numeric} -- infer: an array}
  \item{ind}{\code{numeric} -- infer: index value}
  \item{d}{\code{numeric} -- infer: a dimension}
  \item{big}{\code{numeric} -- infer: minimum size of array dimension}
  \item{matrixList}{\code{numeric} -- infer: matrix list object}
  \item{value}{\code{numeric} -- infer: value to populate the matrix | list}
  \item{x}{\code{character} -- string resembling a numeric | string with or without comments | string to trim | list to validate}
  \item{command}{\code{character} -- user command to call (ie function argument of widget)}
  \item{action}{\code{character} -- action value}
  \item{data}{\code{character} -- list of widget lists}
  \item{inputStr}{\code{character} -- string from win desc file describing a widget}
  \item{fname}{\code{character} -- file name to display with error messages}
  \item{line.start}{\code{numeric} -- line number where widget is first found}
  \item{line.end}{\code{numeric} -- line number of last line of widget (ie extended line)}
  \item{sourcefile}{\code{character} -- source code of the file in question}
  \item{paramOrder}{\code{unknown} -- infer: parameter order}
  \item{widgetName}{\code{character} -- infer: name of a widget}
  \item{varname}{\code{character} -- infer: variable name}
  \item{widget}{\code{list} -- widget list passed to \code{.creatWidget.<widget>}}
  \item{col}{\code{character|numeric} -- the colour to adjust}
  \item{tObj}{\code{tcltk} -- \code{Tk} object}
  \item{what}{\code{unknown} -- infer: what what what}
  \item{args}{\code{character} -- infer: a list of arguments}
  \item{quote}{\code{logical} -- infer: if \code{TRUE}, add | use quaotation marks}
  \item{envir}{\code{environment} -- infer: environment where whatever awaits \code{'what'} works}
  \item{err}{\code{character} -- error string to display}
  \item{errorType}{\code{character} -- type of error to display}
  \item{haystack}{\code{character} -- a vector to search}
  \item{needle}{\code{character} -- a single element to search for}
  \item{widgets}{\code{list} -- infer: widget list}
  \item{vertical}{\code{logical} -- infer: if \code{TRUE}, pack widgets vertically (top to bottom)}
  \item{nRow}{\code{numeric} -- number of grid rows}
  \item{nCol}{\code{numeric} -- number of grid columns}
  \item{nItems}{\code{numeric} -- number of children to select for the menu}
  \item{rownames}{\code{character} -- vector of size 1, or dim[1] naming the rows; 
    if only one name is given, a number (1..dim[1]) will be appended to the name}
  \item{colnames}{\code{character} -- vector of size 1 or dim[2] naming columns; 
    if only one name is given, then (1..dim[2]) is appended}
  \item{dim}{\code{character} -- vector of size 2, dim[1] is \code{nRows}, dim[2] is \code{nCols}}
  \item{wid.debug}{\code{list} -- list of widget codes (created in parsing process)}
  \item{widget_name}{\code{character} -- infer: name of a widget}
}
\details{
\tabular{ll}{
  \bold{Create------}    \tab\cr
  \code{.buildgrid}      \tab used to create a grid on a window\cr
  \bold{Map------}        \tab\cr
  \code{.map.add}        \tab save a new value for a given key; if a previous exists, ignore the new\cr
                         \tab value and return previous value\cr
  \code{.map.get}        \tab returns a value associated with a key\cr
  \code{.map.getAll}     \tab returns all visible items of a map of a certain window\cr
  \code{.map.init}       \tab initialize the data structure that holds the map(s)\cr
  \code{.map.set}        \tab save a new value for a given key, even if it involves overwriting a\cr
                         \tab previously stored value\cr
  \bold{Convert------}   \tab\cr
  \code{.setMatrixElement}    \tab helper function used by \code{.convertMatrixListToMatrix}\cr
                              \tab to assign values from the matrix list into the array\cr
  \code{.getMatrixListSize}   \tab helper function used by \code{.convertMatrixListToMatrix}\cr
                              \tab to determine the minumum required size of the array needed to create\cr
                              \tab  or convert the list into an array\cr
  \code{.matrixHelp}     \tab used to help \code{.extractVar} deal with N-dim matricies; firstly it is\cr
                         \tab converted into a ``matrix list'' once the matrix list is completed\cr
                         \tab (and size known), it should be converted into a true array\cr
  \code{.autoConvertMode}\tab converts \code{x} into a numeric mode, if it looks like a valid number\cr
  \bold{Extract------}   \tab\cr
  \code{.extractData}    \tab called by \code{TK} on button presses (or binds, on changes, slides, ...)\cr
  \code{.extractFuns}    \tab get a list of called functions\cr
  \code{.extractVar}     \tab extracts values from the \code{tclvar} pointers\cr
  \code{.getParamFromStr}\tab returns a list with all parameters extracted from a list\cr
  \code{.table.getvalue} \tab infer: get a value from a table\cr
  \code{.table.setvalue} \tab infer: set a value in a table\cr
  \code{.getValueForWidgetSetup}   \tab returns the value(s) to use for initializing a widget from:\cr
                                   \tab --> \code{widget$value} (if set) or\cr
                                   \tab --> global var matching \code{widget$name} when \code{value} is \code{NULL}\cr
  \bold{Helper------}    \tab\cr
  \code{.getSimilarColour}    \tab obtains a similar colour (slightly brighter/darker) than the argument\cr
  \code{.adjustAllColours}    \tab for the passed Tk widget, attempts to slightly modify all applicable colours\cr
  \code{.do.gui.call}    \tab extends \code{do.call}, which is used to create most \code{Tk} widgets, to immediately\cr
                         \tab adjust a new widget's colours (to get around how \code{tk_setPalette}\cr
                         \tab changes the colour of existing widgets)\cr
  \code{.catError}  \tab used to display parsing errors\cr
  \code{.catError2} \tab infer: a variation of \code{.catError}\cr
  \code{.check.object.exists} \tab call to test for existence of dynamically loaded object for \code{"object"} and\cr
                              \tab \code{"table"}; returns \code{NULL} on no errors; otherwise a \code{tk} display\cr
                              \tab error is created (can be embedded in window)\cr
  \code{.inCollection}   \tab returns true if needle occurs in haystack\cr
  \code{.packWidgetsIntoGrid} \tab pack all widgets into a grid with \code{ncol=1} \code{nrow=}<number of widgets>\cr
  \code{.parsegrid} \tab returns two items in a list:\cr
                    \tab --> \code{$gridData}, which is a list of lists representing columns\cr
                    \tab --> \code{$unparsedData}, which is left over from the grid and still needs parsing\cr
  \code{.parsemenu} \tab very similar to \code{.parsegrid} but for menus;\cr
                    \tab set up a menu with children menus or menuitems\cr
  \code{.PBSdimnameHelper}    \tab adds dimnames to stuff (matrix, data.frame)\cr
  \code{.searchCollection}    \tab searches a haystack for a needle, or a similar longer needle\cr
  \code{.setWinValHelper}     \tab infer: help to set GUI Window values\cr
  \code{.stopWidget}     \tab stop due to fatal error during window creation (not parse)\cr
  \code{.stripComments}  \tab removes any trailing comments from a line, but ignores \code{'#'} in quoted strings\cr
  \code{.stripComments}  \tab infer: strips out comments from a set of strings\cr
  \code{.stripSlashes}   \tab removes slashes from a string\cr
  \code{.stripSlashesVec}\tab given a string, split it into a vector of words, which were seperated by spaces;\cr
                         \tab however, if single quotes are used, space is perserved;\cr
                         \tab e.g., \code{x="a b `c d'"} converts into \code{"a" "b" "c d"}\cr
  \code{.superobject.redraw}  \tab infer: redraw a \code{superobject} (whatever that is)\cr
  \code{.superobject.saveValues}   \tab must be outside of \code{.createwidget.superobject} since \code{getWinVal}\cr
                                   \tab must be able to call this\cr
  \code{.trimWhiteSpace} \tab remove leading and trailing whitespace\cr
  \code{.validateWindowDescList}   \tab determines if the list represents a valid PBS Modelling description list;\cr
                                   \tab if any required fields are missing, it will halt via \code{stop()};\cr
                                   \tab if any fields are omitted which have default values defined in the\cr
                                   \tab  \code{.widgetDefs} list, then those fields and values will be set\cr
  \code{.validateWindowDescWidgets}\tab used by \code{.validateWindowDescList} to validate each widget\cr
}
}
\value{
	Stuff and such.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer,
  Earthly Technologies, Victoria BC

  \href{https://github.com/boersn/}{Nicholas M. Boers},
  Software Engineer, Jobber, Edmonton AB

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-11-01}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
  \code{\link[PBSmodelling]{dot-create.funs}},
  \code{\link[PBSmodelling]{dot-convert.funs}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M02_guiFuns}
% \concept{ ~cpt2 }
% Use only one concept per line.
