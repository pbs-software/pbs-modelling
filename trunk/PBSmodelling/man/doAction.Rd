\name{doAction}
\alias{doAction}
\title{ Execute Action Created by a Widget }
\description{
  Executes the action expression formulated by the user and 
  written as an \samp{action} by a widget.
}
\usage{
doAction(act)
}
\arguments{
  \item{act}{string representing an expression that can be executed}
}
\details{
  If \code{act} is missing, \code{doAction} looks for it in the action
  directory of the window's widget directory in \code{.PBSmod}. This 
  action can be accessed through \code{getWinAct()[1]}.

  Due to parsing complications, the expression \code{act} translates various symbols. \cr
  The function translates:
  
  1. The back tick character \samp{`} to a double quote \samp{"} character. For example,
  \preformatted{"openFile(paste(getWinVal()$prefix,`.tpl`,sep=``))"}
  
  2. Underscore period \samp{_.} to four back slashes and one period \samp{\\\\\\\\.}. For example,
  \preformatted{"poop=strsplit(`some.thing.else`,split=`_.`)"}
}
\value{
  Invisibly returns the string expression \code{expr}.
}
\examples{
  createWin("button text=\"list objects\" func=doAction action=print(ls())",
    astext=TRUE)
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBSmodelling]{createWin}}, \code{\link[PBSmodelling]{evalCall}},
  \code{\link[PBSmodelling]{getWinAct}}
}
\keyword{ utilities }
\keyword{ character }
