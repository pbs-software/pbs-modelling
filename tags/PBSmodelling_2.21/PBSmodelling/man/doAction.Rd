\name{doAction}
\alias{doAction}
\title{ Execute Action Created by a Widget }
\description{
  Executes the action expression formulated by the user and 
  written as an \samp{action} by a widget.
}
\usage{
doAction(act, envir=.GlobalEnv)
}
\arguments{
  \item{act}{string representing an expression that can be executed}
  \item{envir}{the R environment in which to evaluate the action; 
  the default is the global environment or user's workspace. }
}
\details{
  If \code{act} is missing, \code{doAction} looks for it in the action
  directory of the window's widget directory in \code{.PBSmod}. This 
  action can be accessed through \code{getWinAct()[1]}.

  Due to parsing complications, the expression \code{act} must contain 
  the backtick character \samp{`} wherever there is to be an internal 
  double quote \samp{"} character. For example,
  \preformatted{"openFile(paste(getWinVal()$prefix,`.tpl`,sep=``))"}
}
\value{
  Invisibly returns the string expression \code{act}.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\keyword{ utilities }
\keyword{ character }
