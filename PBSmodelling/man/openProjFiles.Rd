\name{openProjFiles}
\alias{openProjFiles}
\title{Open Files with a Common Prefix}

\description{
  Open one or more files from the working directory, given one 
  file prefix and one or more file suffixes.
}
\usage{
openProjFiles(prefix, suffix, package=NULL, warn=NULL, alert=TRUE)
}
\arguments{
  \item{prefix}{a single prefix to prepend to each suffix}
  \item{suffix}{a character vector of suffixes to append to the prefix}
  \item{package}{name of the package that contains templates,
    or \code{NULL} to not use templates}
  \item{warn}{if specified, use to temporarily override the 
    current R warn option during this function's activity; 
    if \code{NULL}, the current warning settings are used.}
  \item{alert}{if \code{TRUE}, an alert message is shown should 
    any files fail to be opened; \cr if \code{FALSE}, no alert is displayed.}
}
\details{
  The suffixes may contain wildcards (\code{"*"} to match 0 or more 
  characters, \code{"?"} to match any single character).

  For any file that does not exist in the working directory, a template can
  optionally be copied from a directory named \code{templates} in the specified
  package. The templates in this directory should have the prefix \code{template},
  followed by the suffix to match when \code{openProjFiles} is called
  (e.g., \code{template.c} to match the suffix \code{.c}. After being copied 
  to the working directory, the new file is renamed to use the specified prefix.

  To use this function in a \emph{window description file}, the
  \code{package} and \code{suffix} arguments must be specified as the action of the widget
  that calls \code{openProjFiles}. Furthermore, \code{package} and each \code{suffix} must be
  separated by commas. For example, \code{action=myPackage,.r,.c} will try to open a \code{.r}
  and \code{.c} file in the working directory, copying templates from the \code{template}
  directory for the package \pkg{myPackage}, if the files didn't already exist. To
  disable templates, leave \code{package} unspecified but keep the leading comma
  (e.g., \code{action=,.r,.c}). When the function is called from a widget in this
  fashion, the prefix is taken from the value of a widget named \code{prefix}.

  If all the required arguments are missing, it is assumed that the 
  function is being called by a GUI widget.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  *** This function is deprecated and will be removed in a future version.
  Use \code{\link{openFile}} or \code{\link{doAction}} with \code{openFile}. ***

  *** Warning: this function is buggy; see examples for an alternative. ***
}
\seealso{
  \code{\link{openFile}}, \code{\link{setPBSext}}, \code{\link{openExamples}},
  \code{\link{openPackageFile}}
}
\examples{
\dontrun{
openProjFiles("foo", c(".r", ".c"), package="myPackage")

#use openFile directly:
openFile( dir(pattern="^prefix.*\\.suffix$") )

#example with multiple suffixes (prefix=foo, suffix=.r, .c)
openFile( dir(pattern="^foo.*\\.(r|c)$") )
}
}
\keyword{file}