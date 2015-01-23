\name{viewCode}
\alias{viewCode}
\title{ View Package R Code }
\description{
  View the R code of all functions in a specified package 
  installed on the user's system.
}
\usage{
viewCode(pkg="PBSmodelling", funs, output=4, ...)
}
\arguments{
  \item{pkg}{ string name of a package installed on the user's computer. }
  \item{funs}{ string vector of explicit function names from \code{pkg} to view. }
  \item{output}{ numeric value: 1 = function names only, 2 = function names with brief description,
    3 = functions and their arguments, and 4 = function R-code (default). }
  \item{...}{allows user to specify two additional arguments for \code{output=2}: \cr
    \code{remote} - character string giving a valid URL for the \code{R_HOME}
    directory on a remote location; \cr
    \code{update} - logical: if \code{TRUE}, attempt to update the package
    index to reflect the currently available packages. 
    (Not attempted if \code{remote} is non-\code{NULL}.) \cr \cr
    Also, if user specifies \code{pat=TRUE}, then \code{funs}, if specified,
    are treated like patterns. }
}
\details{
  If \code{funs} is not specified, then all functions, including
  hidden (dot) functions are displayed. \cr
  If the package has a namespace, functions there are also displayed.
}
\note{
  Output type 2 (function name with brief description) will now only work in
  R (>=3.2.0) or from SVN revision >= 67548.
  The CRAN gurus now disallow direct calls to \code{tools:::httpdPort}.
}
\value{
  Invisibly returns source code of all functions in the specified package.
  The function invokes \code{openFile} to display the results.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{ 
  \code{\link{showHelp}}, \code{\link{view}}
}
\keyword{ character }
\keyword{ package }
