\name{viewCode}
\alias{viewCode}
\title{ View Package R Code }
\description{
  View the R code of all functions in a specified package 
  installed on the user's system.
}
\usage{
viewCode(pkg="PBSmodelling", funs)
}
\arguments{
  \item{pkg}{ string name of a package installed on the user's computer. }
  \item{funs}{ string vector of explicit function names from \code{pkg} to view. }
}
\details{
  If \code{funs} is not specified, then all functions, including
  hidden (dot) functions are displayed. \cr
  If the package has a namespace, functions there can also be displayed.
}
\value{
  Invisibly returns source code of all functions in the specified package.
  The function invokes \code{openFile} to display the results.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\keyword{ character }
\keyword{ package }
