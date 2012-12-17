\name{clearAll}
\alias{clearAll}
\title{Remove all R Objects From a Specified Environment}

\description{
  Generic function to clear all objects from .RData in R
}
\usage{
clearAll(hidden=TRUE, verbose=TRUE, PBSsave=TRUE, pos=".PBSmodEnv")
}

\arguments{
  \item{hidden}{if \code{TRUE}, remove variables that start with a dot(.).}
  \item{verbose}{if \code{TRUE}, report all removed items.}
  \item{PBSsave}{if \code{TRUE}, do not remove \code{.PBSmod}.}
  \item{pos}{The pos argument can specify the environment in which to look
    for the object in any of several ways: as an integer (the position in
    the search list); as the character string name of an element in the
    search list; or as an environment (including using \code{sys.frame} to access
    the currently active function calls).}
}
\author{
  Jon T. Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\keyword{methods}
