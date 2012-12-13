\name{lisp}
\alias{lisp}
\title{
  List Objects in .PBSmodEnv Workspace
}
\description{
   The function \code{lisp} returns a vector of character strings
   giving the names of the objects in \code{.PBSmodEnv}. It is only
   a wrapper for the base function \code{\link[base]{ls}}.
}
\usage{
lisp(name, pos = .PBSmodEnv, envir = as.environment(pos),
   all.names = TRUE, pattern)
}
\arguments{
  \item{name}{which environment to use in listing the available objects.
    See the details section of \code{\link[base]{ls}}.}
  \item{pos}{an alternative argument to \code{name} for specifying the
    environment as a position in the search list.}
  \item{envir}{an alternative argument to \code{name} for specifying the
    environment.}
  \item{all.names}{a logical value.  If \code{TRUE}, all
    object names are returned.  If \code{FALSE}, names which begin with a
    \samp{.} are omitted.}
  \item{pattern}{an optional \link{regular expression}.  Only names
    matching \code{pattern} are returned. See \code{\link[base]{ls}}
    for additional details.}
}
\details{
  See the base function \code{\link[base]{ls}} for details.
}
\author{
  Copyright 1995--2012 R Core Development Team; distributed under GPL 2 or later.
}
\seealso{
  \code{\link[base]{ls}}, \code{\link[PBSmodelling]{tget}} \cr
  \code{\link[utils]{glob2rx}} for converting wildcard patterns to regular expressions.
}
\keyword{environment}

