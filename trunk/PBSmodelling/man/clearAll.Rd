\name{clearAll}
\alias{clearAll}
\title{Remove all R Objects From the Global Environment}

\description{
	Generic function to clear all objects from .RData in R
}
\usage{
clearAll(hidden=TRUE, verbose=TRUE, PBSsave=TRUE)
}

\arguments{
	\item{hidden}{if \code{TRUE}, remove variables that start with a dot(.).}
	\item{verbose}{if \code{TRUE}, report all removed items.}
	\item{PBSsave}{if \code{TRUE}, do not remove \code{.PBSmod}.}
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\keyword{methods}
