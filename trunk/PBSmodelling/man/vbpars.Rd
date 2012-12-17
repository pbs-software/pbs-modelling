\name{vbpars}
\docType{data}
\alias{vbpars}
\title{Data: Initial Parameters for a von Bertalanffy Curve}

\description{
  Starting parameter values for \code{Linf}, \code{K}, and \code{t0} for 
  von Bertalanffy minimization using length-at-age data (\code{\link{vbdata}}) 
  for freshwater mussels (\emph{Anodonta kennerlyi}).
}
\usage{data(vbpars)}

\format{
  A matrix with 3 rows and 3 columns \code{c("Linf","K","t0")}. Each row contains 
  the starting values, minima, and maxima, respectively, for the three parameters.
}
\details{
  Data for demonstration of the von Bertalanffy model used in 
  the \code{\link{calcMin}} example.
}
\references{
  Mittertreiner, A. and Schnute, J. (1985) Simplex: a manual and software package 
  for easy nonlinear parameter estimation and interpretation in fishery research. 
  \emph{Canadian Technical Report of Fisheries and Aquatic Sciences} \bold{1384}, xi + 90 pp. 
}
\keyword{datasets}
