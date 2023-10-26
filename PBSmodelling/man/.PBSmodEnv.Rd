\name{.PBSmodEnv}
\alias{.PBSmodEnv}
\docType{data}
\title{
  PBSmodelling Environment
}
\description{
  An environment set aside for PBSmodelling.
}
\usage{
is.environment(.PBSmodEnv)
env.profile(.PBSmodEnv)

lisp(.PBSmodEnv)
something = tcall(.PBSmodEnv)$.PBSmod
}
\details{
  The environment is created in \code{'zzz.r'} and can be used by
  \code{PBSmodelling} functions \code{'lisp'}, \code{'tget'}, 
  \code{'tput'}, \code{'tprint'}, and \code{'tcall'}.
}
\examples{
lisp(.PBSmodEnv)
}
\keyword{environment}

