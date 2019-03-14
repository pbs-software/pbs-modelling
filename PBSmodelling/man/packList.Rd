\name{packList}
\alias{packList}
\title{ Pack a List with Objects }
\description{
  Pack a list with existing objects using names only.
}
\usage{
packList(stuff, target="PBSlist", value, penv=NULL, tenv=.PBSmodEnv)
}
\arguments{
  \item{stuff}{\code{character} -- vector of object names}
  \item{target}{\code{character} -- name of target list object}
  \item{value}{\code{numeric|character} -- an optional explicit value to assign to \code{stuff}}
  \item{penv}{\code{environment} -- source environment (default=parent) or user-specified environment where \code{stuff} resides}
  \item{tenv}{\code{environment} -- target environment where target list exists or will be located}
}
\details{
  A list object called \code{target} will be located in the 
  \code{tenv} environment. The objects named in \code{stuff} and 
  located in the \code{penv} environment will appear as named 
  components within the list object \code{target}.
  
  If an explicit \code{value} is specified, the function uses this value
  instead of looking for local objects. Essentially, \code{stuff = value}
  which is then packed into \code{target}.
}
\note{
  The function determines the parent environment from within. 
  This environment contains the objects from which the function copies to the target environment.
  Alternatively, the user can specify the environment where \code{stuff} resides.
}
\value{
  No value is returned
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2019-03-12}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{unpackList}},
  \code{\link[PBSmodelling]{readList}},
  \code{\link[PBSmodelling]{writeList}} \cr
Accessor functions:\cr
  \code{\link[PBSmodelling]{tget}},
  \code{tcall},
  \code{tprint}, and
  \code{tput}
}
\examples{
local(envir=.PBSmodEnv,expr={
  fn = function() {
    alpha=rnorm(10)
    beta=letters
    gamma=mean
    delta=longley
    packList(c("alpha","beta","gamma","delta")) }
  fn(); tprint(PBSlist)
})
}
\keyword{list}
\keyword{file}
