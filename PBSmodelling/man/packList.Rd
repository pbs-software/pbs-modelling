\name{packList}
\alias{packList}
\title{ Pack a List with Objects }
\description{
  Pack a list with existing objects using names only.
}
\usage{
packList(stuff, target="PBSlist", value, tenv=.PBSmodEnv)
}
\arguments{
  \item{stuff}{ string vector of object names }
  \item{target}{ target list object }
  \item{value}{ an optional explicit value to assign to \code{stuff} }
  \item{tenv}{ target environment where target list is or will be located }
}
\details{
  A list object called \code{target} will be located in the 
  \code{tenv} environment. The objects named in \code{stuff} and 
  located in the \code{lenv} environment will appear as named 
  components within the list object \code{target}.
  
  If an explicit \code{value} is specified, the function uses this value
  instead of looking for local objects. Essentially, \code{stuff=value}
  which is then packed into \code{target}.
}
\note{
  The function determines the parent environment from within. 
  This environment contains the objects from which the function copies to the target environment.
}
\value{
  No value is returned
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link{unpackList}}, \code{\link{readList}}, \code{\link{writeList}}
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
