\name{clipVector}
\alias{clipVector}
\title{Clip a Vector at One or Both Ends}
\description{
  Clip a vector at one or both ends using the specified clip 
  pattern to match.
}
\usage{
clipVector(vec, clip, end=0)
}
\arguments{
  \item{vec}{vector object to clip}
  \item{clip}{value or string specifying repeated values to clip from ends}
  \item{end}{end to clip \code{clip} from: 0=both, 1=front, 2=back}
}
\details{
  If the vector is named, the names are retained. Otherwise,
  element positions are assigned as the vector's names.
}
\value{
  Clipped vector with names.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{ 
  \code{\link{createVector}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  x=c(0,0,0,0,1,1,1,1,0,0)
  print(clipVector(x,0))

  x=c(TRUE,TRUE,FALSE,TRUE)
  print(clipVector(x,TRUE))

  x=c("red","tide","red","red")
  print(clipVector(x,"red",2))
})
}

\keyword{ data }
\keyword{ utilities }
