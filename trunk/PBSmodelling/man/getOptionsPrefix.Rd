\name{getOptionsPrefix}
\alias{getOptionsPrefix}
\alias{setOptionsPrefix}
\title{
  Get and Set GUI Prefix of Options Class
}
\description{
  The GUI prefix is used for determining which GUI variables are associated with a user option.
}
\usage{
getOptionsPrefix(option.object)
setOptionsPrefix(option.object, prefix)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{prefix}{new prefix to use}
}
\value{
  \code{getOptionsPrefix}: a prefix string used to reference GUI variables
}
\examples{
.mypkg <<- new( "PBSoptions", filename="my_pkg.txt", 
   initial.options=list(pi=3.14), gui.prefix="opt" )

#prefix the option "pi" with "opt" to get "optpi"
createWin( "entry name=optpi", astext = TRUE )

#the GUI variable "optpi" will be loaded with the option "pi"
loadOptionsGUI( .mypkg )
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example using \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}

