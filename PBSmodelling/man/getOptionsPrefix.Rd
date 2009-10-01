\name{getOptionsPrefix}
\Rdversion{1.1}
\alias{getOptionsPrefix}
\alias{setOptionsPrefix}

\title{get and set GUI prefix of options class}
\description{
The prefix is used for determining which GUI variables are associated with a user option.
}
\usage{
getOptionsPrefix(option.object)
setOptionsPrefix(option.object, prefix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{option.object}{TODO}
  \item{prefix}{new prefix to use}
}
\value{
  \code{getOptionsPrefix}: a prefix string used to reference GUI variables
}
\examples{
.mypkg <<- new( "option", filename = "my_pkg.txt", initial.options = list( pi = 3.14 ), gui.prefix = "opt" )

#prefix the option "pi" with "opt" to get "optpi"
createWin( "entry name=optpi", astext = TRUE )

#the GUI variable "optpi" will be loaded with the option "pi"
loadOptionsGUI( .mypkg )
}
\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}
