\name{promptWriteOptions}
\alias{promptWriteOptions}
\title{Prompt the User to Write Changed Options}

\description{
  If changes have been made to PBS options, this function allows 
  the user to choose whether to write PBS options to an external 
  file that can be loaded later by \code{readPBSoptions}.
}
\usage{
promptWriteOptions(fname="")
}
\arguments{
  \item{fname}{name of file where options will be saved.}
}
\details{
  If there are options that have been changed in the GUI but have not been
  commited to PBSmodelling memory in the global R environment, the user
  is prompted to choose whether or not to commit these options.

  Then, if any PBS options have been changed, the user is prompted to choose
  whether to save these options to the file \code{fname}. (When a new R session is
  started or when a call to \code{readPBSoptions} or \code{writePBSoptions} is made,
  PBS options are considered to be unchanged; when an option is set,
  the options are considered to be changed).

  If \code{fname=""}, the user is prompted to save under the file name last used 
  by a call to \code{readPBSoptions} or \code{writePBSoptions} if available. 
  Otherwise, the default file name "PBSoptions.txt" is used.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{writePBSoptions}}, \code{\link{readPBSoptions}},
  \code{\link{setPBSoptions}}
}
\examples{
\dontrun{
promptWriteOptions() #uses default filename PBSoptions.txt
}
}