\name{loadOptions}
\alias{loadOptions}
\alias{saveOptions}
\alias{saveOptionsAs}
\title{
  Save and Load Options to and from Disk
}
\description{
  Save and load options for use from one R session to another. 
  If no file name is given, then the default file name 
  (specified when the option object was created) is used.
}
\usage{
loadOptions(option.object, fname, prompt = FALSE)
saveOptions(option.object, fname, prompt = FALSE)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{fname}{file name to use: if missing the default file name is used; if given, file name becomes the default.}
  \item{prompt}{logical: if \code{TRUE}, prompt the user to select a file from an interactive GUI. 
     If \code{fname} is given, then the value appears as the default selected file.}
}
\details{
  If \code{fname} is given (or selected when \code{prompt=TRUE}), 
  then that file becomes the default file name for subsequent loading and saving.
}
\seealso{
  See \code{\link[PBSmodelling]{PBSoptions-class}} for more details and 
  an example using \pkg{PBSmodelling}'s option management functions.
}
\keyword{methods}

