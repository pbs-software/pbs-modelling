\name{getOptionsFileName}
\Rdversion{1.1}
\alias{getOptionsFileName}
\alias{setOptionsFileName}

\title{get and set filename used for saving and loading of options}
\description{
  functions for retrieving and setting the default filename used by \code{loadOptions} and \code{saveOptions}
}
\usage{
getOptionsFileName(option.object)
setOptionsFileName(option.object, name)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{name}{new name for default filename}
}
\value{
  \code{getOptionsFileName}: the default filename
}
\seealso{
  \code{\link{loadOptions}}
  \code{\link{saveOptions}}
  
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}