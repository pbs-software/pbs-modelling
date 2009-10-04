\name{getOptions}
\Rdversion{1.1}
\alias{getOptions}
\alias{setOptions}

\title{get and set user options}
\description{
functions to set and get user options within an option class object
}
\usage{
getOptions(option.object, key)
setOptions(option.object, ...)
}
\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{\dots}{any number of user options to set, where the named argument is the option key and the value is the option value}
  \item{key}{name of option to retrieve - if missing all options are returned}
}

\value{
  \code{getOptions}: returns the value of the option specified by \code{key}. If \code{key} is missing, a list of all options.
}
\author{
  Alex Couture-Beil
}

\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}
