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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{option.object}{object of options class}
  \item{\dots}{any key=value pairs to set}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
