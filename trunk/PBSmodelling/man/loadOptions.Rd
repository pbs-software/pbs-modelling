\name{loadOptions}
\Rdversion{1.1}
\alias{loadOptions}
\alias{saveOptions}
\alias{saveOptionsAs}

\title{save and load options to and from disk}
\description{
Save and load options for use between subsequent R sessions. If no filename is given, then the default filename (specified when
the option object was created) is used.
}
\usage{
loadOptions(option.object, fname, prompt = FALSE)
saveOptions(option.object, fname, prompt = FALSE)
}
\details{
if fname is given (or selected when \code{prompt=TRUE}, then that file becomes the default filename for subsequent loading and saving.
}

\arguments{
  \item{option.object}{options class object used for storing package options}
  \item{fname}{filename to use - if missing the default filename is used - if given, filename becomes the default}
  \item{prompt}{if true, prompt the user to select a file from an interactive GUI. If fname is given, then the value appears as the default selected file.}
}
\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}