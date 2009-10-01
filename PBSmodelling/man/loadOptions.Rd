\name{loadOptions}
\Rdversion{1.1}
\alias{loadOptions}
\alias{saveOptions}
\alias{saveOptionsAs}

\title{save and load options to and from disk}
\description{
Save and load options for use between subsequent R sessions. If no filename is given, then the default filename specified when
the option class object was created is used.
}
\usage{
loadOptions(option.object, fname)
saveOptions(option.object, fname)
saveOptionsAs(option.object)
}
\details{
\code{saveOptionsAs} prompts the user to select a filename via a GUI. The selected file becomes the default file for future save/load calls
}

\arguments{
  \item{option.object}{TODO}
  \item{fname}{filename to use - if missing the default filename is used - if given, filename becomes the default}
}
\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}