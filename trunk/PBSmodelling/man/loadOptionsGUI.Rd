\name{loadOptionsGUI}
\Rdversion{1.1}
\alias{loadOptionsGUI}
\alias{saveOptionsGUI}

\title{load and save options values to and from a GUI}
\description{
These functions are used to move option values to and from a GUI. Option values are stored with an R object (as referenced by the option.object).
\code{loadOptionsGUI} copies the values from the R object to the GUI.
\code{SaveOptionsGUI} copies the GUI values from the tcltk GUI to the R object.
}
\usage{
loadOptionsGUI(option.object)
saveOptionsGUI(option.object)
}

\arguments{
  \item{option.object}{options class object used for storing package options}
}
\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}