\name{loadOptionsGUI}
\Rdversion{1.1}
\alias{loadOptionsGUI}
\alias{saveOptionsGUI}

\title{load and save options values to and from a GUI}
\description{
these functions are used to move option values to and from a GUI. Option values are stored with an R object (as referenced by the option.object).
loadOptionsGUI copies the values from the R object to the GUI.
SaveOptionsGUI copies the GUI values from the tcltk GUI to the R object.
}
\usage{
loadOptionsGUI(option.object)
saveOptionsGUI(option.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{option.object}{TODO}
}
\seealso{
  see \code{\linkS4class{option}} for an example of using PBSmodelling's option managment functions
}