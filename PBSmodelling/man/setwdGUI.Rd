\name{setwdGUI}
\alias{setwdGUI}
\title{Browse for Working Directory and Optionally Find Prefix}

\description{
  Allows the user to browse a directory tree to set the working directory.
  Optionally, files with given suffixes can be located in the new directory.
}
\usage{
setwdGUI()
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  createWin( "button text=\"Change working directory\" func=setwdGUI", astext=TRUE )
})
}
}

