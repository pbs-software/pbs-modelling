\name{showPacks}
\alias{showPacks}
\title{ Show Packages Required But Not Installed }
\description{
  Show the packages specified by the user and compare these to the 
  installed packages on the user's system. Display packages not installed.
}
\usage{
showPacks(packs=c("PBSmodelling","PBSmapping","PBSddesolve",
    "rgl","deSolve","akima","deldir","sp","maptools","KernSmooth"))
}
\arguments{
  \item{packs}{ string vector of package names that are compared to installed packages. }
}
\value{
  Invisibly returns a list of \code{Apacks} (all packages installed on user's system),
  \code{Ipacks} (packages in \code{packs} that are installed), and
  \code{Mpacks} (packages that are missing).
}
\author{
  Jon Schnute, Pacific Biological Station, Nanaimo BC
}
\keyword{ package }
\keyword{ character }
