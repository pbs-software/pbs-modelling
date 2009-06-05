\name{loadC}
\alias{loadC}
\title{Launch a GUI for Compiling and Loading C Code}
\description{
  A GUI interface allows users to edit, compile, and embed C functions
  in the R environment. 
}
\usage{loadC()}
\details{
  The function \code{loadC()} launches an interactive GUI that can be used
  to manage the construction of C functions intended to be called from R. The
  GUI provides tools to edit, compile, load, and run C functions in the R
  environment.
  
  The \code{loadC} GUI also includes a tool for comparison between the running times
  and return values of R and C functions. It is assumed that the R and C
  functions are named \code{prefix.r} and \code{prefix.c}, respectively, where 
  \code{prefix} can be any user-chosen prefix. If an initialization function 
  \code{prefix.init} exists, it is called before the start of the comparison.

  \bold{The GUI controls:}
  \tabular{ll}{
  \bold{File Prefix}   \tab Prefix for \code{.c} and \code{.r} files.\cr
  \bold{Lib Prefix}    \tab Prefix for shared library object.\cr
  \bold{Set WD}        \tab Set the working directory.\cr
  \bold{Open Log}      \tab Open the log file.\cr
  \bold{Open.c File}   \tab Open the file \code{prefix.c} from the working directory.\cr
  \bold{Open .r File}  \tab Open the file \code{prefix.r} from the working directory.\cr
  \bold{COMPILE}       \tab Compile \code{prefix.c} into a shared library object.\cr
  \bold{LOAD}          \tab Load the shared library object.\cr
  \bold{SOURCE R}      \tab Source the file \code{prefix.r}.\cr
  \bold{UNLOAD}        \tab Unload the shared library object.\cr
  \bold{Options}       \tab \cr
  \bold{Editor}        \tab Text editor to use.\cr
  \bold{Update}        \tab Commit option changes.\cr
  \bold{Browse}        \tab Browse for a text editor.\cr
  \bold{Clean Options} \tab \cr
  \bold{Select All}    \tab Select all check boxes specifying file types.\cr
  \bold{Select None}   \tab Select none of the check boxes.\cr
  \bold{Clean Proj}    \tab Clean the project of selected file types.\cr
  \bold{Clean All}     \tab Clean the directory of selected file types.\cr
  \bold{Comparison}    \tab \cr
  \bold{Times to Run}  \tab Number of times to run the R and C functions.\cr
  \bold{RUN}           \tab Run the comparison between R and C functions.\cr
  \bold{R Time}        \tab Computing time to run the R function multiple times.\cr
  \bold{C Time}        \tab Computing time to run the C function multiple times.\cr
  \bold{Ratio}         \tab Ratio of R/C run times.\cr
  }
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{compileC}}
}
\keyword{programming}
\keyword{interface}
