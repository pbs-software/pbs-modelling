\name{presentTalk}
\alias{presentTalk}
\title{Run an R Presentation}
\description{
Start an R talk from a \emph{talk description file} that launches a control GUI.
}
\usage{
presentTalk(x, debug=FALSE)
}
\arguments{
\item{x}{string name of \emph{talk description file}.}
\item{debug}{logical: if \code{TRUE}, the command line reflects indices and some booleans.}
}
\details{
  \code{presentTalk} is a tool that facilitates lectures and workshops in R.
  The function allows the presenter to show code snippets alongside their 
  execution, making use of R's graphical capabilities. 
  When \code{presentTalk} is called, a graphical user interface (GUI) is 
  launched that allows the user to control the flow of the talk (e.g., 
  switching between talks or skipping to various sections of a talk.
  
  The automatic control buttons allow the user
  to move forward or backward in the talk. The \code{GO} button moves forward 
  one tag segment, the \code{Back} button moves back to the previous tag segment.
  The blue buttons allow movement among sections - \code{Start} to the first 
  section of the talk, \code{Prev} to the previous section, \code{Curr} to the 
  start of the current section, and \code{Next} to the next section.
  
  In addition to the automatic menu items, a user can add buttons to the GUI 
  that accomplish similar purposes.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\note{
  The use of \code{chunk} in the R code is equivalent to the use of 
  \code{segment} in the documentation.\cr
  See the PBSmodelling User's Guide for more information.
}