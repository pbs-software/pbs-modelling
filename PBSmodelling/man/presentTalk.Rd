\name{presentTalk}
\alias{presentTalk}
\title{Run an R Presentation}
\description{
Start an R talk from a \emph{talk description file} that launches a control GUI.
}
\usage{
presentTalk(talk)
}
\arguments{
\item{talk}{talk xml file name}
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
  The blue buttons allow movement among sections - \code{Prev} to the previous 
  section, \code{Restart} to the start of the current section, and \code{Next} to 
  the next section. Drop down lists are provided for both indicating the current
  section and slide number and as an additional interface for jumping between different
  sections or slide numbers.
  
  In addition to the automatic menu items, a user can add buttons to the GUI 
  that accomplish similar purposes.
}
\examples{
wd <- getwd() #save current directory
talk_dir <- system.file("examples", package = "PBSmodelling" )
setwd( talk_dir )
presentTalk( "swisstalk.xml" )
	
#restore working directory once talk GUI is closed
setwd( wd )
}
\note{
  See the PBSmodelling User's Guide for more information.
}