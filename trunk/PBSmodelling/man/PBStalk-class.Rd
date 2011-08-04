\name{talk-class}
\docType{class}
\alias{talk-class}
\alias{section-class}
\alias{text-class}
\alias{file-class}
\alias{code-class}
\alias{break-class}
\title{
  Present Talk Classes
}
\description{
  The function \code{presentTalk} is a tool that facilitates lectures and workshops in R.
  It allows the presenter to show code snippets alongside their 
  execution, making use of R's graphical capabilities. 

  For \code{presentTalk} to work, six S4 class objects are created:
  
  \code{talk......}{root element that constitutes a talk;} \cr
  \code{section...}{branch element that defines a section within a talk;} \cr
  \code{text......}{leaf element that specifies text to be printed on the R console;} \cr
  \code{file......}{leaf element that specifies files to be opened by the OS;} \cr
  \code{code......}{leaf element that specifies R code to be executed;} \cr
  \code{break.....}{leaf element that specifies where to allow a break in the talk.}

  The leaf elements, also termed \emph{primitive} elements, occur in isolation and cannot
  contain other elements. Therefore, only two levels of nesting are supported: sections 
  within a talk and primitives within a section. \cr
  See Appendix B in the \pkg{PBSmodelling} User's Guide for more information.
}
\details{
  This function uses a convenience function called \code{xmlGetAttr}
  (from the package \pkg{XML}) that retrieves the value of a named attribute in an XML node.
  
  The function \code{presentTalk} translates the XML code into a list structure 
  called \code{.presentTalk} below the global object \code{.PBSmod}. The GUI is 
  represented as a list structure called \code{presentwin} under \code{.PBSmod}, 
  as for all GUI objects in \pkg{PBSmodelling}.
}
\section{Slots Available}{
  \tabular{lll}{
    \bold{talk} \tab \tab \cr
    \code{name}       \tab \code{character} \tab string giving the name of the talk (required) \cr
    \code{sections}   \tab \code{list}      \tab list of sections within the talk \cr
    \code{files}      \tab \code{list}      \tab list of files within the talk \cr
    \bold{section}    \tab \tab \cr
    \code{name}       \tab \code{character} \tab string giving the name of the section (required) \cr
    \code{items}      \tab \code{list}      \tab list of the four primitive (leaf-element) S4 classes \cr
    \code{button}     \tab \code{logical}   \tab should GUI have a button that selects section? \cr
    \code{col}        \tab \code{integer}   \tab column in lower section of GUI to place button \cr
    \code{section_id} \tab \code{integer}   \tab specify if section does not immediately follow a talk \cr
    \bold{text}       \tab \tab \cr
    \code{text}       \tab \code{character} \tab text to display on the R console \cr
    \code{"break"}    \tab \code{logical}   \tab break the presentation after displaying the text specified? \cr
    \bold{file}       \tab \tab \cr
    \code{name}       \tab \code{character} \tab string giving the name in the GUI for a group of files to open (required) \cr
    \code{filename}   \tab \code{character} \tab individual file names associated with the group name in the GUI \cr
    \code{"break"}    \tab \code{logical}   \tab break the presentation after opening the group of files? \cr
    \code{button}     \tab \code{logical}   \tab should GUI add a button that opens this group of files? \cr
    \code{col}        \tab \code{integer}   \tab column in lower section of GUI to place button \cr
    \bold{code}       \tab \tab \cr
    \code{show}       \tab \code{logical}   \tab show the code snippet in the R console? \cr
    \code{print}      \tab \code{logical}   \tab print the results of running the R code? \cr
    \code{code}       \tab \code{character} \tab the actual chunk of R code \cr
    \code{"break"}    \tab \code{character} \tab string describing where to introduce breaks in the code segment \cr
    \code{eval}       \tab \code{logical}   \tab evaluate the R code? \cr
    \bold{break}      \tab \tab \cr
    \code{.xData}     \tab \code{NULL}      \tab allows a break in the talk for user interaction on the R console.
  }
}
\section{Creating S4 Objects}{
  Objects can be created by calls of the form:
  \preformatted{
new("talk", name=name)
new("section",
  name     = node$attributes["name"],
  button   = as.logical(xmlGetAttr(node,"button",FALSE)),
  col      = as.integer(xmlGetAttr(node,"col",2)))
new("text", 
  text     = xmlValue(node),
  "break"  = as.logical(xmlGetAttr(node,"break",TRUE)))
new("file",
  name     = xmlGetAttr(node,"name",""), 
  "break"  = as.logical(xmlGetAttr(node,"break",TRUE)),
  filename = xmlValue(node),
  button   = as.logical(xmlGetAttr(node,"button",FALSE)),
  col      = as.integer(xmlGetAttr(node,"col",3)))
new("code",
  show     = as.logical(xmlGetAttr(node,"show",TRUE)), 
  print    = as.logical(xmlGetAttr(node,"print",TRUE)), 
  code     = xmlValue(node), 
  "break"  = tolower(xmlGetAttr(node,"break","print")))
new("break") }
}
\author{ Alex Couture-Beil }
\seealso{
  \code{\link[PBSmodelling]{presentTalk}} for presenting a talk in R.
  
  \code{\link[XML]{xmlGetAttr}} for retrieving the value of a named attribute in an XML node.
  
  \code{\link[methods]{setClass}} for creating a class definition.
  
  \code{\link[PBSmodelling]{PBSoptions-class}} for a complicated S4 class implementation.
}
\keyword{classes}

