\name{dot-talk.funs}
%- Also NEED an '\alias' for EACH other topic documented here.
\alias{.getButton}
\alias{.getButtons}
\alias{.getIndexForSection}
\alias{.getMenus}
\alias{.getSectionNames}
\alias{.getTalkIndexes}
\alias{.nextSlide}
\alias{.parseTalkFile}
\alias{.presentTalkOpenFile}
\alias{.prevSlide}
\alias{.sectiondrop}
\alias{.setsection}
\alias{.slidedrop}
\alias{.startSlide}
\alias{.updateSlide}
\title{
  Dot Talk Functions
}
\description{
  Various dot-talk functions used by PBSmodelling code internally for \code{'presentTalk()'}.
}
\usage{
.getButton(talk_name, obj)
.getButtons(talk)
.getIndexForSection(talk, section_id)
.getMenus(talk)
.getSectionNames(talk)
.getTalkIndexes(talk)
.nextSlide()
.parseTalkFile(talk_fname)
.presentTalkOpenFile()
.prevSlide()
.sectiondrop()
.setsection()
.slidedrop()
.startSlide(talk)
.updateSlide(talk)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{talk_name}{\code{character} -- name of talk (e.g., \code{'Swiss'})}
  \item{obj}{\code{character} -- talk component (e.g., \code{'section'}, \code{'file'})}
  \item{talk}{\code{compound} -- s4 object called \code{'talk'}}
  \item{section_id}{\code{character} -- section identification marker}
  \item{talk_fname}{\code{character} -- name of talk file (e.g., \code{'swisstalk.xml'})}
}
\details{
  Various dot functions used by PBSmodelling code to control the talk GUI for presentations.

  \code{.getButton} -- returns \code{win} description for a button for a file or section.

  \code{.getButtons} -- gets widget descriptions for file and section buttons.

  \code{.getIndexForSection} -- get the slide index which corresponds to the 
    first slide in a given section.

  \code{.getMenus} -- get widget description for menus.

  \code{.getSectionNames} -- given a talk, return a vector of all section names.

  \code{.getTalkIndexes} -- retuns a list of 2-element vectors \code{(i,j)}, 
    where \code{i} is the section index, and \code{j} is the items index;
    each element of the list corresponds to a break point.

  \code{.parseTalkFile} -- returns a talk s4 class,  given a filename to an \code{xml} talk

  \code{.presentTalkOpenFile} -- open files from the \code{win} \code{act}, and supports multiple files.
}
\note{
  The function \code{'presentTalk'} offers a cumbersome system for presenting 
  talking points using R.
  It is likely superceded by more recent appliances like 
  \href{https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html}{Shiny}.
}
\author{
  \href{mailto:alex@mofo.ca}{Alex Couture-Beil}, Software Engineer\cr
  Earthly Technologies, Victoria BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2024-10-31}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
In \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{presentTalk}},
  \code{\link[PBSmodelling]{.tagDefs}},
  \code{\link[PBSmodelling]{.tagOptionChecks}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{programming}
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{M10_presentTalk}
% \concept{ ~cpt2 }
% Use only one concept per line.

