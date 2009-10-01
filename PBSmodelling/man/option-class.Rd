\name{option-class}
\Rdversion{1.1}
\docType{class}
\alias{option-class}
\alias{print,option-method}

\title{Class "option" ~~~ }
\description{
	Projects commonly involve various settings or options such as paths to C compilers or other third-party tools.
	PBSmodelling provides a set of option management functions for managing user specific options. Options can be 
	modified through the provided set of functions on the command line, or through a custom made GUI.
	These options can be saved to disk for use in subsequent R sessions.
	
	To use PBSmodelling's suite of option manangement functions, an instance of an \code{option} class must be
	created for each of your projects. Each \code{option} instance contains a distinct R environment where 
	option values are stored; this allows different projects to use overlapping option names without conflicts
	(provided each project has its own \code{option} instance).

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("option", filename, initial.options = list(), gui.prefix = "option")}.
  \describe{
    \item{\code{filename:}}{ default filename to use when saving and loading options to and from disk }
    \item{\code{initial.options:}}{ a list with distinctly named initial options to use if no previously saved file exists }
    \item{\code{gui.prefix:}}{ a prefix used to identify GUI variables which correspond to user options }
  }
}
\section{Slots}{
  \describe{
    \item{\code{instance}:}{The R environment used to store options. Please do not use this directly; use functions listed under the "see also" section. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "option")}: prints the list of options }
  }
}
\author{ Alex Couture-Beil }

\section{Warning }{
Do not use the slots - use our functions instead. TODO explain why breifly
}
\seealso{
  \code{\link{getOptions}}
  \code{\link{getOptionsFileName}}
  \code{\link{getOptionsPrefix}}
  \code{\link{loadOptions}}
  \code{\link{loadOptionsGUI}}
}
\examples{
#initialize an option manager with a single logical option
.mypkg <<- new( "option", filename = "my_pkg.txt", initial.options = list( sillyhatday = FALSE ) )

#retrieving an option
silly <- getOptions( .mypkg, "sillyhatday" )
cat( "today is", ifelse( silly, "silly hat day!", "monday" ), "\n" )

#set an option
setOptions( .mypkg, sillyhatday = TRUE, photos = "/shares/silly_hat_photos" )

#create a GUI which works with options
createWin( c( 
	"check name=optionsillyhatday text=\"silly hat day\"",
	"entry name=optionphotos mode=character label=\"photos directory\"",
	"button func=doAction text=save action=saveOptionsGUI(.mypkg)" ), astext = TRUE )

#update GUI values based on values stored in .mypkg's options
loadOptionsGUI( .mypkg )
}
\keyword{classes}
