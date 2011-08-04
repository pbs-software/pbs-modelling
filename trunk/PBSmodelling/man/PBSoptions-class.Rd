\name{PBSoptions-class}
\docType{class}
\alias{PBSoptions-class}
\alias{print,PBSoptions-method}
\alias{show,PBSoptions-method}

\title{Project Options Class}
\description{
	Projects commonly involve various settings or options such as paths to C compilers or other third-party tools.
	\pkg{PBSmodelling} provides a set of option management functions for managing user specific options. Options can be 
	modified through the provided set of functions on the command line, or through a custom GUI.
	These options can be saved to disk for use in subsequent R sessions.
	
	To use \pkg{PBSmodelling}'s suite of option management functions, a \code{PBSoptions} object must be
	created for each of your projects. Each \code{PBSoptions} object contains a distinct R environment where 
	option values are stored; this allows different projects to use overlapping option names without conflicts
	(provided each project has its own \code{PBSoptions} class object).

}
\section{Objects from the Class}{
  Objects can be created by calls of the form \cr
  \code{new("PBSoptions",filename,initial.options=list(),gui.prefix="option")}.
  \describe{
    \item{\code{filename:}}{ default file name to use when saving and loading options to and from disk }
    \item{\code{initial.options:}}{ a list with distinctly named initial options to use if no previously saved file exists }
    \item{\code{gui.prefix:}}{ a prefix used to identify GUI variables which correspond to user options }
  }
}
\section{Slots}{
  \describe{
    \item{\code{instance}:}{The R environment used to store options. Please do not use this directly; use the functions listed under the "see also" section. }
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "PBSoptions")}: prints the list of options }
  }
}
\details{
  When a \code{PBSoptions} object is created with the \code{new} function, the \code{initial.options} list, if supplied, is stored
  as initial user options in the object. The initialization routine then attempts to load user set options from the \code{filename} file. If such a file exists,
  these values are stored in the \code{PBSoptions} object overwriting any initial values as specified by \code{initial.options}
  
  Option values are not directly stored in the object, but rather in an environment stored in the \code{instance} slot.
  Using an environment rather than slots for storing options allows us to pass option object by reference rather than value; that is,
  we can save options in the object without the need of returning a new modified class object. It is therefore necessary that users
  use the functions listed in the "see also" section to effectively manage user options.
}
\author{ Alex Couture-Beil }

\section{Warning }{
Do not use the slots -- use the access functions instead.
}
\seealso{
  \code{\link[PBSmodelling]{getOptions}} for retrieving and modifying user options
  
  \code{\link[PBSmodelling]{getOptionsFileName}} for retrieving and modifying the default options file name
  
  \code{\link[PBSmodelling]{loadOptions}} for loading and saving options from and to disk

  \code{\link[PBSmodelling]{getOptionsPrefix}} for retrieving and modifying the GUI prefix (for custom GUI interfaces)
  
  \code{\link[PBSmodelling]{loadOptionsGUI}} for setting GUI values to reflect user options and vice-versa
}
\examples{
#initialize an option manager with a single logical option
.mypkg <<- new( "PBSoptions", filename="my_pkg.txt", 
   initial.options=list( sillyhatday=FALSE ) )

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
print(getOptions( .mypkg ))
}
\keyword{classes}
\keyword{methods}

