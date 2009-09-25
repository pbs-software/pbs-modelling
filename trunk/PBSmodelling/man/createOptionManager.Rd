\name{createOptionManager}
\alias{createOptionManager}
\title{Project Option Manager}
\description{
	Projects commonly involve various settings or options such as paths to C compilers or other third-party tools.
	PBSmodelling provides an option manager for managing user specific options which can be saved to a local file
	and later loaded for subsequent R sessions.
	
	R provides similar functionality with options(); however, saving and loading to and from disk does not exists, furthermore  
	all options are saved to a single scope (namespace) where collisions between multiple project options may occur.
	For example two projects may both contain an "output_dir" option; with different means to each package. Therefore
	two different values must be saved, each in separate locations to avoid collisions.
	
	PBSmodelling's option manager implements collision free storage of options by utilizing separate R environments;
	these environments are created and initialized with the function createOptionManager. Distinct projects should 
	independently create their own instance of an option manager, and store these values in an appropriate location.
	Functions for manipulating, storing, and retrieving user options exist within each returned instance and 
	are described in the details section.
	
	The option manager is capable of integrating into a GUI whereby user options can be modified and saved in 
	the option manager via a familiar user interface.
	
	Please refer to the examples section for a quick introduction and overview of creating a new option manager.
	
}
\usage{
createOptionManager( filename, initial_options = list(),
	gui_prefix = "option" )
}
\arguments{
	\item{filename}{the name used for saving and loading options to/from disk}
	\item{initial_options}{a list with distinctly named initial options to use if no file exists to load from}
	\item{gui_prefix}{a prefix used to identify GUI variables which directly correspond to user options}
}
\value{
	A list of methods used to retreive and manipulate options within the option manager's private R environment.
	createOptionManager should be called once at initialization and the returned list should be stored as a 
	global variable.
}
\details{
	The list returned by createOptionManager should be stored as a global variable (or in some other non-locked environment).
	This list contains the following functions:



  \describe{
    \item{\code{load( fname )}}{
    	\code{fname}: (optional) file to load settings from; 
    	if missing the value from \code{getFileName()} is used
    	
    	Load user options from disk. 
    	If \code{fname} is given, the value is saved as the default filename.
    }
    
    \item{\code{setFileName( fname )}}{
    	\code{fname}: file to use for saving and loading
    	
    	Set the default filename used for saving and loading.
    }
    \item{\code{getFileName()}}{
    	Returns the default filename used for saving and loading.
    }
    \item{\code{saveas()}}{
    	Prompt the user with a GUI to select a file for saving options to.
    	The selected file becomes the default filename used for saving and loading.
    }
    \item{\code{save( fname )}}{
    	\code{fname}: (optional) file to save settings to; 
    	if missing the value from \code{getFileName()} is used
    	
    	Save user options to disk. 
    	If \code{fname} is given, the value is saved as the default filename.
    }
    \item{\code{get( key )}}{
    	\code{key}: (optional) specific option to retrieve.
    	
    	Returns the specified option value; if key is missing, all options are returned
    }
    \item{\code{set( ... )}}{
    	\code{...}: any number of named options keys and values to store
    	
    	Saves the option key-value pair in the option manager.
    }
    \item{\code{savegui()}}{
    	If a GUI is active, all GUI variables with names prefixed with \code{gui_prefix}
    	are stored in the option manager
    }
    \item{\code{loadgui()}}{
    	If a GUI is active, all GUI variables with names prefixed with \code{gui_prefix}
    	are updated with the values stored in the option manager.
    	
    }
    \item{\code{getenv()}}{
    	Returns the internal environment where these functions and private options data
    	are stored. This is useful for calling these functions directly from a GUI 
    	without accessing the functions via the list returned by \code{createOptionManager}.
    	
    	\emph{Use with caution - avoid modifying options directly}
    }
  }
}
\author{
  Alex Couture-Beil
}
\examples{
#initialize an option manager with a single logical option
.mypkg <<- createOptionManager( "my_pkg.txt", initial_options = list( sillyhatday = FALSE ) )

#retrieving an option
silly <- .mypkg$get( "sillyhatday" )
cat( "today is", ifelse( silly, "silly hat day!", "monday" ), "\n" )

#set an option
.mypkg$set( sillyhatday = TRUE, photos = "/shares/silly_hat_photos" )

#create a GUI which works with options
createWin( c( 
	"check name=optionsillyhatday text=\"silly hat day\"",
	"entry name=optionphotos mode=character label=\"photos directory\"" ), astext = TRUE )

#update GUI values based on values stored in .mypkg's options
.mypkg$loadgui()
}
\keyword{methods}
