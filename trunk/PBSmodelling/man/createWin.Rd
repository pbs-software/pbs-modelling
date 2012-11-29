\name{createWin}
\alias{createWin}
\title{Create a GUI Window}

\description{
  Create a GUI window with widgets using instructions from a 
  Window Description (markup) File.
}

\usage{
  createWin( fname, astext=FALSE, env=sys.frame(sys.nframe()) )
}

\arguments{
  \item{fname}{name of \emph{window description file} 
    or list returned from \code{parseWinFile}.}
  \item{astext}{logical: if \code{TRUE}, interpret \code{fname} 
    as a vector of strings with each element representing a line 
    in a \emph{window description file}.}
  \item{env}{an environment in which to evaluate widget callback functions; see example.}
}

\details{
  Generally, the markup file contains a single widget per line. However, widgets 
  can span multiple lines by including a backslash ('\\') character at the end of 
  a line, prompting the suppression of the newline character.

  For more details on widget types and markup file, see \dQuote{PBSModelling-UG.pdf} 
  in the R directory \cr \code{.../library/PBSmodelling/doc}.

  It is possible to use a Window Description List produced by 
  \code{compileDescription} rather than a file name for \code{fname}.

  Another alternative is to pass a vector of characters to \code{fname} and set 
  \code{astext=T}. This vector represents the file contents where each element 
  is equivalent to a new line in the \emph{window description file}.
}
\author{
  Alex Couture-Beil, Malaspina University-College, Nanaimo BC
}
\note{
  Microsoft Windows users may experience difficulties switching focus between the 
  R console and GUI windows. The latter frequently disappear from the screen and 
  need to be reselected (either clicking on the task bar or pressing <Alt><Tab>. 
  This issue can be resolved by switching from MDI to SDI mode. From the R console 
  menu bar, select <Edit> and <GUI preferences>, then change the value of 
  \dQuote{single or multiple windows} to SDI.
}

\seealso{
  \code{\link{parseWinFile}}, \code{\link{getWinVal}}, \code{\link{setWinVal}}

  \code{\link{closeWin}}, \code{\link{compileDescription}}, \code{\link{createVector}}

  \code{\link{initHistory}} for an example of using \code{astext=TRUE}
  
  \code{\link{environment}}
}

\examples{
\dontrun{
# See file .../library/PBSmodelling/testWidgets/LissWin.txt
# Calculate and draw the Lissajous figure
drawLiss <- function() {
   getWinVal(scope="L"); ti=2*pi*(0:k)/k;
   x=sin(2*pi*m*ti);     y=sin(2*pi*(n*ti+phi));
   plot(x,y,type=ptype); invisible(NULL); };
createWin(system.file("testWidgets/LissWin.txt",package="PBSmodelling"));
}

############################################################
# Environment example:
# function in global
hello <- function()
{
  stop( "I shouldn't be called" )
}
newNameGreeter <- function( name )
{
  # method to display window
  greet <- function()
  {
    createWin(c("button \"Say hello\" func=hello"), astext=TRUE, env=parent.env(environment()))
  }
  # hello method will refer to the name in this local scope
  hello <- function()
  {
    cat( "Hello", name, "\n" )
  }
  # return functions which the user can call directly
  return( list( greet=greet, hello=hello ) )
}
alex <- newNameGreeter( "Alex" )
jon  <- newNameGreeter( "Jon" )

alex$hello() # prints hello Alex
jon$hello()  # prints hello Jon
alex$greet() # creates a GUI with a button, which will print "hello Alex" when pushed
}
\keyword{utilities}

