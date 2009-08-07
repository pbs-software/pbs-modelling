\name{compileC}
\alias{compileC}
\title{Compile a C File into a Shared Library Object}

\description{
  This function provides an alternative to using R's \code{SHLIB} 
  command to compile C code into a shared library object.
}
\usage{
compileC(file, lib="", options="", logWindow=TRUE, logFile=TRUE)
}
\arguments{
  \item{file}{name of the file to compile.}
  \item{lib}{name of shared library object (without extension).}
  \item{options}{linker options (in one string) to prepend to a compilation command.}
  \item{logWindow}{if \code{TRUE}, a log window containing the compiler output will be displayed.}
  \item{logFile}{if \code{TRUE}, a log file containing the compiler output will be created.}
}
\details{
  If \code{lib=""}, it will take the same name as \code{file} (with a different extension).

  If an object with the same name has already been dynamically loaded in R, 
  it will be unloaded automatically for recompilation.

  The name of the log file, if created, uses the string value from \code{lib} 
  concatenated with \code{".log"}.
}
\author{
  Anisa Egeli, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{loadC}}
}
\examples{
\dontrun{
compileC("myFile.c", lib="myLib", options="myObj.o")
}
}
\keyword{programming}
\keyword{interface}