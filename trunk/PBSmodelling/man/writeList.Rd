\name{writeList}
\alias{writeList}
\title{Write a List to a File in PBS Modelling Format}

\description{
  Write an ASCII text representation in either \code{"D"} format or \code{"P"} format. 
  The \code{"D"} format makes use of \code{dput} and \code{dget} and produces an R 
  representation of the list. The \code{"P"} format represents a simple list in an 
  easy-to-read, ad hoc \code{PBSmodelling} format.
}
\usage{
writeList(x, fname, format="D", comments="")
}
\arguments{
  \item{x}{R list object to write to an ASCII text file.}
  \item{fname}{file name of the text file to create.}
  \item{format}{format of the file to create: \code{"D"} or \code{"P"}.}
  \item{comments}{vector of character strings to use as initial-line comments in the file.}
}
\details{
  \subsection{The D Format}{
    The \code{"D"} format is equivalent to using R's \code{base} functions 
    \code{dput} and \code{dget}, which support all R objects.
  }

  \subsection{The P Format}{    
    The \code{"P"} format only supports a list that may contain lists,
    vectors, matrices, arrays, and data frames.  Scalars are treated
    like vectors. It writes each list element using the following
    conventions:
    \enumerate{
      \item \code{$} denotes the start of a list element, and the element's
      name follows this character; for nested lists, \code{$} separates
      each nesting level;
      \item \code{$$}, on the next line, denotes a line used to describe
      the element's structure, which includes object type, mode(s), names
      (if vector), rownames (if matrix or data), and colnames (if matrix
      or data); and
      \item subsequent lines contain data (one line for a vector and
      multiple lines for a matrix or other data).
    }
    If a list's elements are unnamed, have the name NA, or have the
    empty string as a name, this function generates names (\code{"P"}
    format only). If two list elements share the same name, the list
    will export correctly, but it will import incorrectly.
    
    Arrays with three or more dimensions have \code{dim} and
    \code{dimnames} arguments. \code{dim} describes the dimension of the
    data (a vector as returned by \code{dim(some_array)}) and
    \code{dimnames} is a vector of length \code{sum(dim(some_array)+1)}
    and is constructed as follows:

    \code{
      foreach dimension d,
        first append the name of the dimension d and
        then append all labels within that dimension
    }
    
    Multiple rows of data for matrices or data frames must have equal 
    numbers of entries (separated by whitespace).
    
    Note that array data are written the same format as they are
    displayed in the R console: \cr
    \code{nrow=dim()[1]}, \code{ncol=dim()[2]} \cr
    repeated by scrolling through successively higher dimensions, increasing the 
    index from left to right within each dimension. The flattened table will have 
    \code{dim()[2]} columns.
  }

  For complete details, see \dQuote{PBSmodelling-UG.pdf} at the location
  described when loading this package.
}
\value{
  String containing the file name.
}
\author{
  Alex Couture-Beil, Vancouver Island University, Nanaimo BC
}
\seealso{
  \code{\link{packList}}, \code{\link{readList}}, \code{\link{unpackList}}
}
\examples{
\dontrun{
local(envir=.PBSmodEnv,expr={
  cwd = getwd(); setwd(tempdir())
  test <- list(a=10,b=euro,c=view(WorldPhones),d=view(USArrests))
  writeList(test,"test.txt",format="P",
    comments=" Scalar, Vector, Matrix, Data Frame")
  openFile("test.txt")
  setwd(cwd)
})
}
local(envir=.PBSmodEnv,expr={
  cwd = getwd(); setwd(tempdir())
  ##Example of dimnames for Arrays
  dimnames(Titanic)
  writeList( list( Titanic ), format="P")
  setwd(cwd)
})
}
\keyword{file}
\keyword{list}
