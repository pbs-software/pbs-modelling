\name{plotBubbles}
\alias{plotBubbles}
\title{Construct a Bubble Plot from a Matrix}
\description{
  Construct a bubble plot for a matrix \code{z}.
}
\usage{
plotBubbles(z, xval=FALSE, yval=FALSE, dnam=FALSE, rpro=FALSE, 
   cpro=FALSE, rres=FALSE, cres=FALSE, powr=0.5, size=0.2, lwd=1,
   clrs=c("black","red","blue"), hide0=FALSE, frange=0.05, prettyaxis=FALSE, ...) 
}
\arguments{
  \item{z}{\code{numeric} -- input matrix, array (2 dimensions) or data frame.}
  \item{xval}{\code{numeric} -- x-values and/or labels for the columns of \code{z}. 
    if \code{xval=TRUE}, the first row contains x-values for the columns.}
  \item{yval}{\code{numeric} -- y-values and/or labels for the rows of \code{z}. 
    If \code{yval=TRUE}, the first column contains y-values for the rows.}
  \item{dnam}{\code{logical} -- if \code{TRUE}, attempt to use \code{dimnames} of input
    matrix \code{z} as \code{xval} and \code{yval}. The \code{dimnames} are 
    converted to numeric values and must be strictly increasing or decreasing. 
    If successful, these values will overwrite previously specified values
    of \code{xval} and \code{yval} or any default indices.}
  \item{rpro}{\code{logical} -- if \code{TRUE}, convert rows to proportions.}
  \item{cpro}{\code{logical} -- if \code{TRUE}, convert columns to proportions.}
  \item{rres}{\code{logical} -- if \code{TRUE}, use row residuals (subtract row means).}
  \item{cres}{\code{logical} -- if \code{TRUE}, use column residuals (subtract column means).}
  \item{powr}{\code{numeric} -- power transform; radii are proportional to \code{z^powr}. 
    Note: \code{powr=0.5} yields bubble areas proportional to \code{z}.}
  \item{size}{\code{numeric} -- size (inches) of the largest bubble.}
  \item{lwd}{\code{numeric} -- line width for drawing circles.}
  \item{clrs}{\code{character} -- colours (3-element vector) used for positive, negative, 
    and zero values, respectively.}
  \item{hide0}{\code{logical} -- if \code{TRUE}, hide zero-value bubbles.}
  \item{frange}{\code{numeric} -- number specifying the fraction by which the range of the axes should be extended.}
  \item{prettyaxis}{\code{logical} -- if \code{TRUE}, apply the pretty function to both axes.}
  \item{...}{\code{dots} -- additional arguments for plotting functions.}
}
\details{
  The function \code{plotBubbles} essentially flips the \code{z} matrix 
  visually. The columns of \code{z} become the x-values while the rows of
  \code{z} become the y-values, where the first row is displayed as the
  bottom y-value and the last row is displayed as the top y-value. The 
  function's original intention was to display proportions-at-age vs. year.
}
\author{
  \href{mailto:schnutej@shaw.ca}{Jon T. Schnute}, Research Scientist Emeritus\cr
  Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC\cr

  Maintainer: \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Offsite, Vancouver BC\cr
  Last modified \code{Rd: 2023-10-18}
}
\seealso{
In package \pkg{PBSmodelling}:\cr
  \code{\link[PBSmodelling]{genMatrix}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  plotBubbles(round(genMatrix(40,20),0),clrs=c("green","grey","red"));
  data(CCA.qbr,envir=.PBSmodEnv)
  plotBubbles(CCA.qbr,cpro=TRUE,powr=.5,dnam=TRUE,size=.15,
    ylim=c(0,70),xlab="Year",ylab="Quillback Rockfish Age")
  par(oldpar)
})
}
\keyword{hplot}
% For non-standard keywords, use \concept instead of \keyword:
\concept{plotFuns}

