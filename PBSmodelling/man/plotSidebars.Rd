\name{plotSidebars}
\alias{plotSidebars}
\title{
  Plot Table as Horizontal Sidebars
}
\description{
  Plot (x,y) table (matrix or data frame) as horizontal sidebars.
}
\usage{
plotSidebars(z, scale = 1, col = lucent("blue", 0.25), ...)
}
\arguments{
  \item{z}{data frame or matrix of z-values (e.g., age frequencies) where 
    rows form the plot's y-values and columns describe the grouping 
    variable along the x-axis.}
  \item{scale}{numeric scale factor controlling the leftward expansion of z-value bars.}
  \item{col}{colour to fill bars.}
  \item{\dots}{additional parameters used by \code{par} and \code{polygon}. 
    The user can also pass in two non-formal arguments to control the function: \cr
    \code{lbl} -- labels for the x- and y-axis; \cr
    \code{margin} -- function to report margin summaries. }
}
\details{
  Plots z-data as horizontal bars arising from an x-coordinate controlled by
  the column names of \code{z}. The bars extend left along the y-coordinate 
  by \code{z*scale} from the central x-coordinate. 
}
\author{
  Steve Martell, International Pacific Halibut Commission, Seattle WA
}
\seealso{
  \code{\link[PBSmodelling]{plotBubbles}}, \code{\link[PBSmodelling]{plotFriedEggs}},
  \code{\link[PBSmodelling]{evalCall}}
}
\examples{
local(envir=.PBSmodEnv,expr={
  oldpar = par(no.readonly=TRUE)
  pbsfun = function () {
    meanmarg = function(x){x=x[!is.na(x)]; 
      if (length(x)==0 | all(x==0)) NA else sum((x/sum(x))*as.numeric(names(x)))}
    data(CCA.qbr,envir=.PBSmodEnv)
    plotSidebars(CCA.qbr,scale=4,las=1,border="navyblue",mar=c(4,4,1,1),
      lbl=c("Year","Quillback Rockfish Age"),margin=function(x){round(meanmarg(x),0)})
    invisible() }
  pbsfun()
  par(oldpar)
})
}
\keyword{hplot}

