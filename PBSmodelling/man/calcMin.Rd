% --- Source file: calcMin.Rd ---
\name{calcMin}
\alias{calcMin}
\title{Calculate the Minimum of a User-Defined Function}
\concept{minimization}
\description{
  Minimization based on the R-stat functions \code{nlm}, \code{nlminb}, and \code{optim}.
  Model parameters are scaled and can be active or not in the minimization.
}
\usage{
calcMin(pvec, func, method="nlm", trace=0, maxit=1000, reltol=1e-8,
        steptol=1e-6, temp=10, repN=0, \dots)
}
\arguments{
  \item{pvec}{Initial values of the model parameters to be optimized.
    \code{pvec} is a data frame comprising four columns (
    \code{"val","min","max","active"}) and as many rows as there are model
    parameters. The \code{"active"} field (logical) determines whether the 
    parameters are estimated (\code{T}) or remain fixed (\code{F}).}
  \item{func}{The user-defined function to be minimized (or maximized).
    The function should return a scalar result.}
  \item{method}{The minimization method to use: one of \code{nlm}, \code{nlminb},
    \code{Nelder-Mead}, \code{BFGS}, \code{CG}, \code{L-BFGS-B}, or 
    \code{SANN}. Default is \code{nlm}.}
  \item{trace}{Non-negative integer. If positive, tracing information on the
    progress of the minimization is produced. Higher values may produce more
    tracing information: for method \code{"L-BFGS-B"} there are six levels of
    tracing. Default is \code{0}.}
  \item{maxit}{The maximum number of iterations. Default is \code{1000}.}
  \item{reltol}{Relative convergence tolerance. The algorithm stops if it is
    unable to reduce the value by a factor of \code{reltol*(abs(val)+reltol)}
    at a step. Default is \code{1e-8}.}
  \item{steptol}{A positive scalar providing the minimum allowable relative step length.
      Default is \code{1e-6}.}
  \item{temp}{Temperature controlling the \code{"SANN"} method. It is the
    starting temperature for the cooling schedule. Default is \code{10}.}
  \item{repN}{Reports the parameter and objective function values on the R-console
    every \code{repN} evaluations. Default is \code{0} for no reporting.}
  \item{\dots}{Further arguments to be passed to the optimizing function chosen:
    \code{nlm}, \code{nlminb}, or \code{optim}.
    Beware of partial matching to earlier arguments.}
}
\details{
  See \code{optim} for details on the following methods: \code{Nelder-Mead},
  \code{BFGS}, \code{CG}, \code{L-BFGS-B}, and \code{SANN}.
}

\value{
  A list with components:
  \item{Fout}{The output list from the optimizer function chosen through \code{method}.}
  \item{iters}{Number of iterations.}
  \item{evals}{Number of evaluations.}
  \item{cpuTime}{The user CPU time to execute the minimization.}
  \item{elapTime}{The total elapsed time to execute the minimization.}
  \item{fminS}{The objective function value calculated at the start of the minimization.}
  \item{fminE}{The objective function value calculated at the end of the minimization.}
  \item{Pstart}{Starting values for the model parameters.}
  \item{Pend}{Final values estimated for the model parameters from the minimization.}
  \item{AIC}{Akaike's Information Criterion}
  \item{message}{Convergence message from the minimization routine.}
}
\author{
  Jon Schnute, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\note{
  Some arguments to \code{calcMin} have no effect depending on the \code{method} chosen.
}
\seealso{
  \code{\link{scalePar}}, \code{\link{restorePar}}, \code{\link{calcMin}}, \code{\link{GT0}} \cr
  In the \code{stats} package: \code{nlm}, \code{nlminb}, and \code{optim}.
}
\examples{
local(envir=.PBSmodEnv,expr={
Ufun <- function(P) {
  Linf <- P[1]; K <- P[2]; t0 <- P[3]; obs <- afile$len;
  pred <- Linf * (1 - exp(-K*(afile$age-t0)));
  n <- length(obs); ssq <- sum((obs-pred)^2 );
  return(n*log(ssq)); };

oldpar = par(no.readonly = TRUE)
afile <- data.frame(age=1:16,len=c(7.36,14.3,21.8,27.6,31.5,35.3,39,
  41.1,43.8,45.1,47.4,48.9,50.1,51.7,51.7,54.1));
pvec <- data.frame(val=c(70,0.5,0),min=c(40,0.01,-2),max=c(100,2,2),
  active=c(TRUE,TRUE,TRUE),row.names=c("Linf","K","t0"),
  stringsAsFactors=FALSE);
alist <- calcMin(pvec=pvec,func=Ufun,method="nlm",steptol=1e-4,repN=10);
print(alist[-1]); P <- alist$Pend;
#resetGraph(); 
expandGraph();
xnew <- seq(afile$age[1],afile$age[nrow(afile)],len=100);
ynew <- P[1] * (1 - exp(-P[2]*(xnew-P[3])) );
plot(afile); lines(xnew,ynew,col="red",lwd=2); 
addLabel(.05,.88,paste(paste(c("Linf","K","t0"),round(P,c(2,4,4)),
  sep=" = "),collapse="\n"),adj=0,cex=0.9);
par(oldpar)
})
}

\keyword{nonlinear}
\keyword{optimize}
