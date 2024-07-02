\name{mcODE-package}
\alias{mcODE-package}
\alias{mcODE}
\docType{package}
\title{
Monte Carlo Integration of First Order ODEs
}
\description{
Monte Carlo solution of first order differential equations
and confidence intervals for the solutions.
}
\details{
ODE.MVT can solve any first order differential equation
of the form g' = F(x, g) with initial condition g(x0) = g0, provided conditions are
satisfied for existence and uniqueness of the solution.  Confidence intervals for the
solution can be obtained through use of ODE.MVT.agg.  An alternate method due to 
Akhtar et al is implemented in ODE.ADA.
}
\author{
Author: W.J. Braun

Maintainer: W.J. Braun
}
\references{
Akhtar, M. N., Durad, M. H., and Ahmed, A. (2015). Solving initial value ordinary 
differential equations by
Monte Carlo method. Proc. IAM, 4:149-174.

Braun, W. J. (2024) Monte Carlo integration of first order ordinary differential equations. Preprint.
}
\keyword{package}

\seealso{
  \code{deSolve} 
}
\examples{
# Solve g' = F(x, g) on (0, 1] with g(0) = -1/1000001
G <- function(x, g) {
    -1000*g + 3000 - 2000*exp(-x)
}
T <- 1
x0 <- 0
g0 <- -1/1000001
nMVT <- 5000
ghat <- ODE.MVT(G, initvalue = g0, endpoint = T, initpoint = x0, Niter = 2, npoints = nMVT)
plot(ghat, type = "l")
}
