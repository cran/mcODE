\name{ODE.MVT.agg}
\alias{ODE.MVT.agg}
\title{
Monte Carlo ODE Solver Based on Mean Value Theorem - Replicated
}
\description{
Given g' = G(x, g) and g(x0) = g0 satisfying conditions for existence and uniqueness
of solution, a Monte Carlo approximation to the solution is found.  The method is
essentially a Monte Carlo version of Euler and Runge-Kutta type methods.
Repeated calls are made to the \code{ODE.MVT} function to obtain replicated estimates
of the solution.
}
\usage{
ODE.MVT.agg(G, initvalue, endpoint, initpoint = 0, Niter = 2, npoints = 1000, nsims = 10)
}
\arguments{
 \item{G}{a function having two numeric vector arguments.
}
  \item{initvalue}{a numeric initial value
}
  \item{endpoint}{a numeric interval endpoint value.
}
  \item{initpoint}{a numeric interval starting point value.
}
  \item{Niter}{an integer value specifying the number of iterations at each step.
}
  \item{npoints}{an integer value specifying the number of subintervals to build
the approximation on.
}
  \item{nsims}{an integer value specifying the number of replicated solutions 
required.
}
}
\value{
A list consisting of
  \item{x}{a vector of length npoints, consisting of the x-coordinate of the solution.}
  \item{y}{a vector of length npoints, consisting of the average of replicated y-coordinates of the solution, i.e. g(x).}
  \item{stdev}{a vector of length npoints, consisting of the standard deviation
estimates of the solution estimate at each point.}
}
\author{
W.J. Braun
}
\examples{
G <- function(x, g) {
    -1000*g + sin(x)
}
g0 <- -1/1000001; x0 <- 0 # initial condition
xF <- 1 # interval endpoint
nMVT <- 1000 # number of subintervals used
# 10 reps of Monte Carlo solution based on two iterations:
ghat2 <- ODE.MVT.agg(G, initvalue = g0, endpoint = xF, initpoint = x0, 
    Niter = 2, npoints = nMVT, nsims = 10)
# 10 reps of Monte Carlo solution based on three iterations:
ghat3 <- ODE.MVT.agg(G, initvalue = g0, endpoint = xF, initpoint = x0, 
    Niter = 3, npoints = nMVT, nsims = 10)
gTrue <- function(x) (1000*sin(x) - cos(x))/1000001 # true solution
oldpar <- par(mfrow=c(1,2))
curve(gTrue(x), from = 0.3, to = 0.3015) # graph of solution on small interval
lines(ghat2, col = 4, lty = 4, ylab="g(x)", lwd = 2)  # estimated MC solution - 2 iterations
lines(ghat2$y - 1.96*ghat2$stdev ~ ghat2$x, lty = 3, col = 4) # lower conf. limits
lines(ghat2$y + 1.96*ghat2$stdev ~ ghat2$x, lty = 3, col = 4) # upper conf. limits
curve(gTrue(x), from = 0.3, to = 0.3015) # graph of solution on small interval
lines(ghat3, col = 4, lty = 4, ylab="g(x)", lwd = 2)  # estimated MC solution - 3 iterations
lines(ghat3$y - 1.96*ghat3$stdev ~ ghat3$x, lty = 3, col = 4) # lower conf. limits
lines(ghat3$y + 1.96*ghat3$stdev ~ ghat3$x, lty = 3, col = 4) # upper conf. limits
par(oldpar)
}

\keyword{datagen}
