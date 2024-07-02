\name{ODE.MVT}
\alias{ODE.MVT}
\title{
Monte Carlo ODE Solver Based on Mean Value Theorem
}
\description{
Given g' = G(x, g) and g(x0) = g0 satisfying conditions for existence and uniqueness
of solution, a Monte Carlo approximation to the solution is found.  The method is
essentially a Monte Carlo version of Euler and Runge-Kutta type methods.
}
\usage{
ODE.MVT(G, initvalue, endpoint, initpoint = 0, Niter = 2, npoints = 1000)
}
\arguments{
  \item{G}{a function having two arguments.
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
}
\value{
A list consisting of
  \item{x}{a vector of length npoints, consisting of the x-coordinate of the solution.}
  \item{y}{a vector of length npoints, consisting of the y-coordinate of the solution, i.e. g(x).}
}
\references{
Braun, W.J. (2024) Preprint.

}
\author{
Braun, W.J.
}
\examples{
# Initial Value Problem:
G <- function(x, g) {
    -1000*g + sin(x)
}
g0 <- -1/1000001; x0 <- 0 # initial condition
xF <- 1 # interval endpoint
nMVT <- 1000 # number of subintervals used
# Monte Carlo solution based on one iteration:
ghat1 <- ODE.MVT(G, initvalue = g0, endpoint = xF, initpoint = x0, Niter = 1, npoints = nMVT) 
# Monte Carlo solution based on five iterations:
ghat5 <- ODE.MVT(G, initvalue = g0, endpoint = xF, initpoint = x0, Niter = 5, npoints = nMVT) 
gTrue <- function(x) (1000*sin(x) - cos(x))/1000001 # true solution
oldpar <- par(mfrow=c(1,3))
curve(gTrue(x)) # 
lines(ghat1, col = 2, lty = 2, ylab="g(x)")
plot(abs(gTrue(ghat1$x) - ghat1$y) ~ ghat1$x, xlab = "x", 
    ylab = "Absolute Error", type = "l", ylim = c(0, 2e-6), main="1 Iteration")
plot(abs(gTrue(ghat5$x) - ghat5$y) ~ ghat5$x, xlab = "x", 
    ylab = "Absolute Error", type = "l", ylim = c(0, 2e-6), main="5 Iterations")
par(oldpar)
}
\keyword{datagen}
