\name{ODE.ADA}
\alias{ODE.ADA}
\title{
Monte Carlo ODE Solver of Akhtar et al
}
\description{
Given g' = G(x, g) and g(x0) = g0 satisfying conditions for existence and uniqueness
of solution, a Monte Carlo approximation to the solution is found.  The method is 
essentially a rejection method.
}
\usage{
ODE.ADA(G, initvalue, endpoint, X0 = 0, npoints = 1000, M, R, N = 1e+05)
}
\arguments{
  \item{G}{a function having two numeric vector arguments.
}
  \item{initvalue}{a numeric initial value
}
  \item{endpoint}{a numeric interval endpoint value.
}
  \item{X0}{a numeric interval starting point value.
}
  \item{npoints}{an integer value specifying the number of subintervals to build
the approximation on.
}
  \item{M}{a numeric value specifying an upper bound for F(x, g).
}
  \item{R}{a numeric value specifying a lower bound for F(x, g).
}
  \item{N}{an integer value specifying the number of Monte Carlo simulations used
for each subinterval.
}
}
\value{
A list consisting of 
  \item{x}{a vector of length npoints, consisting of the x-coordinate of the solution.}
  \item{y}{a vector of length npoints, consisting of the y-coordinate of the solution, i.e. g(x).}
}
\references{
Akhtar, M. N., Durad, M. H., and Ahmed, A. (2015). Solving initial value ordinary 
differential equations by
Monte Carlo method. Proc. IAM, 4:149-174.
}

\examples{
G <- function(x, g) {
    -1000*g + sin(x)
}
gTrue <- function(x) (1000*sin(x) - cos(x))/1000001 
xF <- 1; x0 <- 0
g0 <- -1/1000001  # initial value
NAkhtar <- 800
nAkhtar <- 2500
MAkhtar <- 1e-3
RAkhtar <- 1e-6
gAkhtar <- ODE.ADA(G, initvalue = g0, endpoint = xF, X0 = x0, 
    npoints = nAkhtar, M=MAkhtar, R = RAkhtar, N = NAkhtar)
plot(gAkhtar, type = "l")
}

\keyword{datagen}
