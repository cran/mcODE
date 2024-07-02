ODE.MVT.agg <-
function(G, initvalue, endpoint, initpoint = 0, Niter = 2, npoints = 1000, nsims = 10) {
    g <- matrix(0, nrow = npoints, ncol = nsims)
    for (j in 1:nsims) {
       ghat <- ODE.MVT(G, initvalue, endpoint, initpoint, Niter, npoints)
       g[, j] <- ghat$y
    }
    gbar <- apply(g, 1, mean)
    stan <- apply(g, 1, sd)
    list(x = ghat$x, y = gbar, stdev = stan)
}
