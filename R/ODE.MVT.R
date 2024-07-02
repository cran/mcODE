ODE.MVT <-
function(G, initvalue, endpoint, initpoint = 0, Niter = 2,  npoints = 1000) {
    g <- numeric(npoints)
    g[1] <- initvalue; x0 <- initpoint
    T <- endpoint
    s <- seq(x0, T, len = npoints)
    delta <- s[2] - s[1]
    Ux <- matrix(0, nrow = npoints - 1, ncol = Niter)
    Ux[, 1] <- runif(npoints-1, min = s[1:(npoints-1)], max = s[2:npoints])
    if (Niter > 1) {
        for (j in 2:Niter) {
            Ux[, j] <- runif(npoints-1, min = s[1:(npoints-1)], max = Ux[,j-1])
        }    
    }
    for (i in 2:npoints) {
        g_xi <- g[i-1]
        if (Niter > 1) {
            g_xi <- g[i-1] + (Ux[i-1, Niter - 1] - s[i-1])*G(Ux[i-1, Niter], g[i-1])
            if (Niter > 2) {
                for (jj in (Niter - 1):2) {
                    g_xi <- g[i-1] + (Ux[i-1, jj-1] - s[i-1])*G(Ux[i-1, jj], g_xi)
                }    
            }    
        }
        g_x <- g[i-1] +  delta*G(Ux[i-1, 1], g_xi)
        g[i] <- g_x
    }
    return(list(x = s, y = g))
}
