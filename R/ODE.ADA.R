ODE.ADA <-
function(G, initvalue, endpoint, X0 = 0, npoints = 1000, M, R, N = 100000) {
    Y0 <- initvalue
    X <- seq(X0, endpoint, length = npoints)
    Y <- rep(Y0, npoints)
    U1 <- runif(N, 0, M)
    U2 <- runif(N, -R, 0)
    delta <- X[2] - X[1]
    for (k in 1:npoints) {
        JF <- G(X[k], Y[k])
        if (JF > 0) {
            S <- sum(U1 <= JF)
            Y[k+1] <- Y[k] + M*S/N*delta
        } else { 
            S <- sum(U2 >= JF)
            Y[k+1] <- Y[k] - R*S/N*delta
        }
    }    
    list(x = X, y = Y[1:npoints])
}
