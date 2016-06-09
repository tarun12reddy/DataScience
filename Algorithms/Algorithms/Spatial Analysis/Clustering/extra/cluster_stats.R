# Cluster Stats
#cl is number of clusters
clust.perf.metrics <- function(data, cl) {
  fR2 <- NULL
  fccc <- NULL
  fPseudoF <- NULL
  fE_R2 <- NULL
  for(qq in 1:cl){
    partition <- cutree(h, k)
    
    data <- cbind(data, partition)
    nn <- dim(data)[1]
    pp <- dim(data)[2]
    TT <- t(data) %*% data
    sizeEigenTT <- length(eigen(TT)$value)
    eigenValues <- eigen(TT/(nn - 1))$value
    
    for (i in 1:sizeEigenTT) {
      if (eigenValues[i] < 0) {
        stop("The TSS matrix is indefinite. There must be too many missing values. The index cannot be calculated.")
      }
    }
    
    s1 <- sqrt(eigenValues)
    ss <- rep(1, sizeEigenTT)
    
    for (i in 1:sizeEigenTT) {
      if (s1[i] != 0)
        ss[i] = s1[i]
    }
    
    vv <- prod(ss)
    z <- matrix(0, ncol = qq, nrow = nn)
    clX <- as.matrix(data[ ,pp])
    
    for (i in 1:nn)
      for (j in 1:qq) {
        z[i, j] == 0
        if (clX[i, 1] == j) z[i, j] = 1
      }
    
    xbar <- solve(t(z) %*% z) %*% t(z) %*% data
    B <- t(xbar) %*% t(z) %*% z %*% xbar
    W <- TT - B
    R2 <- 1 - (sum(diag(W))/sum(diag(TT)))
    PseudoF <- (sum(diag(B))/(qq-1))/(sum(diag(W))/(nn-qq))
    
    v1 <- 1
    u1 <- rep(0, pp)
    c1 <- (vv/qq)^(1/pp)
    u1 <- ss/c1
    k1 <- sum((u1 >= 1) == TRUE)
    p1 <- min(k1, qq - 1)
    
    
    if (all(p1 > 0, p1 < pp)) {
      for (i in 1:p1) { v1 <- v1 * ss[i]}
      c <- (v1/qq)^(1/p1)
      u <- ss/c
      b1 <- sum(1/(nn + u[1:p1]))
      b2 <- sum(u[(p1 + 1):pp]^2/(nn + u[(p1 + 1):pp]), na.rm = TRUE)
      E_R2 <- 1 - ((b1 + b2)/sum(u^2)) * ((nn - qq)^2/nn) * (1 + (4/nn))
      ccc <- log((1 - E_R2)/(1 - R2)) * (sqrt(nn * p1/2)/((0.001 + E_R2)^1.2))
      
    } else {
      c <- (v1/qq)^(1/p1)
      u <- ss/c
      b1 <- sum(1/(nn + u))
      E_R2 <- 1 - (b1/sum(u^2)) * ((nn - qq)^2/nn) * (1 + 4/nn)
      ccc <- log((1 - E_R2)/(1 - R2)) * (sqrt(nn * pp/2)/((0.001 + E_R2)^1.2))
    }
    fR2 <- c(fR2, R2)
    fPseudoF <- c(fPseudoF, PseudoF)
    fccc <- c(fccc, ccc)
    fE_R2 <- c(fE_R2, E_R2)
    print(qq)
  }  
  results <- list(R_2= fR2, PseudoF=fPseudoF, CCC = fccc, E_R2=fE_R2)
  return(results)
}

clust.perf.metrics(user_genre, 20)
