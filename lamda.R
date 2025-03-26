#### (2)lamda0～2估计 #### 
lamda.estimation <- function (Yab, nn, param) {
  pp <- param$pp
  W <- param$WW
  Ya <- Yab$Ya # pp*nn
  Yb <- Yab$Yb # pp*(nn-1)
  
  Ip <- diag(rep(1,3))
  
  init_0 <- matrix(0, pp, pp)
  for (ii in 1:(nn-1)) {
    init_0 <- init_0 + matrix(Yb[,ii])%*%t(matrix(Yb[,ii]))
  }
  cov11 <- init_0/(nn-1) 
  
  init_1 <- matrix(0, pp, pp)
  for (ii in 2:nn) {
    init_1 <- init_1 + matrix(Ya[,ii])%*%t(matrix(Yb[,ii-1]))
  }
  cov12 <- init_1/(nn-1)
  
  lamda.hat <- matrix(0, pp, 3)
  for (ii in 1:pp) {
    wi <- W[ii, ]
    ei <- numeric(pp); ei[ii] <- 1
    Y <- t(cov12)%*%ei
    X <- cbind(t(cov12)%*%wi, cov11%*%ei, cov11%*%wi)
    
    lamda.hat[ii, ] <- solve(t(X)%*%X)%*%t(X)%*%Y 
  }
  
  return(lamda.hat = lamda.hat)
}


