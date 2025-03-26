#### (3)beta估计 #### 
param.estimation <- function(serie, param){
  # Ya & Yb
  Yab <- profile(serie = serie, param = param)
  
  # lamda.hat
  nn = serie$nn
  lamda.hat <- lamda.estimation(Yab = Yab, nn=nn, param = param)
  
  # beta.hat
  Z1 <- Yab$Z1
  np1 <- Yab$np1
  pp <- param$pp
  dd <- param$dd
  
  sum.z <- matrix(0, dd, dd)
  for (i in 1:np1) {
    z_it <- Z1[i,]
    z_res <- z_it %*% t(z_it)
    sum.z <- z_res + sum.z
  }
  
  zz <- serie$zz
  yy <- serie$yy
  WW <- param$WW
  sum.y <- matrix(0, dd, 1)
  for (i in 1:pp) {
    for (t in 2:nn) {
      sum.y <- sum.y + zz[i,,t] %*% (yy[i,t]-lamda.hat[i,1]*WW[i,]%*%yy[,t]-lamda.hat[i,2]*yy[i,t-1]-lamda.hat[i,3]*WW[i,]%*%yy[,t-1])
    }
  }
  
  beta.hat <- solve(sum.z)%*%sum.y
  
  return(list(lamda.hat = lamda.hat, beta.hat = beta.hat))
}

# -------------------------------------------------------------------------


