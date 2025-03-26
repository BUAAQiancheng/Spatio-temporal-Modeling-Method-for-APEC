#### (1)profile #### 
# beta_a beta_b 计算
beta.calcu <- function(lens, Z, Y){
  a1 <- 0
  a2 <- 0
  for (i in 1:lens) {
    z_it <- Z[i,]
    z_res <- z_it %*% t(z_it)
    a1 <- z_res + a1
    
    y_it <- Y[i]
    y_res <- z_it * y_it
    a2 <- y_res + a2
  }
  
  beta <- solve(a1) %*% a2
  return(beta)
}

# profile估计
profile <- function(serie, param) {
  # Y=serie$yy: pp*nn, Z=serie$zz: pp*dd*nn
  pp <- param$pp
  nn <- serie$nn
  dd <- param$dd
  yy <- serie$yy
  zz <- serie$zz
  
  # 设置Z0 Y0 Z1 Y1
  np0 <- nn*pp
  Z0 <- matrix(0, nrow = np0, ncol = dd)
  for (ii in 1:nn) {
    Z0[((ii - 1) * pp + 1):(ii * pp),] <- zz[,,ii]
  }
  
  Y0 <- matrix(as.vector(yy), nrow = np0, ncol = 1)
  
  np1 <- (nn-1)*pp
  Z1 <- Z0[-(1:pp),] # 2 <= t <= n
  Y1 <- Y0[-((np1+1):np0), ] # 1 <= t <= n-1
  
  # 计算beta
  beta_a <- beta.calcu(np0, Z0, Y0)
  beta_b <- beta.calcu(np1, Z1, Y1) ##### ?????? 或者LASSO ???
  
  # 计算Ya Yb
  Ya0 <- Y0 - Z0 %*% beta_a
  Ya <- matrix(Ya0, nrow = pp, ncol = nn)
  Yb0 <- Y1 - Z1 %*% beta_b
  Yb <- matrix(Yb0, nrow = pp, ncol = nn-1)
  
  return(list(Ya=Ya, Yb=Yb, Z1=Z1, np1=np1))
}



