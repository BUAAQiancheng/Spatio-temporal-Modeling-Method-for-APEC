myLASSO <- function(x0, y0, Model) {  
  y <- (y0 - mean(y0))
  y <- y / sd(y) # standardize y
  ss <- nrow(x0)
  ncov0 <- ncol(x0)
  x <- x0
  for (j in 1:ncov0) {
    x[, j] <- (x0[, j] - mean(x0[, j]))
  }
  Model=1:ncov0 #x <- x[, Model, drop = FALSE] # relevant design matrix
  ncov <- ncol(x)
  
  if (length(Model) == 0) { # if Model is NULL
    beta <- rep(0, ncov0)
    mLASSO <- NULL
    flag <- 1
  } else {
    XX <- t(x) %*% x / ss
    XY <- t(x) %*% y / ss
    ols <- solve(XX) %*% XY
    ww <- 1 / abs(ols)
    ngrid <- 100
    BIC <- rep(0, ngrid + 1)
    B <- matrix(0, ngrid + 1, ncov)
    for (n in 0:ngrid) {
      lam <- max(abs(XY)) * exp(-0.1 * n)
      dist <- 1.0
      beta <- ols
      niter <- 0
      while (dist > 1.0E-6 & niter < 100) {
        niter <- niter + 1
        beta0 <- beta
        D <- diag(pmax(abs(ww / beta), 1.0E+9))
        beta <- solve(XX + lam * D) %*% XY
        dist <- mean(abs(beta0 - beta))
      }
      beta [abs(beta) < 1.0e-4]<- 0 #beta <- beta * (abs(beta) > 1.0e-4)
      tmpModel <- which(abs(beta) > 0)  #tmpModel <- Model[c(abs(beta) > 0)];
      mX <- x[, tmpModel] #mX <- x[, tmpModel, drop = FALSE]
      ols2 <- solve(t(mX) %*% mX / ss) %*% (t(mX) %*% y / ss)
      rss <- mean((y - mX %*% ols2)^2)
      df <- sum(abs(beta) > 0)
      BIC[n + 1] <- log(rss) + df * (log(ss) + 2 * log(ncov0)) / ss
      B[n + 1, ] <- as.vector(beta) #t
    }
    pos <- which.min(BIC)
    b <- B[pos, ]
    mLASSO <- Model[abs(b) > 0]
    mX <- x0[, mLASSO]#mX <- x0[, mLASSO, drop = FALSE]
    b <- solve(t(mX) %*% mX / ss) %*% (t(mX) %*% y0 / ss)
    flag <- as.integer(pos > 1)
    beta <- rep(0, ncov0)
    beta[mLASSO] <- b
  }
  list(beta = beta, mLASSO = mLASSO, flag = flag)
}

lasso = function(zz,yy,date,CV=F,heat=F,interact=F){
  date0 = c(3:13,16)
  index = match(date,date0)
  if (heat == T){
    flag = 5
  } else {
    flag = 2 
  }
  x = zz[,1:flag]
  for (k in 1:3){
    for (i in 1:(length(index)-1)){
      z = matrix(0,nrow(zz),1)
      for (j in index[i]:(index[i+1]-1)){
        z = z + matrix(zz[,(k-1)*(length(date0)-1)+j+flag])
      }
      if (k == 1){
        city = 'Beijing'
      } else if (k == 2){
        city = 'Hebei'
      } else {
        city = 'Tianjin'
      }
      colnames(z) = paste(city,':11-',date[i],'~11-',date[i+1]-1,sep='')
      x = cbind(x,z)
    }
  }
  if (interact == T){
    for (i in 1:3){
      inter = x[,((5+1+(length(index)-1)*(i-1)):(5+(length(index)-1)*i))]*zz[,2+i]
      colnames(inter) = paste('heat*date:',colnames(x)[((5+1+(length(index)-1)*(i-1)):(5+(length(index)-1)*i))],sep='')
      x = cbind(x,inter)
    }
  }
  if (CV = T){
    library(glmnet)
    r = cv.glmnet(x, yy, family = "gaussian", alpha = 1, intercept = FALSE)
    c = r$lambda.min
    coef = coef(r, s = c)
  } else{
    coef = myLASSO(x, yy, 1:ncol(x))
  }
  
  return(coef = coef)
}