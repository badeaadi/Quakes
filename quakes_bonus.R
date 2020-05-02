
frepcomgen <- function(n, m) {
  
  #Creeaza random xi, yj
  x <- runif(n, 0, 100) 
  y <- runif(m, 0, 200)
  
  #Creeaza probabilitatile pi, qj
  xprob <- runif(n, 0, 1)
  yprob <- runif(m, 0, 1)
  
  #Impartim fiecare probabilitate la toata suma 
  #Pentru normalizare
  xprobsum <- sum(xprob)
  yprobsum <- sum(yprob)
  
  for (i in seq(1, n, 1)) {
    xprob[i] <- xprob[i] / xprobsum
  }
  for (i in seq(1, m, 1)) {
    yprob[i] <-yprob[i] / yprobsum
  }
  
  #pi <- matrix(nrow = n,ncol = m)
  pi <- matrix(0, n, m)
  
  for (i in seq(1, n - 1, 1)) {
    for (j in seq(1, m - 1, 1)) {
      sumlin <- sum(pi[i,1:j-1])
      sumcol <- sum(pi[1:i-1,j])
      minim <- min(xprob[i]-sumlin,yprob[j]-sumcol)
      #pi[i][ = runif(m, )]j] = p[i] * q[i] * runif(1, 0.5, 1.5)
      pi[i,j] <- runif(1,0,minim)
    }
  }
  
  for(i in seq(1,n,1)){
    sumlin <- sum(pi[i,1:m-1])
    pi[i,m]<-xprob[i]-sumlin
  }
  
  for(i in seq(1,m - 1,1)){
    sumcol <- sum(pi[1:n-1,i])
    pi[n,i]<-yprob[i]-sumcol
  }
  for (i in seq(1, n, 1)) {
    j = runif(1, 1, m + 0.9999)
    pi[i,floor(j)] = -1
  }
  return (list(pi = pi,xprob = xprob,yprob = yprob))
}

ls <- frepcomgen(5, 5)
pi2 <- ls$pi
xprob2 <- ls$xprob
yprob2 <- ls$yprob


fcomplrepcom <- function(pi, xprob, yprob)
{
  
  for (k in seq(1, n + m, 1)) {
    
    for (i in seq(1, n, 1)) {
       linsum = sum(pi[i, 1:m])
       rnr <- 0 #number of missing elements on this line
       for (j in seq(1, m, 1)) {
         if (pi[i][j] == -1) {
           rnr <- rnr + 1
         }
       }
       if (rnr == 0 && xprob[i] == 0)
         xprob[i] <- linsum
       if (rnr == 1) {
         for (j in seq(1, m, 1)) {
           if (pi[i][j] == -1) {
             pi[i][j] = xprob[i] - linsum - 1
           }
         }
       }
    }
    for (j in seq(1, m, 1)) {
      colsum = sum(pi[1:n, j])
      rnr <- 0 #number of missing elements on this column
      for (i in seq(1, n, 1)) {
         if (pi[i][j] == -1) {
           rnr <- rnr + 1
         }
      }
      if (rnr == 0 && yprob[i] == 0)
         yprob[i] = colsum;
      if (rnr == 1) { 
        for (i in seq(1, n, 1)) {
          if (pi[i][j] == -1) {
             pi[i][j] = yprob[j] - colsum - 1
             break
          }
        }
      }
    }
  }
  return (list(pi2 = pi,xprob2 = xprob,yprob2 = yprob))      
}


ls <- frepcomgen(pi2, xprob2, yprob2)
pi3 <- ls$pi2
xprob3 <- ls$xprob2
yprob3 <- ls$yprob2


get_var <- function(x, n) {
  
  e_square<- 0
  e <- 0
  
  for (i in (1:n)) {
    e_square <- e_square + i * i *  x[i]
  }
  for (i in (1:n)) {
    e <- e + i *  x[i]
  }
  
  return (e_square - e * e)
}

fcompute <- function(pi, x, xprob, y, yprob)
{
  #Calculam probabilitatea pentru anumite inegalitati
  #pentru X si Y
  
  #Cov(3X, - 5Y)
  x_new <- x
  for (xi in x_new)
    xi <- xi * 3.0
  
  y_new <- x
  for (yi in y_new) 
    yi <- yi * (-5.0)

  c <- cov(x_new, y_new, use = "everything", method = c("pearson", "kendall", "spearman"))
  
  
  #(P(0 < x < 3) && (Y > 2))
  p1 <- 0
  for (i in seq(1, n, 1)) {
     if (0 < x[i] && x[i] < 3)
      for (j in seq(1, m, 1))
        if (y[j] > 2){
          p1 = p1 + pi[i][j]          
      }
    }
  p2 <- 0
  #(P(X > 6) && (Y < 7))
  for (i in seq(1, n, 1)) {
    if (x[i] > 6)
      for (j in seq(1, m, 1))
        if (y[j] < 7){
          p2 = p2 + pi[i][j]          
        }
  }
  return (list(c =c, p1 = p1, p2 = p2))
}
fverind <- function(pi, xprob, yprob)
{
  result <- "Independent"
  for (i in seq(1, n, 1))
    for (j in seq(1, m, 1))
      if (pi[i][j] != xprob[i]  * yprob[j]) {
        result <- "Not independent"
        break
        }
  return (result)
}
fverncor <- function(pi, xprob, yprob)
{
  correlation <- cor(xprob, yprob)
  if (correlation < 0.25)
    result <- "Not correlated"
  else if(correlation < 0.5)
    result <- "Weakly Correlated"
  else 
    result <- "Strongly Correlated"
  return (result)
}