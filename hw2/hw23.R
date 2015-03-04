rs <- function(g){
  if (g=="g1"){
    repeat{
      x <- rexp(1,1)
      u <- runif(1,0,1)
      r <- 1/(1+x^2)
      if (u <= r){
        res <- x
        break
      }
  }
  }
  if (g=="g2"){
    repeat{
      x <- abs(rcauchy(1,0,1))
      u <- runif(1,0,1)
      r <- exp(-x)
      if (u <= r){
        res <- x
        break
      }
    }
  }
  res
}

library(plyr)
n <- 5000
pm1 <- proc.time()
rs.g1 <- laply(1:n, function(i)rs("g1"))
proc.time()-pm1

hist(rs.g1, nclass = 100, prob = T)
lines(density(rs.g1), col = "red")
pm2 <- proc.time()
rs.g2 <- laply(1:n, function(i)rs("g2"))
proc.time()-pm2
hist(rs.g2, nclass = 100)
lines(density(rs.g2), col = "red")
