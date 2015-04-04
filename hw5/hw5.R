#1.
#a)
set.seed(1)
p <- .3
lambda <- 2
n <- 100
r <- rbinom(n, 1, p)
y <- rpois(n, lambda)
x <- r*y
x
#b) 
a<- 1;b <- 1
gibbs <- function(a,b){
  r <- rep(1, n)
  cnt <- 0
  burn.in <- 10000
  keep <- 0
  ns <- 10000
  pkeep <- lambdakeep <- NULL
  repeat{
    lambda <- rgamma(1,a +sum(x), b +sum(r))
    p <- rbeta(1,1+sum(r), n+1-sum(r))
    pnew <- p*exp(-lambda)/(p*exp(-lambda) + (1-p)*(x==0))
    r <- rbinom(n, 1, pnew)
    cnt <- cnt +1
    if (cnt >burn.in){
      keep <- keep +1
      pkeep[keep] <- p
      lambdakeep[keep] <- lambda
    }
    if (cnt >burn.in + ns) break
  }
  
  cip <- quantile(pkeep, prob = c(.025, .975))
  cilambda <- quantile(lambdakeep, prob = c(.025, .975))
  res <- rbind(p=cip, lambda=cilambda)
  res  
}

gibbs(1,1)
gibbs(1,2)
gibbs(1,3)
gibbs(1000,1000)
gibbs(20, 2)


#2. 
theta1 <- 1.5
theta2 <- 2

f <- function(z, theta1 = 1.5, theta2 = 2){
  z^(-3/2)*exp(-theta1*z-theta2/z +2*sqrt(theta1*theta2) + log(sqrt(2*theta2)))
}

mh <- function(a, b){
  i <- 1
  x <- NULL
  x[1] <- 1
  repeat{
    y <- rgamma(1, a, b)
    gy <-dgamma(y, a, b) 
    r <- f(y)*dgamma(x[i], a, b)/(f(x[i])*dgamma(y, a, b))
    u <- runif(1,0,1)
    x[i+1] <- ifelse(u <r, y, x[i])
    i <- i+1
    if (i == 10000) break
  }
  hist( x, nclass = 100, prob = T)
  c(mx = mean(x), m1x = mean(1/x)) - #2. 
    theta1 <- 1.5
  theta2 <- 2
  
  f <- function(z, theta1 = 1.5, theta2 = 2){
    z^(-3/2)*exp(-theta1*z-theta2/z +2*sqrt(theta1*theta2) + log(sqrt(2*theta2)))
  }
  
  mh <- function(a, b){
    i <- 1
    x <- NULL
    x[1] <- 1
    repeat{
      y <- rgamma(1, a, b)
      gy <-dgamma(y, a, b) 
      r <- f(y)*dgamma(x[i], a, b)/(f(x[i])*dgamma(y, a, b))
      u <- runif(1,0,1)
      x[i+1] <- ifelse(u <r, y, x[i])
      i <- i+1
      if (i == 10000) break
    }
    hist( x, nclass = 100, prob = T)
    abs(c(mx.diff = mean(x), m1x.diff = mean(1/x))-c(mz = sqrt(theta2/theta1), 
                                       m1z=sqrt(theta1/theta2) + 1/(2*theta2)))
  }
  
  mh(1,.1)
