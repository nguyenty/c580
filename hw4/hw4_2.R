#a. 
set.seed(1)
n <- 5000
x <- runif(n, 0, 1)
h <- x^2
mu <- mean(h)
varmu <- var(h)/n
cbind(mu, varmu)
#b. 

x <- runif(n, -2,2)
y <- runif(n, 0,1)
h <- x^2*cos(x*y)
mu <- mean(h)
varmu <- var(h)/n
cbind(mu, varmu)

#c.
# let $x^3/4 = t$ then $3x^2/4dx = dt$ and 
# $\int_0^\infty 3x^4/4 e^{-x^3/4}dx = 4^{2/3}\int_0^\infty t^{2/3} e^{-t}dt$
x <- rexp(n, 1)  
h <- (4*x)^(2/3)
mu <- mean(h)
varmu <- var(h)/n
cbind(mu, varmu)

#3
# 
# install.packages("msm")
set.seed(1)
library(msm)
x <- rtnorm(n, mean = 1.5, sd = .1)
h <- 1/sqrt(2*pi)*exp(-x^2/2)
q <- 1
g <-dtnorm(x, mean = 1.5, sd = .1) 
w <- q/g
w <- w/sum(w)
hw <- h*w
mu <- mean(hw)
varmu <- var(hw)/n
cbind(mu,varmu)
boxplot(hw)
  
iss <- function(muu, nu){
  x <- rtnorm(n, mean = muu, sd = nu)
  h <- 1/sqrt(2*pi)*exp(-x^2/2)
  q <- 1
  g <-dtnorm(x, mean = muu, sd = nu) 
  w <- q/g
  w <- w/sum(w)
  hw <- h*w
  mu <- mean(hw)
  varmu <- var(hw)/n
  hist(hw, nclass = 100)
  res <- cbind(mu,varmu)
  return(res)
}

iss(1.5,.1)
iss(1.5,1)
iss(1.5,10)


#4. 
#(a)
n <- 1500
u <- runif(n, 0, 1)
hu <- 1/(u+1)
Ihat <- mean(hu)

#sum((hu-Ihat)^2)/(n*(n-1))

#(b)
ecu <- 3/2
cu <- (u+1)
mcu <- mean(cu)
hu <- 1/(u+1)

b <- (1-log(2)*(1+1/2))/(1/12)

Icv <- mean(hu) - b*(mcu - ecu)
Icv
#(c)
Ihat
Icv
vIhat <- var(hu)/n
vIhat
rho2 <- abs((1-log(2)*(1+1/2)))/sqrt((1/12)*(1/2-(log(2))^2))
vIcv <- vIhat*(1-rho2)
# the variance of Icv less than that of Ihat 98.4%

#(d)
If we can chose the another function of x, i.e., g(x), such that correlation coefficient of 
g(x) and h(x) is larger than that one of the previous part, then we can obtain a new
Icv_g which has smaller variance comparing to the Icv above
