#1. a
#1.b) write on the paper

#1.c)
l <- function(theta,x){
  n <- length(x)
  lik <- -n*log(pi) - sum(log(1+(theta - x)^2))
  lik
}
dl <- function(theta,x){
  dlik <- -2*sum((theta-x)/(1+(theta-x)^2))
  dlik
}

d2l <- function(theta,x){
  d2lik <- -2*sum((1-(theta-x)^2)/(1+(theta-x)^2)^2)
  d2lik
}

x <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, 
       -1.05, -0.23, -0.07, 0.27, 1.77, 2.76, 
       3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)

theta <- seq(-100,100, length = 1000)
library("plyr")
ll <- laply(1:length(theta), function(i) l(theta[i],x))
dll <- laply(1:length(theta), function(i) dl(theta[i],x))

plot(theta, ll, type = "l", main = "Log Like Function", 
     xlab = "theta", ylab = "l")

plot(theta, dll, type = "l", main = "Log Like Function", 
     xlab = "theta", ylab = "l")

#1.d)
# the Newton Raphson method
nr <- function(dl, d2l, theta0, eps,...){
  cnt <- 0
  repeat{
    cnt <- cnt +1
    if (d2l(theta0, ...) == 0) {
      print (paste("iter = ", cnt, ", d2l = 0, change the initial values!"))
      return (NULL)
      break
    }
    theta1 <- theta0 - dl(theta0, ...)/d2l(theta0, ...)
    if (((abs(theta0 -theta1)/(abs(theta0)+0.000001) <= eps)&
           (abs(dl(theta1, ...)) <= eps))| (cnt ==10000)){
      print(paste("iter = ", cnt, "theta0 = ", theta0,
                  "theta1 = ", theta1,
                  "err = ", abs(theta0-theta1)/(abs(theta1)+0.000001),
                  "dl = ", abs(dl(theta1, ...))))
      break
    } 
    print(paste("iter = ", cnt, "theta0 = ", theta0,
                "theta1 = ", theta1,
                "err = ", abs(theta0-theta1)/(abs(theta1)+0.000001),
                "dl = ", abs(dl(theta1, ...))))
    
    theta0 <- theta1
    
  }
  return(theta1)
}

eps <- 0.000001
ini <- c(-11, -1, 0, 1.4, 4.1, 4.8, 7, 8, 38)

for(i in 1:length(ini)){
  print (paste("initial value = ", ini[i]))
  print(paste("solution = ", nr(dl, d2l, ini[i], eps, x )))
}


#e)
# the Fisher scoring method
fs <- function(dl, theta0, eps, n, ...){
  dl0 <- dl(theta = theta0, ...)
  cnt <- 0
  repeat{
    cnt <- cnt +1
    theta1 <- theta0 + dl(theta0, ...)/(n/2)
    if (((abs(theta0 -theta1)/(abs(theta0)+0.000001) <= eps)&
           (abs(dl(theta1, ...)) <= eps))| (cnt ==10000)){
      print(paste("iter = ", cnt, "theta0 = ", theta0,
                  "theta1 = ", theta1,
                  "err = ", abs(theta0-theta1)/(abs(theta1)+0.000001),
                  "dl = ", abs(dl(theta1, ...))))
      break
    } 
    theta0 <- theta1
    
  }
  return(theta1)
}

eps <- 0.000001
ini <- c(-11, -1, 0, 1.4, 4.1, 4.8, 7, 8, 38)

for(i in 1:length(ini)){
  print (paste("initial value = ", ini[i]))
  print(paste("solution = ", fs(dl,  ini[i], eps, length(x),x )))
}

#2.

# 2a)
f <- function(theta, x){
  sum(log((1-cos(x-theta))/(2*pi)))
}

x <- c(0.52,1.96, 2.22, 2.28, 2.28, 2.46, 2.50, 2.53, 2.54, 2.99, 3.47, 3.53, 3.70, 3.88, 3.91, 4.04, 4.06, 4.82, 4.85, 5.46)

thetaseq <- seq(-pi, pi, length = 1000)
library("plyr")
ff <- laply(1:length(thetaseq), function(i)f(thetaseq[i], x))
plot(thetaseq, ff, type = "l")

dff <- laply(1:length(thetaseq), function(i)df(thetaseq[i], x))
plot(thetaseq, dff, type = "l")

# 2b) moment estimate of theta
theta0 <- asin(mean(x)- pi)

# 2c)

nr <- function(df, d2f, theta0, eps,...){
  dl0 <- dl(theta = theta0, ...)
  d2l0 <- d2l(theta0, ...)
  cnt <- 0
  repeat{
    cnt <- cnt +1
    if (d2f(theta0, x) == 0) {
      print (paste("iter = ", cnt, ", d2f = 0, change the initial values!"))
      return (NULL)
      break
    }
    theta1 <- theta0 - df(theta0,...)/d2f(theta0, ...)
    if (((abs(theta0 -theta1)/(abs(theta0)+0.000001) <= eps))| (cnt ==10000)){
      print(paste("iter = ", cnt, "theta0 = ", theta0,
                  "theta1 = ", theta1,
                  "err = ", abs(theta0-theta1)/(abs(theta1)+0.000001),
                  "df = ", abs(df(theta1, ...))))
      break
    }
    print(paste("iter = ", cnt, "theta0 = ", theta0,
                "theta1 = ", theta1,
                "err = ", abs(theta0-theta1)/(abs(theta1)+0.000001),
                "df = ", abs(df(theta1, ...))))
    
    theta0 <- theta1
    
  }
  return(theta1)
}

df <- function(theta, x){ #theta <- 0.0148500730170354
  sum(1/tan((theta-x)/2))
}

d2f <- function(theta, x){
  sum(1/sin((theta-x)/2)^2)
}
nr(df, d2f,theta0 , 0.00001, x)

sol <- NULL
theta0 <- seq(-pi, pi, length = 200)
for(i in 1:length(theta0)){
sol[i] <- nr(df, d2f, theta0[i], 0.000000001,x)  
}

sol
table(round(sol,digits = 5))
