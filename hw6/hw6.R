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
           (abs(dl(theta1, ...)) <= eps))| (cnt ==1000)){
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
           (abs(dl(theta1, ...)) <= eps))| (cnt ==1000)){
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
    if (((abs(theta0 -theta1)/(abs(theta0)+0.000001) <= eps))| (cnt ==1000)){
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

#3a)
x <- c(.02, .06, .11, .22, .55, 1.1)
y <- c(47, 97, 123, 152, 191, 200)
xi <- 1/x
yi <- 1/y
est <- summary(lm(yi~xi))$coef[,1]
theta1 <- 1/est[1]
theta2 <- theta1*est[2]
theta0 <- c(theta1, theta2)
attr(theta0, "names") <- NULL
theta0

#3b)
# function f need to be minimize  
f <- function(theta, x, y){
  sum((y-theta[1]*x/(x+theta[2]))^2)
}

# derivative of f 

df <- function(theta, x, y){
  df1 <- -2*sum((y-theta[1]*x/(x+theta[2]))*x/(x+theta[2]))
  df2 <- 2*sum((y - theta[1]*x/(x+theta[2]))*(theta[1]*x/(x+theta[2])^2))
  c(df1, df2)
}


# second  derivative of f 

d2f <- function(theta, x, y){
  d2f11 <- 2*sum((x/(x+theta[2]))^2)
  d2f12 <- -2*sum(2*theta[1]*x^2/(x+theta[2])^3 - x*y/(x+theta[2])^2)
  d2f22 <- 2*sum(-2*theta[1]*y*x/(x+theta[2])^3 +
                   3 * (theta[1]*x)^2/(x+theta[2])^4)
  d2 <- matrix(c(d2f11, d2f12, 
          d2f12, d2f22), nrow = 2, byrow = T
          )
  d2
}

# the Newton Raphson method
nr2 <- function(dl, d2l, theta0, eps,...){
  cnt <- 0
  repeat{
    cnt <- cnt +1
    if (det(d2l(theta0, ...)) == 0) {
      print (paste("iter = ", cnt, ", d2l = 0, change the initial values!"))
      return (NULL)
      break
    }
    theta1 <- theta0 - solve(d2l(theta0, ...))%*%dl(theta0, ...)
    if ((  sum(abs(theta0 -theta1)/(abs(theta0)+0.000001) <= eps)==2 &
           sum((abs(dl(theta1, ...)) <= eps))==2)| (cnt ==1000)){
      print(paste("iter = ", cnt, "theta0 = ", theta0[1],", ",  theta0[2],
                  "theta1 = ", theta1[1], ", ", theta1[2],
                  "err = ", max(abs(theta0-theta1)/(abs(theta1)+0.000001)),
                  "dl = ", max(abs(dl(theta1, ...)))))
      break
    } 
#     print(paste("iter = ", cnt, "theta0 = ", theta0[1],", ",  theta0[2],
#                 "theta1 = ", theta1[1], ", ", theta1[2],
#                 "err = ", max(abs(theta0-theta1)/(abs(theta1)+0.000001)),
#                 "dl = ", max(abs(dl(theta1, ...)))))
    
    theta0 <- theta1
    
  }
  return(theta1)
}

theta0
out <- nr2(df, d2f, theta0, .00001, x, y)
out


#3c) Using steepest descent algorithm
steep <- function(f, df, theta0, eps, ...){
  alpha <- 1
  cnt <- 0
  repeat{
    cnt <- cnt +1
    theta1 <- theta0 - alpha*df(theta0, ...)
    if (f(theta1, ...) > f(theta0, ...)) alpha <- alpha/2
    if ((sum(abs(theta1-theta0) <= eps) == length(theta0)) &
          sum(abs(df(theta1, ...)) <= eps)) {
      print(paste0("iter = ", cnt, ", theta0 = ", theta0[1], " ", 
                   theta0[2], " ", 
                   "theta1 = ", theta1[1], "  ", theta1[2], " ",
                   "eps = ", max(abs(theta1-theta0)) ))
      break
    }
    if (cnt ==1000){
      print(paste0("iter = ", cnt, ", theta0 = ", theta0[1], " ",  theta0[2], " ", 
                   "theta1 = ", theta1[1], " ", theta1[2], " ",
                   "eps = ", max(abs(theta1-theta0)) ))
      print("too many iternation without convergence, change initial values!")
      break
    }
    theta0 <- theta1
  }
  return(theta1)
  }
out
steep(f, df, theta0, 0.0000001, x, y)

#d) Gauss-Newton Algorithm


# function f need to be minimize  
f0 <- function(theta, x, y){
  (y-theta[1]*x/(x+theta[2]))^2
}

# derivative of f 

df0 <- function(theta, x, y){
  df1 <- -2*((y-theta[1]*x/(x+theta[2]))*x/(x+theta[2]))
  df2 <- 2*((y - theta[1]*x/(x+theta[2]))*(theta[1]*x/(x+theta[2])^2))
  c(df1, df2)
}

gn <- function(f0, df0, theta0, eps, x, y){
  cnt <- 0
  repeat{
    cnt <- cnt + 1
    yp <- laply(1:length(y), function(i){
      y[i] - f0(theta0, x[i], y[i])
    })
    xp <- laply(1:length(x), function(i) df0(theta0, x[i], y[i]))
    u <- summary(lm (yp ~xp+0))$coef[,1]
    if (sum(abs(u) <= eps) == 2 | cnt ==1000){
      print(paste0("iter = ", cnt, " ", "theta = ", theta0[1], " ", 
            theta0[2], ", eps = ", max(abs(u)))) 
      break
    }
    theta0 <- theta0 + u
  }
  theta0
}
theta0 <- c(theta1, theta2)
attr(theta0, "names") <- NULL
out <- gn(f0, df0, theta0, 0.01, x,y)
out
#out <- gn(f0, df0, theta0, 0.0001, x,y)

#4) 
dist <- matrix(
  c(0,1,2,4,9,8,3,2,1,5,7,1,2,9,3,
    1,0,5,3,7,2,5,1,3,4,6,6,6,1,9,
    2,5,0,6,1,4,7,7,1,6,5,9,1,3,4,
    4,3,6,0,5,2,1,6,5,4,2,1,2,1,3,
    9,7,1,5,0,9,1,1,2,1,3,6,8,2,5,
    8,2,4,2,9,0,3,5,4,7,8,3,1,2,5,
    3,5,7,1,1,3,0,2,6,1,7,9,5,1,4,
    2,1,7,6,1,5,2,0,9,4,2,1,1,7,8,
    1,3,1,5,2,4,6,9,0,3,3,5,1,6,4,
    5,4,6,4,1,7,1,4,3,0,9,1,8,5,2,
    7,6,5,2,3,8,7,2,3,9,0,2,1,8,1,
    1,6,9,1,6,3,9,1,5,1,2,0,5,4,3,
    2,6,1,2,8,1,5,1,1,8,1,5,0,9,6,
    9,1,3,1,2,2,1,7,6,5,8,4,9,0,7,
    3,9,4,3,5,5,4,8,4,2,1,3,6,7,0
    ), ncol = 15, byrow = T)
dist

f <- function(theta, dist){
  n <- length(theta)
  d <- dist[theta[n], theta[1]]
  for (i in 1:(n-1)) d <- d + dist[theta[i], theta[i+1]]
  d
}



# travelling saleman problem 
# define a neighborhood 
N <- function(theta){
  n <-length(theta)
  cnt <- 1
  nb <- list()
  nb[[1]] <- theta
  if (any(sort(theta) != 1:n) ) {
    mes <- paste0("theta must be a permulation of 1:", n) 
    stop(mes)
  }
  for(i in 1:(n-1)){
    for(j in (i+1):n) {
      cnt <- cnt +1
      theta1 <- theta
      theta1[i] <- theta[j]
      theta1[j] <- theta[i]
      nb[[cnt]] <- theta1
    }
  }
  nb
}

sa  <- function(theta0, dist, t1, p, eps, maxstage, maxm){
  n <- dim(dist)[1]
  theta <- theta0
  outj <- NULL
  j <- 0
  repeat{
    j <- j +1
    outm <- NULL
    thetam <- list()
    
    m <- 0
    while(m < maxm){
      m <- m+1
      N.theta <- N(theta)
      nl <- length(N.theta)
      thetastar <- N.theta[[sample(nl, 1)]]
      delta <- f(thetastar, dist) - f(theta, dist)
      if (delta <= 0){
        theta <- thetastar
      } else{
        z <- rbinom(1, 1, exp(-delta/(p^(j-1)*t1)))
        theta <- z*thetastar + (1-z)*theta
      }
      thetam[[m]] <- theta
      outm[m] <- f(theta, dist)
    }
    ind <- NULL
    for (m in 1:maxm) ind[m] <-  (all(thetam[[1]]==thetam[[m]]))
    if (all(ind) & (f(theta, dist) == 17)){
      
      print(thetam)
      print(paste("stage = ", j, "min = ", f(theta, dist)))
      break
    }
    
    if (j ==maxstage){
      print(paste0("Max iteration met, stage = ", maxstage))
      print(paste("stage = ", j, "min = ", f(theta, dist)))
      print(thetam)
      print(paste("stage = ", j, "min = ", f(theta, dist)))
      break
    }
  }
  theta
}
theta0 <- c(4, 14, 6, 2, 8, 13, 11, 15, 10, 7, 5, 3, 9, 1, 12)
theta <- sa(theta0, dist, 400, .99, eps = .01, 10000, 200)
theta
f(theta, dist)
