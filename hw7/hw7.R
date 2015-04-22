l(θ)=x1log(2+θ)+(x2 +x3)log(1−θ)+x4logθ+c.

#1a)
l <- function(theta, x){
  ll <- x[1]*log(2+theta) + (x[2]+x[3])*log(1-theta) +x[4]*log(theta)
  -ll
}

dl <- function(theta, x){
  dll <- x[1]/(2+theta) - (x[2] + x[3])/(1-theta) + x[4]/theta
  dll
}

d2l <- function(theta, x){
  d2ll <- -x[1]/(2+theta)^2  - (x[2]+x[3])/(1-theta)^2 - x[4]/theta^2
  d2ll
}

nr <- function(dl, d2l, theta0, eps, ...){
  cnt <- 0
  repeat{
    cnt <- cnt +1
    if (d2l(theta0, ...) ==0){
     stop("d2l = 0, change initial value!")
      break
      }else{
      theta1 <- theta0 - dl(theta0, ...)/d2l(theta0, ...)
    }
    if (abs(theta1 - theta0)/(abs(theta0)+0.001) < eps) {
      print(paste0("cnt = ", cnt, "theta1 = ", theta1, "theta0 = ", theta0, "eps  = ", eps))
      break
    }
    if (cnt == 10000) {
      print("too many iteration without convergence!")
      break
    }
    theta0 <- theta1
  }
  theta1
}


x <- c(125, 18, 20, 34)
nr(dl, d2l, theta0 = .4, eps = 0.00000001, x)

# or using optim( .4, l, x = x)$par

#1b) 
# x_12|x2, x3, x4 has negative binomial distribution NB(theta, x4)
# solving EM problem give the formula
theta0 <- 0.5
cnt <- 0
eps <- 0.00000001
repeat{
  cnt <- cnt + 1
  theta1 <- (x[1]*theta0/(2+theta0) + x[4])/(x[1]*theta0/(2+theta0) + x[4] + x[2] + x[3])
  if (abs(theta1 - theta0) <= eps) {
    break
  }
  theta0 <- theta1
  
}

theta1
#1.c) The solutions are the same for the two methods.

#2) 
truefunction<-function(x){
  t <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
  h <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
  temp <- 0
  for(i in 1:11) {
    temp <- temp + h[i]/2 * (1 + sign(x - t[i]))
  }
  return(temp)
}
n<-512
x<-(0:(n-1))/n
f<-truefunction(x)
set.seed(0401)
y<-f+rnorm(f)/3
plot(x,y)
lines(x,f)
x
# step 1: generating first generation 
# theta = (0, 1, 0, 1, 1,  ..., 0, 1) including (n-2)-elements 
# theta[i] = 1 implies that x[i+1] is the break point
# theta[i] = 0 implies that x[i+1] is not the break points

P <- 100
# first generation is a matrix of dimention P x (n-2)
f1 <- function(P){
  chro <- matrix(0, ncol = n-2, nrow = P)
  for (i in 1:P){
    #set.seed(i)
    chro[i, ] <- sample(c(0,1), n-2, replace = TRUE)
  }
  chro
}

fi1 <- f1(P)
chro <- fi1[1,]
## draw a parents
## MDL function 

score <- function(chro, method){
  n <- length(chro) + 2
  B <- sum(chro)+1
  bp <- x[which(chro == 1)+ 1]
  bp <- c(0, bp, 1)
  fi <- NULL
  ni <- NULL
  for (i in 1:B){
    ni[i] <- sum((bp[i]<=x) & (x < bp[i+1]))
    fi[i] <- mean(y[(bp[i]<=x) & (x < bp[i+1])])
  }
  fh <- function(xx){
    out <- 0
    for (i in 1:B){
      out <- out + fi[i]*((bp[i]<=xx) & (xx < bp[i+1]))
    }
    out
  }
  fhx <- fh(x)
  if(method == "mdl") res <- B*log(n) + 
    1/2*sum(log(ni)) + n/2*log(mean((y-fhx)^2))
  if(method == "aic") res <- n*log(mean((y-fhx)^2)) + 2*log(n)*B
  -res
}


current_generation <- fi1
method <- "aic"
cg_score <- apply(current_generation, 1, function(chro) score(chro, method) )
max(cg_score)
parents <- function(current_generation, cg_score){
  Pn <- dim(current_generation)
  P1 <- Pn[1]
  n <- Pn[2] + 2
  temp <- 1:P1/sum(1:P1)
  prob <- temp
  prob[sort(cg_score, index.return = T)$ix] <- temp
  ind_parents <- sample(P1, 1, prob = prob)
  return(current_generation[ind_parents, ])
}

#p <- parents(fi1, cg_score)

offsprings <- function(current_generation, cg_score){
  pcross <- rbinom(1, 1,0.95)
  if (pcross ==1){
    pa1 <- parents(current_generation, cg_score)
    pa2 <- parents(current_generation, cg_score)
    m <- length(pa1)
    ind_gene <- sample(m, 2)
    temp1 <- pa1[ind_gene[1]]
    pa1[ind_gene[1]] <- pa2[ind_gene[1]]
    pa2[ind_gene[1]] <- temp1
    
    temp2 <- pa1[ind_gene[2]]
    pa1[ind_gene[2]] <- pa2[ind_gene[2]]
    pa2[ind_gene[2]] <- temp2
    out <- rbind(pa1=pa1, pa2=pa2)
  }else{
    pa1 <- parents(current_generation, cg_score)
    m <- length(pa1)
    z <- rbinom(m,1, .05)
    pa1 <- z + (-1)^z*pa1
    out <- pa1
  }
out
}
# os <- offsprings(current_generation, cg_score)


new_generation <- function(current_generation, cg_score){
  next_generation <-  data.frame(Date=as.Date(character()),
                                 File=character(), 
                                 User=character(), 
                                 stringsAsFactors=FALSE) 
  repeat{
    os <- offsprings(current_generation, cg_score)
    next_generation <- rbind(next_generation, os)
    if (dim(next_generation)[1] %in% c(P, P-1)) break
  }
  
  next_generation <- rbind(next_generation, current_generation[which.max(cg_score),])
  next_generation
}

ga <- function(current_generation, method, max_iter=200, max_unchange=30){
  max_score <- NULL
  cnt <- 0
  repeat{
    cnt <- cnt+1
    print(cnt)
    cg_score <- apply(current_generation, 1, function(chro) score(chro, method) )
    next_generation <- new_generation(current_generation, cg_score)
    max_score[cnt] <- max(cg_score)
    print(max_score[cnt])
    if (cnt >max_iter){
      print("Exceed max iteration!")
      break
    }
    if ((cnt > max_unchange) && length(unique(max_score[(cnt-max_unchange):cnt])) == 1){
      print("Max score constant too many time!")
      break
    }
    current_generation <- next_generation
  }
  nt_score <- apply(next_generation, 1, function(chro) score(chro, method) )
  best_chro <- next_generation[which.max(nt_score),]
  return(best_chro)
  }

pm <- proc.time()
out <- ga(fi1, "aic")
proc.time <- pm