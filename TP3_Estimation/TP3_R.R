runifa <- function(n){
  if(!exists("param"))param <<- sample(10:20, 1)
  runif(n,min=0,max=param)
}

#1
# â = 2 * X_

#2
estim <- function(x){
  return (2*mean(x))
}

#3
estim(runifa(100))
  # environ 13

#4
a <- replicate(1000, estim(runifa(100)))
boxplot(a)

#5
# â(k)= (m(k+1))^(1/k)

estim2 <- function(x,k){
  f <- ((k+1)*mean(x^k))^(1/k)
  return(f)
}

boxplot(replicate(1000, estim2(runifa(100),1)))
boxplot(replicate(1000, estim2(runifa(100),2)))
boxplot(replicate(1000, estim2(runifa(100),3)))
boxplot(replicate(1000, estim2(runifa(100),5)))
boxplot(replicate(1000, estim2(runifa(100),10)))


#6
runknown <- function(n){
  bn <- rbinom(n,1,0.2)
  bn*rnorm(n,mean=-4,sd=1)+(1-bn)*rnorm(n,mean=10,sd=1)
}
x <- runknown(1000)
mean(x)
sd(x)*sd(x)

#7
hist(x)

#8
plot(ecdf(x))

#9
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))

#10
x <- runknown(1000)
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))
x <- runknown(1000)
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))
x <- runknown(1000)
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))
x <- runknown(1000)
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))
x <- runknown(1000)
(mean(x)-7.2)/(sqrt(32.36)/sqrt(1000))
# distribution centrée en zéro avec un petit écart-type

#11
random.T <- function(n){
  x <- runknown(n)
  real <- (mean(x)-7.2)/(sqrt(32.36)/sqrt(n))
  return(real)
}
t.1000 <- replicate(1000,random.T(1000))
mean(t.1000)
sd(t.1000)
# on a bien un résultat proche de N(0;1)

#12
plot(ecdf(t.1000))

#13
curve(pnorm,add=TRUE)

#14
t <- replicate(1000,random.T(1))
plot(ecdf(t))
curve(pnorm,add=TRUE)

t <- replicate(1000,random.T(2))
plot(ecdf(t))
curve(pnorm,add=TRUE)

t <- replicate(1000,random.T(5))
plot(ecdf(t))
curve(pnorm,add=TRUE)

t <- replicate(1000,random.T(10))
plot(ecdf(t))
curve(pnorm,add=TRUE)

t <- replicate(1000,random.T(20))
plot(ecdf(t))
curve(pnorm,add=TRUE)

t <- replicate(1000,random.T(50))
plot(ecdf(t))
curve(pnorm,add=TRUE)
# convergence

#15
f <- function(lambda,x){
  dexp(x, rate = lambda)
}

#16
L <- function(lambda,x){
  prod(f(lambda,x))
}

#17
logL <- function(lambda,x){
  sum(log(f(lambda,x)))
}

#18
x <- rexp(100,rate=3)
logL(3.1,x)
logL(2.8,x)

#19
  #lambdas <- seq(0, 6, 0.01)
    # lambdas est composé des nombres entre 0 et 6 avec un pas de 0,001
  #logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
    # retourne une liste de la taille de "lambdas" composée de valeurs
    # aléatoires à qui on a appliqué "logL"
  #plot(lambdas, logL.lambdas, type = "l")
    # dessine la liste précédente en fonction de lambdas

#20
opt<-optimize(logL,lower=-2,upper=8,maximum=TRUE,x=rexp(100,rate=3))
opt$maximum

#21
sim.EMV <- function(){
  x <- rexp(100,rate=3)
  opt<-optimize(logL,lower=-2,upper=8,maximum=TRUE,x=x)
  return(opt$maximum)
}

#22
t <- replicate(10000,sim.EMV())

#23
mean(t)
vr(t)

#24
mean(t)-3
(100/99)*3-3

#25
install.packages("pracma")
library(pracma)

#26
sim.Fischer <- function(){
  x<-rexp(100,rate=3)
  logLx <- function(lambda) logL(lambda,x)
  (grad(logLx,3))^2
}

#27
inf_fischer <- mean(replicate(1000,sim.Fischer()))

#28
100/9

#29
1/inf_fischer
var(replicate(10000,sim.EMV()))

#30
grad2 <- function(f,x){
  df <- function(x){
    grad(f,x)
  }
  grad(df,x)
}

x<-rexp(100,rate=3)
logLx <- function(lambda) logL(lambda,x)
-grad2(logLx,3)






