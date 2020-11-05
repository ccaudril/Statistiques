#1
mu <- 10
sigma <- 3
n <- 1000

chisq1 <- function(){
  x <- rnorm(n, mean = mu, sd = sigma)
  (n-1)*(sd(x)^2)/(sigma^2)
}

#2
echantillon <- replicate(1000,chisq1())
hist(echantillon, freq = FALSE)

#3
curve(dchisq(x, df = n-1), add = TRUE)

#4
student1 <- function(){
  x <- rnorm(n, mean = mu, sd = sigma)
  (mean(x)-mu)/(sigma/sqrt(n))
}
echantillon <- replicate(1000,student1())
hist(echantillon, freq = FALSE)
curve(dt(x, df = n-1), add = TRUE)

#6
alpha <- 0.05
x <- rnorm(n, mean = mu, sd = sigma)
mean(x)-sigma*(qnorm(1-alpha/2,mean=mu,sd=sigma))/sqrt(n)
mean(x)+sigma*(qnorm(1-alpha/2,mean=mu,sd=sigma))/sqrt(n)

#8
mean(x)-sd(x)*(qt(1-alpha/2,df = n-1))/sqrt(n)
mean(x)+sd(x)*(qt(1-alpha/2,df = n-1))/sqrt(n)

t.test(x, conf.level = 1-alpha)$conf.int

#9
gen_IC <- function(x, alpha){
  n <- length(x)
  mean(x) + c(-1,1)*sd(x)*(qt(1-alpha/2,df=n-1))/sqrt(n)
}

#10
IC <- replicate(100,gen_IC(rnorm(n, mean = mu, sd = sigma),0.05))

#11
source("~/UTC/SY02/TP4_Intervalle_de_confiance/src/utils.R")
plot_ICs(IC,mu,plot=TRUE)
  # environ 95% d'intervalles verts et 5% d'intervalles rouges

#12
IC <- replicate(100,gen_IC(rnorm(10, mean = mu, sd = sigma),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
IC <- replicate(100,gen_IC(rnorm(100, mean = mu, sd = sigma),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
IC <- replicate(100,gen_IC(rnorm(1000, mean = mu, sd = sigma),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
  # n augmente => largeur de l'echantillon diminue

#13
IC <- replicate(100,gen_IC(rnorm(100, mean = mu, sd = 1),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
IC <- replicate(100,gen_IC(rnorm(100, mean = mu, sd = 2),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
IC <- replicate(100,gen_IC(rnorm(100, mean = mu, sd = 5),0.05))
plot_ICs(IC,mu,plot=TRUE,xlim=c(9,11))
  # sd diminue => largeur de l'echantillon diminue

#14
inter <- function(n,param,alpha){
  IC <- gen_IC(rnorm(n, mean = param),alpha)
  param >= IC[1] & param <= IC[2]
}

t <- replicate(100,inter(100,10,0.05))
mean(t)

#15
slutsky <- function(p,n,k,alpha){
  verif <- function(){
    x <- rbinom(n,1,p)
    phat = mean(x)
    IC <- phat+c(-1,1)*qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
    p >= IC[1] & p <= IC[2]
  }
  mean(replicate(k,verif()))
}
slutsky(0.5,100,100,0.05)

#16
ns = floor(10^seq(1,4,length.out=30))
slpt <- sapply(ns,function(n) slutsky(0.02,n,10000,0.05))
plot(log10(ns),slpt,col="red")



