library(MASS)
head(painters)
 #Composition Drawing Colour Expression School
 #Da Udine               10       8     16          3      A
 #Da Vinci               15      16      4         14      A
 #Del Piombo              8      13     16          7      A
 #Del Sarto              12      16      9          8      A
 #Fr. Penni               0      15      8          0      A
 #Guilio Romano          15      16      4         14      A

#1
hist(painters$Composition, main = "COmposition", xlab = "Note")
hist(painters$Drawing, main = "Dessin", xlab = "Note")
hist(painters$Colour, main = "Couleur", xlab = "Note")
hist(painters$Expression, main = "Expression", xlab = "Note")

#2
moyenne <- (painters[,1]+painters[,2]+painters[,3]+painters[,4])/4
moyenne

#3
n <- length(moyenne)
moyenne_empirique <- sum(moyenne)/n
moyenne_empirique
variance <- sum((moyenne-moyenne_empirique)^2/n)
variance
ecart_type <- sqrt(variance)
ecart_type
variance_corrigee <- (n/(n-1))*variance
variance_corrigee
ecart_type_corrige <- sqrt(variance_corrigee)
ecart_type_corrige

#4
mean(moyenne)
var(moyenne) # variance corrigee
sd(moyenne) # ecart-type corrige
var(moyenne)*(n-1)/n #variance
sd(moyenne) * sqrt((n-1)/n) # ecart-type

#5
hist(moyenne, main = "Moyennes")
        # La moyenne semble en accord avec une loi normale.

#6
  #1
  1 - pnorm(3)
  #2
  pnorm(42, mean = 35, sd = 6)
  #3
  1 - ( pnorm(40,mean = 35, sd = 6) + 1 - pnorm(50, mean = 35, sd = 6) )
  #4
  dbinom(4,5,0.5)
  dbinom(9,10,0.5)
  dbinom(29,30,0.5)
  #5
  1 - pbinom(14,20,0.5)
  #6
  pbinom(15, 20, 0.5) - pbinom(10,20,0.5)

#7
  alpha <- c(0.05, 0.1, 0.9)
  #1
  qnorm(alpha)
  #2
  qchisq(alpha, 10)
  #3
  qt(alpha,5)
  #4
  qf(alpha,2,5)

#8
  dloi <- function(x, b){
          a <- 2 / (b*b)
          f <- a*x
          f[x<0] <- 0
          f[x>b] <- 0
          return (f)
  }
  dloi(c(1,2,3),1)

#9
  dloi(c(-1,0,1,2,3,4,5),3)
  curve(dloi(x,3), from = -1, to = 5)
  
#10
  ploi <- function(x,b){
          f <- (x*x)/(b*b)
          f[x<0] <- 0
          f[x>=b] <- 1
          return(f)
  }
  
#11
  curve(ploi(x,3), from = -5, to = 5)

#12
  qloi <- function(alpha2,b){
          f <- b*sqrt(alpha)
          f[alpha2==0] <- 0
          f[alpha2==1] <- b
          return(f)
  }
  
#13
  # sur papier
  
#14
  rloi <- function(n,b){
          echan <- qloi(runif(n),b)
          return(echan)
  }
  
#15
  rloi(10,3)
  hist(rloi(10,3))
  hist(rloi(50,3))
  hist(rloi(100,3))
  hist(rloi(1000,3))


  
  
  
    
