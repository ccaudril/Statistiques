library(MASS)

#1
bottles <- read.csv("bottles.data")
head(bottles)

t.test(bottles$Volume, mu=500, alternative = "less", conf.level = 0.9)
  # rejet de Ho
t.test(bottles$Volume, mu=500, alternative = "less", conf.level = 0.95)
  # pas de rejet de Ho

#2
mm <- read.csv("MM.data")
head(mm)

prop.test(mm$Red,n=1713,p=(1/6)) # 0.1681261
prop.test(mm$Green,n=1713,p=(1/6)) # 0.1295972 
prop.test(mm$Blue,n=1713,p=(1/6)) # 0.1266783 
prop.test(mm$Orange,n=1713,p=(1/6)) # 0.1161705 
prop.test(mm$Yellow,n=1713,p=(1/6)) # 0.2410975 
prop.test(mm$Brown,n=1713,p=(1/6)) # 0.2183304 

#3
head(immer)

t.test(immer$Y1,immer$Y2,paired = TRUE, conf.level = 0.95)

#4
signe <- immer$Y1 < immer$Y2
nsuccess <- length(signe[signe])
n <- length(signe)

prop.test(nsuccess,n,p=0.5)
# on rejette donc Ho

#5
head(shoes)
var.test(shoes$A, shoes$B)

#6
t.test(shoes$A, shoes$B,var.equal = TRUE)
# pas de difference notable

#7
head(galaxies)
shapiro.test(galaxies)
# Ho rejetée => galaxies ne suit pas une loi normale

#8
delai<-read.csv("delai-data.data")
head(delai)
lambda <- 1/(mean(delai$delai))
lambda

ks.test(delai$delai, pexp, lambda)

#9
parfum <- data.frame(chocolat=c(100,350),vanille=c(120,200),fraise=c(60,90),row.names = c("hommes","femmes"))
head(parfum)

#10
chisq.test(parfum)

#11
ct <- chisq.test(parfum)

#12
head(ct$observed) #donnees observees
head(ct$expected) #effectifs theoriques si independance

#13
d2 <- sum((ct$expected - parfum)^2/ct$expected)
d2

#14
head(sleep)

x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]
t.test(x1, mu = 0, alternative = "greater")
  # pas de rejet de Ho
  # => pas d'effet sur la duree du sommeil
t.test(x2, mu = 0, alternative = "greater")
  # rejet de Ho
  # => effet sur la durée du sommeil



