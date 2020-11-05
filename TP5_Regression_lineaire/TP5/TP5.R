#1
donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6), vary = c(1.01, 1.44, 1.55, 2.1))
lm(vary ~ varx, data = donnees)

#Coefficents :
# â = 1.033
# b = 1.789 

#2
plot(donnees$varx,donnees$vary)
m <- lm(vary ~ varx, data = donnees)
a <- m$coefficients[1]
b <- m$coefficients[2]
abline(a,b)

#3
sum(m$residuals)
(a + b*mean(donnees$varx))
mean(donnees$vary)
# on a bien l'egalite entre ces 2 dernieres donnees

#4
S2Y <- sum((donnees$vary-mean(donnees$vary))^2)/length(donnees$vary)
Sreg <- sum((m$fitted.values-mean(donnees$vary))^2)/length(donnees$vary)
Sres <- sum((donnees$vary-m$fitted.values)^2)/length(donnees$vary)

S2Y
Sreg + Sres

#5

R <- Sreg/S2Y
R

SXY <- sum((m$fitted.values-mean(m$fitted.values))*(donnees$vary-mean(donnees$vary)))/length(donnees$vary)
S2X <- sum((m$fitted.values-mean(m$fitted.values))^2)/length(m$fitted.values) 

SXY/sqrt(S2X*S2Y)

#6
attach(anscombe)
r1 <- lm(y1 ~ x1)
summary(r1)
r2 <- lm(y2 ~ x2)
summary(r2)
r3 <- lm(y3 ~ x3)
summary(r3)
r4 <- lm(y4 ~ x4)
summary(r4)

  # toujours environ les mêmes coefficients

#7
qqnorm(r1$residuals)
qqline(r1$residuals)
qqnorm(r2$residuals)
qqline(r2$residuals)
qqnorm(r3$residuals)
qqline(r3$residuals)
qqnorm(r4$residuals)
qqline(r4$residuals)

#8
plot(r1$fitted.values,r1$residuals) # independance
plot(r2$fitted.values,r2$residuals)
plot(r3$fitted.values,r3$residuals)
plot(r4$fitted.values,r4$residuals)

#9
setwd("C:/Users/camil/Documents/UTC/SY02/TP5_Regression_lineaire/data")
X<-read.csv("hooker-data.data")

rh <- lm(X$Pression ~ X$Temp, X)
summary(rh)
qqnorm(rh$residuals)
qqline(rh$residuals)
plot(rh$fitted.values,rh$residuals)

#10
confint(object = rh,level = 0.99)

#11
newdata <- data.frame(Temp = c(97,100))
#predict(rh, newdata, interval="confidence")

#12
setwd("C:/Users/camil/Documents/UTC/SY02/TP5_Regression_lineaire/data")
X<-read.csv("moore-data.data")

rm <- lm(log(Transistor.count) ~ Date.of.introduction, data = X)
summary(rm)

plot(X$Date.of.introduction, log(X$Transistor.count))

confint(rm, "Date of introduction")

newdata <- data.frame(Date.of.introduction = c(2018))
exp(predict(rm, newdata, interval="confidence"))

















