# GLMs with binary data, survival analysis and Generalized Additive Models

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
island <- read.table(file="isolation.txt", header=TRUE, sep="\t")

# Example with binary data
model1 <- glm(incidence ~ area*isolation, binomial, data=island)
summary(model1)

model2 <- glm(incidence ~ area+isolation, binomial, data=island)
anova(model1, model2, test="Chi")
summary(model2)

modela <- glm(incidence~area, binomial, data=island)
modeli <- glm(incidence~isolation, binomial, data=island)
par(mfrow=c(2,2))
xv <- seq(0,9,0.01)
yv <- predict(modela, list(area=xv), type="response")
plot(island$area, island$incidence)
lines(xv,yv)
xv2 <- seq(0,10,0.1)
yv2 <- predict(modeli, list(isolation=xv2),type="response")
plot(island$isolation, island$incidence)
lines(xv2, yv2)

# ANCOVA with binary response variable
infection <- read.table(file="infection.txt", header=TRUE, sep="\t")
names(infection)

model <- glm(infected ~ age*weight*sex, family=binomial, data=infection)
summary(model)

model2 <- step(model)
summary(model2)

model3 <- update(model2,~.-age:weight)
anova(model2, model3, test="Chi")

model4 <- update(model2,~.-age:sex)
anova(model2, model4, test="Chi")

model6 <- glm(infected ~ age+weight+sex+I(weight^2)+I(age^2), family=binomial, data=infection)
summary(model6)

model9 <- glm(infected ~ sex+age+I(age^2)+I((weight- 8)*(weight>8)), family=binomial, data=infection)
summary(model9)

model8 <- glm(infected ~ sex+age+I(age^2)+I((weight-12)*(weight>12)), family=binomial, data=infection)
summary(model8)

model9 <- update(model8,~.-sex)
anova(model8,model9,test="Chi")
summary(model9)

library(arm)
par(mfrow=c(1,1))
binnedplot(predict(model, type="response"), resid(model, type="response"))


# Survival analysis
deathRateA = 0.1
deathRateB = 0.2
n = 1000
days = 100
death1<-numeric(n)
for (i in 1:n) { 
  rnos<-runif(days)
  death1[i] <- which(rnos<= deathRateA)[1]
}
death2<-numeric(n)
for (i in 1:n) {
  rnos<-runif(days)
  death2[i] <- which(rnos<= deathRateB)[1]
}

1/mean(death1)
1/mean(death2)

death <- c(death1,death2)
factory <- factor(c(rep(1,n), rep(2,n)))
plot(factory, death)

model1 <- glm(death~factory, Gamma)
summary(model1)

survivingA = n-cumsum(table(death1))
survivingB = n-cumsum(table(death2))
plot(survivingA~sort(unique(death1)),type="l",col="blue",xlab="day",ylab="number surviving")
lines(survivingB~sort(unique(death2)),type="l",col="red")
legend(40,800,c("sps A","sps B"),lty=c(1,1), col=c("blue","red"))

# Survivorshp types
costRate = rep(0.2,days)

failRateIncr = costRate*exp(0.01*daysSeq)
death1 <-numeric(n)
for (i in 1:n) {
  rnos <- runif(length(failRateIncr))
  death1[i] <- which(rnos <= failRateIncr)[1]
}
death2 <- numeric(n)
for (i in 1:n) { 
  rnos <- runif(length(costRate))
  death2[i] <- which(rnos <= costRate)[1]
}
death3 <-numeric(n)
for (i in 1:n) { 
  rnos<-runif(length(failRateDecr))
  death3[i]<- which(rnos <= failRateDecr)[1] 
}

surviving1 = n-cumsum(table(death1)) 
surviving2 = n-cumsum(table(death2))
surviving3 = n-cumsum(table(death3))
plot(log(surviving1+1)~sort(unique(death1)), type="l",col="blue",xlim=c (1,days), xlab="day",ylab="log of number surviving")
lines(log(surviving2+1)~sort(unique(death2)), type="l",col="red")
lines(log(surviving3+1)~sort(unique(death3)), type="l",col="black")

# Hazard, survivor and density functions
library(survival)
insects <- read.table(file="roaches.txt", header=TRUE, sep="\t")
names(roaches)

plot(survfit(Surv(insects$death,insects$status) ~ insects$group),
     lty=c(1,3,5),col=c("blue","orange","black"),
     ylab="Survivorship",xlab="Time")

model1 <- survreg(Surv(death,status) ~ weight*group, dist="exponential", data=insects)
summary(model1)

model2 <- survreg(Surv(death,status) ~ weight*group, data=insects)
summary(model2)

anova(model1, model2)
AIC(model1, model2)

model3 <- step(model2)
summary(model3)

model10 <- coxph(Surv(death,status) ~ weight*group,data=insects)
summary(model10)
model11 <- step(model10)
summary(model11)

tapply(insects$death, insects$group, mean)
23.08/8.02
23.08/14.42
plot(survfit(model11))
legend(35,0.8, c("Group A","Group B","Group C"), lty=c(2,1,2))


# Generalised Additive Models
library(mgcv)

# Bioluminescence Example
ISIT <- read.table(file="ISIT.txt", header=TRUE, sep="\t")
names(ISIT)
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
Sources16 <- ISIT$Sources[ISIT$Station == 16]
Depth16 <- ISIT$SampleDepth[ISIT$Station == 16]
plot(Depth16, Sources16, type = "p")
M3 <- gam(Sources16 ~ s(Depth16, fx = FALSE, k=-1, bs="cr"))
plot(M3, se=TRUE)

M3pred <- predict(M3, se=TRUE, type="response")
plot(Depth16, Sources16, type="p")
I1 <- order(Depth16)
lines(Depth16[I1], M3pred$fit[I1], lty=1)
lines(Depth16[I1], M3pred$fit[I1]+2*M3pred$se[I1], lty=2)
lines(Depth16[I1], M3pred$fit[I1]-2*M3pred$se[I1], lty=2)

# Try GAM with Ozone data
ozone.data <- read.table(file="ozone.data.txt", header=TRUE, sep="\t")
names(ozone.data)

model <- gam(ozone~s(rad)+s(temp)+s(wind), data=ozone.data)
summary(model)

model2 <- gam(ozone~s(temp)+s(wind), data=ozone.data) 
anova(model, model2, test="F")

model3 <- gam(ozone~s(temp)+s(wind)+s(rad )+s(wind,temp), data=ozone.data)
par(mfrow=c(2,2))
plot(model3, residuals=T, pch=16)
