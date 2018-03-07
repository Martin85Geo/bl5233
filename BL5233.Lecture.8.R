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