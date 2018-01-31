# On regression


# Linear regression
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data1 <- read.table(file="regression.txt", header=TRUE, sep="\t")

# Regression
model1 <- lm(growth ~ tannin, data=data)
summary(model1)
anova(model1)

# Prediction
plot(data$tannin, data$growth, xlab="tannin", ylab="growth")
lines(seq(0,8,0.1), predict(model1,list(tannin=seq(0,8,0.1))))

# Model checking
par(mfrow=c(2,2))
plot(model1)


# Piecewise regression
data2 <- read.table(file="spArea.txt", header=TRUE, sep="\t")
names(data2)
plot(log(data2$Species)~log(data2$Area), pch=16, xlab="Area", ylab="Species")

model2<- lm(log(Species)~log(Area),data=data2)
plot(log(data2$Area),resid(model2))

model2<-lm(log(Species)~(log(Area)*(Area<148))+(log(Area)*(Area>=148)),data=data2)
area=sort(unique(data2$Area))
plot(log(data2$Species)~log(data2$Area ))
lines(log(area), predict(model2, list(Area=area)))

    # Homework: Then, compare the two models by using AIC or ANOVA test


# Non-linear regression
deer <- read.table(file="jaws.txt", header=TRUE, sep="\t")
names(deer)
plot(deer$age,deer$bone)

# Try asymptotic exponential model
model3 <- nls(bone~a-b*exp(-c*age), start=list(a=120, b=115, c=0.065), data=deer)
summary(model3)
# Try Michaelis-Menten model
model4 <-nls(bone~a*age/(1+b*age), start=list(a=10.6, b=0.06), data=deer)
summary(model4)
# Compare the two models (which one is better)
AIC(model3, model4) 

  # Note: Result of AIC or the difference between two AICs is very close... 
  # Then, think about how significant is the difference?


# Multiple regression
data3 <- read.table(file="ozone.data.txt", header=TRUE, sep="\t")
names(data3)
pairs(data3, panel=panel.smooth)

model5 <- lm(ozone~temp*wind*rad +I(rad^2)+I(temp^2)+I(wind^2), data=data3)
summary(model5)
# Remove most complex model (three-way interaction) first
model5a <- update(model5, ~. -temp:wind:rad)
anova(model5, model5a)
summary(model5a)

  # Homework: keep removing complex models by step-wise simplification

# Stepwise selection of models
modelStep <- step(model5)
summary(modelStep)

  # Homework: model selection using information theory


