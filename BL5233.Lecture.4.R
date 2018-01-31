# On regression

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="regression.txt", header=TRUE, sep="\t")

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

    # Then, compare the two models by using AIC or ANOVA test
