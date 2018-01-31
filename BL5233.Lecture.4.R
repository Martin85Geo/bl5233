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