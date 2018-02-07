# On ANOVA


# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
dataY <- read.table(file="yields.txt", header=TRUE, sep="\t")

# Arrange variables
y <- c(dataY$sand,dataY$clay,dataY$loam)
soil <- factor(rep(1:3,c(10,10,10)))
cbind(y, soil)

# Plot
plot(soil,y,names=c("sand","clay","loam"), ylab="yield")

# ANOVA
model1 <- aov(y~soil)
summary(model1)
plot(model1)
summary.lm(aov(y~soil))

# Factorial experiments
weights <- read.table(file="growth.txt", header=TRUE, sep="\t")

barplot(tapply(weights$gain, list(weights$diet, weights$supplement), mean),
        beside=T, ylim=c(0,30), col=rainbow(3), legend.text=T, args.legend=list(x="top"))

tapply(weights$gain, list(weights$diet, weights$supplement), mean)

model2 <- aov(gain~diet*supplement, data=weights)
summary(model2)

summary.lm(model2)

# Main effects only, no interactions
model3 <- aov(gain~diet+supplement, data=weights) 
summary.lm(model3)

# Contrasts

clip <- read.table(file="clipping.txt", header=TRUE, sep="\t")
names(clip)
plot(clip$clipping, clip$biomass ,xlab="Competition treatment", ylab="Biomass")

