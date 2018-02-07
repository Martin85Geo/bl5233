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

