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
plot(clip$clipping, clip$biomass, xlab="Competition Treatment", ylab="Biomass")
model4 <- aov(biomass~clipping, data = clip)
summary.lm(model4)

contrasts(clip$clipping) <- cbind(c(4,-1,-1,-1,-1), c(0,1,1,-1,-1), 
                                  c(0,0,0,1,-1), c(0,1,-1,0,0))
contrasts(clip$clipping)

model5 <- aov(biomass~clipping, data=clip)
summary.lm(model5)

# ANOVA for split plots
yields <- read.table(file="splityield.txt", header=TRUE, sep="\t")
names(yields)
model6 <- aov(yield~irrigation*density*fertilizer + Error(block/irrigation/density), data=yields)
summary(model6)
interaction.plot(yields$fertilizer, yields$irrigation, yields$yield)
interaction.plot(yields$density, yields$irrigation, yields$yield)

    # Visualise interactions through plots using package effects or package visreg

# ANCOVA
regrowth <- read.table(file="ipomopsis.txt", header=TRUE, sep="\t")
names(regrowth)

plot(regrowth$Root, regrowth$Fruit, pch=16+as.numeric(regrowth$Grazing),
     col=c("blue","red")[as.numeric(regrowth$Grazing)])
abline(lm(Fruit[Grazing=="Grazed"]~Root[Grazing=="Grazed"], data=regrowth),lty=2,col= "blue")
abline(lm(Fruit[Grazing=="Ungrazed"]~Root[Grazing=="Ungrazed"], data=regrowth),lty=2, col="red")

tapply(Fruit,Grazing, mean)
t.test(regrowth$Fruit~regrowth$Grazing)
model7 <- lm(Fruit~Grazing*Root, data=regrowth)
model8 <- update(model7, ~ .- Grazing:Root)
anova(model7,model8)

    # Homework: multiple comparisons