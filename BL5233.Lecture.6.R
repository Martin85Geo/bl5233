# On Generalised Linear Models

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
clusters <- read.table(file="clusters.txt", header=TRUE, sep="\t")
names(clusters)

# GLMs for count data
model1 <- glm(Cancers~Distance, poisson, data=clusters)
summary(model1)

# Fit the model using a quasi-Poisson to deal with the overdispersion
model2 <- glm(Cancers~Distance, quasipoisson, data=clusters)
summary(model2)

# Plot the model
xv <- seq(0,100,0.1)
yv <- predict(model2, list(Distance=xv))
lines(xv, exp(yv))

# Analysis of deviance
count <- read.table(file="cells.txt", header=TRUE, sep="\t")
names(count)
table(count$cells) # Frequencies in the counts of a variable
tapply(count$cells, count$smoker, mean)

# Create the model
model1 <- glm(cells~smoker*sex*age*weight, poisson, data=count)
summary(model1)

# Since residual deviance is larger that the degrees of freedom, then it is a problem of
# overdispersion. Hence we need to re-fit the model using quasi-Poisson to account for
# overdispersion
model2 <- glm(cells~smoker*sex*age*weight, quasipoisson, data=count)
summary(model2)

# Remove the most complicated terms by step-wise simplification approach
model3 <- update(model2,~. -smoker:sex:age:weight) # 4-way interaction
anova(model2, model3, test="F")

model4 <- update(model3,~. - sex:age:weight) # 3-way interaction
anova(model3, model4, test="F")

    # Homework: continue to simplify model then test with ANOVA

# Plot to see interactions graphically
barplot(tapply(count$cells, list(count$smoker,count$weight),mean), col=c(2,7), beside=T)
legend(1.2,3.4,c("non","smoker"), fill=c(2,7))

    # Homework: follow-up using diagnostic plots

# ANCOVA with count data within the GLM framework
species <- read.table(file="species.txt", header=TRUE, sep="\t")
plot(species$Biomass, species$Species, type="n")
spp <- split(species$Species, species$pH)
bio <- split(species$Biomass, species$pH)

points(bio[[1]],spp[[1]],pch=16)
points(bio[[2]],spp[[2]],pch=17)
points(bio[[3]],spp[[3]])
legend("topright",1,legend=c( "high","low","mid"),pch=c(16, 17,1))

model1 <- glm(Species~ Biomass*pH, poisson, data=species)
summary(model1)

  # Note: there is no probelm of overdispersion, but one of the interactions is not
  # significant; hence need to simplify the model

model2 <- glm(Species~Biomass+pH, poisson, data=species)
anova(model1, model2, test="Chi")

# Plot the fit of the model
xv <- seq(0,10,0.1)
phv <- rep("high",length(xv))
yv <- predict(model1, list(pH=factor(phv), Biomass=xv), type="response")
lines(xv,yv)
phv <- rep("mid",length(xv))
yv <- predict(model1, list(pH=factor(phv), Biomass=xv), type="response")
lines(xv,yv)
phv <- rep("low",length(xv))
yv <- predict(model1, list(pH=factor(phv), Biomass=xv), type="response")
lines(xv,yv)

# Logistic regression with binomial errors
numbers <- read.table(file="sexratio.txt", header=TRUE)
numbers
par(mfrow=c(1,2))
p <- numbers$males/(numbers$males+numbers$females)
plot(numbers$density, p, ylab="Proportion male")
plot(log(numbers$density), p, ylab="Proportion male")

y <- cbind(numbers$males, numbers$females)
model <- glm(y~density, binomial, data=numbers)
model

# Note: theresidual deviance is grater than the degrees of freedom; hence we try
# transforming the density with logs to improve the fit. Then, do diagnostic plots.

par(mfrow=c(1,1))
xv <- seq(0,6,0.1)
plot(log(numbers$density), p, ylab="Proportion male")
lines(xv, predict(model, list(density=exp(xv)), type="response"))

# Proportion data with categorical explanatory variables
germination <- read.table(file="germination.txt", header=TRUE)
names(germination)

y <- cbind(germination$count, germination$sample - germination$count)
levels(germination$Orobanche)

model <- glm(y ~ Orobanche * extract, binomial, data=germination)
summary(model)
# Correct for overdispersion
model <- glm(y ~ Orobanche * extract, quasibinomial, data = germination)
summary(model)
# Remove interactions
model2 <- update(model,~.-Orobanche:extract)
anova(model,model2,test="F")
summary(model2)

model3 <- update(model2,~.-Orobanche)
anova(model2,model3,test="F")
coef(model3)
1/(1+1/(exp(-0.5122))) # 0.3746779
1/(1+1/(exp(-0.5122+1.0574))) # 0.6330212
tapply(predict(model3,type="response"), germination$extract,mean)

  # Homework: then use diagnostic plots to test

