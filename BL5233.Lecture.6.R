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
