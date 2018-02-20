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