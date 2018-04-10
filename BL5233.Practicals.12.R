# BL5233: Tutorial 10

# Exercise A. The data used in this exercise comprise statistical characterizations of the
# vocalizations of 234 individual birds of the species Macropygia amboinensis (the Brown 
# Cuckoo-Dove; thanks to Frank Rheindt for the data and Ryan Chisholm for the exercise). 
# The goal of the exercise is to see whether the individual birds can be separated into 
# groups based on the characteristics of their vocalizations.

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="vocalisation_data.txt", header=TRUE, sep="\t")
names(data)

# 1. Do a principal components analysis on the vocalization data (note that the dataset
# is also tab delimited). What proportion of the variance is explained by the first 
# principal component? How many principal components do you need to explain ~ 95% of the 
# variance?

model0 <- prcomp(data, scale=F)
plot(model0$x[,1], model0$x[,2], pch=19, xlab="PC1", ylab= "PC2")
summary(model0)

# PC1 explains 31.3% of the variance. Five principal components (up to PC5) needed to 
# explain 99.7% of the variance.

# 2. Make a scree plot.

plot(model0)

# 3. Make a scatter plot of the first two principal components. Are there obvious 
# groupings to the data? How many groups would you say do the data split into? What does 
# this suggest biologically?

biplot(model0)

# 4. With which of the raw variables is principal component 1 most strongly associated? 
# With which is principal component 2 associated? Again, try to give this result a 
# biological interpretation.

# PC1: positively associated with notelength_second
# PC2: negatively associated with notelength_third, notelength_first

# 5. Do a cluster analysis on the vocalization data. Compare this with an analysis of the
# first and second principal components in 1 (extract the clustering predictions and use 
# them to put different colors in the graph of the principal components). Why are there 
# differences? Why is the grouping of all the data differ from the grouping expected from 
# the plot of the principal components?

model1 <- kmeans(data,7)
summary(model1)
names(model1)
plot(data$hifreq, data$lofreq, col=model1[[1]], main='kmeans 5 groups')
plot(data$hifreq, data$freqrange, col=model1[[1]], main='kmeans 5 groups')
plot(data$lofreq, data$freqrange, col=model1[[1]], main='kmeans 5 groups')

# Using hclust
model2 <- hclust(dist(data), method="complete")
plot(model2)

biplot(model0$x[,1],model0$x[,2], col=model1$cluster)

