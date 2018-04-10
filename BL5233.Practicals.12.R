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

model0 <- prcomp(data, scale=T)
plot(model0$x[,1], model0$x[,2], pch=19, xlab="PC1", ylab= "PC2")
summary(model0)

# PC1 explains 31.3% of the variance. Five principal components (up to PC5) needed to 
# explain 99.7% of the variance.

# 2. Make a scree plot.

plot(model0)
biplot(model0)
