# BL5233: Tutorial 10

# Exercise A. The data used in this exercise comprise statistical characterizations of the
# vocalizations of 234 individual birds of the species Macropygia amboinensis (the Brown 
# Cuckoo-Dove; thanks to Frank Rheindt for the data and Ryan Chisholm for the exercise). 
# The goal of the exercise is to see whether the individual birds can be separated into 
# groups based on the characteristics of their vocalizations.

data <- read.table(file="vocalisation_data.txt", header=TRUE, sep="\t")
names(data)

# 1. Do a principal components analysis on the vocalization data (note that the dataset
# is also tab delimited). What proportion of the variance is explained by the first 
# principal component? How many principal components do you need to explain ~ 95% of the 
# variance?