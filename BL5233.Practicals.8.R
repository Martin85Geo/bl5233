# BL5233: Tutorial 6

# Exercise A. Let’s look at a presence-absence type dataset. The example looks at the 
# presence of a trypanosome blood parasite in cod. The dependent variable is “Prevalence”
# (1 if infected and 0 otherwise). The explanatory variables are the area, year and depth
# the fish where caught at, the length weight and age of the fish.

# 1. Read the dataset ParasiteCod in. Year and area should be factors, convert the
# variables. Remove the missing data from the dataset (hint: use na.omit). 

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="ParasiteCod.txt", header=TRUE, sep="\t")
data$Year <- as.factor(data$Year)
data$Area <- as.factor(data$Area)
data <- na.omit(data) # Original 1254 entries reduced to 1191 entries after removing missing data


