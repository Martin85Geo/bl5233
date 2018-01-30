# BL5233: Tutorial 1

# Exercise A 

# Load library
library(dplyr)

# Set working directory
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")

# Load text data
data <- read.table(file="datasetTutorial1corrected.txt", header=TRUE, sep="\t")

# Create new vector
newVar <- seq(1,nrow(data),1)

# Combine new vector with text data
data2 <- mutate(data, newVar)

# Write combined data into a text file
write.table(data2, "data2.txt")

# Calculate mean and standard deviation of second column
mean(data2$Fruit)
sd(data2$Fruit)

# Subset new data
subFruit <- data2[data2$Fruit < 50,]
subGrazed <- subFruit[subFruit$Grazing == "Grazed",]

# Raise 2nd column to power of 3.5, then divide by log of 1st column, and round off result
Raised <- ((subGrazed$Fruit)^3.5 / log(subGrazed$Root))
Rounded <- round(Raised)

# Estimate the mean fruit weight for grazed versus ungrazed plants
Grazed <- data2[data2$Grazing=="Grazed", na.omit=TRUE] # Do this!
meanGrazed <- mean(Grazed$Fruit)
Ungrazed <- data2[data2$Grazing=="Ungrazed",]
meanUngrazed <- mean(Ungrazed$Fruit, na.omit=TRUE) # Do this! 

