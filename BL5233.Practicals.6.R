# BL5233: Tutorial 5

# Exercise A. Let’s look at an example of Australian children learning. Response variable:
# number of days absent from school per year; explanatory variables: gender; culture 
# (aboriginal or not); age group (4 levels: F0, F1, F2, F3); learning status: average (AL)
# and slow (SL).

# Read the dataset “ipomopsis” into R where Fruit is the dependent variable.
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="ipomopsis.txt", header=TRUE, sep="\t")

