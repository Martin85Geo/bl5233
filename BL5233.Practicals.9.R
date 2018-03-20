# BL5233: Tutorial 7

# Exercise A. The study looks at the number of follicles of more than 10mm of diameter in 
# the ovaries of 11 mares observed repeatedly through time. The time has been scaled so
# that ovulation occurs at time 0 and 1. Previous studies show that the relationship 
# between the number of follicles (y) and time (t) is well modeled as:
# y =a+bsin(2pt)+dcos(2pt)+eij
# This dataset has an added difficulty. The measures are not expected to be independent 
# between mares. We are going to jump ahead a bit and use also random effects to indicate 
# the nested structure (this is pretty similar to what we were doing with split-plot 
# ANOVAs). 

# 1. Read the dataset Ovary in. Create an exploratory plot in which the number of 
# follicles are plotted against time for each mare (hint: use xyplot() from the lattice 
# library).

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="Ovary.txt", header=TRUE, sep="\t")
names(data)
