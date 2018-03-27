# BL5233: Tutorial 8

# Exercise A. We are interested in the yield of 56 different varieties of wheat distributed
# across the world. The farms where the yield analysis were performed are located at 
# different latitudes and longitudes with some nearer to others, perhaps creating problems
# of spatial correlation. In addition, the plots where the experiments were carried out 
# were nested within blocks, perhaps also violating the assumption of independence.

# 1. Load package nlme. Read dataset “spatialdata” into R. Explore the names of the variables.

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="spatialdata.txt", header=TRUE, sep="\t")
names(data)

