# BL5233: Tutorial 8

# Exercise A. We are interested in the yield of 56 different varieties of wheat distributed
# across the world. The farms where the yield analysis were performed are located at 
# different latitudes and longitudes with some nearer to others, perhaps creating problems
# of spatial correlation. In addition, the plots where the experiments were carried out 
# were nested within blocks, perhaps also violating the assumption of independence.

# 1. Load package nlme. Read dataset “spatialdata” into R. Explore the names of the variables.

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
library(nlme)
data <- read.table(file="spatialdata.txt", header=TRUE, sep="\t")
names(data)

# 2. Develop a two sub-plot graph to explore the effects of latitude and longitude on yield.

model0 <- gls(yield~Block, data=data)
plot(Variogram(model0, form=~data$latitude + data$longitude))

# 3. Develop a barplot to study the difference in mean yield among wheat varieties. Also 
#the mean yield per block.

