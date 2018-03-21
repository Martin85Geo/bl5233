# Spatial correlation

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
Boreality <- read.table(file="Boreality.txt", header=TRUE, sep="\t")
names(Boreality)

Boreality$Bor<-sqrt(1000*(Boreality$nBor+1)/(Boreality$nTot))