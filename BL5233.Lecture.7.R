# On Plotting in R

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
Rconf <- read.table(file="R confidence.txt", header=TRUE, sep="\t")

# Plotting in Base R
plot(Confidence~Month, data=Rconf)

par(mar=c(5.1,4.1,4.1,2.1))

pa

