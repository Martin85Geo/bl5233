# On Plotting in R

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
Rconf <- read.table(file="R confidence.txt", header=TRUE, sep="\t")

# Plotting in Base R
plot(Confidence~Month, data=Rconf)

# Other plot settings
par(mar=c(5.1,4.1,4.1,2.1))
par(family="serif",font=2)
plot(Confidence~Month, data=Rconf, col=3, font.axis =2, font.lab = 2)
par(cex=2)
par(bty="l")
par(mar=c(5.1,4.1,4.1,2.1))
plot(Confidence~Month, data=Rconf, bg="grey", family="serif", cex.lab=1.2, bty="l", pch=15)

# Save plot as pdf file
pdf("Fig1.pdf")
plot(Confidence~Month, data=Rconf)
dev.off()

# Save plot as tiff file
tiff(file ="Fig1.tiff", width = 2500, height = 1800, units = "px", res = 300)
plot(Confidence~Month,data=Rconf)
dev.off()

# Adding text
text(28,1,"refresher")

# Using ggplot2 and gcookbook
library(gcookbook)
library(ggplot2)
library(grid)




