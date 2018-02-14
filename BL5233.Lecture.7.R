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

# Comparing plot in Base R and in ggplot2
plot(mtcars$wt, mtcars$mpg)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")

# Histogram
hist(mtcars$mpg, breaks=10)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)

# Boxplot
boxplot(len ~ supp, data = ToothGrowth)
  
boxplot(len ~ supp + dose, data = ToothGrowth)
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()

# Dodge makes the bars to be beside each other
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity",position="dodge")

# Change colors of the bar chart
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) + 
  geom_bar(stat="identity", colour="black") + 
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("State")
