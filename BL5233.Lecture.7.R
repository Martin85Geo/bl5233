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

# Negative bars of different color
csub <- subset(climate, Source=="Berkeley" & Year >= 1900) 
csub$pos <- csub$Anomaly10y >= 0
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + 
  geom_bar(stat="identity", position="identity")

# Change colors manually
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

# Adding labels to a graph
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + 
  geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=-0.4)

# Dotplots
tophit <- tophitters2001[1:25, ]
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) +
  theme_bw() + theme(panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.grid.major.y = element_line(colour="grey100", linetype="dashed"))

# Making a line graph with multiple lines
library(plyr)
data(ToothGrowth)
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len)) 
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()

ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()

ggplot(tg, aes(x=dose, y=length, fill=supp)) +
  geom_line() + geom_point(size=4, shape=21)

# Making a stacked area graph
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) + scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

# Adding a confidence region
clim <- subset(climate, Source == "Berkeley", select=c("Year", "Anomaly10y", "Unc10y"))
# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) + 
  geom_ribbon(aes(ymin=Anomaly10y- Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2) + 
  geom_line()

# Making a scatterplot
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=21)
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) + geom_point()

ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) + 
  geom_point() + 
  scale_shape_manual(values= c(1,2)) + 
  scale_colour_manual(values= c("red","blue"))

ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()

# Mapping a Continuous Variable to Color or Size: both continuous and categorical
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
  geom_point(alpha=.5) +
  scale_size_area() + 
  scale_colour_brewer(palette="Set1")

# Dealing with overplotting
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point()
sp + geom_point(alpha=.1) 
sp + geom_point(alpha=.01)

sp + stat_bin2d()
sp + stat_bin2d(bins=50) + 
  scale_fill_gradient(low="lightb lue", high="red", limits=c(0, 6000))

# Adding fitted regression model lines
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) 
sp + geom_point() + stat_smooth(method=lm)
# 99% confidence region
sp + geom_point() + stat_smooth(method=lm, level=0.99)

sp + geom_point(colour="grey60") + stat_smooth()
sp + geom_point(colour="grey60") + stat_smooth(method=loess)

# GLM with binomial errors
library(MASS) # For the data set
b <- biopsy
b$classn[b$class=="benign"] <- 0 
b$classn[b$class=="malignant"] <- 1
ggplot(b, aes(x=V1, y=classn)) + 
  geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4, shape=21, size=1.5) + 
  stat_smooth(method=glm, method.args = list(family = "binomial"))

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + 
  geom_point() + 
  scale_colour_brewer(palette="Set 1")
sps + geom_smooth()

# Adding fitted lines from an existing model
model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predicted <- data.frame(ageYear=seq(xmin, xmax, length.out=100)) 
predicted$heightIn <- predict(model, predicted)
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(colour="grey40")
sp + geom_line(data=predicted, size=1)

# Making multiple histograms with grouped data
library(MASS)
ggplot(birthwt, aes(x=bwt)) +
  geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke) levels(birthwt1$smoke) 
library(plyr) # For the revalue() function
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke")) 
ggplot(birthwt1, aes(x=bwt)) + 
  geom_histogram(fill="white", colour="black") + 
  facet_grid(smoke ~ .)

# Making a violin plot
p <- ggplot(heightweight, aes(x=sex, y=heightIn))
p + geom_violin()
p + geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) + 
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)

# Adding lines
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
# Add horizontal and vertical lines
p + geom_hline(yintercept=60) + geom_vline(xintercept=14)
# Add angled line
p + geom_abline(intercept=37.4, slope=1.75)

# Adding error bars
# With a bar graph
a = ggplot(ce, aes(x=Date, y=Weight)) + 
  geom_bar(stat= "identity",fill="white", colour="black") + 
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)
# With a line graph
b = ggplot(ce, aes(x=Date, y=Weight)) + geom_line(aes(group=1)) + geom_point(size=4) + geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat= "identity",position="dodge") + 
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), position=position_dodge(0.9), width=.2)

# Using grid package to put plots together
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1))
print(b, vp = vplayout(1, 2))

# Swapping x- and y-axes
data(PlantGrowth)
PlantGrowth <- as.data.frame(data(PlantGrowth))
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot() ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot() + coord_flip()

# Splitting data into subplots with facets
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
# Faceted by drv, in vertically 40 arranged subpanels
p + facet_grid(drv ~ .)


