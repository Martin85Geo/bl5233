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
library(lattice)
xyplot(follicles ~ Time, data=data) # w/o groups
xyplot(follicles ~ Time, data=data, groups=factor(Mare), xlab="Time", ylab="Follicles") # w mares as group

# 2. We would expect the dependent variable to be correlated in time. Let’s start by 
# fitting a model that ignores temporal correlation. We start by fitting a linear 
# mixed-effects model (lme function from the nlme package that needs to be loaded). The
# only thing new is to introduce a new argument “random =~1|Mare”. This just means that 
# the errors are nested within mare.

library(nlme)
model0 <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), data=data, random=~1|Mare)
summary(model0) # AIC=1669.36

# 3. Investigate the autocorrelation structure of the residuals of the model just fitted
# and plot it.

E <- residuals(model0, type="normalized")
I <- data$follicles
plot(data$Time[I], E, ylab="residuals")
plot(ACF(model0), alpha=0.05)

# Note: the ACF plot indicates there is significant correlation for a lag of 1

model1 <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), data=data, random=~1|Mare, correlation=corAR1(form=~Time))
summary(model1) # AIC=1602.387

# Hence, model1 is improvement from model0 due to lower AIC
# Also, phi1=0.5929379

# 4. Fit different models considering different ARMA correlation structures with 
# autoregression and moving average coefficients ranging from 0 to 2. Compare the models
# with AIC and choose an adequate correlation structure.

