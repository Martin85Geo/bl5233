# BL5233: Tutorial 6

# Exercise A. Let’s look at a presence-absence type dataset. The example looks at the 
# presence of a trypanosome blood parasite in cod. The dependent variable is “Prevalence”
# (1 if infected and 0 otherwise). The explanatory variables are the area, year and depth
# the fish where caught at, the length weight and age of the fish.

# 1. Read the dataset ParasiteCod in. Year and area should be factors, convert the
# variables. Remove the missing data from the dataset (hint: use na.omit). 

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="ParasiteCod.txt", header=TRUE, sep="\t")
names(data)
data$Year <- as.factor(data$Year)
data$Area <- as.factor(data$Area)
data <- na.omit(data) # Original 1254 entries reduced to 1191 entries after removing missing data

# 2. Some of the variables are highly correlated. Perform a multicollinearity analysis and
# remove those that are highly correlated (hint: you need to load package “car” and use the
# function “vif” on a GLM model fitted only to the main effects. Those variables with a 
# score above 3 should be removed one at a time).
library(car)
model0 <- glm(Prevalence ~ Area+Year+Depth+Length+Weight+Age, family=binomial, data=data)
summary(model0)
vif(model0) # Check for multicollinearity
model1 <- glm(Prevalence ~ Area+Year+Depth+Weight+Age, family=binomial, data=data) # Remove Length
summary(model1)
vif(model1)
model2 <- glm(Prevalence ~ Area+Year+Depth+Length+Age, family=binomial, data=data) # Remove Weight
summary(model2)
vif(model2)

# 3. Fit a GLM model to the data considering the main effects of the variables and the 
# one-way interaction between area and year.
model3 <- glm(Prevalence ~ Area*Year+Depth+Weight+Age, family=binomial, data=data) # One-way interaction of
summary(model3)

# 4. Try to simplify the model.
anova(model0, model1, test="Chi")
anova(model0, model2, test="Chi")
anova(model2, model3, test="Chi")

models <- list(model0, model1, model2, model3)
lapply(models, anova)

# 5. Fit the same model but now using GAM. Use smoothers to model Depth and Weight.


