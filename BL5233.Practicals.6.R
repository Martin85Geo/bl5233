# BL5233: Tutorial 5

# Exercise A. Let’s look at an example of Australian children learning. Response variable:
# number of days absent from school per year; explanatory variables: gender; culture 
# (aboriginal or not); age group (4 levels: F0, F1, F2, F3); learning status: average (AL)
# and slow (SL).

# 1. Read the dataset “quine” from the MASS library into R. Check the names. Check the 
# levels of the categorical variables.

library(MASS)
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data(quine)
names(quine)
levels(quine$Eth)
levels(quine$Sex)
levels(quine$Age)
levels(quine$Lrn)

# 2. Given that the response variable is a count, choose the appropriate model. Fit a
# maximal model.

model1 <- glm(Days ~ Eth*Sex*Age*Lrn, poisson, data=quine)
summary(model1)

  # Result of summary shows residual deviance is higher than the degrees of freedom,
  # which indicates overdispersion (or extra unexplained variation in the response).
  # Hence, need to use a quasi-Poisson error structure to correct for overdispersion.

model2 <- glm(Days ~ Eth*Sex*Age*Lrn, poisson, data=quine)
summary(model1)



