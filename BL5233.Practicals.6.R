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

# 3. Check whether the model has overdispersion.
summary(model1)

  # Result of summary shows residual deviance is higher than the degrees of freedom,
  # which indicates overdispersion (or extra unexplained variation in the response).
  # Hence, need to use a quasi-Poisson error structure to correct for overdispersion.

# 4. Fit a new model correcting for overdispersion.
model2 <- glm(Days ~ Eth*Sex*Age*Lrn, quasipoisson, data=quine)
summary(model2)

# 5. Simplify the model. To proceed to simplify the model we start removing the most 
# complicated terms. Neither AIC nor step() are defined for quasi-likelihood so we 
# simplify manually one step at a time.

model3a <- update(model2, ~. -Eth:Sex:Age:Lrn)
anova(model2, model3a, test="F") # p=0.6911 -> not significant hence can remove

model3b <- update(model3a, ~. -Sex:Age:Lrn)
anova(model3a, model3b, test="F") # p=0.1715 -> not significant hence can remove

model3c <- update(model3b, ~. -Eth:Age:Lrn)
anova(model3b, model3c, test="F") # p=0.08937

model3d <- update(model3b, ~. -Eth:Sex:Lrn)
anova(model3b, model3d, test="F") # p=0.0.0468

model3e <- update(model3b, ~. -Eth:Sex:Age)
anova(model3b, model3e, test="F") # p=0.6223 -> not significant hence can remove

model3f <- update(model3e, ~. -Age:Lrn)
anova(model3e, model3f, test="F") # p=0 
