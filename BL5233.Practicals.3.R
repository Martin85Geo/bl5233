# BL5233: Tutorial 2

# Exercise A 

# Read the dataset “ipomopsis” into R where Fruit is the dependent variable.
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
data <- read.table(file="ipomopsis.txt", header=TRUE, sep="\t")

# Propose models using the function lm() in which:

# 1. Fit the dependent variable against the main effects only
model1 <- lm(Fruit ~ Root+Grazing, data=data)

# 2. The same but forcing the model through the origin and log transforming the dependent variable
model2 <- lm(log(Fruit) ~ Root+Grazing - 1, data=data)

# 3. Fit the main effects and first order interactions
model3 <- lm(Fruit ~ (Root+Grazing)^2, data=data)

# 4. Fit up to two-way interactions and the quadratic term of Root
model4 <- lm(Fruit ~ (Root*Grazing) + I(Root^2), data=data)

# 5. Extract the AIC and BIC of the four models. Which model seems more supported?
modelAIC1 <- AIC(model1)
modelAIC2 <- AIC(model2)
modelAIC3 <- AIC(model3)
modelAIC4 <- AIC(model4)

modelBIC1 <- BIC(model1)
modelBIC2 <- BIC(model2)
modelBIC3 <- BIC(model3)
modelBIC4 <- BIC(model4)

# 6. Produce a summary of each of the models and check what is the value of the 
# coefficient for Root size. Then do this while putting the models in a list.
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# 7. Extract the coefficients from all models and put them in a list.
listModels <- list(model1, model2, model3, model4)

