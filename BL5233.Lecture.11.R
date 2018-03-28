# GLMM

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
# Squid <- read.table(file="Squid.txt", header=TRUE, sep="\t")
# names(Squid)

# Bacteria example with binary data
library(MASS)
data(bacteria)
names(bacteria)

bacteria$y <- 1*(bacteria$y=="y")

library(lme4)
model1 <- glmer(y~trt+(week|ID), family=binomial, nAGQ=1, data=bacteria)
summary(model1)
