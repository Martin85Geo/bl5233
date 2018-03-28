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

model2 <- glmer(y~trt+(1|ID), family=binomial, nAGQ=1, data=bacteria)
anova(model1, model2)

model3 <- glmer(y~1+(week|ID), family=binomial, nAGQ=1, data=bacteria)
anova(model1, model3)

# Deer example
DeerEcervi <- read.table(file="DeerEcervi.txt", header=TRUE, sep="\t")
names(DeerEcervi)

#convert Ecervi into binary variable
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi>0] <- 1
#convert sex and farm into factors
DeerEcervi$fSex <- factor(DeerEcervi$Sex)
DeerEcervi$fFarm <- factor(DeerEcervi$Farm)
#center length
DeerEcervi$CLength <- DeerEcervi$Length -
mean(DeerEcervi$Length)

DE.PQL <- glmmPQL(Ecervi.01 ~ CLength * fSex, random=~1|fFarm, family=binomial, data=DeerEcervi)
summary(DE.PQL)
(DE.PQL$sigma)^2

library(lme4)
DE.lme4 <- glmer(Ecervi.01 ~ CLength * fSex +(1|fFarm), family=binomial, data=DeerEcervi)
summary(DE.lme4)

install.packages("glmmML")
library(glmmML)
DE.glmmML<-glmmML(Ecervi.01 ~ CLength * fSex, cluster = fFarm, family=binomial, data = DeerEcervi)
summary(DE.glmmML)

# Homework!
# Information-theoretic model selection: koalas in fragmented landscapes





