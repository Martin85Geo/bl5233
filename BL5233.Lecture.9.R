# GLS

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
Squid <- read.table(file="Squid.txt", header=TRUE, sep="\t")
names(Squid)

Squid$fMONTH <- factor(Squid$MONTH)
M1 <- lm(Testisweight ~ DML*fMONTH, data=Squid)
par(mfrow=c(2, 2), mar=c(4, 4, 2, 2))

plot(M1, which=c(1), col=1, add.smooth=FALSE, caption = "")
plot(Squid$fMONTH, resid(M1), xlab="Month", ylab= "Residuals")
plot(Squid$DML, resid(M1), xlab="DML", ylab = "Residuals")

# Variance structures: fixed variance
library(nlme)
M.lm <- gls(Testisweight ~ DML*fMONTH, data=Squid)
vf1Fixed <- varFixed(~DML)
M.gls1 <- gls(Testisweight ~ DML*fMONTH, weights=vf1Fixed, data=Squid)
anova(M.lm, M.gls1)

vf2 <- varIdent(form= ~ 1 | fMONTH)
M.gls2 <- gls(Testisweight ~ DML*fMONTH, data =Squid, weights=vf2)
anova(M.lm, M.gls1, M.gls2)
summary(M.gls2)
varIdent(form= ~ 1 | fMONTH*factor(LOCATION))
E <- resid(M.lm)
coplot(E ~ DML | fMONTH, data=Squid)

# varPower
vf3 <- varPower(form =~ DML)
M.gls3 <- gls(Testisweight ~ DML * fMONTH, weights=vf3, data=Squid)
AIC(M.gls3)
vf4 <- varPower(form =~ DML | fMONTH)
M.gls4 <- gls(Testisweight ~ DML * fMONTH, weights=vf4, data=Squid)
AIC(M.gls4)
summary(M.gls4)

# varEXp
vf5 <- varExp(form =~ DML)
M.gls5 <- gls(Testisweight ~ DML * fMONTH, weights=vf5, data=Squid)
AIC(M.gls5)
vf5b <- varExp(form =~ DML|fMONTH)
M.gls5b <- gls(Testisweight ~ DML * fMONTH, weights=vf5b, data=Squid)
AIC(M.gls5b)

# varConstPower
vf6 <- varConstPower(form =~DML)
M.gls6 <- gls(Testisweight ~DML * fMONTH, weights = vf6, data = Squid)
AIC(M.gls6)
vf7 <- varConstPower(form =~ DML | fMONTH)
M.gls7 <- gls(Testisweight ~DML * fMONTH, weights = vf7,data = Squid)
AIC(M.gls7)

# varComb
vf8 <- varComb(varIdent(form =~ 1 | fMONTH), varExp(form =~ DML))
M.gls8 <- gls(Testisweight ~ DML * fMONTH, weights = vf8, data = Squid)

# Compare all models
AIC(M.lm, M.gls1, M.gls2, M.gls3, M.gls4,M.gls5, M.gls6, M.gls7, M.gls8)

# Graphical validation
plot(M.lm, which = c(1), main ="linear model")
plot(M.gls4, which = c(1), main ="M.gls4")

E1 <- resid(M.gls4)
coplot(E1 ~ DML | fMONTH, ylab = "Ordinary residuals", data = Squid)
E2 <- resid(M.gls4, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")
