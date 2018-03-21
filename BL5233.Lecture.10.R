# Spatial correlation

# Prepare data
setwd("/Users/dondealban/Desktop/BL5233/Datasets/")
Boreality <- read.table(file="Boreality.txt", header=TRUE, sep="\t")
names(Boreality)

Boreality$Bor<-sqrt(1000*(Boreality$nBor+1)/(Boreality$nTot))

# Fit regression model
B.lm<-lm(Bor~Wet,data=Boreality)
summary(B.lm)
mpar(c(2,2))
plot(B.lm)

library(gstat)
library(sp)

E <- rstandard(B.lm)
residAndCoord <- data.frame(E,Boreality$x,Boreality$y)

coordinates(residAndCoord) <- c("Boreality.x","Boreality.y")
bubble(residAndCoord,"E", col= c("black","grey"), main="Residuals", xlab="X-coordinates", ylab="Y-coordinates")

# Variogram
library(nlme)
model<-gls(Bor~Wet, data=Boreality)
plot(Variogram(model, form=~Boreality$x + Boreality$y))

f1 <- formula(Bor ~ Wet)
B1.gls <- gls(f1, data=Boreality)
var1 <- Variogram(B1.gls, form=~x+y, maxDist=2000, resType="pearson")
plot(var1,smooth=T)

# Adding spatial correlation structure
B1A <- gls(f1, correlation=corSpher(form=~x+y,nugget=T), data=Boreality)
control1 = glsControl(maxIter = 10, msMaxIter = 10, tolerance = 1e-5)
B1B <- gls(f1, correlation=corLin(form=~x+y,nugget=T), data=Boreality, control = control1)
B1C <- gls(f1, correlation=corRatio(form=~x+y,nugget=T), data=Boreality)
B1D <- gls(f1, correlation=corGaus(form=~x+y,nugget=T), data=Boreality)
B1E <- gls(f1, correlation=corExp(form=~x+y,nugget=T), data=Boreality)
AIC(B1.gls,B1A,B1C,B1D,B1E)

# Note: The model without considering spatial correlation has the highest AIC. We get a 
# big improvement with spatial correlation. This is the definitive answer to our question:
# we need to correct for spatial correlation. B1C and B1E (corRatio and corExp) present 
# the lowest AICs and are the better candidates.

Vario1E <- Variogram(B1E, form =~ x + y, robust = TRUE, maxDist = 2000, resType = "pearson")
plot(Vario1E,smooth=FALSE)
                       
Vario2E <- Variogram(B1E,form =~ x + y, maxDist = 2000, resType = "normalized")
plot(Vario2E, smooth = FALSE)

# Example: acid-sensitive rivers
library(geoR)
SDI2003 <- read.table(file="SDI2003.txt", header=TRUE, sep="\t")
coords <- matrix(0, length(SDI2003$pH),2)

coords[,1] <- SDI2003$Easting
coords[,2] <- SDI2003$Northing
gb <- list(data=SDI2003$pH, coords=coords)
plot(variog(gb, max.dist=200000))

SDI2003$fForested <- factor(SDI2003$Forested)
SDI2003$LAltitude <- log(SDI2003$Altitude)
M1 <- gls(pH~SDI*fForested*LAltitude, data = SDI2003)
Vario1 <- Variogram(M1,form =~ Easting+Northing, data=SDI2003, nugget=T, maxDist=200000)
plot(Vario1)

M1A <- gls(pH ~ SDI*fForested*LAltitude, correlation=corSpher(form =~ Easting+Northing, nugget=TRUE), data=SDI2003)
M1B <- gls(pH ~ SDI*fForested*LAltitude, correlation=corLin(form =~ Easting+Northing, nugget=TRUE), data=SDI2003)
M1C <- gls(pH ~ SDI*fForested*LAltitude, correlation=corRatio(form =~ Easting+Northing, nugget=TRUE), data=SDI2003)
M1D <- gls(pH ~ SDI*fForested*LAltitude, correlation=corGaus(form =~ Easting+Northing, nugget=TRUE), data=SDI2003)
M1E <- gls(pH~SDI*Forested*LAltitude, correlation=corExp(form =~ Easting+Northing, nugget=TRUE), data=SDI2003)
AIC(M1,M1A,M1B,M1C,M1D,M1E)

Vario1C <- Variogram(M1C,form =~ Easting + Northing, data=SDI2003, nugget=T,maxDist=200000, resType="normalized")
plot(Vario1C, smooth=FALSE)

# Another example...
library(gstat)
E <- resid(M1C,type="normalized")
mydata <- data.frame(E,SDI2003$Easting, SDI2003$Northing)
coordinates(mydata)<- c("SDI2003.Easting","SDI2003.Northing")
bubble(mydata,"E",col=c("black","grey"), main="Normalised residuals", xlab="X-coordinates",ylab="Y-coordinates")

# Spatial autocorrelation using GLMs

require(MASS)
require(nlme)
data1 <- read.table("data.txt", head=T)
# define a grouping factor that assigns all observations to the same group
group <- factor(rep("a",nrow(data1)))
data1 <- cbind(data1, group)
# The data have to be attached AND specified in the formula.
attach(data1)
# Exponential correlation structure (the others also work)
model.e <- glmmPQL(y ~ x + z, random=~1|group, data=data1, correlation=corExp(form=~lat+long), family=binomial)


# Linear mixed-effects models I

# Random Intercept Model
library(nlme)
RIKZ <- read.table(file="RIKZ.txt", header=TRUE, sep="\t")
names(RIKZ)

RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random=~1 | fBeach, data=RIKZ)
summary(Mlme1)

F0 <- fitted(Mlme1,level=0)
F1 <- fitted(Mlme1,level=1)
I <- order(RIKZ$NAP)
NAPs <- sort(RIKZ$NAP)
plot(NAPs,F0[I], lwd=4, type="l",  ylim=c(0,22), ylab="Richness",xlab="NAP")
for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach==i]
  y1 <- F1[RIKZ$Beach==i]
  K <- order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZ$NAP,RIKZ$Richness,RIKZ$Beach,cex=0.9)

# Random Intercept and Slope Model
Mlme2 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

F0 <- fitted(Mlme2,level=0)
F1 <- fitted(Mlme2,level=1)
I <- order(RIKZ$NAP)
NAPs <- sort(RIKZ$NAP)
plot(NAPs,F0[I], lwd=4, type="l",  ylim=c(0,22), ylab="Richness",xlab="NAP")
for (i in 1:9){
  x1 <- RIKZ$NAP[RIKZ$Beach==i]
  y1 <- F1[RIKZ$Beach==i]
  K <- order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZ$NAP,RIKZ$Richness,RIKZ$Beach,cex=0.9)

# Random Effects Model
Mlme3 <- lme(Richness ~ 1, random = ~1 | fBeach, data = RIKZ)
summary(Mlme3)

# Diagnostic plots
plot(Mlme2)
plot(Mlme2, resid(., scaled=TRUE) ~ fitted(.), abline = 0)

# Diagnostic plot to check for heterodeodasticity
plot(Mlme2, resid(., scaled=TRUE) ~ fitted(.) | fBeach, abline = 0)

# Diagnostics for residuals
plot(Mlme2, fBeach ~ resid(., scaled=TRUE))
plot(Mlme2, resid(., scaled=TRUE) ~ NAP | fBeach, abline = 0)

# Check normality
qqnorm(Mlme2, ~ resid(., type = "p") | fBeach, abline = c(0, 1))

# Check assumption of normality of the random effects
qqnorm(Mlme2, ~ranef(.))
plot(ranef(Mlme2))

#Compare with AIC
AIC(Mlme1, Mlme2, Mlme3)

# Plot standard errors
library(lme4)
Mlme2 <- lmer(Richness ~ NAP + (NAP | fBeach), data = RIKZ)
library(lattice) 
reAndSE <- ranef(Mlme2, condVar = TRUE) 
dotplot(reAndSE)

#R2 in mixed-effects model
library(MuMIn)
r.squaredGLMM(Mlme1)
# The random intercept model explains 61% of the variance and 27% is explained by the 
# fixed effects alone.

# Models with multiple non-nested random effects
library(lme4)
earnings <- read.table(file="earnings2.csv", header=TRUE, sep=",")
earnings$ageF = factor(earnings$age)
earnings$ethF = factor(earnings$eth)
M1 <- lmer (y ~ x.centered + (1 + x.centered | ethF) + (1 + x.centered | ageF) + (1 + x.centered | ethF:ageF), data=earnings)
M1


