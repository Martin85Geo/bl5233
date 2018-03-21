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

# Continue example at home...


