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
