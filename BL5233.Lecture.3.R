# On bootstrapping

setwd("/Users/dondealban/Desktop/BL5233/Datasets/")

regdat <- read.table(file="regdat.txt", header=TRUE, sep="\t")

names(regdat)

modelReg <- lm(response ~ explanatory, data=regdat)

confint(modelReg)



b.boot<-numeric(10000)

for (i in 1:10000){
  indices <- sample(1:35,replace=T)
  xv<- regdat$explanatory[indices]
  yv<-regdat$response[indices]
  model<-lm(yv~xv)
  b.boot[i]<-coef(model)[2] }

hist(b.boot)
quantile(b.boot,c(0.025,0.975))


# Normality test

x <- rnorm(n=1000, mean = 0, sd = 1)
qqnorm(x)
shapiro.test(x)

y <- runif(n=1000, 0, 1)
qqnorm(y)
shapiro.test(y)


# Power analysis

mean.untreat <- 5;
variance <- 2;
changeToDetect <- 0.1;
effect <- 5*changeToDetect # effect size we want to detect
numSimul <- 1000;
sampleSize <- 10;

power <- rep(0,numSimul)

for (i in 1:numSimul) { 
  sample.untreat <- rnorm(sampleSize, mean = mean.untreat, sd = variance^0.5) 
  sample.treat <- rnorm(sampleSize, mean = mean.untreat+effect, sd = variance^0.5)
  res <- t.test(sample.untreat,sample.treat)
  pval <- res$p.value
  power[i] <- pval<=0.05
}

empPower = sum(power)/numSimul

# Maximum likelihood

install.packages("emdbook")
library(emdbook)
library(bbmle)
data(MyxoTiter_sum)
myxdat = subset(MyxoTiter_sum, grade == 1)

gammaNLL1 = function(shape, scale) {
  -sum(dgamma(myxdat$titer, shape = shape, scale = scale,log = TRUE))
}

scaleInit = var(myxdat$titer)/mean(myxdat$titer)
shapeInit = (mean(myxdat$titer))^2/var(myxdat$titer)

m3 = mle2(gammaNLL1, start = list(shape = shapeInit, scale = scaleInit))

cmle <- coef(m3)


plot(density(myxdat$titer),main="",xlab="Virus titer",
     ylab="Probability density",zero.line=FALSE,col="darkgray")

n <- nrow(myxdat)
points(myxdat$titer,runif(n,0,0.02))

curve(dgamma(x, shape=cmle["shape"], scale=cmle["scale"]), add=TRUE)

text(7.8,0.45,"titer density",col="darkgray",adj=0)

text(4.2,0.38,"Gamma")
